matriz <- function(n){
  mat = matrix(0, nrow = 2*n+1, ncol = n^2+n)

  for(i in 1:n){
    for(j in 1:n){
      mat[i,n*(i-1) + j] = 1
    }
  }

  for(i in (n+1):(2*n)){
    for(j in (n+1):(2*n)){
      mat[i,n*(j-n-1)+i-n] = 1
    }
    mat[i,n^2+i-n] = -315
  }


  mat[2*n+1,(n^2+1):(n^2+n)] = rep(1, times = n)
  return(mat)
}

i_directions <- function(n) {c(rep('==', times = n),rep("<=",times = n),'==')}

i_rhs <- function(n, numvaras){rhs <- c(rep(1,n),rep(0,n),numvaras)}

objective <- function(d){
  dist_comarcas <- d[1:nrow(d), 3:(nrow(d)+2)] %>%
    as.matrix %>%
    igraph::graph.adjacency(mode="undirected") %>%
    igraph::distances %>%
    as.vector

  perc_processos <- round(unlist(d[,2]/sum(d[,2])), 4) %>%
    sapply(rep, times = n)

  obj = c(perc_processos*dist_comarcas, rep(0, nrow(d)))
  return(obj)
}

aloca <- function(d,numvaras){

  n_comarcas <- nrow(d)

  obj <- objective(d)
  mat = matriz(n_comarcas)
  dir <-i_directions(n_comarcas)
  rhs <- i_rhs(n_comarcas, numvaras)
  types <- rep('B', n_comarcas^2+n_comarcas)
  max <- F

  solucao <- Rglpk::Rglpk_solve_LP(obj = obj,mat = mat,dir = dir,rhs = rhs,types = types,max = max,verbosity = 0)
  res <- solucao$optimum
  solution <- solucao %>%
    '$'('solution') %>%
    matrix(nrow = n_comarcas, ncol = n_comarcas, byrow = T) %>%
    reshape2::melt %>%
    dplyr::filter(value > 0) %>%
    dplyr::transmute(comarca = c_metro_sp[Var1],
                     vara = c_metro_sp[Var2])

  list('alocacao' = solution, 'obj' = res)
}
