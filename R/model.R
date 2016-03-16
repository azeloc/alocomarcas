#' @export
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
    mat[i,n^2+i-n] = -n
  }


  mat[2*n+1,(n^2+1):(n^2+n)] = rep(1, times = n)
  return(mat)
}

#' @export
i_directions <- function(n) {c(rep('==', times = n),rep("<=",times = n),'==')}

#' @export
i_rhs <- function(n, numvaras){rhs <- c(rep(1,n),rep(0,n),numvaras)}

#' @export
dist_comarcas <- function(d){
  d %>%
  dplyr::filter(stringr::str_detect(comarca,'coma_')) %>%
  dplyr::select(starts_with('coma_')) %>%
  as.matrix %>%
  igraph::graph.adjacency(mode="undirected") %>%
  igraph::distances() %>%
  as.vector()
}

#' @export

perc_processos <- function(x){
  round(unlist(d[,2]/sum(d[,2])), 4) %>%
  sapply(rep, times = nrow(d))
}

#' @export
objective <- function(d){

  d_comarcas <- dist_comarcas(d)

  p_processos <- perc_processos(d)

  obj = c(p_processos*d_comarcas, rep(0, nrow(d)))
  return(obj)
}

#' @export
aloca <- function(d,numvaras){

  n_comarcas <- nrow(d)
  comarcas <- stringr::str_replace(d$comarca,'coma_','')

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
    reshape2::melt() %>%
    dplyr::filter(value > 0) %>%
    dplyr::transmute(comarca = comarcas[Var1],
                     vara = comarcas[Var2]) %>%
    dplyr::as_data_frame()

  list('alocacao' = solution, 'obj' = res)
}
