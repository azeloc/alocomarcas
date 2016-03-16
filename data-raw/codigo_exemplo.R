data_varas <- readRDS('/home/fcorrea/Projects/alocacao-master/data-raw/dados_comarcas_empresas.rds')
muni_comarca <- readRDS('/home/fcorrea/Projects/alocacao-master/data-raw/d_comarcas.rds')
muni_metro_sp <- readRDS('/home/fcorrea/Projects/alocacao-master/data-raw/d_muni_metro_sp.rds')

c_metro_sp <- muni_comarca %>%
  dplyr::filter(municipio %in% muni_metro_sp$muni) %>%
  dplyr::distinct(comarca_foro_distrital_sede) %>%
  dplyr::select(comarca_foro_distrital_sede) %>%
  dplyr::first() %>%
  c('comarca','empresas')

data_varas_sp <- data_varas %>%
  dplyr::filter(gsub('coma_','',comarca) %in% c_metro_sp) %>%
  '['(,stringr::str_detect(names(data_varas), paste(c_metro_sp,collapse = '|')))

aloca(data_varas_sp,3)



