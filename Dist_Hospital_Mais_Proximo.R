# distancia para o equipamento de saude mais prox 
# 08.04.20 
# João Lucas 


rm(list = ls()); gc()


require(geobr);require(dplyr);require(data.table);require(sf)
require(h3jsr);require(osrm);require(ggplot2);require(gridExtra); require(purrr)

# lendo dados de códigos do municípios

codes_municipios <- read.csv('data/RELATORIO_DTB_BRASIL_MUNICIPIO.csv',header = T,sep = ';')

# carregando estabelcimentos de saúde 2015

estabelecimentos_saude <- read_health_facilities(showProgress = T) %>% 
  st_transform(4326)

# grid 200  m 

# grid_total <- read_statistical_grid(code_grid = exe)

# lendo somente os municípios do Ceará 

ceara_municipios <- as.data.table(codes_municipios); ceara_municipios <- ceara_municipios[Nome_UF == 'Ceará',1:9]

# municipio exemplo <- 

exe <- (unlist(ceara_municipios[Nome_Município == 'Fortaleza','Código.Município.Completo']))

# codigos 

codigos <- ceara_municipios$Código.Município.Completo

dist_saude <- function(code){
  
  # lendo municipio 
  
  mun <- read_municipality(code_muni = code,year = 2010) %>% st_transform(4326)
  
  # mapview::mapview(mun)
  
  # cria hexagonos 
  
  mun2 <- mun %>%  
  st_transform(crs=4326) %>% # projecao
    # Buffer para extender a area do municipio e assim evitar que os hexagonos nao considerem areas de borda
    st_buffer(dist = 0.003)
  
  hex_ids <- h3jsr::polyfill(mun2, res = 7, simple = FALSE)
  
  hex_grid <- unlist(hex_ids$h3_polyfillers) %>% 
    h3jsr::h3_to_polygon(simple = FALSE) %>%
    rename(id_hex = h3_address) %>%
    as_tibble() %>% 
    mutate(sigla_muni = exe) %>%
    st_sf()
  
  
  # bairros <- st_read('data/Bairros_Fortal_Cod_IBGE_v1.shp') %>% st_transform(4326) %>% filter(MUNICPIO == 'FORTALEZA')
  
  # mapview::mapview(bairros)
  # mapview::mapview(hex_grid)
  
  # teste <- st_read(dsn = 'data/Equipamentos_Saude.shp')

  # estabelecimentos de saude dentro do municipio 
    
  health_in_area <- st_join(estabelecimentos_saude,mun) %>% na.omit()
  # health_in_area <- st_join(teste,bairros) %>% na.omit()
  
  # cauculando distância de cada hex pro estabelecimento de saude mais prox 
  
  centroides_hex <- hex_grid %>% st_centroid()
  # centroides_hex <- bairros %>% st_centroid() 
  
  x <- data.frame()
  
  for (i in 1:length(centroides_hex$id_hex)){

    dist <- as.vector(st_distance(centroides_hex[i,],health_in_area))

    x1 <- data.frame(orig = centroides_hex$id_hex[i], hospital_mais_prox = health_in_area$code_cnes[which(dist %in% min(dist))], dist = min(dist))

    # x1 <- data.frame(orig = centroides_hex$id_hex[i], hospital_mais_prox = health_in_area$NOME[which(dist %in% min(dist))], dist = min(dist))

    x <- rbind(x,x1)


  }
  
  # i <- 1
  # 
  # for (i in 1:length(bairros$ID)){
  #   
  #   dist <- as.vector(st_distance(bairros[i,],health_in_area))
  #   
  #   # x1 <- data.frame(orig = centroides_hex$id_hex[i], hospital_mais_prox = health_in_area$code_cnes[which(dist %in% min(dist))], dist = min(dist))
  #   
  #   x1 <- data.frame(orig = bairros$BAIRRO_MUN[i], hospital_mais_prox = health_in_area$NOME[which(dist %in% min(dist))], dist = min(dist))
  #   
  #   x <- rbind(x,x1) 
  #   
  #   
  # }
  
  
  res <- x %>% group_by(orig) %>% summarise(dist_min = min(dist))  
  
  tot <- hex_grid %>% left_join(res, by = c('id_hex'='orig'))
  # tot <- bairros %>% left_join(res, by = c('BAIRRO_MUN'='orig'))
  
  # tmap::qtm(shp = tot,fill = 'dist_min')
  
  map <- ggplot() + geom_sf(data = tot, aes(fill = dist_min)) + scale_fill_viridis_c() +
    geom_sf(data = health_in_area)
  
  ggsave(plot = map,filename = paste0('outputs/',mun$name_muni,'.png'))
  
  # 
  # map_pop <- ggplot() + geom_sf(data = bairros, aes(fill = HB_IBG_10)) + scale_fill_viridis_c() +
  #   geom_sf(data = health_in_area)
  # 
  # 
  # map_total <- grid.arrange(map,map_pop, ncol = 2)
  # 
  # fortal_pop <- st_read(dsn = 'data/fortal_ipea.shp')
  
  
}

code.teste <- codigos[1:10]

walk(.x = codigos[5:10],.f = dist_saude)

