# Shape das UCS

library(tidyverse)
library(sf)
library(geobr)
sf_use_s2(FALSE)

# Idea: TI versus SEA/SPA/SUPA ----

# "unit conservation" shape 
PA_shp<-read_sf("DATA/BR_UC_CD2022/BR_UC_Publicacao_CD2022.shp") %>%
  mutate(
    across(.cols = c(1, 3:6, 8), as.factor),
    Bioma=if_else(is.na(Bioma), "Marinha", Bioma), 
    new_cat=as.factor(case_when(
      CATEGORIA=="Área de Proteção Ambiental" ~ "APA", 
      CATEGORIA=="Área de Relevante Interesse Ecológico" ~ "ARIE", 
      CATEGORIA%in%c("Estação Ecológica",
                     "Monumento Natural",                       
                     "Parque",                                  
                     "Refúgio de Vida Silvestre",               
                     "Reserva Biológica" ) ~ "PI", 
      CATEGORIA%in%c("Floresta",
                     "Reserva de Desenvolvimento Sustentável",  
                     "Reserva de Fauna",                        
                     "Reserva Extrativista") ~ "US",
      CATEGORIA=="Reserva Particular do Patrimônio Natural" ~ "RPPN", 
    ))) %>% 
  #st_crs()#4674
  glimpse

# exploring data
conferir_dados_ano<-PA_shp %>%
 #st_drop_geometry %>% summarise(n_distinct(COD_UC)) #2365
 #summarise(n_distinct(geometry)) #2365
 #DataExplorer::plot_missing() # Just Cnuc code had NA values
  st_drop_geometry %>% count(NOME_UC) %>% 
  filter(n>1) %>%  print(n = 15)
# just RPPN cachoeira and RPPN fazenda boa esperança featured NA in CNUC column

cnuc_vazio<-PA_shp %>% 
  filter(is.na(CD_CNUC)) %>%  glimpse #99
# when we search by these names o cnuc panel, we can't find any PA even though a extinct. So probably, iit is better remove it

# Gathering data from the UC year creation
cnuc_year_info<-read.csv("DATA/cnuc_2024_10.csv", sep=";") %>% 
  select(2, 4, 9) %>% 
  mutate(Nome.da.UC=str_to_lower(Nome.da.UC), 
         Código.UC=as.factor(Código.UC)) %>% 
  glimpse

PA_shp %>% 
  filter(!is.na(CD_CNUC)) %>% #-99
  mutate(nome_uc=str_to_lower(NOME_UC), 
         CD_CNUC=as.factor(CD_CNUC)) %>%
  left_join(cnuc_year_info %>% 
              filter(Ano.de.Criação<=2022), by=c(
    "CD_CNUC"="Código.UC"
  )) %>% 
  filter(is.na(Ano.de.Criação)) %>% 
  glimpse -> PA_to_add_year_mannually
# i manually search on cnuc panel and the PA here do no exist anymore in the same format that in here. 
# To be parsimoniously, we will remove it. 

PA_shape_already<-PA_shp %>% 
  filter(!is.na(CD_CNUC)) %>% #-99
  mutate(nome_uc=str_to_lower(NOME_UC), 
         CD_CNUC=as.factor(CD_CNUC)) %>%
  left_join(cnuc_year_info %>% 
              filter(Ano.de.Criação<=2022), by=c(
                "CD_CNUC"="Código.UC"
              )) %>% 
  filter(!is.na(Ano.de.Criação)) %>%
  #mutate(t_geom=sf::st_is_valid(geometry)) %>% filter(t_geom==FALSE) %>% glimpse#1
  sf::st_make_valid(geometry) %>% 
  #mutate(t_geom=sf::st_is_valid(geometry)) %>% filter(t_geom==FALSE) %>% glimpse#0
  glimpse #2,248

saveRDS(PA_shape_already, "Outputs/PA_clean_by_year.rds")

