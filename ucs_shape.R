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


# Idea: presence-governance versus absent-governance ----
# from IBGE 
PA_shp<-read_sf("DATA/BR_UC_UF_Publicacao_CD2022/BR_UC_UF_Publicacao_CD2022.shp") %>% 
  mutate(
    across(.cols = c(1:3, 5, 7:10), as.factor),
    Bioma=if_else(is.na(Bioma), "Marinha", Bioma)) %>% 
  #st_crs()#4674
  glimpse

# from CNUC
cnuc_info<-read.csv("DATA/cnuc_2024_10.csv", sep=";") %>% 
  select(2, 4, 9, 14, 15) %>%
  mutate(
    across(.cols=c(1, 4:5), .fns=as.factor), 
    Nome.da.UC=str_to_lower(Nome.da.UC)) %>% 
  #summarise(n_distinct(Código.UC))#3119
  glimpse

# Merging both datasets
PA_shp %>% 
  mutate(nome_uc=str_to_lower(NOME_UC)) %>%
  left_join(cnuc_info, by=c(
    "CD_CNUC"="Código.UC",
    "nome_uc"="Nome.da.UC"
  )) %>%
  #understanding data
  #st_drop_geometry %>% summarise(n_distinct(COD_UC)) #2365
  #st_drop_geometry %>% summarise(n_distinct(CD_UC_UF)) #2429
  #summarise(n_distinct(geometry)) #2429
  #DataExplorer::plot_missing() # ~ 200 NA
  #separate_rows(Bioma, sep = ",\\s*") %>%# \\s* to remove spaces #add 88 rows
  mutate(new_cat=case_when(
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
  ),
  gov_level=case_when(
    Plano.de.Manejo=="Não" & Conselho.Gestor=="Não" ~ "nao",
    Plano.de.Manejo=="Sim" & Conselho.Gestor=="Não" ~ "so_plan",
    Plano.de.Manejo=="Não" & Conselho.Gestor=="Sim" ~ "so_comt",
    Plano.de.Manejo=="Sim" & Conselho.Gestor=="Sim" ~ "sim")) %>% 
  #summary() # It has just UCs created until 2022
  glimpse -> PA_step01

cat_shape<-read_biomes() %>%
  filter(name_biome=="Caatinga") %>%  glimpse

#PA_step01 %>% 
#  ggplot() +
#  geom_sf() +
#  geom_sf(data = cat_shape)

Cat_PA_sh<-st_filter(PA_step01, cat_shape) %>% # i use filter instead intersect because i wanna maintain UCs that has more than one biome.  
  #ggplot() + geom_sf()
  #dim() #237 rows
  #DataExplorer::plot_missing() #23/24 NA
  glimpse

# UCS that had NA values 
#Cat_PA_sh %>% 
#  filter(is.na(Ano.de.Criação)) %>% 
#  st_drop_geometry() %>% select(nome_uc) %>%  print(n = 25)

# how many PA we have to each set of UC and government status 
Cat_PA_sh %>% 
  filter(!is.na(Ano.de.Criação)) %>% 
  mutate(across(.cols=c(18, 19), .fns=as.factor)) %>% 
  #summary() 
  filter(!gov_level%in%c("so_comt", "so_plan")) %>%
  group_by(new_cat, gov_level) %>% 
  summarise(n_distinct(COD_UC))

# understand if they are different UC or the same UC in more than one UF

Cat_PA_sh %>% 
  filter(!is.na(Ano.de.Criação)) %>% 
  mutate(across(.cols=c(18, 19), .fns=as.factor)) %>% 
  select(-16:-17, -CATEGORIA:-Bioma) %>%
  filter(!gov_level%in%c("so_comt", "so_plan")) %>%
  group_by(COD_UC) %>% 
  st_drop_geometry() %>% 
  summarise(countuc=n_distinct(CD_UC_UF)) %>% 
  filter(countuc>1)

Cat_PA_sh %>% 
  filter(!is.na(Ano.de.Criação)) %>% 
  mutate(across(.cols=c(18, 19), .fns=as.factor)) %>% 
  select(-16:-17, -CATEGORIA:-Bioma) %>%
  filter(COD_UC=="51185") %>%  glimpse
  #ggplot() + geom_sf()
  
shp_intermediario<-Cat_PA_sh %>% 
  filter(!is.na(Ano.de.Criação)) %>% 
  mutate(across(.cols=c(18, 19), .fns=as.factor), 
         idadeUC=(Ano.de.Criação-2022)*-1) %>% 
  select(-16:-17, -CATEGORIA:-Bioma) %>%
  filter(!gov_level%in%c("so_comt", "so_plan")) %>%
  rename(Ano_criacao=Ano.de.Criação) %>% 
  glimpse

#shp_intermediario %>% 
# mutate(test=st_is_valid(geometry)) %>% 
# filter(test==FALSE) %>%  glimpse

#dir.create("Outputs", recursive = T)
saveRDS(shp_intermediario, "cat_intermed.rds")

  
