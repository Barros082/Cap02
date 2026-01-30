library(tidyverse)
library(sf)
library(geobr)
library(here)
library(readxl)

# unzinping and tidying QUI shape ----
#dir.create("DATA/QUI_try2/QUI_all")

#arq_zip<-list.files("DATA/QUI_try2/zipfiles", 
#           pattern = "\\.zip$", 
#           full.names = TRUE)

#for (arq in arq_zip) {
#  unzip(arq, 
#        exdir = here("DATA/QUI_try2/QUI_all"))
#}

arq_shp<-list.files("DATA/QUI_try2/QUI_all", 
           pattern = "\\.shp$", 
           full.names = TRUE)

qui_list <- map(arq_shp, ~ {
  sf::read_sf(.x) %>% 
    st_transform(4674)
})

qui_shp <- do.call(rbind, qui_list)
#qui_shp %>% ggplot() +geom_sf()

# cleanning ----
Br_qui<-qui_shp %>% #dim() #445
  filter(esfera=="FEDERAL") %>%  
  #mutate(fase=as.factor(fase)) %>%  summary()
  filter(fase %in% c("DECRETO", 
                     "CCDRU", 
                     "TITULADO",
                     "TITULO PARCIAL")) %>% 
  mutate(nm_comunid=str_to_lower(nm_comunid)) %>% 
  glimpse

# we need understand this data - INCRA without metadados
#https://www.gov.br/incra/pt-br/assuntos/governanca-fundiaria/perguntas_respostas.pdf
# FASE 
#RTID>portaria>decreto>CCDRU>titulo(parcial)>titulo(total)
# portaria é o reconhecimenoto pelo estado das QUI
# decreto é a oficialização, mas precisa desapropriar
# CCDRU é quando dá o direito de uso da terra
# titulo parcial - quando há conflito
# titulo total - qui são realmente titulares

# mannual csv to match the names with IBGE data
#manual_metadata<-Br_qui %>% 
#  st_drop_geometry() %>% 
#  select(nm_comunid)
#write.csv(manual_metadata, 
#          "DATA/QUI_try2/names_check.csv")

#Without duplicates
#Br_qui %>% 
#  st_drop_geometry() %>% 
#  count(nm_comunid, name = "n") %>% 
#  filter(n > 1)


# IBGE 2022 data ----

pop_qui<-read_xlsx("DATA/garbage/pop_QUI2022.xlsx")[-1:-5,c(-1, -4)] %>% 
  drop_na() %>% 
  rename(fakeid=`...2`, 
         name_qui=`...3`,
         pop_qui=`...5`) %>% 
  mutate(name_qui=str_to_lower(name_qui)) %>% 
  glimpse #495

water_qui<-read_xlsx("DATA/QUI_try2/QUI_all/water_QUI.xlsx")[-1:-4,c(-3:-5)] %>% 
  drop_na() %>% 
  rename(fakeid=contains("Tabela 10100"), 
         name_qui=`...2`,
         water_qui=`...6`) %>% 
  mutate(name_qui=str_to_lower(name_qui)) %>% 
  glimpse #495

sani_qui<-read_xlsx("DATA/QUI_try2/QUI_all/sanitation_QUI.xlsx")[-1:-4,c(-3:-5)] %>% 
  drop_na() %>% 
  rename(fakeid=contains("Tabela 10100"), 
         name_qui=`...2`,
         sanit_qui=`...6`, 
         withoutsanit_qui=`...7`) %>% 
  select(-withoutsanit_qui) %>% 
  mutate(name_qui=str_to_lower(name_qui)) %>% 
  glimpse #495
  
health_qui<-read_xlsx("DATA/QUI_try2/QUI_all/inf_mort_QUI.xlsx")[-1:-6,-3] %>% 
  drop_na() %>% 
  rename(fakeid=contains("Tabela 10114"), 
         name_qui=`...2`,
         dead4_qui=`...4`) %>%
  mutate(name_qui=str_to_lower(name_qui)) %>% 
  glimpse #495
  
socioeco_QUI<-full_join(pop_qui, water_qui, 
          by=c("fakeid", "name_qui")) %>% 
  full_join(sani_qui, 
            by=c("fakeid", "name_qui")) %>% 
  full_join(health_qui, 
            by=c("fakeid", "name_qui")) %>%
  mutate(
    across(.cols = c(3:6), 
           .fns = ~ if_else(.x == "-", 
                            "0", .x))) %>%
  rowwise() %>%
  mutate(
    X_count = sum(c_across(3:6) == "X")) %>%
  ungroup() %>%
  filter(X_count==0) %>% #dim()#-20
  select(-X_count) %>% 
  mutate(across(.cols = c(3:6), 
                .fns = as.numeric)) %>% 
  glimpse

#duplicated?Yes. I nees remove it. 
socioeco_QUI_end<-socioeco_QUI %>% #475
  count(name_qui, name = "n") %>% #16 duplicated
  filter(n == 1) %>%  #475-16=459 
  left_join(socioeco_QUI, 
            by="name_qui") %>% 
  select(-n, -fakeid) %>% 
  glimpse #477


# Merging IBGE and QUI shape ----

dum_namesQUI<-read_xlsx("DATA/QUI_try2/QUI_all/names_check.xlsx") %>% 
  filter(new_code!="QUI_exclude") %>% 
  glimpse

done_QUI<-socioeco_QUI_end %>%
  right_join(dum_namesQUI, 
             by="name_qui") %>% 
  left_join(Br_qui, 
            by="nm_comunid") %>% 
  #select(new_code, fase, contains("dt_")) %>% View()
  mutate(new_code = str_replace(new_code,
                                "__", "_"), 
         across(starts_with("dt_"),
           ~ year(dmy(.x))), 
         year_ref=case_when(
           fase%in%c("TITULADO", 
                     "TITULO PARCIAL") &
             is.na(dt_titulac) &
             is.na(dt_decreto) ~ dt_public1,
           fase%in%c("TITULADO", 
                     "TITULO PARCIAL") &
             is.na(dt_titulac) ~ dt_decreto,
           fase%in%c("TITULADO", 
                     "TITULO PARCIAL") ~ dt_titulac,
           fase%in%c("CCDRU", 
                     "DECRETO") &
             is.na(dt_decreto)~ dt_public1,
           fase%in%c("CCDRU", 
                     "DECRETO") ~ dt_decreto
         ), 
         new_cat=rep("QUI", 101)) %>%
  #select(new_code, fase, contains("dt_"), year_ref) %>% View()
  #group_by(cd_uf) %>% summarise(n_distinct(new_code)) %>%  print(n=30)
  select(new_code, nm_comunid, new_cat,
         year_ref, ends_with("_qui"), 
         -name_qui, geometry) %>% 
  rename(
    PA_name=nm_comunid,
    Pop=pop_qui,  
    water=water_qui,  
    sanitation=sanit_qui,  
    dead_less4year=dead4_qui, 
    geom=geometry
  ) %>% 
  glimpse

# saving ;) ----
saveRDS(done_QUI, 
        "Outputs/QUI_socio_data.gpkg")

