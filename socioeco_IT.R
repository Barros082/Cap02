# Socioeconomic outcomes

library(tidyverse)
library(readxl)

# Manual download through SIDRA platform. 

# IT ----

#Population - cofactor
popIT<-read_xlsx("DATA/pop_IT2022.xlsx")[-1:-5,c(-1, -4)] %>% 
  rename(COD_IT=`...2`, 
         Name_IT=`...3`,
         Pop=`...5` ) %>% 
  filter(!is.na(COD_IT)) %>% # lose one row
  mutate(
    Pop=if_else(Pop=="-", "0", Pop), 
    Pop=as.numeric(Pop),
    COD_IT=as.factor(COD_IT)) %>%
  glimpse

# Literacy - number of people aged 15 or over that were literacy
litIT<-read_xlsx("DATA/lit_npeople_IT2022.xlsx")[-1:-5,c(-1, -4:-5)] %>%
  rename(COD_IT=`...2`, 
         Name_IT=`...3`, 
         lit_total=`...6`, 
         lit=`...7`, 
         nao_lit=`...8`) %>% 
  filter(!is.na(COD_IT)) %>%
  select(-3, -5) %>% 
  mutate(
    lit=if_else(lit=="-", "0", lit),
    COD_IT=as.factor(COD_IT)) %>%
  glimpse


# Access - water
waterIT<-read_xlsx("DATA/water_IT2022.xlsx")[-1:-4, c(-1, -4:-7)] %>% 
  rename(COD_IT=`...2`, 
         Name_IT=`...3`, 
         water=`...8`) %>% 
  filter(!is.na(COD_IT)) %>%
  mutate(
    water=if_else(water=="-", "0", water),
    COD_IT=as.factor(COD_IT)) %>%
  glimpse


# Access - sanitation or bath
sanitIT<-read_xlsx("DATA/sanitation_IT2022.xlsx")[-1:-4, c(-1, -4:-7)] %>%
  rename(COD_IT=`...2`, 
         Name_IT=`...3`, 
         sanitation=`...8`) %>% 
  filter(!is.na(COD_IT)) %>%
  mutate(
    sanitation=if_else(sanitation=="-", "0", sanitation),
    COD_IT=as.factor(COD_IT)) %>%
  glimpse


# Access - waste collection
wasteIT<-read_xlsx("DATA/waste_IT2022.xlsx")[-1:-4, c(-1, -4:-7)] %>%
  rename(COD_IT=`...2`, 
         Name_IT=`...3`,
         waste=`...8`) %>% 
  filter(!is.na(COD_IT)) %>%
  mutate(
    waste=if_else(waste=="-", "0", waste),
    COD_IT=as.factor(COD_IT)) %>%
  glimpse


# Health - infantile mortality
healthIT<-read_xlsx("DATA/inf_mort_IT2022.xlsx") [-1:-6, c(-1, -5)] %>%
  rename(COD_IT=`...2`, 
         Name_IT=`...3`, 
         dead_less1year=`...4`) %>%
  filter(!is.na(COD_IT)) %>%
  mutate(
    dead_less1year=if_else(dead_less1year=="-", "0", dead_less1year),
    COD_IT=as.factor(COD_IT)) %>%
  glimpse

## Merging socioeconomic outcomes
socioeco_IT_join<-popIT %>% 
  left_join(litIT) %>%
  left_join(waterIT) %>%  
  left_join(sanitIT) %>%
  left_join(wasteIT) %>% 
  left_join(healthIT) %>%  glimpse

## Merging with FUNAI IT data
read_sf("Outputs/IT_shape.gpkg") %>% 
  left_join(socioeco_IT_join, by=c(
    "terrai_nom"="Name_IT"
  )) %>% #506
  #st_drop_geometry %>% count(terrai_nom) %>% filter(n > 1) # there is some duplicated names
  #DataExplorer::plot_missing() # Na in data is ok, but NA is outcomes is unaceptable. 
  filter(is.na(dead_less1year)) %>%  View() #13 NA

# see what is the issue
#socio -> Guarani de Bracuí | funai -> Guarani de Bracui
#socio -> NA | funai -> Tuxá de Ibotirama
#socio -> Kulina do Médio Juruá | funai -> Kulina do Medio Jurua
#socio -> Rio Paru D'Este  | funai -> Rio Paru DEste
#socio -> São Jerônimo  | funai -> São Jeronimo
#socio -> NA  | funai -> Trocará - Doação
#socio -> Tikuna de Santo Antônio  | funai -> Tikuna de Santo Antonio
#socio -> Waiwái  | funai -> WaiWái
#socio -> Zo'é | funai -> Zoe
#socio -> NA  | funai -> Aldeia Katurama
#socio -> NA  | funai -> Vera Tupã?i
#socio -> NA  | funai -> Aldeia Escola Floresta
#socio -> NA  | funai -> Tekoá Guavirá

# from 13 NA, 6 we can fix and need to exclude. 
IT_socio<-read_sf("Outputs/IT_shape.gpkg") %>% 
  mutate(
   terrai_nom = case_when(
     terrai_nom == "Guarani de Bracui" ~ "Guarani de Bracuí",
     terrai_nom == "Kulina do Medio Jurua" ~ "Kulina do Médio Juruá",
     terrai_nom == "Rio Paru DEste" ~ "Rio Paru D'Este",
     terrai_nom == "São Jeronimo" ~ "São Jerônimo",
     terrai_nom == "Tikuna de Santo Antonio" ~ "Tikuna de Santo Antônio",
     terrai_nom == "WaiWái" ~ "Waiwái",
     terrai_nom == "Zoe" ~ "Zo'é",
     TRUE ~ terrai_nom)) %>% 
  left_join(socioeco_IT_join, by=c(
    "terrai_nom"="Name_IT"
  )) %>% 
  filter(!is.na(dead_less1year)) %>% #500
  #DataExplorer::plot_missing()
  filter(!Pop==0) %>% #dim() #491 # there is 9 IT with 0 population. I don't know how, but we need to exclude it because we cannot estimate socio outcomes with 0 population
  rowwise() %>%
  mutate(
    X_count = sum(c_across(lit:dead_less1year) == "X"),
    other_count = sum(c_across(lit:dead_less1year) == "..")) %>%
  ungroup() %>%
  filter(!X_count>0 & !other_count>0) %>% #dim() #465
  select(-X_count, -other_count) %>% 
  mutate(
    across(.cols=c(lit:dead_less1year), .fns=as.numeric)) %>% 
  glimpse()

IT_socio %>%  st_crs()
write_sf(IT_socio, "Outputs/IT_socio_data.gpkg")
