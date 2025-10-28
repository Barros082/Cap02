# Socioeconomic outcomes

library(tidyverse)
library(readxl)

# Manual download through SIDRA platform. 

# UCS ----
#Population - cofactor
popUC<-read_xlsx("DATA/pop_UC2022.xlsx")[-1:-5, c(-1, -4:-5)] %>%
  rename(COD_UC=`...2`, 
         Name_UF=`...3`,
         Pop=`...6` ) %>% 
  filter(!is.na(COD_UC)) %>% # lose one row
  mutate(
    Pop=if_else(Pop=="-", "0", Pop), 
    COD_UC = str_pad(COD_UC, width = 5, 
                            side = "left",
                            pad = "0"), 
    COD_UC=as.factor(COD_UC), 
    Pop=as.numeric(Pop)) %>%
  glimpse

# Literacy - number of people aged 15 or over that were literacy
litUC<-read_xlsx("DATA/lit_npeople_UC2022.xlsx")[-1:-6, c(-1, -4:-5)] %>%
  rename(COD_UC=`...2`, 
         Name_UF=`...3`, 
         lit_total=`...6`, 
         lit=`...7`, 
         nao_lit=`...8`) %>% 
  filter(!is.na(COD_UC)) %>%
  select(-3, -5) %>% 
  mutate(
    lit=if_else(lit=="-", "0", lit), 
    COD_UC = str_pad(COD_UC, width = 5, 
                     side = "left",
                     pad = "0"), 
    COD_UC=as.factor(COD_UC)) %>%
  glimpse
# "X" - it is sensible data. Probably, we need exclude this PA. 

# Access - water
waterUC<-read_xlsx("DATA/water_UC2022.xlsx")[-1:-4, c(-1, -4)] %>% 
  rename(COD_UC=`...2`, 
         Name_UF=`...3`, 
         water=`...5`) %>% 
  filter(!is.na(COD_UC)) %>%
  mutate(
    water=if_else(water=="-", "0", water), 
    COD_UC = str_pad(COD_UC, width = 5, 
                     side = "left",
                     pad = "0"), 
    COD_UC=as.factor(COD_UC), 
    water=as.numeric(water)) %>%
  glimpse

# Access - sanitation or bath
sanitUC<-read_xlsx("DATA/sanitation_UC2022.xlsx")[-1:-4, c(-1, -4)] %>%
  rename(COD_UC=`...2`, 
         Name_UF=`...3`, 
         sanitation=`...5`) %>% 
  filter(!is.na(COD_UC)) %>%
  mutate(
    sanitation=if_else(sanitation=="-", "0", sanitation),
    COD_UC = str_pad(COD_UC, width = 5, 
                     side = "left",
                     pad = "0"), 
    COD_UC=as.factor(COD_UC)) %>%
  glimpse

# Access - waste collection
wasteUC<-read_xlsx("DATA/waste_UC2022.xlsx")[-1:-4, c(-1, -4)] %>%
  rename(COD_UC=`...2`, 
         Name_UF=`...3`, 
         waste=`...5`) %>% 
  filter(!is.na(COD_UC)) %>%
  mutate(
    waste=if_else(waste=="-", "0", waste),
    COD_UC = str_pad(COD_UC, width = 5, 
                     side = "left",
                     pad = "0"),
    COD_UC=as.factor(COD_UC)) %>%
  glimpse

# Health - infantile mortality
healthUC<-read_xlsx("DATA/inf_mort_UC2022.xlsx") [-1:-6, -1] %>%
  rename(COD_UC=`...2`, 
         Name_UF=`...3`, 
         dead_less1year=`...4`) %>% 
  filter(!is.na(COD_UC)) %>%
  mutate(
    dead_less1year=if_else(dead_less1year=="-", "0", dead_less1year),
    COD_UC = str_pad(COD_UC, width = 5, 
                     side = "left",
                     pad = "0"),
    COD_UC=as.factor(COD_UC)) %>%
  glimpse

## Merging socioeconomic outcomes
socioeco_UC_join<-popUC %>% 
  left_join(litUC) %>%
  left_join(waterUC) %>%  
  left_join(sanitUC) %>%
  left_join(wasteUC) %>% 
  left_join(healthUC) %>%  glimpse

## Merging with IBGE UC data
UC_socio<-readRDS("Outputs/PA_clean_by_year.rds") %>% 
  st_as_sf() %>%
  left_join(socioeco_UC_join, by=c(
    "COD_UC"="COD_UC"
  )) %>% #dim()#2248
  #DataExplorer::plot_missing() # any NA value
  filter(!Pop==0) %>% #dim()#2248-1166=1082 
  select(1:15, water, lit, sanitation:dead_less1year) %>% 
  rowwise() %>%
  mutate(
    X_count = sum(c_across(lit:dead_less1year) == "X"),
    other_count = sum(c_across(lit:dead_less1year) == "..")) %>%
  ungroup() %>%
  filter(!X_count>0 & !other_count>0) %>% #dim() #-295=787
  #sf::st_drop_geometry() %>% 
  #select(COD_UC, Bioma, new_cat) %>% 
  #group_by(Bioma, new_cat) %>% 
  #summarise(n_distinct(COD_UC)) %>% 
  #print(n=100)
  select(-X_count, -other_count) %>%
  mutate(
    across(.cols=c(lit:dead_less1year), .fns=as.numeric)) %>% 
  glimpse

saveRDS(UC_socio, "Outputs/UC_socio_data.rds")

  