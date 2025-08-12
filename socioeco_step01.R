# Socioeconomic outcomes

library(tidyverse)
library(readxl)

# Manual download trhough SIDRA plataform. 

# Population - cofactor
popUC<-read_xlsx("DATA/pop_UC2022.xlsx")[-1:-5, c(-1, -4:-5)] %>%
  rename(COD_UC_UF=`...2`, 
         Name_UF=`...3`,
         Pop=`...6` ) %>%
  filter(!is.na(COD_UC_UF)) %>% # lose one row
  mutate(
    Pop=if_else(Pop=="-", "0", Pop), 
    COD_UC_UF=as.factor(COD_UC_UF), 
    Pop=as.numeric(Pop)) %>%
  glimpse

# Residence - cofactor
residenceUC<-read_xlsx("DATA/house_UC2022.xlsx") [-1:-6, c(-1, -4:-5)]%>% 
  rename(COD_UC_UF=`...2`, 
         Name_UF=`...3`,
         resid=`...6` ) %>%
  filter(!is.na(COD_UC_UF)) %>% 
  mutate(
    resid=if_else(resid=="-", "0", resid), 
    COD_UC_UF=as.factor(COD_UC_UF))%>%
  glimpse


# Literacy - number of people aged 15 or over that were literacy
litUC<-read_xlsx("DATA/lit_npeople_UC2022.xlsx")[-1:-6, c(-1, -4:-5)] %>%
  rename(COD_UC_UF=`...2`, 
         Name_UF=`...3`, 
         lit_total=`...6`, 
         lit=`...7`, 
         nao_lit=`...8`) %>% 
  filter(!is.na(COD_UC_UF)) %>%
  select(-3, -5) %>% 
  mutate(
    lit=if_else(lit=="-", "0", lit), 
    COD_UC_UF=as.factor(COD_UC_UF)) %>%
  glimpse
# "X" - it is sensible data. Probably, we need exclude this PA. 
# if we discovery how many people one PA need to have to classified as sensible, we can say that we exclude due to low number of people

# Access - water
waterUC<-read_xlsx("DATA/water_UC2022.xlsx")[-1:-5, c(-1, -4:-5)] %>%
  rename(COD_UC_UF=`...2`, 
         Name_UF=`...3`, 
         water_type1=`...6`, 
         water_type2=`...7`) %>% 
  filter(!is.na(COD_UC_UF)) %>%
  mutate(
    water_type1=if_else(water_type1=="-", "0", water_type1), 
    water_type2=if_else(water_type2=="-", "0", water_type2),
    COD_UC_UF=as.factor(COD_UC_UF)) %>%
  glimpse

# Access - sanitation or bath
sanitUC<-read_xlsx("DATA/sanitation_UC2022.xlsx")[-1:-6, c(-1, -4)] %>%
  rename(COD_UC_UF=`...2`, 
         Name_UF=`...3`, 
         sanit_type1=`...5`, 
         sanit_type2=`...6`) %>% 
  filter(!is.na(COD_UC_UF)) %>%
  mutate(
    sanit_type1=if_else(sanit_type1=="-", "0", sanit_type1), 
    sanit_type2=if_else(sanit_type2=="-", "0", sanit_type2),
    COD_UC_UF=as.factor(COD_UC_UF)) %>%
  glimpse

# Access - waste collection
wasteUC<-read_xlsx("DATA/waste_UC2022.xlsx")[-1:-6, c(-1, -4)] %>%
  rename(COD_UC_UF=`...2`, 
         Name_UF=`...3`, 
         waste_type1=`...5`, 
         waste_type2=`...6`) %>% 
  filter(!is.na(COD_UC_UF)) %>%
  mutate(
    waste_type1=if_else(waste_type1=="-", "0", waste_type1), 
    waste_type2=if_else(waste_type2=="-", "0", waste_type2),
    COD_UC_UF=as.factor(COD_UC_UF)) %>%
  glimpse
  
# Health - infantile mortality
healthUC<-read_xlsx("DATA/inf_mort_UC2022.xlsx") [-1:-6,-1] %>%
  rename(COD_UC_UF=`...2`, 
         Name_UF=`...3`, 
         less1year=`...4`, 
         btw1_4years=`...5`) %>% 
  filter(!is.na(COD_UC_UF)) %>%
  mutate(
    less1year=if_else(less1year=="-", "0", less1year), 
    btw1_4years=if_else(btw1_4years=="-", "0", btw1_4years),
    COD_UC_UF=as.factor(COD_UC_UF)) %>%
  glimpse

## Merging socioeconomic outcomes
socioeco_join<-popUC %>% 
  left_join(residenceUC) %>%  
  left_join(litUC) %>%
  left_join(waterUC) %>%  
  left_join(sanitUC) %>%
  left_join(wasteUC) %>% 
  left_join(healthUC) %>%  glimpse

## Merging with IBGE UC data
socioeco_join%>%
  left_join(PA_step01, by=c(
    "COD_UC_UF"="CD_UC_UF"
  )) %>% 
  #DataExplorer::plot_missing() # just NA about CNUC data
  # 2,429 rows
  select(-COD_UF, -NOME_UC_UF, -CD_CNUC:-QUALI_POL, 
         -nome_uc:-Conselho.Gestor) %>% 
  filter(!is.na(gov_level)) %>%  #dim()#-453=1976 # CNUC data lackage
  filter(!gov_level%in%c("so_comt", "so_plan")) %>% #dim() #-469=1507 # out treatment
  filter(!Pop==0) %>% #dim() #-893=614
  rowwise() %>%
  mutate(
    X_count = sum(c_across(resid:btw1_4years) == "X")) %>%
  ungroup() %>%
  filter(!X_count>0) %>% #dim() #-192=422
  sf::st_drop_geometry() %>% 
  select(1, Bioma, new_cat, gov_level) %>% 
  group_by(Bioma, new_cat, gov_level) %>% 
  summarise(n_distinct(COD_UC_UF)) %>% 
  print(n=100)
  
# we need to think again if vale a pena continuar
  



















# understanding sensible data (i.e "X") and UC with non-PA

socioeco_data<-socioeco_join%>% 
  rowwise() %>%
  mutate(
    X_count = sum(c_across(resid:btw1_4years) == "X"),
    X_type = case_when(
      X_count == ncol(across(resid:btw1_4years)) ~ "X_all",
      X_count == 1 ~ "X_unique",
      X_count > 1 ~ "X_multi",
      X_count == 0 ~ "non_X"), 
    X_type=as.factor(X_type)) %>%
  ungroup() %>%
  filter(!X_count>0) %>%  # we lose 350, where 280 were all X and 70 were multi X ( the minimum was 3 columns with X and they were inf_mort and lit). 
  #summary() 
  select(-contains("X_")) %>% 
  mutate(across(resid:btw1_4years, .fns=as.numeric)) %>% 
  glimpse

UC_whithout_pop<-socioeco_data %>% 
  filter(Pop==0) %>% glimpse #1,245 (remain just 834 UC)


#
socioeco_data %>%
  left_join(PA_step01, by=c(
    "COD_UC_UF"="CD_UC_UF"
  )) %>% 
  DataExplorer::plot_missing()

socioeco_join%>%
  left_join(PA_step01, by=c(
    "COD_UC_UF"="CD_UC_UF"
  )) %>% 
  DataExplorer::plot_missing()

# garbage
PA_ok_to_use<-PA_step01 %>% 
  filter(!is.na(Ano.de.Criação)) %>%  
  filter(!gov_level%in%c("so_comt", "so_plan")) %>%  
  select(CD_UC_UF, NOME_UC_UF, Bioma, new_cat, 
         gov_level) %>% 
  glimpse

socioeco_data %>% 
  filter(Pop!=0) %>% 
  left_join(PA_step01, by=c(
    "COD_UC_UF"="CD_UC_UF"
  )) %>%  View()
  