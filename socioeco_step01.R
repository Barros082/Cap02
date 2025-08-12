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
  
# Health - infantil mortality
heathUC<-read_xlsx("DATA/inf_mort_UC2022.xlsx") [-1:-6,-1] %>%
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


