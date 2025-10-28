# Excluir

#### from ucs_shape.R (28/10/2025) ----

#excluding the idea 2, about compare governance differences between each PA. 
# Idea: presence-governance versus absent-governance 
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




#### from socioeco_UC.R  (28/10/2025) ----

#excluding the idea 2, about compare governance differences between each PA. 

# old idea 
# if you wanna ran it, please fix the name of each xlsx add garbage/
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

# estimating the percent of PA that we had from the total of PA

PA_step01 %>% 
  st_drop_geometry() %>% 
  group_by(Bioma, new_cat) %>% 
  summarise(n_distinct(CD_UC_UF)) %>%
  print(n=100)

# Amazon - APA 36 / 11 ~ 30.55%
# Amazon - ARIE 6 / 0 
# Amazon - PI 110 / 27 ~ 24.54%
# Amazon - RPPN 16 / 0
# Amazon - US 166 / 58 ~ 34.93%

# Caatinga - APA 43 / 18 ~ 41.86%
# Caatinga - ARIE 8 / 0
# Caatinga - PI 64 / 17 ~ 26.56%
# Caatinga - RPPN 74 / 0
# Caatinga - US 8 / 2 ~ 25 % 

# Cerrado - APA 101 / 33 ~ 32.67%
# Cerrado - ARIE 20 / 3
# Cerrado - PI 141 / 16 ~ 11.34%
# Cerrado - RPPN 85 / 0
# Cerrado - US 15 / 0

# Mata Atlântica - APA 247 / 87 ~ 35.22%
# Mata Atlântica - ARIE 48 / 5
# Mata Atlântica -  PI 504 / 96 ~ 19.04%
# Mata Atlântica - RPPN 530 / 0 
# Mata Atlântica - US 57 / 9 ~ 15.78%

# estimating the percent of PA that we had from PA with population
socioeco_join%>%
  left_join(PA_step01, by=c(
    "COD_UC_UF"="CD_UC_UF"
  )) %>% 
  filter(!Pop==0) %>% 
  group_by(Bioma, new_cat) %>% 
  summarise(n_distinct(COD_UC_UF)) %>%
  print(n=100)

# Amazon - APA 33 / 11 ~ 33.33%
# Amazon - ARIE 3 / 0 
# Amazon - PI 53 / 27 ~ 50.94% 
# Amazon - RPPN 3 / 0
# Amazon - US 139 / 58 ~ 41.72%

# Caatinga - APA 43 / 18 ~ 41.86 %
# Caatinga - ARIE 6 / 0 
# Caatinga - PI 37 / 17 ~ 45.94%
# Caatinga - RPPN 6 / 0
# Caatinga - US 6 / 2 ~ 33.33%

# Cerrado - APA  85 / 33 ~ 38.82%
# Cerrado - ARIE  10 / 3
# Cerrado - PI  71 / 16 ~ 22.53%
# Cerrado - RPPN  2 / 0
# Cerrado - US  11 / 0

# Mata Atlântica - APA  227 / 87 ~ 38.32%
# Mata Atlântica - ARIE  30 / 5
# Mata Atlântica -  PI  258 / 96 ~ 37.20%
# Mata Atlântica - RPPN  28 / 0 
# Mata Atlântica - US  40 / 9 ~ 22.50%




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