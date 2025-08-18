# Understand UC Dataset ----

library(tidyverse)
library(sf)
library(readxl)
sf_use_s2(FALSE)

PA_shp<-read_sf("DATA/BR_UC_UF_Publicacao_CD2022/BR_UC_UF_Publicacao_CD2022.shp") %>% 
  mutate(
    across(.cols = c(1:3, 5, 7:10), as.factor),
    Bioma=if_else(is.na(Bioma), "Marinha", Bioma)) %>% 
  #st_crs()#4674
  glimpse

pop2022<-read_excel("DATA/Tabela_de_resultado_02.xlsx")[-1:-6,] %>% 
  rename(Cod_UF=`Censo Demográfico 2022`, 
         Abrev_UF=`...2`, UC_code=`...3`,
         UC_codeUF=`...4`, UC_nome_clean=`...5`,
         UC_nome_UF=`...6`, Cod_cnuc=`...7`,
         pop_total=`...8`, pop_ind=`...9`, 
         pop_quil=`...10`) %>% 
  mutate(
   across(.cols = c(8:10), ~ case_when(
      . == "-" ~ "0",
      . == "X" ~ "1",
      TRUE ~ .
    )),
    across(.cols = c(8:10), as.numeric),
    across(.cols = c(9:10), ~ case_when(
      .>=1 & .<=10 ~ "ate10",
      .>10 ~ "maisde10",
      .==0 ~ "sem_ind"
    ), 
    .names = "{.col}_cat"),
   across(.cols = c(1:4, 7, 11:12), as.factor))  %>% 
  filter(!is.na(pop_total)) %>% #removing endnote: ""Fonte: IBGE [;...]"
  glimpse

summary(PA_shp)
summary(pop2022)

# understand the UC codes to join 
dim(PA_shp)
PA_shp %>% 
  st_drop_geometry() %>% 
  summarise(
    n_distinct(COD_UC),
    n_distinct(CD_UC_UF),
    n_distinct(CD_CNUC),
    n_distinct(NOME_UC), 
    n_distinct(NOME_UC_UF)) %>% 
  glimpse
# just CD_UC_UF had the same rows amount


dim(pop2022)
pop2022 %>% 
  summarise(
    n_distinct(UC_code),
    n_distinct(UC_codeUF),
    n_distinct(Cod_cnuc),
    n_distinct(UC_nome_clean), 
    n_distinct(UC_nome_UF)) %>% 
  glimpse
# just UC_codeUF had the same rows amount

# join by code UC + UF

UC_pop<-left_join(PA_shp, pop2022, 
          by=c("CD_UC_UF"="UC_codeUF")) %>%
  #filter(is.na(pop_total)) %>%  glimpse # there is no NA on population data
  select(1:2, 5, 8:13, 16:24) %>%  glimpse

DataExplorer::plot_missing(UC_pop)

# preparing data to ggplot

data_plots<-UC_pop %>% 
  st_drop_geometry() %>% 
  separate_rows(Bioma, sep = ",\\s*") %>% # \\s* to remove spaces
  select(CD_UC_UF, Bioma, ESFERA, CATEGORIA, 13:15) %>% 
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
  )) %>% 
  group_by(Bioma, new_cat) %>% 
  summarise(
    pa_amount=n_distinct(CD_UC_UF),
    pop_t=sum(pop_total),
    pop_i=sum(pop_ind),
    pop_q=sum(pop_quil)) %>% #print(n=50)
  glimpse


# counting PA with pop and without pop

UC_pop %>% 
  st_drop_geometry() %>% 
  separate_rows(Bioma, sep = ",\\s*") %>% # \\s* to remove spaces
  select(CD_UC_UF, Bioma, ESFERA, CATEGORIA, 13:15) %>% 
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
  pop_cat=case_when(
    pop_total>=1 ~ "with_pop",
    pop_total==0 ~ "without_pop"
  )) %>% 
  group_by(Bioma, new_cat, pop_cat) %>%
  summarise(
    pa_amount=n_distinct(CD_UC_UF)) %>% 
  print(n=50)
#R. Amazon APA/RPPN -> hard compare
#R. Caatinga APA só tem with pop, RPPN não nda
#R. Cerrado RPPN -> hard compare
#R. Mata atlântiva -> we can compare all
#R. Pampa APA/RPPN -> só com pop e só sem pop (respectivamente)
#R. Pantanal APA só tem pop e RPPN hard compare

# add management data from (https://dados.gov.br/dados/conjuntos-dados/unidadesdeconservacao)

cnuc_info<-read.csv("DATA/cnuc_2024_10.csv", sep=";") %>% 
  select(2, 4, 9, 10, 14, 15) %>%
  mutate(
    across(.cols=c(1, 5:6), .fns=as.factor), 
    Nome.da.UC=str_to_lower(Nome.da.UC )) %>% 
  #summarise(n_distinct(Código.UC))#3119
  glimpse

# não pode juntar apenas por CD_CNUC pq tem NA.
PA_shp %>% st_drop_geometry() %>%
  summarise(n_distinct(CD_CNUC))#2267
# devemos usar o nome tbm

PA_shp %>% 
  mutate(nome_uc=str_to_lower(NOME_UC)) %>%
  left_join(cnuc_info, by=c(
    "CD_CNUC"="Código.UC",
    "nome_uc"="Nome.da.UC"
  )) %>%
  DataExplorer::plot_missing() # ainda tem uns 200 que tão errados ou com NA


# amount of management by PA type

UC_pop %>% 
  mutate(nome_uc=str_to_lower(UC_nome_clean)) %>%
  left_join(cnuc_info, by=c(
    "Cod_cnuc"="Código.UC",
    "nome_uc"="Nome.da.UC"
  )) %>% 
  st_drop_geometry() %>% 
  separate_rows(Bioma, sep = ",\\s*") %>%# \\s* to remove spaces
  select(CD_UC_UF, Bioma, ESFERA, 
         CATEGORIA, 13:15, 21:22) %>% 
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
  group_by(Bioma, new_cat, gov_level) %>%
  summarise(
    pa_amount=n_distinct(CD_UC_UF)) %>% 
  print(n=103)
  

# just to plot
y<-c("pa_amount", "pop_t", "pop_i", "pop_q")
plotss<-list()
for (i in seq_along(y)) {
  varname <- y[i]
  plotss[[varname]] <- data_plots %>%
    filter(Bioma!="Marinha") %>% 
    ggplot() +
    geom_point(aes(x = new_cat, 
                   y = .data[[varname]], 
                   color = new_cat)) +
    facet_wrap(~Bioma, scales = "free_y") +
    labs(y = varname) +
    theme_classic()
}
plotss[["pa_amount"]]
plotss[["pop_t"]]

# Understand IT dataset ----
#https://www.gov.br/funai/pt-br/atuacao/terras-indigenas/geoprocessamento-e-mapas
# TI = Em estudo < Delimitada < Declarada < Homologada < Regularizada
# REserva Indigena = Encaminhada RI <Regularizada
IT_shp<-read_sf("DATA/garbage/tis_poligonais/tis_poligonaisPolygon.shp",
        options = "ENCODING=LATIN1") %>% 
  glimpse

valid_IT<-st_make_valid(IT_shp) %>% 
  mutate(
    across(.cols=c(1, 2, 5, 
                   6, 8, 9,
                   12, 17), .fns=as.factor)
  ) %>% 
  select(-cr, -undadm_cod:-dominio_un, -epsg) %>% 
  glimpse

dim(valid_IT)
summary(valid_IT)
#any(!st_is_valid(valid_IT))

right_TI<-valid_IT %>% 
  filter(fase_ti%in%c("Homologada", 
                      "Regularizada")) %>% 
  glimpse

dup_shp <- right_TI %>% 
  st_drop_geometry() %>% 
  group_by(uf_sigla) %>% 
  count(terrai_nom) %>% 
  filter(n > 1) 
# Kariri-Xocó, Uneiuxi

#right_TI %>%
#  filter(terrai_nom=="Uneiuxi") %>%
#  ggplot() + geom_sf(aes(color=terrai_cod),alpha=0.5)

right_TI %>%
  filter(terrai_nom%in%dup_shp$terrai_nom) %>% 
  group_by(terrai_nom) %>% 
  summarise(.groups = "drop") %>% 
  glimpse

teste_funai<-right_TI %>% 
  filter(!terrai_nom%in%dup_shp$terrai_nom) %>% 
  glimpse

pop_ind %>% 
  left_join(teste_funai, by=c(
  "name_TI"="terrai_nom"
)) %>%  glimpse

pop_ind<-read_xlsx("DATA/garbage/pop_ind.xlsx")[-1:-5, c(-1, -4:-5)] %>%
  rename(code_TI_IBGE=`...2`, 
         name_TI=`...3`, 
         pop=`...6`)

dup_pop <- pop_ind %>%
  count(name_TI) %>%
  filter(n > 1)

teste_ibge<-pop_ind %>%
  filter(!name_TI%in%dup_pop$name_TI) %>% #View()
  glimpse
  
teste_ibge %>% left_join(teste_funai, by=c(
    "name_TI"="terrai_nom"
  )) %>%
  #count(name_TI) %>% filter(n > 1)
  DataExplorer::plot_missing()
# acho que o certo pé realmente tirar os duplicatos
# e precisamos ver o que estão com Na realmente rpas resolver da melhor forma




# Understand Quilombolas dataset ----
# https://www.gov.br/insa/pt-br/centrais-de-conteudo/mapas/mapas-em-shapefile/quilombos-incra.zip/view

QUI_shp<-read_sf("DATA/garbage/Quilombos_INCRA/Quilombos-SAB-INCRA.shp",
                options = "ENCODING=LATIN1") %>%
  mutate(name=str_to_lower(name)) %>% 
  glimpse

pop_QUI<-read_xlsx("DATA/pop_QUI2022.xlsx") [-1:-5, c(-1, -4)] %>% 
  rename(code_QUI_IBGE=`...2`, 
         name_QUI=`...3`, 
         pop=`...5`) %>% 
  filter(!is.na(pop)) %>% 
  mutate(
    pop=if_else(pop=="-", "0", pop),
    pop=as.numeric(pop), 
    name_QUI=str_to_lower(name_QUI)) %>% 
  glimpse


QUI_shp %>% 
  left_join(pop_QUI, by=c(
    "name"="name_QUI"
  )) %>% 
  filter(!is.na(pop)) %>%  glimpse
