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

IT_shp<-read_sf("DATA/tis_poligonais/tis_poligonaisPolygon.shp",
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









