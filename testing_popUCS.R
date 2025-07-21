# Understand Data

library(tidyverse)
library(sf)
library(readxl)

PA_shp<-read_sf("DATA/BR_UC_UF_Publicacao_CD2022/BR_UC_UF_Publicacao_CD2022.shp") %>%
  mutate(
    across(.cols = c(1:3, 5, 7:10), as.factor)) %>% 
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

summary(pop2022)

# join 
left_join(PA_shp %>% 
            mutate(
              NOME_UC_UF = case_when(
                str_detect(NOME_UC_UF, "d\\?Ouro") ~ str_replace(NOME_UC_UF, "d\\?Ouro", "d'Ouro"),
                str_detect(NOME_UC_UF, "d\\?Ostra") ~ str_replace(NOME_UC_UF, "d\\?Ostra", "d'Ostra"),
                str_detect(NOME_UC_UF, "Itapará \\? Boiaçu") ~ str_replace(NOME_UC_UF, "\\?", "–"),
                TRUE ~ str_replace_all(NOME_UC_UF, fixed("?"), "-")  # substituição genérica
              ),
              NOME_UC = case_when(
                str_detect(NOME_UC, "d\\?Ouro") ~ str_replace(NOME_UC, "d\\?Ouro", "d'Ouro"),
                str_detect(NOME_UC, "d\\?Ostra") ~ str_replace(NOME_UC, "d\\?Ostra", "d'Ostra"),
                str_detect(NOME_UC, "Itapará \\? Boiaçu") ~ str_replace(NOME_UC, "\\?", "–"),
                TRUE ~ str_replace_all(NOME_UC, fixed("?"), "-")
              )),
          pop2022, by=c(
  "COD_UC"="UC_code", 
  "NOME_UC"="UC_nome_clean", 
  "CD_UC_UF"="UC_codeUF",
  "NOME_UC_UF"="UC_nome_UF", 
  "CD_CNUC"="Cod_cnuc", 
  "COD_UF"="Cod_UF", 
  "SG_UF"="Abrev_UF"
)) %>% 
  filter(is.na(pop_total)) %>%  View()

pop2022 %>% 
  filter(UC_nome_clean=="Reserva de Desenvolvimento Sustentável Itapará Boiaçu") %>% 
  View()

PA_shp%>% 
  filter(NOME_UC=="Área de Proteção Ambiental de Guaraqueçaba") %>% 
  glimpse
