# matching 

library(tidyverse)
library(sf)
library(MatchIt)
library(cobalt)

PA_matching_list<-readRDS("Outputs/data_to_match.rds")

matching_results_FM <- list()
matching_results_GEN <- list()
balance_tab_FM <- list()
balance_tab_GEN <- list()
balance_summary <- list()
love_plots <- list()
love_plots2 <- list()
kvalue<-c(1, 5, 10)

for (i in seq_along(PA_matching_list)) {
  
  df <- PA_matching_list[[i]]
  df_name <- names(PA_matching_list)[i]
  
  names_cofc <- df %>% dplyr::select(6:14, -PA_scale) %>% colnames()
  names_cofc_vector <- unlist(strsplit(paste(names_cofc,
                                  collapse = " + "), " \\+ "))
  formula_match <- as.formula(paste("Treat ~",
                                        paste(names_cofc_vector, 
                                              collapse = " + ")))
  # Genetic matching with K=c(1, 5, 10)
  for (k in kvalue) {
    set.seed(12345)
    gen_model <- matchit(formula_match,
      data = df,
      method = "genetic",
      ratio = k, 
      distance = "glm",
      link = "logit", 
      exact = ~ name_biome,
      estimand = "ATT",
      pop.size = 500,
      verbose = FALSE,
      include.obj = FALSE
    )
    matching_results_GEN[[paste0(df_name, "ratio_", k)]] <- gen_model
    
    b_tab_gen<-bal.tab(gen_model, thresholds = c(m = .25), 
                   v.threshold = 2, un = TRUE)
    balance_tab_GEN[[paste0(df_name, "ratio_", k)]] <- b_tab_gen
    }
  
  # Full Matching with GLM PS 
  match_model_fm <- matchit(formula_match, 
                             data = df, 
                             method = "full", 
                             distance = "glm",
                             link = "logit", 
                             exact = ~ name_biome,
                             estimand = "ATT",
                             verbose = TRUE,
                             include.obj = FALSE)
  
  matching_results_FM[[df_name]] <- match_model_fm
  
  # Balance
  balance_summ <- summary(match_model_fm, standardize = TRUE)
  balance_summary[[df_name]] <- balance_summ
  
  b_tab_fm<-bal.tab(match_model_fm, thresholds = c(m = .25), 
                     v.threshold = 2, un = TRUE)
  balance_tab_FM[[df_name]] <- b_tab_fm
  
  # LovePlot - mean
  p_love <- love.plot(match_model_fm,
                          estimand = "ATT",
                          stat = "mean.diffs", 
                          thresholds = c(m = 0.1),
                          shapes = c("circle filled", "circle filled"),
                          colors = c("red", "blue")) + 
    geom_vline(xintercept = c(-0.25, -0.5, 0.25, 0.5), 
               linetype = "solid", 
               color = c("green", "orange", "green", "orange")) +
    ggtitle(paste("Love Plot -", df_name))
  
  love_plots[[df_name]] <- p_love
  print(p_love) 
  
  # LovePlot - variance
  p_love2 <- love.plot(match_model_fm,
                           stat =  "variance.ratios", 
                           thresholds = c(v = 2),
                           shapes = c("circle filled", "circle filled"),
                           colors = c("red", "blue")) + 
    ggtitle(paste("Love Plot -", df_name))
  
  love_plots2[[df_name]] <- p_love2
  print(p_love2) 
}
# results 
# FM _______________
#supa x spa 
# pop, expo time, long
#it x spa
# elevation, expo time, long, lat
#it x supa
# prec, elevation
# GEN ______________
## using pop.size=100 --> All were really bad than FM
## using pop.size=500 --> All were really bad than FM
# SUPA x SUP -> 10 and 5 were better than 1, and really similar. But fM was better 
# ITxSPA and ITxSUPA -> 1 was better than 5/10. But FM was just better than it. 

# balancing vars ----
# SUPAxSPA
rmk.SUPAxSPA<-PA_matching_list[[1]] %>% 
  select(-Pop, -expo_time, -long, 
         - prec, -PA_area) %>% glimpse
rmk.SUPAxSPA_formula_match <- update(formula_match, . ~ .
                                    - Pop
                                    - expo_time
                                    - long
                                    - prec
                                    - PA_area
                                    )
rmk.SUPAxSPA_match_model <- matchit(rmk.SUPAxSPA_formula_match, 
                                   data = rmk.SUPAxSPA, 
                                   method = "full", 
                                   distance = "glm",
                                   link = "logit", 
                                   exact = ~ name_biome,
                                   estimand = "ATT",
                                   verbose = TRUE,
                                   include.obj = FALSE)
balance_tab[["SUPAxSPA"]]
bal.tab(rmk.SUPAxSPA_match_model, thresholds = c(m = .25), 
        v.threshold = 2, un = TRUE) # non balanced
# if we remove the three unbalanced, appear more 2 (PA_area, precp)
# if we remove all these five, distance will be unbalanced

# ITxSPA
rmk.ITxSPA<-PA_matching_list[[2]] %>% 
  select(-expo_time, -elevation_mean 
         ) %>% glimpse
rmk.ITxSPA_formula_match <- update(formula_match, . ~ .
                                   - expo_time
                                   - elevation_mean 
                                   )
rmk.ITxSPA_match_model <- matchit(rmk.ITxSPA_formula_match, 
                                  data = rmk.ITxSPA, 
                                  method = "full", 
                                  distance = "glm",
                                  link = "logit", 
                                  exact = ~ name_biome,
                                  estimand = "ATT",
                                  verbose = TRUE,
                                  include.obj = FALSE)
balance_tab[["ITxSPA"]]
bal.tab(rmk.ITxSPA_match_model, thresholds = c(m = .25), 
        v.threshold = 2, un = TRUE) # balanced
# removed expo_time and elevation_mean 

# ITxSUPA
rmk.ITxSUPA<-PA_matching_list[[3]] %>% 
  select(-elevation_mean, -prec
  ) %>% glimpse
rmk.ITxSUPA_formula_match <- update(formula_match, . ~ .
                                   - elevation_mean
                                   - prec
)
rmk.ITxSUPA_match_model <- matchit(rmk.ITxSUPA_formula_match, 
                                  data = rmk.ITxSUPA, 
                                  method = "full", 
                                  distance = "glm",
                                  link = "logit", 
                                  exact = ~ name_biome,
                                  estimand = "ATT",
                                  verbose = TRUE,
                                  include.obj = FALSE)
balance_tab[["ITxSUPA"]]
bal.tab(rmk.ITxSUPA_match_model, thresholds = c(m = .25), 
        v.threshold = 2, un = TRUE) # balanced
#removed elevation and precipitation

# preparing data after match ----
match_models<-list(
  rmk.SUPAxSPA_match_model, # PA area, expo_time,and prec
  rmk.ITxSPA_match_model, # dist, expo_time
  rmk.ITxSUPA_match_model #elev, prec
)

# preparing data 

outc_and_remv_PA<-PA_data %>% 
  select(1, water:dead_less1year, 
         inc_pcp_by_area, defor_amount, 
         PA_area, prec, 
         dist_to_urban, 
         expo_time, elevation_mean) %>%
  mutate(new_code=as.factor(new_code), 
         PA_area=as.numeric(PA_area)) %>% 
  glimpse

scale<-readRDS("Outputs/UC_socio_data.rds") %>% 
  st_drop_geometry() %>% 
  mutate(new_code=paste(new_cat, COD_UC, sep = "_"), 
         new_code=as.factor(new_code)) %>% 
  select(new_code, ESFERA) %>%  glimpse

based_match_data<-list()
df_ifull <- list()
for (i in seq_along(match_models)) {
  df_real<-match.data(match_models[[i]]) %>% 
    as.data.frame()  
  based_match_data[[i]]<-df_real
  
  if(i==1){
    df_ifull[[1]]<-df_real %>% 
      left_join(outc_and_remv_PA, 
                by=c("new_code", 
                     "dist_to_urban", 
                     "elevation_mean")) %>% 
      left_join(scale, by="new_code") %>% 
      mutate(scale=case_when(
        is.na(ESFERA) ~ "Federal", 
        TRUE ~ ESFERA)) %>% 
      select(-ESFERA) 
  }
  if(i==2){
    df_ifull[[2]]<-df_real %>% 
      left_join(outc_and_remv_PA, 
                by=c("new_code", 
                     "PA_area", "prec", 
                     "elevation_mean")) %>% 
      left_join(scale, by="new_code") %>% 
      mutate(scale=case_when(
        is.na(ESFERA) ~ "Federal", 
        TRUE ~ ESFERA)) %>% 
      select(-ESFERA) 
  }
  if(i==3){
    df_ifull[[3]]<-df_real %>% 
      left_join(outc_and_remv_PA, 
                by=c("new_code", 
                     "PA_area", 
                     "dist_to_urban", 
                     "expo_time")) %>% 
      left_join(scale, by="new_code") %>% 
      mutate(scale=case_when(
        is.na(ESFERA) ~ "Federal", 
        TRUE ~ ESFERA)) %>% 
      select(-ESFERA) 
  }
}

#lapply(based_match_data, dim)
#lapply(df_ifull, dim)

# saving ----
saveRDS(list(SUPAxSPA=match_models[[1]],
        ITxSPA=match_models[[2]],
        ITxSUPA=match_models[[3]]),
        "Outputs/Matched_models.rds")

saveRDS(list(SUPAxSPA=df_ifull[[1]],
        ITxSPA=df_ifull[[2]],
        ITxSUPA=df_ifull[[3]]),
        "Outputs/Matched_data.rds")

# pos-matching regression and robustness test ----






#### Garbage ----
# Idea 2 - one match to each biome

PA_list<-readRDS("Outputs/PA_balanced.rds") %>% #1134
  filter(!new_cat%in%c("RPPN", "ARIE")) %>% #1087
  filter(!name_biome%in%c("Pampa", "Pantanal")) %>% #1073
  group_by(name_biome, new_cat) %>% 
  group_split()

AM_IT_SEA<-full_join(PA_list[[2]], PA_list[[1]]) 
AM_IT_SPA<-full_join(PA_list[[2]], PA_list[[3]]) 
AM_IT_SUPA<-full_join(PA_list[[2]], PA_list[[4]]) 

CAAT_IT_SEA<-full_join(PA_list[[6]], PA_list[[5]]) 
CAAT_IT_SPA<-full_join(PA_list[[6]], PA_list[[7]]) 
CAAT_IT_SUPA<-full_join(PA_list[[6]], PA_list[[8]]) 

CERR_IT_SEA<-full_join(PA_list[[10]], PA_list[[9]]) 
CERR_IT_SPA<-full_join(PA_list[[10]], PA_list[[11]]) 
CERR_IT_SUPA<-full_join(PA_list[[10]], PA_list[[12]]) 

AF_IT_SEA<-full_join(PA_list[[14]], PA_list[[13]]) 
AF_IT_SPA<-full_join(PA_list[[14]], PA_list[[15]]) 
AF_IT_SUPA<-full_join(PA_list[[14]], PA_list[[16]])

alldata<-list(AM_IT_SEA, AM_IT_SPA, AM_IT_SUPA, 
     CAAT_IT_SEA, CAAT_IT_SPA, CAAT_IT_SUPA, 
     CERR_IT_SEA, CERR_IT_SPA, CERR_IT_SUPA, 
     AF_IT_SEA, AF_IT_SPA, AF_IT_SUPA)

allnames<-c()

