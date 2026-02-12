# matching 

library(tidyverse)
library(sf)
library(MatchIt)
library(cobalt)
library(corrplot)

PA_matching_list<-readRDS("Outputs/data_to_match.rds")


PA_matching_list<-readRDS("Outputs/data_to_match.rds")
pageral<-readRDS("Outputs/PA_balanced_with_incpcp.rds")


lapply(PA_matching_list, function(x){
  x %>% 
    mutate(yr_creation=pageral$yr_creation[match(new_code, 
                                                 pageral$new_code)],
           yr_class=case_when(
             yr_creation>=2001 ~ "inc_>=2001", 
             yr_creation==2000 ~ "inc_=2000", 
             TRUE~ "exclude"
           ),
           yr_class=as.factor(yr_class)) %>%
    filter(yr_class=="inc_>=2001") %>% 
    group_by(name_biome, new_cat, yr_class) %>% 
    summarise(n=n())
})






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
  
  names_cofc <- df %>% dplyr::select(6:16) %>% colnames()
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
      pop.size = 500, #100 and 500
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
# Pop, prec, dist_roads, expo_time, inc2000
# it x spa
# elevation, expo time
# qui x spa
# completely separated between T and C
PA_matching_list$QUIxSPA %>% 
  mutate(ps = match_model_fm$distance) %>%
  group_by(name_biome, Treat) %>%
  summarise(min_ps = min(ps),
            max_ps = max(ps),
            .groups = "drop")
PA_matching_list$QUIxSPA %>% 
  group_by(name_biome, Treat) %>%
  summarise(n = n())

glm_QUIxSPA <- glm(formula_match,
                   data = PA_matching_list$QUIxSPA,
                   family = binomial)
summary(glm_QUIxSPA)

library(caret)
model_matrix <- model.matrix(formula_match, 
                             data = PA_matching_list$QUIxSPA)
caret::findLinearCombos(model_matrix)

glm_QUIxSPA <- glm(Treat ~ lat + long,
                   data = PA_matching_list$QUIxSPA,
                   family = binomial)
summary(glm_QUIxSPA)

glm_QUIxSPA_2 <- glm(Treat ~ lat + long + prec + elevation_mean,
                     data = PA_matching_list$QUIxSPA,
                     family = binomial)
summary(glm_QUIxSPA_2)

glm_QUIxSPA_3 <- glm(Treat ~ lat + long + prec + elevation_mean + expo_time,
                     data = PA_matching_list$QUIxSPA,
                     family = binomial)
summary(glm_QUIxSPA_3)

glm_QUIxSPA_4 <- glm(Treat ~ lat + long + prec + elevation_mean + expo_time + PA_area,
                     data = PA_matching_list$QUIxSPA,
                     family = binomial)
summary(glm_QUIxSPA_4)

glm_QUIxSPA_5 <- glm(Treat ~ PA_area,
                     data = PA_matching_list$QUIxSPA,
                     family = binomial)
summary(glm_QUIxSPA_5)
# AREA DETERMITCALLY PREDICTS QUI/PI

glm_QUIxSPA_6 <- glm(Treat ~ lat + long + prec + elevation_mean + expo_time + Pop,
                     data = PA_matching_list$QUIxSPA,
                     family = binomial)

glm_QUIxSPA_7 <- glm(Treat ~ lat + long + prec + elevation_mean + expo_time + Pop + inc_pcp2000_by_area,
                     data = PA_matching_list$QUIxSPA,
                     family = binomial)

glm_QUIxSPA_8 <- glm(Treat ~ lat + long + prec + elevation_mean + expo_time + Pop + inc_pcp2000_by_area,
                     data = PA_matching_list$QUIxSPA,
                     family = binomial)

glm_QUIxSPA_9 <- glm(Treat ~ lat + long + prec + elevation_mean + expo_time + Pop + inc_pcp2000_by_area + dist_roads,
                     data = PA_matching_list$QUIxSPA,
                     family = binomial)

glm_QUIxSPA_10 <- glm(Treat ~ dist_roads,
                      data = PA_matching_list$QUIxSPA,
                      family = binomial)
# DIST_ROADS DETERMITCALLY PREDICTS QUI/PI

glm_QUIxSPA_11 <- glm(Treat ~ lat + long + prec + elevation_mean + expo_time + Pop + inc_pcp2000_by_area + dist_agr,
                      data = PA_matching_list$QUIxSPA,
                      family = binomial)
glm_QUIxSPA_11_1 <- glm(Treat ~ dist_agr,
                        data = PA_matching_list$QUIxSPA,
                        family = binomial)
# DIST_AGR DETERMITCALLY PREDICTS QUI/PI

glm_QUIxSPA_12 <- glm(Treat ~ lat + long + prec + elevation_mean + expo_time + Pop + inc_pcp2000_by_area + dist_urb,
                      data = PA_matching_list$QUIxSPA,
                      family = binomial)
glm_QUIxSPA_12_1 <- glm(Treat ~ dist_urb,
                        data = PA_matching_list$QUIxSPA,
                        family = binomial)




# GEN ______________
## using pop.size=100 -->
## using pop.size=500 --> All were really bad than FM
# SUPA x SUP -> 10 and 5 were better than 1, and really similar. But fM was better 
# ITxSPA and ITxSUPA -> 1 was better than 5/10. But FM was just better than it. 

# balancing vars ----
# SUPAxSPA
rmk.SUPAxSPA<-PA_matching_list[[1]] %>% 
  select(-Pop, -expo_time, -long, 
         #- prec, -PA_area
         ) %>% glimpse
rmk.SUPAxSPA_formula_match <- update(formula_match, . ~ .
                                    - Pop
                                    - expo_time
                                    - long
                                    #- prec
                                    #- PA_area
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
balance_tab_FM[["SUPAxSPA"]]
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
balance_tab_FM[["ITxSPA"]]
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
balance_tab_FM[["ITxSUPA"]]
bal.tab(rmk.ITxSUPA_match_model, thresholds = c(m = .25), 
        v.threshold = 2, un = TRUE) # balanced
#removed elevation and precipitation

# preparing data after match ----
match_models<-list(
  matching_results_FM[["SUPAxSPA"]], # non-balanced, full
  rmk.ITxSPA_match_model, # balanced, -expo_time and -elevation
  rmk.ITxSUPA_match_model # balanced, -elevation and -precipitation
)

# preparing data 
PA_data<-readRDS("Outputs/PA_balanced_with_incpcp.rds") %>% #591
  filter(!new_cat%in%c("RPPN", "ARIE", "APA")) %>%
  filter(!name_biome%in%c("Pampa", "Pantanal")) %>% 
  select(-code_tract, -lit) %>% 
  glimpse

outc_and_remv_PA<-PA_data %>% 
  select(1, water:dead_less4year, 
         inc_pcp_by_area, defor_amount, 
         prec, expo_time, elevation_mean #covariates that we remove to match
         ) %>%
  mutate(new_code=as.factor(new_code)) %>% 
  glimpse

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
                     "prec", 
                     "elevation_mean", 
                     "expo_time")) %>% 
      select(new_code:long, 
             prec, expo_time, elevation_mean,
             geom:subclass, everything(.))
  }
  if(i==2){
    df_ifull[[2]]<-df_real %>% 
      left_join(outc_and_remv_PA, 
                by=c("new_code", 
                     "prec")) %>% 
      select(new_code:long, 
             prec, expo_time, elevation_mean,
             geom:subclass, everything(.))
  }
  if(i==3){
    df_ifull[[3]]<-df_real %>% 
      left_join(outc_and_remv_PA, 
                by=c("new_code", 
                     "expo_time")) %>% 
      select(new_code:long, 
             prec, expo_time, elevation_mean,
             geom:subclass, everything(.))
  }
}

lapply(based_match_data, dim)
lapply(df_ifull, dim)

# loveplots FMxGEN ---- 
lvplot_SUPAxSPA<-love.plot(formula_match,
          data = df_ifull[[1]], estimand = "ATT",
          weights = list(w1 = get.w(match_models[[1]]),
                         w2 = get.w(matching_results_GEN[["SUPAxSPAratio_1"]])),
          thresholds = c(m = .1),
          shapes = c("circle filled", "circle", "circle"),
          colors = c("black", "blue", "darkgreen"),
          sample.names = c("full matching",
                           "genetic matching (K=1)")
) + 
  geom_vline(xintercept = c(-0.25, -0.5, 0.25, 0.5), 
             linetype = "dashed",
             color = c("green", "orange", "green", "orange"))+
  ggtitle("SUPAxSPA")


lvplot_ITxSPA<-love.plot(formula_match,
                           data = df_ifull[[2]], estimand = "ATT",
                           weights = list(w1 = get.w(match_models[[2]]),
                                          w2 = get.w(matching_results_GEN[["ITxSPAratio_1"]])),
                           thresholds = c(m = .1),
                           shapes = c("circle filled", "circle", "circle"),
                           colors = c("black", "blue", "darkgreen"),
                           sample.names = c("full matching",
                                            "genetic matching (K=1)")
) + 
  geom_vline(xintercept = c(-0.25, -0.5, 0.25, 0.5), 
             linetype = "dashed",
             color = c("green", "orange", "green", "orange"))+
  ggtitle("ITxSPA")


lvplot_ITxSUPA<-love.plot(formula_match,
                         data = df_ifull[[3]], estimand = "ATT",
                         weights = list(w1 = get.w(match_models[[3]]),
                                        w2 = get.w(matching_results_GEN[["ITxSUPAratio_1"]])),
                         thresholds = c(m = .1),
                         shapes = c("circle filled", "circle", "circle"),
                         colors = c("black", "blue", "darkgreen"),
                         sample.names = c("full matching",
                                          "genetic matching (K=1)")
) + 
  geom_vline(xintercept = c(-0.25, -0.5, 0.25, 0.5), 
             linetype = "dashed",
             color = c("green", "orange", "green", "orange"))+
  ggtitle("ITxSUPA")

# saving ----
saveRDS(list(SUPAxSPA=match_models[[1]],
        ITxSPA=match_models[[2]],
        ITxSUPA=match_models[[3]]),
        "Outputs/Matched_models.rds")

saveRDS(list(SUPAxSPA=df_ifull[[1]],
        ITxSPA=df_ifull[[2]],
        ITxSUPA=df_ifull[[3]]),
        "Outputs/Matched_data.rds")



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

