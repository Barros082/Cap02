# matching 

library(tidyverse)
library(sf)
library(MatchIt)
library(cobalt)
library(corrplot)

# Idea 1 - one match to all using the biome as exact ----
PA_data<-readRDS("Outputs/PA_balanced.rds") %>% #1134
  filter(!new_cat%in%c("RPPN", "ARIE")) %>% #1087
  filter(!name_biome%in%c("Pampa", "Pantanal")) %>% #1073
  glimpse

# correlation
corr_data<-PA_data %>% 
  st_drop_geometry() %>% 
  select(-1:-3, 
         -yr_creation, 
         -name_biome:-centroid, 
         -water:-dead_less1year, 
         -defor_amount) %>%  glimpse

corr_matrix <- cor(corr_data)

corrplot(corr_matrix, method = "circle", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")
#considering 0.7 as trehshold, we will remove:
# tmin, tmax

PA_matching_step01 <- PA_data %>%
  st_drop_geometry() %>% 
  select(-tmin, -tmax, # correlated
         -yr_creation, -centroid, #dummies
         -water:-dead_less1year, #outputs
         -defor_amount) %>% 
  mutate(Treat=case_when(
    new_cat=="IT"~1,
    TRUE ~0
  )) %>% 
  group_by(new_cat) %>% 
  group_split()

ITxSEA<-full_join(PA_matching_step01[[2]],
                  PA_matching_step01[[1]]) 
ITxSPA<-full_join(PA_matching_step01[[2]], 
                  PA_matching_step01[[3]]) 
ITxSUPA<-full_join(PA_matching_step01[[2]], 
                   PA_matching_step01[[4]]) 

PA_matching_list<-list(ITxSEA, ITxSPA, ITxSUPA)
PA_matching_names<-c("ITxSEA", "ITxSPA", "ITxSUPA")

PA_matching_list_step02<-lapply(PA_matching_list, function(x){
  df_x<-x %>% 
    select(new_code:new_cat, name_biome, Treat,
           everything(.)) %>% 
    mutate(
      across(.cols = c(new_code, new_cat, 
                       name_biome), .fns=as.factor), 
      PA_area=as.numeric(PA_area)
    ) %>% 
    glimpse
  
  print(df_x %>%dim())
  print(df_x %>% distinct(new_code) %>%  dim())
  
  return(df_x)
})

matching_results <- list()
balance_summary <- list()
balance_tab <- list()
love_plots <- list()
love_plots2 <- list()

for (i in seq_along(PA_matching_list_step02)) {
  
  df <- PA_matching_list_step02[[i]]
  df_name <- PA_matching_names[i]
  
  names_cofc <- df %>% dplyr::select(6:13) %>% colnames()
  names_cofc_vector <- unlist(strsplit(paste(names_cofc,
                                  collapse = " + "), " \\+ "))
  formula_match <- as.formula(paste("Treat ~",
                                        paste(names_cofc_vector, 
                                              collapse = " + ")))
  
  # Full Matching with GLM PS 
  match_model <- matchit(formula_match, 
                             data = df, 
                             method = "full", 
                             distance = "glm",
                             link = "logit", 
                             exact = ~ name_biome,
                             estimand = "ATT",
                             verbose = TRUE,
                             include.obj = FALSE)
  
  matching_results[[df_name]] <- match_model
  
  # Balance
  balance_summ <- summary(match_model, standardize = TRUE)
  balance_summary[[df_name]] <- balance_summ
  
  b_tab<-bal.tab(match_model, thresholds = c(m = .25), 
                     v.threshold = 2, un = TRUE)
  balance_tab[[df_name]] <- b_tab
  
  # LovePlot - mean
  p_love <- love.plot(match_model,
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
  p_love2 <- love.plot(match_model,
                           stat =  "variance.ratios", 
                           thresholds = c(v = 2),
                           shapes = c("circle filled", "circle filled"),
                           colors = c("red", "blue")) + 
    ggtitle(paste("Love Plot -", df_name))
  
  love_plots2[[df_name]] <- p_love2
  print(p_love2) 
}

love_plots
love_plots2
balance_tab
matching_results

# balancing vars ----
# SEA 
rmk.ITxSEA<-PA_matching_list_step02[[1]] %>% 
  select(-Pop, -PA_area) %>% glimpse
rmk.ITxSEA_formula_match <- update(formula_match, . ~ .
                                    - Pop
                                    - PA_area)
rmk.ITxSEA_match_model <- matchit(rmk.ITxSEA_formula_match, 
                                   data = rmk.ITxSEA, 
                                   method = "full", 
                                   distance = "glm",
                                   link = "logit", 
                                   exact = ~ name_biome,
                                   estimand = "ATT",
                                   verbose = TRUE,
                                   include.obj = FALSE)
balance_tab[["ITxSEA"]]
bal.tab(rmk.ITxSEA_match_model, thresholds = c(m = .25), 
        v.threshold = 2, un = TRUE) # balanced! removed pop and area

# SPA
rmk.ITxSPA<-PA_matching_list_step02[[2]] %>% 
  select(-elevation_mean, -dist_to_urban
         ) %>% glimpse
rmk.ITxSPA_formula_match <- update(formula_match, . ~ .
                                   - elevation_mean
                                   - dist_to_urban
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
        v.threshold = 2, un = TRUE) # balanced! removed elevation and urban distance

# SUPA
rmk.ITxSUPA<-PA_matching_list_step02[[3]] %>% 
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
        v.threshold = 2, un = TRUE) # balanced! removed elevation and precipitation




# Idea 2 - one match to each biome ----

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

