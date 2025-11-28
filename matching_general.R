# matching 

library(tidyverse)
library(sf)
library(MatchIt)
library(cobalt)
library(corrplot)

# preparing data before matching ----
PA_data<-readRDS("Outputs/PA_balanced_with_incpcp.rds") %>% #591
  filter(!new_cat%in%c("RPPN", "ARIE", "APA")) %>%  #557
  filter(!name_biome%in%c("Pampa", "Pantanal")) %>% #550
  select(-code_tract, -lit) %>% 
  glimpse

# correlation
corr_data<-PA_data %>% 
  sf::st_drop_geometry() %>% 
  select(-1:-3, 
         -yr_creation, -PA_scale,
         -name_biome:-centroid,
         #-water:-dead_less4year,
         #-defor_amount,
         #-tmin, -tmax,
         -geom) %>%  glimpse

corr_matrix <- cor(corr_data)

corrplot(corr_matrix, method = "circle", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")
#correlation vars:
# tmin~tmax
# pop~water e pop~dead
# sanitation~waste

#considering 0.7 as threshold, we will remove only the covariates:
# tmin and tmax

PA_matching_step01 <- PA_data %>%
  st_drop_geometry() %>% 
  select(-tmin, -tmax, # correlated
         -yr_creation, -centroid, #dummies
         -water:-dead_less4year, #outputs 
         -inc_pcp_by_area, #outputs
         -defor_amount) %>% 
  group_by(new_cat) %>% 
  group_split()

SUPAxSPA<-full_join(PA_matching_step01[[3]],
                  PA_matching_step01[[2]]) %>% 
  mutate(Treat=case_when(
    new_cat=="US"~1,
    TRUE ~0)) %>% 
  glimpse
ITxSPA<-full_join(PA_matching_step01[[1]], 
                  PA_matching_step01[[2]]) %>% 
  mutate(Treat=case_when(
    new_cat=="IT"~1,
    TRUE ~0))%>% 
  glimpse
ITxSUPA<-full_join(PA_matching_step01[[1]], 
                   PA_matching_step01[[3]]) %>% 
  mutate(Treat=case_when(
    new_cat=="IT"~1,
    TRUE ~0))%>% 
  glimpse

PA_matching_list<-list(SUPAxSPA, ITxSPA, ITxSUPA)
PA_matching_names<-c("SUPAxSPA", "ITxSPA", "ITxSUPA")

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

# testing PA overlap ----
# geometry that are inside each other
test_within_geom<-list()
for (i in seq_along(PA_matching_list_step02)) {
  t_df<-PA_matching_list_step02[[i]]
  
  list_t_df<-t_df %>% 
    st_as_sf() %>% 
    group_by(new_cat) %>% 
    group_split()
  
  test_within_geom[[i]]<-st_intersection(list_t_df[[1]],
                                list_t_df[[2]],
                                sparse = FALSE) %>%
    st_drop_geometry() %>% 
    select(new_code, PA_name, expo_time, new_cat,
           new_code.1, PA_name.1, expo_time.1, new_cat.1)
  
  sums_code<-test_within_geom[[i]] %>% 
    summarise(cod1=n_distinct(new_code), 
              cod2=n_distinct(new_code.1))
  print(sums_code)
  
  count_bio_cat<-tibble(
    new_code=c(test_within_geom[[i]]$new_code, 
               test_within_geom[[i]]$new_code.1)) %>% 
    as.data.frame() %>% 
    left_join(t_df %>% 
                select(new_code, name_biome,
                       new_cat), 
              by="new_code") %>%
    group_by(name_biome, new_cat) %>% 
    summarise(cod1=n_distinct(new_code))
  print(count_bio_cat)
  
  count_original<-t_df %>% 
    group_by(name_biome, new_cat) %>% 
    summarise(cod1=n_distinct(new_code))
  print(count_original)
}

#plotting each graphic

graphics_overlap <- list()
for(i in 1:length(test_within_geom)) {
  all_combinations <- test_within_geom[[i]] %>%
    select(new_code, new_code.1)
  
  for(j in 1:nrow(all_combinations)) {
    code1 <- as.character(all_combinations$new_code[j])
    code2 <- as.character(all_combinations$new_code.1[j])
    unique_key <- paste0("L", i, "_R", j)
    
    spatial_data <- PA_matching_list_step02[[i]] %>%
      filter(new_code %in% c(code1, code2)) %>%
      st_as_sf()
    
    if(nrow(spatial_data) >= 1) {
      point_data <- spatial_data %>%
        st_drop_geometry() %>%
        st_as_sf(coords = c("long", "lat"),
                 crs = st_crs(spatial_data))
      
      p <- ggplot() +
        geom_sf(data = spatial_data, 
                aes(fill = new_code), alpha = 0.3) +
        geom_sf(data = point_data, 
                color = "red", size = 3, shape = 16) +
        labs(title = paste("Overlap:", code1, "&", code2),
             subtitle = paste("List", i, "- Row", j),
             fill = "Protected Area") +
        scale_fill_viridis_d() +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      graphics_overlap[[unique_key]] <- p
    }
  }
}
#lapply(graphics_overlap, names)





#### Matching ----
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
#supa x spa
# prec e expo_time
#it x spa
# expo_time e dist_to_urban(0.2503)
#it x supa
# elev e precp


# balancing vars ----
# SUPAxSPA
rmk.SUPAxSPA<-PA_matching_list_step02[[1]] %>% 
  select(-prec, -expo_time, -PA_area) %>% glimpse
rmk.SUPAxSPA_formula_match <- update(formula_match, . ~ .
                                    - prec
                                    - expo_time
                                    - PA_area)
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
        v.threshold = 2, un = TRUE) # balanced (+/-)
# balanced if we remove PA area, expo_time, and prec

# ITxSPA
rmk.ITxSPA<-PA_matching_list_step02[[2]] %>% 
  select(-expo_time, -dist_to_urban
         ) %>% glimpse
rmk.ITxSPA_formula_match <- update(formula_match, . ~ .
                                   - expo_time
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
        v.threshold = 2, un = TRUE) # balanced (+)
# removed expo_time and urban distance

# ITxSUPA
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
        v.threshold = 2, un = TRUE) # balanced! (+)
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

