# sensitive checking

library(sf)
library(tidyverse)
library(lmtest)
library(sandwich)
library(rbounds)
library(spdep)
library(geobr)
library(sensemakr)

# OLS and sensitive test after matching ----
matched_data<-readRDS("Outputs/Matched_data.rds")

# adding brazilian state code to our data

code_states<-geobr::read_state(year = 2020) %>% 
  select(1, 2) %>% 
  mutate(across(.cols = c(1,2), .fns=as.factor)) %>% 
  st_transform(., "EPSG:5880")

matched_data_rmk<-list()
for (i in seq_along(matched_data)) {
  dfs_i<-matched_data[[i]]
  dfs_i_names<-names(matched_data[i])
  
  df_i_final<-dfs_i%>%
    st_as_sf(coords = c("long", "lat"), 
             crs=5880) %>% 
    st_join(
      code_states %>% select(code_state, abbrev_state),
      join = st_intersects, left = TRUE) %>%  
    select(-geometry) %>%
    left_join(dfs_i %>% 
                select(new_code, long, lat), 
              by="new_code") %>%
    mutate(across(.cols=c(Treat, name_biome, subclass),
                  .fns = as.factor)) %>% 
    glimpse
  matched_data_rmk[[dfs_i_names]]<-df_i_final
}

matched_data<-matched_data_rmk

# solving NA issues from state codes (PA: Reserva Extrativista de Cururupu)
#matched_data[[3]] %>% # change 1 to  
#  filter(is.na(code_state)) %>%
#  ggplot()+
#  geom_sf(
#    data = code_states %>% 
#      filter(abbrev_state=="MA")
#  )+
#  geom_sf(aes(fill=PA_name))

matched_data[[1]]<-matched_data[[1]] %>% 
  mutate(abbrev_state=replace_na(abbrev_state, "MA"), 
         code_state=replace_na(code_state, "21")) %>%  
  #summary()
  glimpse
  
matched_data[[3]]<-matched_data[[3]] %>% 
  mutate(abbrev_state=replace_na(abbrev_state, "MA"), 
         code_state=replace_na(code_state, "21")) %>%  
  #summary()
  glimpse

#lapply(matched_data, summary)

y_lm<-c("water", "sanitation",
     "dead_less4year", "inc_pcp_by_area", 
     "defor_amount")

form_base<- "Treat + name_biome + Treat:name_biome + subclass + Pop + elevation_mean + lat + long + dist_to_urban + PA_area + prec + expo_time + inc_pcp_by_area"
form_inc <- "Treat + name_biome + Treat:name_biome +subclass + Pop + elevation_mean + lat + long + dist_to_urban + PA_area + prec + expo_time"
# PS: I need remove the scale because all PA here was 'federal'

out_glm<-list()
out_glm_summary<-list()
htcest_glm<-list()
cluster_glm<-list()
bounds_psens<-list()
sense_models<-list()

for (i in seq_along(matched_data)) {
  df<-matched_data[[i]]
  df_name<-names(matched_data)[i]
  
  for (w in seq_along(y_lm)) {
    outc<-y_lm[w]
    model_id <- paste(df_name, outc, sep = "_")
    
    if (outc == "inc_pcp_by_area") {
      f_model <- as.formula(paste(outc, 
                                  form_inc, 
                                  sep=" ~ "))
    } else {
      f_model <- as.formula(paste(outc, 
                                  form_base,
                                  sep=" ~ "))
    }
    
    model <- glm(
      formula = f_model,
      data = df,
      weights = df$weights,
      family = "gaussian")
    
    out_glm[[model_id]] <- model
    out_glm_summary[[model_id]] <- summary(model)
    
    # error - heterocedasticity
    vcov_robust <- vcovHC(model,
                          type = "HC3")
    htcest_glm[[model_id]] <- coeftest(model,
                                       vcov = vcov_robust)
    
    # error - states cluster
    vcov_cluster <- vcovCL(model, 
                           cluster = ~ df$abbrev_state)
    cluster_glm[[model_id]] <-coeftest(model, 
                                       vcov = vcov_cluster)
    
    # Rousebound
    psens_bounds <- psens(
      x = as.numeric(as.character(df$Treat)), 
      y = df[[outc]],
      Gamma = 3
    )
    bounds_psens[[model_id]] <- psens_bounds
    
    # sensitive 
    # names(coef(model)) # run it outside of for when sensemark feature an Error
    
    if(outc %in% c("inc_pcp_by_area", "defor_amount")){
      benchmark<-"dist_to_urban"
    }
    else(benchmark<-"inc_pcp_by_area")
    
    sense_md<-sensemakr::sensemakr(
      model = model,  
      treatment= "Treat1", # I do not understand it well. 
      benchmark_covariates = benchmark, 
      kd = 1:3)
    
    sense_models[[model_id]]<- sense_md
  }
}

# matching results
bounds_psens 
#good, Except for ITxSUPA and SUPAxSPA_dead_less4year

# mudança. interação
out_glm_summary$ITxSPA_inc_pcp_by_area -- mata atlantica e saiu treat
out_glm_summary$ITxSUPA_water -- caatinga e cerrado
out_glm_summary$ITxSUPA_sanitation -- cerrado
out_glm_summary$ITxSUPA_dead_less4year -- treat
out_glm_summary$ITxSUPA_inc_pcp_by_area -- treat, cerrado e mata atlântica
out_glm_summary$ITxSUPA_defor_amount -- cerrado e mata atlântica
Precisamos avaliar se o que vale é interação ou dividir os modelos

# OLS results
out_glm_summary$ITxSUPA_defor_amount #naive
htcest_glm$ITxSUPA_defor_amount # robust erro
cluster_glm$ITxSUPA_defor_amount # state cluster erro
# treatment was significant to:
## SUPAxSPA -> (. - cluster) sanitation, 
## ITxSPA -> (*** - naive) income
## ITxSUPA -> (. - cluster) water
#             (** - naive and cluster) dead_less4year

#Just make sense test the sensibility of significant
## SUPAxSPA -> (. - cluster) sanitation
sense_models$SUPAxSPA_sanitation # sensitive
summary(sense_models$SUPAxSPA_sanitation)
## treatment is no significant

## ITxSPA -> (*** - naive) income
summary(sense_models$ITxSPA_inc_pcp_by_area)
## treatment is significant (strong)

## ITxSUPA -> (. - cluster) water
summary(sense_models$ITxSUPA_water)
## treatment is no significant

#  ITxSUPA -> (** - naive and cluster) dead_less4year
summary(sense_models$ITxSUPA_dead_less4year)
## treatment is significant weekly


# Spatial weight and Moran I test (spatial autocorrelation) ----

#chosen K value
for (k in c(4, 6, 8, 10)) {
  for (i in seq_along(matched_data)) {
    i_df<-matched_data[[i]] %>% 
      st_as_sf()
    i_df_names<-names(matched_data)[i]
    
    centroids <- st_centroid(i_df$geom)
    coords <- st_coordinates(centroids)
    
    knn_df <- knn2nb(knearneigh(coords, k = k))
    print(summary(nb2listw(knn_df, style = "W",
                           zero.policy = TRUE)))
    comp <- n.comp.nb(knn_df)
    sizes_tab <- table(comp$comp.id)
    print(as.data.frame(sizes_tab))
  }
}
#k=6 to SUPAxSPA and ITxSPA
#k=8 to ITxSUPA

# Estimating spatial weights
spt_weights_kneighbor<-list()
for (i in seq_along(matched_data)) {
  i_df<-matched_data[[i]] %>% 
    st_as_sf()
  i_df_names<-names(matched_data)[i]
  
  centroids <- st_centroid(i_df$geom)
  coords <- st_coordinates(centroids)
  
  if(i==3){k_value<-8} 
  else {k_value<-6}
  
  knn_df <- knn2nb(knearneigh(coords, k = k_value)) 
  wght_spatial <- nb2listw(knn_df, style = "W", 
                         zero.policy = TRUE)
  spt_weights_kneighbor[[i_df_names]] <- wght_spatial
}

# run MoranI test
#names(out_glm)
#names(spt_weights_kneighbor)

moranI_kneighbor <- list()
for (w_name in names(spt_weights_kneighbor)) {
  lw <- spt_weights_kneighbor[[w_name]]
  
  ols_idx <- grep(
    paste0("^", w_name, "_"),
    names(out_glm))
  
  for (i in ols_idx) {
    model <- out_glm[[i]]
    model_name <- names(out_glm)[i]
  
    res <- residuals(model)
    moranI_kneighbor[[model_name]] <- moran.test(
      res, lw,
      alternative = "two.sided",
      zero.policy = TRUE)
  }
}

# Results
#Just both below were spatial autocorrelated:
moranI_kneighbor$ITxSPA_inc_pcp_by_area
#ITxSPA_inc_pcp_by_area p-value = 0.04628
#Moran I statistic       Expectation          Variance 
#      0.046791645      -0.002262443       0.000605892
moranI_kneighbor$ITxSPA_defor_amount
#ITxSPA_defor_amount p-value = 5.675e-05
#Moran I statistic       Expectation          Variance 
#     0.0750583148     -0.0022624434      0.0003688609




# Saving important outputs ----



