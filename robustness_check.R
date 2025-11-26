# Robutness checking

library(sf)
library(tidyverse)
library(lmtest)
library(sandwich)
library(rbounds)
library(spdep)

# linear models and Rousebound sensitive test after matching ----

#initial_data<-readRDS("Outputs/PA_balanced_with_incpcp.rds") %>% #1122
#  filter(!new_cat%in%c("RPPN", "ARIE", "APA")) %>% 
#  filter(!name_biome%in%c("Pampa", "Pantanal")) %>% 
#  glimpse

matched_data<-readRDS("Outputs/Matched_data.rds")

y_lm<-c("water", "sanitation", "waste", #"lit",
     "dead_less1year", "inc_pcp_by_area", 
     "defor_amount")

form_base<- "as.factor(Treat) + as.factor(name_biome) + as.factor(subclass) + Pop + elevation_mean + lat + long + dist_to_urban + distance + PA_area + prec + expo_time + inc_pcp_by_area"
form_inc <- "as.factor(Treat) + as.factor(name_biome) + as.factor(subclass) + Pop + elevation_mean + lat + long + dist_to_urban + distance + PA_area + prec + expo_time "
# PS: I need remove the scale because IT just has federal. So, the model won't run. 

out_glm<-list()
out_glm_summary<-list()
htcest_glm<-list()
bounds_psens<-list()
cluster_glm<-list()

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
      family = "gaussian"
    )
    
    out_glm[[model_id]] <- model
    out_glm_summary[[model_id]] <- summary(model)
    
    # error - heterocedasticity
    vcov_robust <- vcovHC(model,
                          type = "HC3")
    htcest_glm[[model_id]] <- coeftest(model,
                                       vcov = vcov_robust)
    
    # error - subclass cluster
    vcov_cluster <- vcovCL(model, 
                           cluster = ~ df$subclass)
    cluster_glm[[model_id]] <-coeftest(model, 
                                       vcov = vcov_cluster)
    
    # Rousebound
    psens_bounds <- psens(
      x = as.numeric(as.character(df$Treat)), 
      y = df[[outc]],
      Gamma = 3
    )
    bounds_psens[[model_id]] <- psens_bounds
  }
}
out_glm_summary$SUPAxSPA_water #naive
htcest_glm$SUPAxSPA_water # robust erro

#results
cluster_glm$SUPAxSPA_water #treat effect (-)
cluster_glm$SUPAxSPA_sanitation #treat effect (-)
cluster_glm$SUPAxSPA_waste  #treat effect (-)
cluster_glm$SUPAxSPA_inc_pcp_by_area
cluster_glm$SUPAxSPA_defor_amount

cluster_glm$ITxSPA_water 
cluster_glm$ITxSPA_sanitation #treat effect (-)
cluster_glm$ITxSPA_waste  #almost treat effect (-)
cluster_glm$ITxSPA_inc_pcp_by_area #treat effect (-)
cluster_glm$ITxSPA_defor_amount

cluster_glm$ITxSUPA_water 
cluster_glm$ITxSUPA_sanitation
cluster_glm$ITxSUPA_waste  
cluster_glm$ITxSUPA_inc_pcp_by_area #treat effect (-)
cluster_glm$ITxSUPA_defor_amount

bounds_psens # Except for children mortality, all the models has 1.
# 0.3 was the maximum score from children mortality models.


# Moran I test (spatial autocorrelation) ----

spt_weights<-list()

for (i in seq_along(matched_data)) {
  i_df<-matched_data[[i]] %>% 
    st_as_sf()
  i_df_names<-names(matched_data)[i]
  
  i_df_coords<-i_df %>% 
    mutate(point = st_centroid(geom),
           coords = st_coordinates(point))
  
  ktest <- knn2nb(knearneigh(i_df_coords$coords))
  k_test_dists <- unlist(nbdists(ktest, 
                                 i_df_coords$coords))
  print(summary(k_test_dists))
}

# Saving important outputs ----



