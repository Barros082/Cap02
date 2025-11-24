# Robutness checking

library(sf)
library(tidyverse)
library(lmtest)
library(sandwich)
library(rbounds)

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
out_glm_summary$SUPAxSPA_water #treat effect
htcest_glm$SUPAxSPA_water
cluster_glm$SUPAxSPA_water #treat effect


bounds_psens # Except for children mortality, all the models has 1.
# 0.3 was the maximum score from children mortality models.

# Moran I test (spatial autocorrelation) ----