# Robutness checking

library(sf)
library(tidyverse)


# linear models after matching ----

initial_data<-readRDS("Outputs/PA_balanced_with_incpcp.rds") %>% #1122
  filter(!new_cat%in%c("RPPN", "ARIE", "APA")) %>% 
  filter(!name_biome%in%c("Pampa", "Pantanal")) %>% 
  glimpse

matched_data<-readRDS("Outputs/Matched_data.rds")


y_lm<-c("water", "lit", "sanitation", "waste", 
     "dead_less1year", "inc_pcp_by_area", 
     "defor_amount")

form_base<- Treat + Pop + elevation_mean +
  dist_to_urban + lat + long + distance + weights +
  PA_area + prec + expo_time + scale


# Rousebound sensitive test ----

# Moran I test (spatial autocorrelation) ----