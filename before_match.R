# preparing data to matching

library(tidyverse)
library(sf)
library(corrplot)

PA_data<-readRDS("Outputs/PA_balanced_with_incpcp.rds") %>% #591
  filter(!new_cat%in%c("RPPN", "ARIE", "APA")) %>%  #557
  filter(!name_biome%in%c("Pampa", "Pantanal")) %>% #550
  select(-code_tract, -lit) %>% 
  glimpse

# correlation ----
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

# splitting data by binomial treatments comparisons ---- 
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

PA_matching_list_step02[[2]] %>% 
  filter(new_code=="IT_5517") %>% # too small that we can't see
  st_as_sf() %>% 
  ggplot()+geom_sf()

# removing UCS with overposition ----
data_to_match<-list()
for (i in seq_along(PA_matching_list_step02)) {
  df_i<-PA_matching_list_step02[[i]]
  
  if(i==1){
    data_to_match[[i]]<-df_i %>% 
      filter(!new_code%in%c(
        "UC_00221", "UC_50220", 
        "UC_50405", "UC_50225",
        "UC_50687", "UC_50392"
      ))
  }
  if(i==2){
    data_to_match[[i]]<-df_i %>% 
      filter(!new_code%in%c(
        "IT_50100", "UC_00027",
        "IT_6840", "UC_00043",
        "IT_5800", "UC_00124",
        "IT_50048", "UC_00159", 
        "IT_8656",
        "IT_8699",
        "IT_5517", "UC_00175", 
        "IT_8524", "UC_00221",
        "IT_6610", "UC_01988",
        "IT_50340", "UC_03360",
        "IT_50587", 
        "IT_9474", "UC_11185"
      ))
  }
  if(i==3){
    data_to_match[[i]]<-df_i %>% 
      filter(!new_code%in%c(
        "IT_8656", "UC_11193",
        "IT_8532", "UC_14320", 
        "IT_8788", 
        "UC_14583",
        "IT_7609", "UC_15903", 
        "IT_50014", "UC_50205", 
        "IT_9580",
        "IT_9504", "UC_50208", 
        "IT_50011", "UC_50209", 
        "IT_50455",
        "IT_9589",
        "IT_50025", "UC_50214", 
        "IT_8524", "UC_50220", 
        "IT_9423", "UC_50375",
        "IT_9490",
        "IT_14923", "UC_50407", 
        "IT_6696"
      ))
  }
}

lapply(data_to_match, function(x){
  df_x<-x %>%
    group_by(name_biome, new_cat) %>% 
    summarise(n_distinct(new_code))
})

names(data_to_match) = PA_matching_names

#saving
saveRDS(data_to_match, "Outputs/Data_to_match.rds")
