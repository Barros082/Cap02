library(rvest)
library(tidyverse)
library(sf)
library(geobr)
sf::sf_use_s2(F)

fbdemdata<-read_sf("DATA/FABDEM_data_elevation/FABDEM_v1-2_tiles.geojson") %>%
  st_transform(., "EPSG:4674") %>% 
  glimpse()

PA_shape<-read_sf("Outputs/PA_IT_shape.gpkg")%>% 
  st_transform(., "EPSG:4674") %>% 
  select(1)

polygons_PA<-st_intersection(fbdemdata, PA_shape) %>% 
  mutate(across(.cols=c(1:3), ~as.factor(.))) %>% 
  glimpse()

#polygons_PA %>% ggplot()+ geom_sf() # to see

# zip that i need to download
zipfilenames<-levels(unique(polygons_PA$zipfile_name)) %>% 
  glimpse() #17

# and the tif archives that we need (670 0.0)
levels(unique(polygons_PA$file_name)) %>% 
  glimpse()

code_to_filter <- zipfilenames %>% 
  gsub("-S-", "-S", .) %>% 
  paste(., collapse = "|")

webpage <- read_html("https://data.bris.ac.uk/data/dataset/s5hqmjcdj8yo2ibzi9b4ew3sn")

links <- webpage %>%
  html_elements("a") %>%
  html_attr("href") %>%
  str_subset(code_to_filter) %>%
  unique()

#dir.create("DATA/FABDEM_data_elevation")
setwd("DATA/FABDEM_data_elevation")

options(timeout = Inf)

for (link in links) {
  file_name <- basename(link)
  download.file(link, file_name, mode = "wb")
}

