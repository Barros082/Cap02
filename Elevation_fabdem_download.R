library(rvest)
library(tidyverse)
library(sf)
sf::sf_use_s2(F)

malha_cat<-read_sf("Outputs/CAT_malha_PA_completed.gpkg") %>% glimpse()

fbdemdata<-read_sf("DATA/FABDEM_data_elevation/FABDEM_v1-2_tiles.geojson") %>%
  st_transform(., "EPSG:4674") %>% 
  glimpse()

polygons_cat<-st_intersection(fbdemdata, malha_cat) %>% 
  mutate(across(.cols=c(1:3), ~as.factor(.))) %>% 
  glimpse()

#polygons_cat %>% ggplot()+ geom_sf() # to see

# zip that i need to download
zipfilenames<-levels(unique(polygons_cat$zipfile_name)) %>% 
  glimpse()

# and the tif archives that we need (122 0.0)
levels(unique(polygons_cat$file_name)) %>% 
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

dir.create("DATA/FABDEM_data_elevation")
setwd("DATA/FABDEM_data_elevation")

options(timeout = Inf)

links_1 <- links[1]
links_2 <- links[2]
links_3 <- links[3]
links_4 <- links[4]

for (link in links_1) {
  file_name <- basename(links_1)
  download.file(links_1, file_name, mode = "wb")
}

for (link in links_2) {
  file_name <- basename(links_2)
  download.file(links_2, file_name, mode = "wb")
}

for (link in links_3) {
  file_name <- basename(links_3)
  download.file(links_3, file_name, mode = "wb")
}


for (link in links_4) {
  file_name <- basename(links_4)
  download.file(links_4, file_name, mode = "wb")
}
