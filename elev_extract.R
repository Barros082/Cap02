# Elevation vars extraction

library(here)
library(tidyverse)
library(sf)        
library(raster)   
library(terra)
library(exactextractr)
library(geobr)

sf::sf_use_s2(F)
Sys.setenv(TMPDIR = "D:/Arthur_Barros/Doutorado/CAP02/Cap02/temp")
terraOptions(tempdir = "D:/Arthur_Barros/Doutorado/CAP02/Cap02/temp")

PA_shape<-read_sf("Outputs/PA_IT_shape.gpkg")

elev_fabdem<-read_sf("DATA/FABDEM_data_elevation/FABDEM_v1-2_tiles.geojson") %>% 
  st_transform("EPSG:4674")

files_br <- st_intersection(elev_fabdem, PA_shape %>% 
                              dplyr::select(new_code)) %>% 
  glimpse


tile_files <- files_br %>%
  dplyr::select(new_code, file_name, zipfile_name) %>%
  distinct()

dir("DATA/FABDEM_data_elevation")

folders <- list.dirs("DATA/FABDEM_data_elevation", recursive = FALSE)

files_all <- unlist(lapply(folders, function(x) {
  list.files(x, pattern = "\\.tif$", full.names = TRUE)
}))

# filtering

path_code<-tile_files %>%  
  mutate(dum01="DATA", dum02="FABDEM_data_elevation", 
         dum03=str_remove(zipfile_name, "\\.zip$"),
         path=paste(dum01, dum02, dum03, file_name, sep="/"), 
         new_code=as.factor(new_code)) %>% 
  dplyr::select(new_code, path) %>% 
  glimpse

#for cada new_code aqui, temoos que abrir os raster deles
#colocar o raster no crs 4674
#agregar o crs na escala de 60 usando a media
#agrupar eles em um único raster por new_code
#e salvar esses raster em uma lista com base no new_code
#depois nos vamos voltar ao dado de origem (PA_shape) que tem uma geometria pra cada new_code
#usando um for com base nos new_code, nos vamos fazer com o para cada geom no PA_Shape consigamos extrair o desvio padrão da elevação
#e depois vamos juntar tudo isso em um único dado organizdo por new_Code
#e depois agrupar eles com o nosso dado original chamado PA_shape


rasters_by_code <- path_code %>%
  split(.$new_code) %>%
  lapply(function(df) {
    
    raster_list <- lapply(df$path, function(p) {
      r <- terra::rast(p)
      r <- terra::project(r, "EPSG:4674", method = "bilinear")
      r <- terra::aggregate(r, fact = 2, fun = "mean") # 60m
      return(r)
    })
    r_merged <- do.call(terra::merge, raster_list)
    return(r_merged)
  })
# o erro tá no nome. Alguns tem 0 antes do 23, 23, outros n tem. E o nome de algumas pastas estão errados. É por isso q n ta dando certo
names(rasters_by_code) <- names(split(path_code, path_code$new_code))


extract_results <- lapply(names(rasters_by_code), function(code) {
  message("Processando ", code)
  r <- rasters_by_code[[code]]
  
  pa <- PA_shape %>% filter(new_code == code)
  
  values <- exactextractr::exact_extract(r, pa, fun = "stdev")
  
  tibble::tibble(new_code = code, stdev = values)
})

results_df <- dplyr::bind_rows(extract_results)

# --- 3. Juntar resultados com o shapefile original ---
PA_with_elevation <- PA_shape %>%
  left_join(results_df, by = "new_code")



# garbage -----

# Data - PA
PA_shape<-read_sf("Outputs/PA_IT_shape.gpkg")

elev_fabdem<-read_sf("DATA/FABDEM_data_elevation/FABDEM_v1-2_tiles.geojson") %>% 
  st_transform("EPSG:4674")

files_br <- st_intersection(elev_fabdem, PA_shape %>% 
                              dplyr::select(new_code)) %>% 
  glimpse

# Data - Elevation
Sys.setenv(TMPDIR = "D:/Arthur_Barros/Doutorado/CAP02/Cap02/temp")
terraOptions(tempdir = "D:/Arthur_Barros/Doutorado/CAP02/Cap02/temp")

dir("DATA/FABDEM_data_elevation")
folders <- list.dirs("DATA/FABDEM_data_elevation", recursive = FALSE)

files_all <- unlist(lapply(folders, function(x) {
  list.files(x, pattern = "\\.tif$", full.names = TRUE)
}))

raster_elevation<-lapply(files_all, function(r){
  r<-terra::rast(r)
  r<-terra::project(r, "EPSG:4674", method="bilinear")
  r<-terra::aggregate(r, fact=2, fun="mean") # 60 m resolution
})

sapply(raster_elevation, terra::ext) # different extensions
sapply(raster_elevation, terra::res) 

#

files_br <- st_intersection(elev_fabdem, PA_shape %>% 
                              dplyr::select(new_code)) %>% 
  st_drop_geometry() %>%
  as_tibble()


process_uc <- function(uc_code, uc_geom, tile_files) {
  cat("Processando UC:", uc_code, "\n")
  
  # Lista de arquivos para esta UC
  uc_files <- tile_files %>%
    filter(new_code == uc_code) %>%
    pull(file_name) %>%
    unique()
  
  if (length(uc_files) == 0) {
    return(tibble(ID = uc_code, stdev = NA_real_))
  }
  
  # Carregar e combinar os rasters necessários
  rasters <- lapply(uc_files, function(file) {
    # Encontrar o caminho completo do arquivo
    full_path <- list.files(path = "DATA/FABDEM_data_elevation", 
                            pattern = file, 
                            recursive = TRUE, 
                            full.names = TRUE)
    
    if (length(full_path) == 0) {
      warning("Arquivo não encontrado: ", file)
      return(NULL)
    }
    
    r <- terra::rast(full_path[1])
    r <- terra::project(r, "EPSG:4674", method = "bilinear")
    r <- terra::aggregate(r, fact = 2, fun = "mean") # 60 m resolution
    return(r)
  })
  
  # Remover NULLs
  rasters <- rasters[!sapply(rasters, is.null)]
  
  if (length(rasters) == 0) {
    return(tibble(ID = uc_code, stdev = NA_real_))
  }
  
  # Se houver múltiplos rasters, fazer mosaic
  if (length(rasters) > 1) {
    # Criar um virtual raster para evitar carregar tudo na memória
    vrt_file <- tempfile(fileext = ".vrt")
    terra::vrt(rasters, filename = vrt_file, overwrite = TRUE)
    combined_raster <- terra::rast(vrt_file)
  } else {
    combined_raster <- rasters[[1]]
  }
  
  # Extrair estatísticas
  tryCatch({
    values <- exactextractr::exact_extract(combined_raster, uc_geom, fun = "stdev")
    tibble(ID = uc_code, stdev = values)
  }, error = function(e) {
    message("Erro na UC ", uc_code, ": ", e$message)
    tibble(ID = uc_code, stdev = NA_real_)
  })
}

tile_files <- files_br %>%
  dplyr::select(new_code, file_name) %>%
  distinct()

uc_codes <- unique(PA_shape$new_code)

results_list <- list()

for (i in seq_along(uc_codes)) {
  uc_code <- uc_codes[i]
  uc_geom <- PA_shape %>% filter(new_code == uc_code)
  
  result <- process_uc(uc_code, uc_geom, tile_files)
  results_list[[i]] <- result
  
  if (i %% 10 == 0) gc()
}

# Versão alternativa com lapply
results_list <- lapply(uc_codes, function(uc_code) {
  uc_geom <- PA_shape %>% filter(new_code == uc_code)
  process_uc(uc_code, uc_geom, tile_files)
})

# Combinar resultados
results <- bind_rows(results_list)

# Juntar de volta às PAs
PA_elevation <- PA_shape %>%
  left_join(results, by = c("new_code" = "ID"))





#2
process_uc <- function(uc_code, uc_geom, tile_files) {
  cat("Processando UC:", uc_code, "\n")
  
  # Lista de arquivos para esta UC
  uc_files <- tile_files %>%
    filter(new_code == uc_code) %>%
    pull(file_name) %>%
    unique()
  
  cat("Arquivos encontrados para esta UC:", paste(uc_files, collapse = ", "), "\n")
  
  if (length(uc_files) == 0) {
    cat("Nenhum arquivo encontrado para UC:", uc_code, "\n")
    return(tibble(ID = uc_code, stdev = NA_real_))
  }
  
  # Carregar e combinar os rasters necessários
  rasters <- lapply(uc_files, function(file) {
    cat("Buscando arquivo:", file, "\n")
    
    # Encontrar o caminho completo do arquivo
    full_path <- list.files(path = "DATA/FABDEM_data_elevation", 
                            pattern = file, 
                            recursive = TRUE, 
                            full.names = TRUE)
    
    cat("Caminhos encontrados:", paste(full_path, collapse = ", "), "\n")
    
    if (length(full_path) == 0) {
      warning("Arquivo não encontrado: ", file)
      return(NULL)
    }
    
    # Usar o primeiro arquivo encontrado
    cat("Carregando:", full_path[1], "\n")
    r <- terra::rast(full_path[1])
    r <- terra::project(r, "EPSG:4674", method = "bilinear")
    r <- terra::aggregate(r, fact = 2, fun = "mean")
    return(r)
  })
  
  # Remover NULLs
  rasters <- rasters[!sapply(rasters, is.null)]
  
  if (length(rasters) == 0) {
    cat("Nenhum raster válido carregado para UC:", uc_code, "\n")
    return(tibble(ID = uc_code, stdev = NA_real_))
  }
  
  cat("Número de rasters carregados:", length(rasters), "\n")
  
  # Se houver múltiplos rasters, fazer mosaic
  if (length(rasters) > 1) {
    cat("Combinando múltiplos rasters...\n")
    vrt_file <- tempfile(fileext = ".vrt")
    terra::vrt(rasters, filename = vrt_file, overwrite = TRUE)
    combined_raster <- terra::rast(vrt_file)
  } else {
    combined_raster <- rasters[[1]]
  }
  
  # Extrair estatísticas
  tryCatch({
    cat("Extraindo estatísticas...\n")
    values <- exactextractr::exact_extract(combined_raster, uc_geom, fun = "stdev")
    cat("Valor extraído:", values, "\n")
    tibble(ID = uc_code, stdev = values)
  }, error = function(e) {
    message("Erro na UC ", uc_code, ": ", e$message)
    tibble(ID = uc_code, stdev = NA_real_)
  })
}

# Testar com uma UC específica primeiro
test_uc <- uc_codes[1]  # Primeira UC
test_geom <- PA_shape %>% filter(new_code == test_uc)
test_files <- files_br %>% filter(new_code == test_uc)

cat("=== TESTE DA PRIMEIRA UC ===\n")
cat("UC:", test_uc, "\n")
cat("Geometria:", st_geometry_type(test_geom), "\n")
cat("Arquivos associados:", paste(test_files$file_name, collapse = ", "), "\n")

# Testar a função
test_result <- process_uc(test_uc, test_geom, tile_files)
print(test_result)


# 3
process_uc <- function(uc_code, uc_geom, tile_files) {
  cat("Processando UC:", uc_code, "\n")
  
  # Lista de arquivos para esta UC com informações da pasta
  uc_files_info <- tile_files %>%
    filter(new_code == uc_code) %>%
    dplyr::select(file_name, zipfile_name) %>%
    distinct()
  
  cat("Arquivos encontrados para esta UC:", paste(uc_files_info$file_name, collapse = ", "), "\n")
  
  if (nrow(uc_files_info) == 0) {
    cat("Nenhum arquivo encontrado para UC:", uc_code, "\n")
    return(tibble(ID = uc_code, stdev = NA_real_))
  }
  
  # Carregar e combinar os rasters necessários
  rasters <- lapply(1:nrow(uc_files_info), function(i) {
    file <- uc_files_info$file_name[i]
    zip_folder <- sub("\\.zip$", "", uc_files_info$zipfile_name[i])
    
    cat("Buscando arquivo:", file, "na pasta:", zip_folder, "\n")
    
    # Encontrar o caminho completo do arquivo dentro da pasta específica
    full_path <- list.files(path = file.path("DATA/FABDEM_data_elevation", zip_folder), 
                            pattern = file, 
                            recursive = FALSE, 
                            full.names = TRUE)
    
    # Se não encontrar na pasta específica, tentar busca recursiva geral
    if (length(full_path) == 0) {
      cat("Não encontrado na pasta específica, buscando recursivamente...\n")
      full_path <- list.files(path = "DATA/FABDEM_data_elevation", 
                              pattern = file, 
                              recursive = TRUE, 
                              full.names = TRUE)
    }
    
    cat("Caminhos encontrados:", paste(full_path, collapse = ", "), "\n")
    
    if (length(full_path) == 0) {
      warning("Arquivo não encontrado: ", file)
      return(NULL)
    }
    
    # Usar o primeiro arquivo encontrado
    cat("Carregando:", full_path[1], "\n")
    r <- terra::rast(full_path[1])
    r <- terra::project(r, "EPSG:4674", method = "bilinear")
    r <- terra::aggregate(r, fact = 2, fun = "mean")
    return(r)
  })
  
  # Remover NULLs
  rasters <- rasters[!sapply(rasters, is.null)]
  
  if (length(rasters) == 0) {
    cat("Nenhum raster válido carregado para UC:", uc_code, "\n")
    return(tibble(ID = uc_code, stdev = NA_real_))
  }
  
  cat("Número de rasters carregados:", length(rasters), "\n")
  
  # Se houver múltiplos rasters, fazer mosaic
  if (length(rasters) > 1) {
    cat("Combinando múltiplos rasters...\n")
    vrt_file <- tempfile(fileext = ".vrt")
    terra::vrt(rasters, filename = vrt_file, overwrite = TRUE)
    combined_raster <- terra::rast(vrt_file)
  } else {
    combined_raster <- rasters[[1]]
  }
  
  # Extrair estatísticas
  tryCatch({
    cat("Extraindo estatísticas...\n")
    values <- exactextractr::exact_extract(combined_raster, uc_geom, fun = "stdev")
    cat("Valor extraído:", values, "\n")
    tibble(ID = uc_code, stdev = values)
  }, error = function(e) {
    message("Erro na UC ", uc_code, ": ", e$message)
    tibble(ID = uc_code, stdev = NA_real_)
  })
}

# Atualizar o tile_files para incluir zipfile_name
tile_files <- files_br %>%
  dplyr::select(new_code, file_name, zipfile_name) %>%
  distinct()

# Testar novamente
test_result <- process_uc(test_uc, test_geom, tile_files)
print(test_result)







# old
unido_elevation <- do.call(terra::merge, raster_elevation)

plot(unido_elevation)

# cropping and masking to our shape

crop_and_mask<-function(r){
  r_crop <- crop(r, PA_shape)
  r_masked <- mask(r_crop, PA_shape)
  
  return(r_masked)
}

raster_elevation <- crop_and_mask(unido_elevation)

plot(raster_elevation)

PA_elevation<-PA_shape %>% 
  mutate(elevation_area=exact_extract(raster_elevation, ., fun = "stdev"))

write_sf(PA_elevation, "Outputs/PA_elevation.gpkg")
