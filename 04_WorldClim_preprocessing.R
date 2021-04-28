# Подготовка даных WorldClim для анализа
# Задача - обрезать и перепроецировать данные

library(tidyverse)
library(raster)
library(terra)
library(sf)
library(mapview)

# ================
# 0. Preprocessing
# ================

# Настройки raster и terra
if (!dir.exists("data/Temp_for_raster")) { dir.create("data/Temp_for_raster") }
terraOptions(tempdir = 'data/Temp_for_raster/')
rasterOptions(tmpdir = 'data/Temp_for_raster/')
# supply some additional flags to writeRaster() to employ compression to achieve smaller file sizes
tifOptions <- c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=6")

# Admin border
load("data/processed/voronezh_admin_border.Rdata")

# Land mask
land_mask <- rast("data/processed/Elevation/land_mask.tif")
plot(land_mask)

# ================
# 1. Preprocessing
# ================

list.files("data/raw/WorldClim2.1./")
# We need min and mean temp in March and April
worldclim_files <- c("wc2.1_30s_tmin_03.tif", "wc2.1_30s_tmin_04.tif",
                     "wc2.1_30s_tavg_03.tif", "wc2.1_30s_tavg_04.tif")


input_dir <- "data/raw/WorldClim2.1./"
output_dir <- "data/processed/WorldClim/"


# Loop over files
for (i in worldclim_files) {
  print(i)
  r <- rast(paste0(input_dir, i))
  r <- r %>% crop(voronezh_admin_border_simplified) %>% 
    project(land_mask) %>% 
    crop(land_mask) %>% 
    mask(land_mask)
  
  writeRaster(raster(r), 
              filename = paste0(output_dir, i), # fix file name to avoid error in envirem
              format = 'GTiff', options = tifOptions, overwrite = TRUE)
}

