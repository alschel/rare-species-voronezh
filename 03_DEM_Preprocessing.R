# Rare species in Voronezh Oblast

# Задача - соединить, обрезать по границам региона и перепроецировать файл с высотами
# Рассчитать уклон и экспозицию

library(tidyverse)
library(raster)
library(terra)
library(sf)
library(mapview)
library(RColorBrewer)

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

# ============
# 1. Elevation
# ============

# List files
dem_files <- list.files("data/raw/ASTER_GDEM_v3/", pattern = ".tif$", full.names = T)

# Merge into single file
dem <- Reduce(merge, lapply(dem_files, FUN = raster))
dem <- rast(dem)

# Create land mask (the same we used for envirem variables)
land_mask <- raster("data/raw/CHELSA1.2/CHELSA_prec_01_V1.2_land.tif") %>% 
  crop(voronezh_admin_border_simplified) %>% 
  projectRaster(crs = crs(voronezh_admin_border_simplified_aea), res = 100, method = "bilinear") %>% 
  mask(mask = voronezh_admin_border_simplified_aea)

land_mask[land_mask < 0 ] <- NA
land_mask[!is.na(land_mask)] <- 1
land_mask <- rast(land_mask)

plot(land_mask)

writeRaster(raster(land_mask), filename = "data/processed/Elevation/land_mask.tif",
            format = 'GTiff', options = tifOptions, overwrite = TRUE)


# Обрежем по границе области и перепроецируем
elevation_100 <- dem %>% 
  project(land_mask) %>% 
  crop(land_mask) %>% 
  mask(land_mask)

plot(elevation_100)

# Сохраним в tif файл
writeRaster(raster(elevation_100), filename = "data/processed/Elevation/elevation_100.tif",
            format = 'GTiff', options = tifOptions, overwrite = TRUE)

# 500 м
elevation_500 <- dem %>% 
  crop(ext(38, 43, 49, 52.5)) %>% 
  project(land_mask) %>% 
  aggregate(fact = 5, fun = "mean")

plot(elevation_500)

writeRaster(raster(elevation_500), filename = "data/processed/Elevation/elevation_500.tif",
            format = 'GTiff', options = tifOptions, overwrite = TRUE)

# 1000 m
elevation_1000 <- dem %>% 
  crop(ext(38, 43, 49, 52.5)) %>% 
  project(land_mask) %>% 
  aggregate(fact = 10, fun = "mean")

plot(elevation_1000)

writeRaster(raster(elevation_1000), filename = "data/processed/Elevation/elevation_1000.tif",
            format = 'GTiff', options = tifOptions, overwrite = TRUE)

# ===================
# 2. Slope and aspect
# ===================

elevation_100 <- rast("data/processed/Elevation/elevation_100.tif")
elevation_500 <- rast("data/processed/Elevation/elevation_500.tif")
elevation_1000 <- rast("data/processed/Elevation/elevation_1000.tif")
land_mask <- rast("data/processed/Elevation/land_mask.tif")

# 100 m
slope_100 <- terrain(elevation_100, v="slope", neighbors=8, unit="degrees") %>% crop(land_mask) %>% mask(land_mask)
aspect_100 <- terrain(elevation_100, v="aspect", neighbors=8, unit="degrees") %>% crop(land_mask) %>% mask(land_mask)
plot(aspect_100)

# 500 m
slope_500 <- terrain(elevation_500, v="slope", neighbors=8, unit="degrees")
aspect_500 <- terrain(elevation_500, v="aspect", neighbors=8, unit="degrees")
# Dissolve back to 100m resolution
slope_500 <- disaggregate(slope_500, fact = 5) %>% crop(land_mask) %>% mask(land_mask)
aspect_500 <- disaggregate(aspect_500, fact = 5) %>% crop(land_mask) %>% mask(land_mask)

# В 1 км разрешении
slope_1000 <- terrain(elevation_1000, v="slope", neighbors=8, unit="degrees")
aspect_1000 <- terrain(elevation_1000, v="aspect", neighbors=8, unit="degrees")
# Dissolve back to 100m resolution
slope_1000 <- disaggregate(slope_1000, fact = 10) %>% crop(land_mask) %>% mask(land_mask)
aspect_1000 <- disaggregate(aspect_1000, fact = 10) %>% crop(land_mask) %>% mask(land_mask)


# Сохраним в tif
writeRaster(raster(slope_100), filename = "data/processed/Elevation/slope_100.tif",
            format = 'GTiff', options = tifOptions, overwrite = TRUE)
writeRaster(raster(aspect_100), filename = "data/processed/Elevation/aspect_100.tif",
            format = 'GTiff', options = tifOptions, overwrite = TRUE)
writeRaster(raster(slope_500), filename = "data/processed/Elevation/slope_500.tif",
            format = 'GTiff', options = tifOptions, overwrite = TRUE)
writeRaster(raster(aspect_500), filename = "data/processed/Elevation/aspect_500.tif",
            format = 'GTiff', options = tifOptions, overwrite = TRUE)
writeRaster(raster(slope_1000), filename = "data/processed/Elevation/slope_1000.tif",
            format = 'GTiff', options = tifOptions, overwrite = TRUE)
writeRaster(raster(aspect_1000), filename = "data/processed/Elevation/aspect_1000.tif",
            format = 'GTiff', options = tifOptions, overwrite = TRUE)


# Categorize aspect values
aspect_100 <- raster("data/processed/Elevation/aspect_100.tif")
aspect_500 <- raster("data/processed/Elevation/aspect_500.tif")
aspect_1000 <- raster("data/processed/Elevation/aspect_1000.tif")

aspect_1000_cat <- reclassify(aspect_1000, 
                              c(0, 22.5, 1, 
                                22.5, 67.5, 2,
                                67.5, 112.5, 3,
                                112.5, 157.5, 4,
                                157.5, 202.5, 5, 
                                202.5, 247.5, 6,
                                247.5, 292.5, 7,
                                292.5, 337.5, 8,
                                337.5, 360, 1))
aspect_500_cat <- reclassify(aspect_500, 
                              c(0, 22.5, 1, 
                                22.5, 67.5, 2,
                                67.5, 112.5, 3,
                                112.5, 157.5, 4,
                                157.5, 202.5, 5, 
                                202.5, 247.5, 6,
                                247.5, 292.5, 7,
                                292.5, 337.5, 8,
                                337.5, 360, 1))
aspect_100_cat <- reclassify(aspect_100, 
                             c(0, 22.5, 1, 
                               22.5, 67.5, 2,
                               67.5, 112.5, 3,
                               112.5, 157.5, 4,
                               157.5, 202.5, 5, 
                               202.5, 247.5, 6,
                               247.5, 292.5, 7,
                               292.5, 337.5, 8,
                               337.5, 360, 1))


plot(aspect_1000_cat, col = topo.colors(8))


# Create table for levels/labels for rasters
rat <- levels(aspect_1000_cat)[[1]]
rat[["class"]] <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")

# Change rasters to factor
aspect_1000_cat <- ratify(aspect_1000_cat)
aspect_500_cat <- ratify(aspect_500_cat)
aspect_100_cat <- ratify(aspect_100_cat)

# Add table with classes to layers
levels(aspect_1000_cat) <- rat
levels(aspect_500_cat) <- rat
levels(aspect_100_cat) <- rat

## Plot
rasterVis::levelplot(aspect_1000_cat, col.regions= c(rev(brewer.pal(4, "Spectral")), brewer.pal(4, "Spectral")))

# Write as tif files
writeRaster(aspect_100_cat, filename = paste0("data/processed/Elevation/aspect_100_cat.tif"),
            format = 'GTiff', options = tifOptions, overwrite = TRUE)
writeRaster(aspect_500_cat, filename = paste0("data/processed/Elevation/aspect_500_cat.tif"),
            format = 'GTiff', options = tifOptions, overwrite = TRUE)
writeRaster(aspect_1000_cat, filename = paste0("data/processed/Elevation/aspect_1000_cat.tif"),
            format = 'GTiff', options = tifOptions, overwrite = TRUE)

# ===================
# 3. Points
# ===================
load("data/processed/aea_voronezh.Rdata")

observations <- read_sf("data/raw/Observations/observations.shp")
plot(observations)
observations %>% st_transform(crs = aea_voronezh) -> observations

save(observations, file = "data/processed/observations.Rdata")

plot(observations)
