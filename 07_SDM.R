# SDM

library(tidyverse)
library(raster)
library(sf)
library(mapview)
library(sdm)
library(RColorBrewer)

# ================
# 0. Preprocessing
# ================

# Настройки raster
if (!dir.exists("data/Temp_for_raster")) { dir.create("data/Temp_for_raster") }
rasterOptions(tmpdir = 'data/Temp_for_raster/')
# supply some additional flags to writeRaster() to employ compression to achieve smaller file sizes
tifOptions <- c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=6")

# Admin border
load("data/processed/voronezh_admin_border.Rdata")

# # Observations
# load("data/processed/observations.Rdata")
# # Add column Occurrence and set it equal to 1
# observations %>% 
#   mutate(Occurrence = 1) %>% 
#   dplyr::select(Occurrence) -> observations
# 
# # Create random points that represent locations with no observations
# set.seed(12)
# random_points <- st_sample(voronezh_admin_border_simplified_aea, 100)
# 
# # Add column Occurrence and set it equal to 0
# random_points %>% 
#   st_as_sf() %>% 
#   mutate(Occurrence = 0) %>% 
#   dplyr::select(Occurrence) %>% 
#   as(., "Spatial") %>% 
#   st_as_sf() -> random_points
# 
# # Merge observations and random points
# observations %>% 
#   rbind(random_points) -> species
# 
# # Change to sp object
# species <- as(species, "Spatial")

# Observations
load("data/processed/BV_observations_upd_aea.Rdata")

# Change to sp object
species <- as(BV_observations_upd_aea[,"Occurrence"], "Spatial")

# Predictors
predictors_file <- list.files("data/processed/Predictors_02/", full.names = T)
predictors <- stack(predictors_file)

# ================
# 1. SDM
# ================

# Prepare data
d <- sdmData(formula = Occurrence~., train = species, predictors = predictors)
save(d, file = "data/processed/sdm_data.Rdata")

# SDM
m1 <- sdm(Occurrence~., data=d, 
          methods = c('glm'),  # тип модели
          replicatin = 'sub',  # subsampling
          test.percent = 25,   # размер проверочной выборки в %
          n = 5)               # пять итераций


m2 <- sdm(Occurrence~., data=d, 
          methods = c('glm'),  # тип модели
          replicatin = 'cv',   # cross-validation
          cv.folds = 5,       
          n = 2)               # пять итераций

# Check the results
m1
m2

# ROC
roc(m1)
# Get the importance of variables
sdm::getVarImp(m1)
# ROC
roc(m2)
# Get the importance of variables
sdm::getVarImp(m2)

# Create map of prediction
p1 <- predict(m1, newdata=predictors)
plot(p1, col = rev(brewer.pal(name = "Spectral", n = 10)))
p2 <- predict(m2, newdata=predictors)
plot(p2, col = rev(brewer.pal(name = "Spectral", n = 10)))

# writeRaster(p1, filename = "data/processed/Predictions/glm_sub.tif",
#             format = 'GTiff', options = tifOptions, overwrite = TRUE)
# writeRaster(p2, filename = "data/processed/Predictions/glm_cv.tif",
#             format = 'GTiff', options = tifOptions, overwrite = TRUE)

# Ensemble model
e1 <- ensemble(m1, 
               newdata=predictors,
               setting=list(method='weighted',stat='AUC'))
Sys.time()
plot(e1, col = rev(brewer.pal(name = "Spectral", n = 10)), main = "Ensemle prediction")



