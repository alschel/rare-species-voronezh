# Задача - рассчитать переменные osm для Воронежской области

library(dplyr)
library(envirem)
library(raster)
library(sf)
library(mapview)
library(viridis)

# ================
# 1. Preprocessing
# ================

# We are using CHELSA 1.2 data: precipitation, min t, max t and average t
list.files("data/raw/CHELSA1.2/")

# In order not to fill up hard drive, whin working with large rasters,
# we create and define temporary directory, where raster package can store temporary rasters.
if (!dir.exists("data/Temp_for_raster")) { dir.create("data/Temp_for_raster") }
rasterOptions(tmpdir = 'data/Temp_for_raster/')
# supply some additional flags to writeRaster() to employ compression to achieve smaller file sizes
tifOptions <- c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=6")

# Projection: Albers equal area
load("data/processed/aea_voronezh.Rdata")

# Administrative border of Voronezh Oblast
load("data/processed/voronezh_admin_border.Rdata")

# =======================
# 1.1. Creating land mask

# We need to crop rasters by land mask of Voronezh oblast
# For that we use precipitation file masked by administrative border

# Create land mask
land_mask <- raster("data/raw/CHELSA1.2/CHELSA_prec_01_V1.2_land.tif") %>% 
  crop(voronezh_admin_border_simplified) %>% 
  projectRaster(crs = crs(voronezh_admin_border_simplified_aea), res = 100, method = "bilinear") %>% 
  mask(mask = voronezh_admin_border_simplified_aea)

land_mask[land_mask < 0 ] <- NA
land_mask[!is.na(land_mask)] <- 1

# ==============================
# 1.2. CHELSA data preprocessing


input_dir <- "data/raw/CHELSA1.2/"
output_dir <- "data/raw/CHELSA_Base_data/"

for (i in list.files(input_dir)) {
  print(i)
  r <- raster(paste0(input_dir, i))
  r %>% crop(voronezh_admin_border_simplified) %>% 
    projectRaster(crs = crs(voronezh_admin_border_simplified_aea), res = 100, method = "bilinear") %>% 
    mask(land_mask) -> r
  
  # Change units in temperature files from C*10 to C*1
  if (grepl('tmin|tmax|temp', i)) {
    r <- r / 10
  }
  
  writeRaster(r, 
              filename = paste0(output_dir, sub("-", "_", i)), # fix file name to avoid error in envirem
              format = 'GTiff', options = tifOptions, overwrite = TRUE)
}

# ===============================
# 2. Generating ENVIREM variables
# ===============================

# Set varibales names
assignNames(tmax = "CHELSA_tmax10_##_1979_2013_V1.2_land",
            tmin = "CHELSA_tmin10_##_1979_2013_V1.2_land",
            tmean = "CHELSA_temp10_##_1979_2013_V1.2_land",
            precip = "CHELSA_prec_##_V1.2_land",
            solrad = "srad_##"
)
# Check the namespace
varnames()

# Load solar radiation data
# read in a climatic raster for use as a template
rasterTemplate <- raster('data/raw/CHELSA_Base_data/CHELSA_prec_01_V1.2_land.tif')
# calculate monthly solar radiation
ETsolradRasters(rasterTemplate = rasterTemplate, 
                year = 40,   # recommended for CHELSA data
                outputDir = output_dir, overwrite = TRUE)

# Verify file structure and raster names
verifyFileStructure('data/raw/CHELSA_Base_data/', returnFileNames = T)
verifyRasterNames('data/raw/CHELSA_Base_data/')

# create output directory
if (!dir.exists("data/processed/ENVIREM_vars")) { 
  dir.create("data/processed/ENVIREM_vars") 
}

# Generate rasters
generateRasters(var = 'all',
                maindir = "data/raw/CHELSA_Base_data/",
                outputDir = "data/processed/ENVIREM_vars/",
                gdalinfoPath = "/usr/local/bin/gdalinfo", 
                gdal_translatePath = "/usr/local/bin/gdal_translate", 
                outputFormat = 'GTiff', useCompression = T, overwriteResults = T)

# Check
envirem_vars <- stack(list.files(path = "data/processed/ENVIREM_vars/", full.names = T)) 

par(mfrow = c(4, 4), mar = c(0.5, 0.5, 2, 0))

for (i in 1:nlayers(envirem_vars)) {
  plot(envirem_vars[[i]], col = inferno(100), box = FALSE, axes = FALSE)
  title(main = names(envirem_vars)[i])
}
