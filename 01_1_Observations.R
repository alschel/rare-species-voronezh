# Подготовка данных о наблюдениях

library(tidyverse)
library(sf)
library(mapview)

# Проекция
load("data/processed/aea_voronezh.Rdata")

# ============
# 1. Брандушка
# ============

observations <- read_sf("data/raw/Localities/Observations/observations.shp")
plot(observations)
observations %>% st_transform(crs = aea_voronezh) -> observations

save(observations, file = "data/processed/observations.Rdata")

plot(observations)

# ===========================
# 2. Брандушка - upd Jan 2022
# ===========================

# Read raw data
BV_observations_upd <- read_sf("data/raw/Localities/Bulbocodium_versicolor_upd_Jan2022/Bulbocodium_versicolor_obnovlenny_nabor/проба.shp", 
                               options = "ENCODING=CP1251")

# Change colnames
BV_observations_upd %>% 
  mutate(Species = "Bulbocodium versicolor",
         Occurrence = 1,
         Quality = id,
         Description = discript) %>% 
  select(-id, -discript) -> BV_observations_upd

# Read data on absent locations
BV_absence <- read_csv("data/raw/Localities/Bulbocodium_versicolor_upd_Jan2022/BV_absence.csv", 
                       col_types = c("cdddcd"))

# Create sf obj
BV_absence <- st_as_sf(BV_absence, coords = c("Lon", "Lat"), crs = 4326, agr = "constant")

# Merge layers and project 
BV_observations_upd %>% 
  rbind(BV_absence) %>% 
  st_transform(aea_voronezh) -> BV_observations_upd_aea

mapview(BV_observations_upd_aea)

# Save as Rdata file
save(BV_observations_upd_aea, file = "data/processed/BV_observations_upd_aea.Rdata")

# ==========================================
# 3. Paeonia tenuifolia L (Пион узколистный)
# ==========================================

# Read raw data
PTL_observations <- read_sf("data/raw/Localities/Paeonia_tenuifolia_L/Paeonia tenuifolia.shp", 
                               options = "ENCODING=CP1251")

# Change colnames and project
PTL_observations %>% 
  mutate(Species = "Paeonia tenuifolia L",
         Occurrence = 1,
         Quality = `источник`,
         Description = `метаданные`) %>% 
  select(-id, -`источник`, -`метаданные`) %>% 
  st_transform(aea_voronezh) -> PTL_observations_aea

# Save as Rdata file
save(PTL_observations_aea, file = "data/processed/PTL_observations_aea.Rdata")
