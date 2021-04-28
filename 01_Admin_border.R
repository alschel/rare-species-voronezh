# Задача - выгрузить гарницы Воронежской области из osm
# Определить проекцию проекта

library(tidyverse)
library(sf)
library(osmdata)
library(mapview)
library(rmapshaper)

# ========================
# 1. Administrative border
# ========================

# Download osm data on administrative border of Voronezh Oblast
voronezh_admin_border <- opq(bbox = "Воронежская область") %>% 
  add_osm_feature(key = "admin_level", value = 4) %>% 
  osmdata_sf()

# # In case of 451 error change server
# set_overpass_url("https://overpass.kumi.systems/api/interpreter")

# Filter the region, remove attributes we do not need
voronezh_admin_border$osm_multipolygons %>%
  filter(name == "Воронежская область") %>% 
  select(name, int_name) %>% as('Spatial') %>% st_as_sf() -> voronezh_admin_border

# Simplify geometry
voronezh_admin_border %>% 
  ms_simplify(keep = 0.08, weighting = 0.5, 
              keep_shapes = T, explode = F) -> voronezh_admin_border_simplified

mapview(voronezh_admin_border_simplified)

# ========================
# 2. Equal Area Projection
# ========================

voronezh_admin_border_simplified
# Центральный меридиан
mean(38.13775, 42.94734) # 38.13775
# Параллели
49.55795 + (52.10305 -  49.55795)/6  # 49.98213
52.10305 - (52.10305 -  49.55795)/6  # 51.67887

# В качестве рабочей проекции мы будем использовать равновеликую коническую Альберса
aea_voronezh <- "+proj=aea +lat_1=49.98213 +lat_2=51.67887 +lat_0=0 +lon_0=38.13775 +x_0=8500000 +y_0=0 +ellps=krass +units=m +towgs84=28,-130,-95,0,0,0,0 +no_defs"

# Перепроецируем границы
voronezh_admin_border %>% st_transform(aea_voronezh) -> voronezh_admin_border_aea
voronezh_admin_border_simplified %>% st_transform(aea_voronezh) -> voronezh_admin_border_simplified_aea

# Save as Rdata files
save(aea_voronezh, file = "data/processed/aea_voronezh.Rdata")

save(voronezh_admin_border, voronezh_admin_border_aea, 
     voronezh_admin_border_simplified, voronezh_admin_border_simplified_aea, 
     file = "data/processed/voronezh_admin_border.Rdata")