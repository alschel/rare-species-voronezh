# Задача - определить значимость экспозиции для расопложения наблюдений

library(tidyverse)
library(raster)
library(terra)
library(sf)
library(mapview)
library(ggsci)

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

# Observations
load("data/processed/observations.Rdata")

# Aspect
aspect_files <- c("data/processed/Elevation/aspect_500_cat.tif", 
                  "data/processed/Elevation/aspect_1000_cat.tif")

aspect_cat <- stack(aspect_files)
plot(aspect_cat)

# ===============
# 1. Calculations
# ===============

# Distribution of values within the region
aspect_500_vals <- getValues(aspect_cat[[1]]) %>% na.omit() %>% table()
aspect_1000_vals <- getValues(aspect_cat[[2]]) %>% na.omit() %>% table()

# Merge
as.data.frame(aspect_500_vals) %>% 
  mutate(aspect = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
         region = Freq,
         scale = "500") %>% 
  tibble() %>% 
  dplyr::select(aspect, region, scale) %>% 
  full_join(
    as.data.frame(aspect_500_vals) %>% 
      mutate(aspect = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
             region = Freq,
             scale = "1000") %>% 
      tibble() %>% 
      dplyr::select(aspect, region, scale)
  ) -> aspect_vals_region


# Extract values by observations
aspect_obs_vals <- extract(aspect_cat, as(observations, "Spatial")) %>% as.data.frame()
colnames(aspect_obs_vals) <- c("500", "1000")

# Calculate number of observations by aspect
aspect_obs_vals %>% 
  pivot_longer(`500`:`1000`, names_to = "scale", values_to = "aspect") %>% 
  group_by(aspect, scale) %>% 
  summarise(observations = n()) %>% 
  arrange(scale, aspect) %>% 
  ungroup() %>% 
  mutate(aspect = rep(c("N", "NE", "E", "SE", "S", "SW", "W", "NW"), 2)) -> aspect_obs_vals

# Merge two tables
aspect_vals_region %>% 
  left_join(aspect_obs_vals) %>% 
  dplyr::select(scale, aspect, region, observations) %>% 
  pivot_longer(region:observations, names_to = "type", values_to = "n") -> aspect_vals

# Calculation portions
aspect_vals %>% 
  group_by(scale, type) %>% 
  mutate(portion = n / sum(n)) %>% 
  ungroup() -> aspect_vals

# Change aspect two factor
aspect_vals %>% 
  mutate(aspect = factor(aspect, levels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"))) -> aspect_vals

# Save as Rdata file
save(aspect_vals, file = "data/processed/aspect_vals.Rdata")


# ===================================
# 2. Test Differences in distribution
# ===================================



# ===============
# 3. Visualiztion
# ===============

aspect_EA_plot <- aspect_vals %>% 
  ggplot(aes(x = aspect, y = portion, col = type, group = type, fill = type))+
  geom_col(alpha = 0.5, position = "dodge")+
  coord_polar(start = -0.39, clip = T)+
  scale_color_uchicago(name = element_blank())+
  scale_fill_uchicago(name = element_blank())+
  scale_x_discrete(name = element_blank())+
  theme_minimal(base_family = "Helvetica", base_size = 11)+
  theme(legend.position = "right", legend.title = element_blank(),
        axis.ticks.y = element_line())+
  guides(fill = guide_legend(direction = "vertical"),
         color = "none")+
  facet_wrap(.~scale)

ggsave(aspect_EA_plot, filename = "plots/aspect_EA_plot.jpg", 
       device = "jpeg", dpi = 320, width = 18, height = 8, units = "cm")
