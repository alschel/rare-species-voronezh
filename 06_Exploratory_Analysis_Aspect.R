# Задача - определить значимость экспозиции для расположения наблюдений

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

# Брандушка - upd Jan 2022
load("data/processed/BV_observations_upd_aea.Rdata")
# Пион узколистный
load("data/processed/PTL_observations_aea.Rdata")
# Ирис карликовый
load("data/processed/IPL_observations_aea.Rdata")

# Bind tables
localities <-
  rbind(BV_observations_upd_aea, PTL_observations_aea, IPL_observations_aea) %>% 
  filter(Occurrence == 1)

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
aspect_obs_vals <- extract(aspect_cat, as(localities, "Spatial")) %>% as.data.frame()
colnames(aspect_obs_vals) <- c("500", "1000")
# Add Species name
aspect_obs_vals %>% 
  mutate(Species = localities$Species) -> aspect_obs_vals

# Calculate number of observations by aspect
aspect_obs_vals %>% 
  pivot_longer(`500`:`1000`, names_to = "scale", values_to = "aspect") %>% 
  group_by(aspect, scale, Species) %>% 
  summarise(observations = n()) %>% 
  arrange(Species, scale, aspect) %>% 
  ungroup() %>% 
  mutate(aspect = rep(c("N", "NE", "E", "SE", "S", "SW", "W", "NW"), 6)) -> aspect_obs_vals

# Merge two tables: regional and by species
aspect_vals_region %>% 
  mutate(type = "region",
         n = region) %>% 
  dplyr::select(-region) %>% 
  rbind(aspect_obs_vals %>% mutate(type = Species, n = observations) %>% dplyr::select(-Species, -observations)) -> 
    aspect_vals

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
  coord_polar(start = -0.39, clip = "on")+
  scale_color_uchicago(name = element_blank())+
  scale_fill_uchicago(name = element_blank())+
  scale_x_discrete(name = element_blank())+
  theme_minimal(base_family = "Helvetica", base_size = 10)+
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        axis.ticks.y = element_line(),
        legend.margin = margin(0,0,0,0,"pt"))+
  guides(fill = guide_legend(ncol = 2),
         color = "none")+
  facet_wrap(.~scale, labeller = as_labeller(c("500" = "500 m", "1000" = "1000 m")))

ggsave(aspect_EA_plot, filename = "plots/BV_aspect_EA_plot_upd.jpg", 
       device = "jpeg", dpi = 320, width = 16, height = 12, units = "cm")
