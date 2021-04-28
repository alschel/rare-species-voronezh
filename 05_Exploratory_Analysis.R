# Резведочный анализ

# Задача - выделить самые выжные параметры

library(tidyverse)
library(raster)
library(terra)
library(sf)
library(mapview)
library(corrplot)
library(usdm)
library(FactoMineR)
library(factoextra)

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


# ================
# 1. Random points
# ================
# Создадим набор из 5000 случайных точек, вытащим по ним значения растров и отдадим в vif

set.seed(12)
random_points <- st_sample(voronezh_admin_border_simplified_aea, 5000)
mapview(random_points)
random_points <- as(random_points, "Spatial")

# list of predictors
predictors_01_files <- list.files("data/processed/Predictors_01/", full.names = T)
# Create SpatRaster
predictors_01 <- stack(predictors_01_files)

# Extract values from rasters by points
predictors_01_df <- extract(predictors_01, random_points, df = T)

predictors_01_df %>% 
  filter(!is.na(annualPET)) %>% 
  dplyr::select(-ID) -> predictors_01_df

save(predictors_01_df, file = "data/processed/predictors_01_df.Rdata")

# ======================
# 2. Correlation and PCA
# ======================

load("data/processed/predictors_01_df.Rdata")

# Scale variables
predictors_01_df %>% 
  dplyr::select(-aspect_500_cat) %>%  # уберем экспозицию
  mutate_all(.funs = scale) -> predictors_01_df_scale

summary(predictors_01_df_scale)

# Correlation
predictors_01_cor <- cor(predictors_01_df_scale)
corrplot(predictors_01_cor)

# PCA (http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/)
predictors_01_pca <- PCA(predictors_01_df_scale, graph = FALSE)

# Вклад главных компонент
fviz_eig(predictors_01_pca, addlabels = TRUE, ylim = c(0, 50))

# Биплот
fviz_pca_biplot(predictors_01_pca, label = "var", repel = TRUE, 
                alpha.ind = 0.5,
                pointsize = 0.5,
                palette = "jco",
                col.var = "red", # Variables color
                col.ind = "#696969"  # Individuals color
)

# ======
# 3. VIF
# ======

predictors_01_vif <- vifcor(predictors_01_df_scale)

predictors_01_vif@results %>% 
  tibble() %>%
  ggplot(aes(x = reorder(Variables, VIF), y = VIF))+
  geom_col()+
  coord_flip()+
  scale_x_discrete(name = element_blank())+
  theme_minimal(base_size = 12, base_family = "Arial")

# PCA для отсавшихся переменных
predictors_01_pca2 <- PCA(predictors_01_df %>% 
                            dplyr::select(predictors_01_vif@results$Variables), graph = FALSE)

# Вклад главных компонент
fviz_eig(predictors_01_pca2, addlabels = TRUE, ylim = c(0, 50))

# Биплот
fviz_pca_biplot(predictors_01_pca2, label = "var", repel = TRUE, 
                alpha.ind = 0.5,
                pointsize = 0.5,
                palette = "jco",
                col.var = "red", # Variables color
                col.ind = "#696969"  # Individuals color
)


