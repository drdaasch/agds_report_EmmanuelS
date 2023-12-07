# load libraries
library(geodata)
library(ggplot2)
library(tidyterra)
library(MODIStsp)
library(ggpubr)


# download SRTM DEM data ----
# This stores file srtm_38_03.tif in tempdir()
geodata::elevation_3s(
  lat = 46.6756,
  lon = 7.85480,
  path = tempdir()
)

# read the downloaded data
# use file.path() to combine
# a directory path with a filename
dem <- terra::rast(
  file.path(
    tempdir(),
    "srtm_38_03.tif"
  )
)

# download phenology data from MODIS ----
# load libraries
library(MODISTools)

# get information about MCD12Q2 Land Cover Dynamics Data:
MODISTools::mt_bands("MCD12Q2")
# band "Greenup.Num_Modes_01" means the Onset_Greenness_Increase days since 1-1-1970
# and corresponds to one valid vegetation cycle --> there is also modes_02

# How does this relate to other bands within this product?
# There are other bands, each denoting a different variable of phenology, such as onsets of dormancy or greenness
# but also Enhanced Vegetation Index (EVI) specifics such as amplitude or area, but also Quality controls

# What are the characteristics of the downloaded data?
# from here https://modis-land.gsfc.nasa.gov/pdf/MODIS_Land_Cover_Dynamics_MCD12Q2_C61_Product.pdf
# it contains a composite dataset for the year 2012 of greenup dates since 1.1.1970 at 500 m resolution
# it is already tidy dataframe

# download and save phenology data
phenology <- MODISTools::mt_subset(
  product = "MCD12Q2",
  lat = 46.6756,
  lon = 7.85480,
  band = "Greenup.Num_Modes_01",
  start = "2012-01-01",
  end = "2012-12-31",
  km_lr = 100,
  km_ab = 100,
  site_name = "swiss",
  internal = TRUE,
  progress = FALSE
)

# screening of data
phenology_swiss <- phenology |>
  dplyr::mutate(
    value = ifelse(value > 32656, NA, value),
    value = as.numeric(format(as.Date("1970-01-01") + value, "%j")),
    value = ifelse (value < 200, value, NA)
  )

phenology_raster <- MODISTools::mt_to_terra(
  phenology_swiss,
  reproject = TRUE
)

ggplot() +
  tidyterra::geom_spatraster(data = phenology_raster) +
  scale_fill_viridis_c(
    na.value = NA,
    name = "DOY"
  ) +
  theme_bw()


MODIS_products <- mt_products()
head(MODIS_products)

# so we need MOD11A2, get info on band to use
MODISTools::mt_bands("MOD11A2")


LST_swiss <- MODISTools::mt_subset(
  product = "MOD11A2",
  lat = 46.6756,
  lon = 7.85480,
  band = "LST_Day_1km",
  start = "2012-01-01",
  end = "2012-12-31",
  km_lr = 100,
  km_ab = 100,
  site_name = "swiss",
  internal = TRUE,
  progress = FALSE
)

# average the LST for each pixel across the year
LST_swiss_mean <- LST_swiss |> 
  dplyr::group_by(pixel) |> 
  dplyr::summarize(mean = mean(value))

# append mean values to original dataframe
LST_swiss_mean <- LST_swiss |> 
  dplyr::mutate(mean = mean(value), .by = pixel) |>
  dplyr::group_by(pixel) |> 
  dplyr::slice(1) |> 
  subset(select = -value) |> 
  rename(value = mean)

LST_swiss_raster <- MODISTools::mt_to_terra(
  LST_swiss_mean,
  reproject = TRUE
)

ggplot() +
  tidyterra::geom_spatraster(data = LST_swiss_raster) +
  scale_fill_viridis_c(
    na.value = NA,
    name = "Yearly average temp. deg. K \n"
  ) +
  theme_bw()

# crop the LST_swiss_raster
LST_swiss_raster <- terra::crop(
  x = LST_swiss_raster,
  y = phenology_raster
)

# resample the LST using the mean LST value in a MODIS pixel
LST_swiss_raster <- terra::resample(
  x = LST_swiss_raster,
  y = phenology_raster,
  method = "average"
)

# mask the locations which have no data
LST_swiss_raster <- terra::mask(
  LST_swiss_raster,
  is.na(phenology_raster),
  maskvalues = TRUE
)


p <- ggplot() +
  tidyterra::geom_spatraster(data = LST_swiss_raster) +
  scale_fill_viridis_c(
    na.value = NA,
    name = "Temperature deg. K"
  ) +
  theme_bw()

p2 <- ggplot() +
  tidyterra::geom_spatraster(data = phenology_raster) +
  scale_fill_viridis_c(
    na.value = NA,
    name = "DOY"
  ) +
  theme_bw()

# show relationship between temperature and DOY phenology
# convert to data frame and merge
LST_swiss_df <- as.vector(LST_swiss_raster)
phenology_df <- as.vector(phenology_raster)
sct_df <- data.frame(
  LST = LST_swiss_df,
  doy = phenology_df
)

ggplot(
  data = sct_df,
  aes(
    LST,
    doy
  )
) +
  geom_hex() +
  scale_fill_viridis_c(trans="log10") +
  geom_smooth(
    method = "lm",
    se = FALSE,
    colour = "white",
    lty = 2
  ) +
  labs(
    x = "Temp. deg. K",
    y = "MODIS vegetation greenup (DOY)"
  ) +
  theme_bw()


# fit a linear regression to the data of the figure above
fit <- lm(doy ~ LST, data = sct_df)
print(summary(fit))
