if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,
               terra,
               tidyterra,
               mapview,
               stars)

# raster data format ------------------------------------------------------

# rast() is a function to read data from your directory
spr_ex <-  rast("data/spr_example.tif")

#this function exports raster data
writeRaster(x = spr_ex,
            filename = "data/spr_elev.tiff",
            overwrite = TRUE)
# visualize raster data; geom_spatraster() (from tidyterra)
ggplot() + geom_spatraster(data = spr_ex)

# mapview does not work with raster, conversion needed
star_ex <- st_as_stars(spr_ex)
mapview(star_ex)

# data type in raster -----------------------------------------------------


# continuous
v_elev <- values(spr_ex)
extract(spr_ex, y = cbind(6.000,
                          50.000))

extract(spr_ex, y = cbind(6.2,
                          49.9))

extract(spr_ex, y = cbind(6.0,
                          50.1))

extract(spr_ex, y = cbind(6.0,
                          49.7))
#extract data at multiple points
df_point <- tibble(lon = c(6, 5.9),
                   lat = c(50, 49.96)
                   )
extract(spr_ex, y = df_point)

# discrete data
spr_for <- rast("data/spr_forest_nc.tif")

ggplot() + 
  geom_spatraster(data = spr_for)

unique(spr_for)
v_binary <- values(spr_for)
mean(v_binary) * 100

# multiple classes - values w/ 2+ categories
spr_land <-  rast("data/spr_land_reclass.tif")
unique(spr_land) 
#1001 = forest
#1010 = crop
#1100 = urban

#coordinate, long -79.8063 lat 36.0701 - sullivan science building
spr_land %>% extract(y = cbind(y = -79.8063, 36.0701))

#reclass

cm <- cbind(
  c(0, 1001, 1010, 1100),
  c(0, 1, 0, 0)
)
cm

spr_bin <-  classify(spr_land, rcl = cm)
unique(spr_bin)
v_bin <- values(spr_bin)
mean(v_bin) * 100
#calc % crop

cm_crop <- cbind(
  c(0, 1001, 1010, 1100),
  c(0, 0, 1, 0)
)
cm_crop

spr_bin_crop <-  classify(spr_land, rcl = cm_crop)
unique(spr_bin_crop)
v_bin_crop <- values(spr_bin_crop)
mean(v_bin_crop) * 100
#calc % urban
cm_urban <- cbind(
  c(0, 1001, 1010, 1100),
  c(0, 0, 0, 1)
)
cm_urban

spr_bin_urb <-  classify(spr_land, rcl = cm_urban)
unique(spr_bin_crop)
v_bin_urb <- values(spr_bin_urb)
mean(v_bin_urb) * 100


# exercise ----------------------------------------------------------------

spr_prec_ncne <-  rast("data/spr_prec_ncne.tif")
# nrow = 162, ncol = 532
# resolution = .0083
# spatial extent = -79.89181, -75.45847
# lat extent = 35.24153, 36.59153
# CRS = WGS84
# min max precipitation = 1063.1 - 1501.5

#visualize
ggplot() + geom_spatraster(data = spr_prec_ncne)

#extract values
sf_site = readRDS("data/sf_finsync_nc.rds")
df_xy <- st_coordinates(sf_site)

df_land <- extract(spr_land, df_xy)
# forest
df_land %>% 
  filter(code == 1001) %>% 
  nrow()
# cropland
df_land %>% 
  filter(code == 1010) %>% 
  nrow()
# urban
df_land %>% 
  filter(code == 1100) %>% 
  nrow()
#quicker
table(df_land)


