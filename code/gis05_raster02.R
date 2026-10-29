if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               terra,
               tidyterra,
               mapview,
               stars)

spr_prec <- rast("data/spr_prec_us.tif")
ext(spr_prec)

ggplot() +
  geom_spatraster(data = spr_prec)

spr_prec_crop <- crop(x = spr_prec,
                      y = c(-80, -75, 34, 37))

 
sf_nc_county <- readRDS("data/sf_nc_county.rds")

ggplot() +
  geom_spatraster(data = spr_prec_crop) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25) 

spr_prec_nc <-  crop(x = spr_prec,
                     y = sf_nc_county)

ggplot() +
  geom_spatraster(data = spr_prec_nc) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25) 

#merge
spr_nw <- rast("data/spr_prec_ncnw.tif")
spr_ne <- rast("data/spr_prec_ncne.tif")
spr_sw <- rast("data/spr_prec_ncsw.tif")  
spr_se <- rast("data/spr_prec_ncse.tif")
#nw
ggplot() + 
  geom_spatraster(data = spr_nw) + 
  geom_sf(data = sf_nc_county,
          alpha = 0.25)
#sw
ggplot() + 
  geom_spatraster(data = spr_sw) + 
  geom_sf(data = sf_nc_county,
          alpha = 0.25)
## merge to combine two panels
spr_n <- merge(spr_nw, spr_ne)
## more than 2 panel
list_spr <- list(spr_ne,
                 spr_nw,
                 spr_se,
                 spr_sw)
#convert list to spr collection
spr_col <- sprc(list_spr)
spr_merge <- merge(spr_col)

ggplot() + 
  geom_spatraster(data = spr_merge) + 
  geom_sf(data = sf_nc_county,
          alpha = 0.25,
          fill = "purple")
## export
writeRaster(spr_merge,
            "data/spr_prec_nc.tif",
            overwrite = TRUE)
# stack
spr_prec_nc <- rast("data/spr_prec_nc.tif")
spr_tmp_nc <- rast("data/spr_tmp_nc.tif")

ggplot() +
    geom_spatraster(data = spr_tmp_nc) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)
#create stack
spr_pt_nc <- c(spr_prec_nc,
               spr_tmp_nc)
#access one layer
spr_pt_nc$precipitation
spr_pt_nc$temperature

#stack sprne and sprtempnc
# c(spr_ne, spr_tmp_nc)
# wouldnt work because they are not the same size, spatial extent mismatch

#reprojection
(spr_prec_nc_proj <- project(x = spr_prec_nc,
                             y = "EPSG:32617"))

# exercise ----------------------------------------------------------------


spr_tmp_nw <- rast("data/spr_tmp_ncnw.tif")
spr_tmp_ne <- rast("data/spr_tmp_ncne.tif")
spr_tmp_sw <- rast("data/spr_tmp_ncsw.tif")  
spr_tmp_se <- rast("data/spr_tmp_ncse.tif")

list_tmp <- list(spr_tmp_nw,
                 spr_tmp_ne,
                 spr_tmp_sw,
                 spr_tmp_se)

tmp_col <- sprc(list_tmp)
tmp_merge <- merge(tmp_col)

ggplot() +
  geom_spatraster(data = tmp_merge) +
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

sf_camden <- sf_nc_county %>% 
  group_by(county) %>%
  filter(county == "camden")
 (ext(sf_camden))

spr_tmp_camden <- crop(x = tmp_merge,
     y = sf_camden)


ggplot() + 
  geom_spatraster(data = spr_tmp_camden) + 
  geom_sf(data = sf_nc_county,
          alpha = 0.25,
          fill = "purple")


spr_tmp_camden_proj <- project(x = spr_tmp_camden,
                             y = "EPSG:32618",
                             method = "bilinear")
(spr_tmp_camden_proj)