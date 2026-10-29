if (!require(pacman)) install.packages("pacman")

# exercise ----------------------------------------------------------------


pacman::p_load(tidyverse,
               sf,
               terra,
               tidyterra,
               exactextractr)

## data
sf_site <- readRDS("data/sf_finsync_nc.rds")
sf_nc_county <- readRDS("data/sf_nc_county.rds")
spr_prec_nc <- rast("data/spr_prec_nc.tif")

## point-wise extraction
ggplot() +
  geom_spatraster(data = spr_prec_nc) +
  geom_sf(data = sf_site) +
  scale_fill_viridis_c() + 
  theme_void()

extract(
  x = spr_prec_nc,
  y = sf_site,
  bind = TRUE
) %>% 
  st_as_sf()
## zonal statistics
sf_nc_county_proj <- st_transform(sf_nc_county,
             crs = 32617)
spr_prec_nc_proj <-  project(x = spr_prec_nc,
                             y = crs(sf_nc_county_proj)
)
 ggplot() +
   geom_spatraster(data = spr_prec_nc)
 geom_sf(data = sf_nc_county,
         alpha = .25)
 
df_prec_county <-  exact_extract(x = spr_prec_nc_proj,
               y = sf_nc_county_proj,
               fun = "mean",
               append_cols = "county") %>% 
   as_tibble() %>% 
   rename(precipitation = mean)

sf_nc_county_prec <- sf_nc_county %>% 
  left_join(df_prec_county,
            by = "county")
ggplot() + 
  geom_sf(data = sf_nc_county_prec, 
          aes(fill = precipitation)) + 
            scale_fill_viridis_c() 

sf_nc_county_prec %>% 
arrange(precipitation)

## buffer-based analysis
sf_site_proj <- sf_site %>%
  st_transform(crs = 32617)

sf_site_buff_proj <- sf_site_proj %>%
  st_buffer(dist = 10000)


ggplot() + geom_sf(data = sf_site_buff_proj)

ggplot() + 
geom_sf(data = sf_site_buff_proj) +
geom_sf(data = sf_site_proj)
  
df_prec_buff <- exact_extract(x = spr_prec_nc_proj,
              y = sf_site_buff_proj,
              fun = "mean",
              append_cols = "site_id") %>% 
  as_tibble() %>% 
  rename(precipitation = mean)

sf_site_prec_buff <- sf_site %>% 
  left_join(df_prec_buff,
            by = "site_id")

df_prec_buff %>% 
  arrange(precipitation)

ggplot() + 
  geom_sf(data = sf_nc_county) +
  geom_sf(data = sf_site_prec_buff,
          aes(color = precipitation)) +
  scale_color_viridis_c()


# exercise ----------------------------------------------------------------

# 1001 = forest
# 1010 = crop
# 1100 = urban

spr_land <- rast ("data/spr_land_reclass.tif")
unique(spr_land)
spr_reclass <- cbind(c(0, 1001, 1010, 1100),
                c(0,0,1,0))

spr_bin <- classify(spr_land,
                    rcl = spr_reclass) %>% 
project(x = .,
        y = crs(sf_nc_county_proj)
)
vbin <- values(spr_bin)

df_crop <- exact_extract(x = spr_bin,
              y = sf_nc_county_proj,
              fun = "mean",
              append_cols = "county") %>% 
  as_tibble() %>% 
  rename(frac_crop = mean)

df_crop %>% arrange(desc(frac_crop)
  
)
ggplot() + 
  geom_sf(aes(fill = frac_crop), data = left_join(sf_nc_county_proj, df_crop))
