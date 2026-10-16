if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               sf,
               mapview)
# erase all objects in the environment
rm(list = ls())

sf_site <-  readRDS("data/sf_finsync_nc.rds")
sf_nc_county <- readRDS("data/sf_nc_county.rds")

mapview(sf_nc_county, legend = FALSE) + mapview(sf_site, legend = FALSE)
# join county information to sf_site
sf_site_join <- st_join(x = sf_site,
                        y = sf_nc_county) # overlaying layer
mapview(sf_site_join)

# count # of fish sites in guilford
sf_site_guilford <- sf_site_join %>% 
  filter(county == "guilford")
sf_site_guilford

# count # of fish sites in clay
sf_site_clay <- sf_site_join %>% 
  filter(county == "clay")
sf_site_clay

# reread stream data
sf_str <- readRDS("data/sf_stream_gi.rds")

sf_county_guilford <- sf_nc_county %>% filter(county == "guilford")

ggplot() + geom_sf(data = sf_county_guilford) +  
  geom_sf(data = sf_str) +
  geom_sf(data = sf_site_guilford, color = "yellow")


# geometric analysis ------------------------------------------------------

sf_str_proj <- st_transform(sf_str, crs = 32617)

# length of each stream line
v_str_l <-st_length(sf_str_proj)
head(v_str_l)

sf_str_w_len <- sf_str %>% 
  mutate(length = v_str_l)
sf_str_w_len

# area #

sf_nc_county_proj <- st_transform(sf_nc_county, crs = 32617)
v_area <- st_area(sf_nc_county_proj)
head(v_area)

sf_nc_county_w_area <- sf_nc_county %>% 
                            mutate(area = as.numeric(v_area) / 1E+6)
arrange(sf_nc_county_w_area, desc(area))

# subset for mapping
sf_county1k <- sf_nc_county_w_area %>% 
  filter(area > 1E+9)


# exercise ----------------------------------------------------------------

sf_quakes <- readRDS("data/sf_quakes.rds")

sf_nz <-  readRDS("data/sf_nz.rds")

mapview(sf_nz) + mapview(sf_quakes)

sf_quakes_join <- st_join(x = sf_quakes,
                          y = sf_nz)

sf_quakes_nz <- drop_na(sf_quakes_join, fid)

nrow(sf_quakes_nz)

df_n <- as_tibble(sf_site_join) %>% 
  group_by(county) %>% 
 summarize(n = n())

sf_n_site <- left_join(sf_nc_county, df_n)
sf_n10 <- sf_n_site %>% filter(n > 10)

ggplot() + 
  geom_sf(data = sf_nc_county) + 
  geom_sf(data = sf_n_site %>% filter(n > 1), fill = "gray") + 
  geom_sf(data = sf_n10, fill = "salmon")