# From Fitzpatrick 
# github page: https://github.com/brfitzpatrick/stars-sf-example/blob/master/stars_sf_example.md


library(MASS)
library(tidyverse)
library(sf)
library(stars)
library(ggspatial)

####
raster::getData('ISO3')

mex.dem.rast <- raster::getData(name = 'alt', country = 'MEX')

mex.dem.rast <- raster::aggregate(x = mex.dem.rast, fact = 4)

mex.bound.sp.poly.l1.df <- raster::getData(name = 'GADM', country = 'MEX', level = 1)

mex.bound.sp.poly.l2.df <- raster::getData(name = 'GADM', country = 'MEX', level = 2)

raster::plot(mex.dem.rast)

sp::plot(mex.bound.sp.poly.l2.df, add = TRUE, border = 'darkgrey')

sp::plot(mex.bound.sp.poly.l1.df, add = TRUE, border = 'black', lwd = 2)

mex.slope.rast <- raster::terrain(x = mex.dem.rast, opt = 'slope')

mex.aspect.rast <- raster::terrain(x = mex.dem.rast, opt = 'aspect')

mex.hs.rast <- raster::hillShade(slope = mex.slope.rast, aspect = mex.aspect.rast, normalize = TRUE)

raster::plot(mex.hs.rast, col=grey(0:100/100), legend=FALSE)

raster::plot(mex.dem.rast, col = rainbow(n = 100, start = 0, end = 0.8, alpha = 0.35), add=TRUE)

sp::plot(mex.bound.sp.poly.l2.df, add = TRUE, border = 'black', lwd = 0.5)

sp::plot(mex.bound.sp.poly.l1.df, add = TRUE, border = 'black', lwd = 2)

# stars and sf steps

MEX.HS.stars <- st_as_stars(.x = mex.hs.rast)

MEX.DEM.stars <- st_as_stars(.x = mex.dem.rast)

MEX.Bound.L1.sf <- st_as_sf(x = mex.bound.sp.poly.l1.df)

MEX.Bound.L2.sf <- st_as_sf(x = mex.bound.sp.poly.l2.df)

MEX.Bound.L1.sf <- MEX.Bound.L1.sf %>% mutate(NAME_1 = case_when(NAME_1 == "Distrito Federal" ~ "D.F", TRUE ~ NAME_1))

mex.dem.hs.stars.plot <- ggplot() +
  geom_stars(data = MEX.HS.stars, aes(x = x, y = y, alpha = -layer), fill = 'black') +
  scale_alpha_continuous(na.value = 0) +    
  coord_equal() +
  #geom_sf(data = MEX.Bound.L2.sf, fill = NA, colour = 'grey', alpha = 0.25, size = 0.125) +
  geom_sf(data = MEX.Bound.L1.sf, fill = NA, colour = 'grey', alpha = 0.25, size = 0.75) +
  geom_stars(data = MEX.DEM.stars, aes(x = x, y = y, fill = MEX_msk_alt), alpha = 0.35) +
  geom_sf_label(data = MEX.Bound.L1.sf, aes(label = NAME_1), alpha = 0.5, fill = 'white', size = 2.5) + 
  guides(alpha = FALSE) + 
  labs(fill = 'Elevation (m)', x = NULL, y = NULL) +
  theme_bw() +
  theme(legend.key.height = unit(x = 2, units = 'cm'))

mex.dem.hs.stars.plot + scale_fill_viridis_c(na.value = 'white')

mex.dem.hs.stars.plot + scale_fill_distiller(palette = 'Spectral', na.value = 'white')

ggsave("img/mex_w_labels.png", width = 20, height = 16)

