library(tidyverse)
library(rayshader)
library(magick)
library(sf)

sessionInfo()
getwd()

# load elevation data
elev_img <- raster::raster("/Users/matthewlaw/OneDrive - The University of Liverpool/Dissertation/maps/BCN_elevation.tif")

elev_matrix <- raster_to_matrix(elev_img)

elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)

watermap <- detect_water(elev_matrix)

elev_matrix %>%
  sphere_shade(texture = "imhof4") %>%
  add_water(watermap, color = "imhof4") %>%
  # add_shadow(raymat, max_darken = 0.5) %>%
  # add_shadow(ambmat, max_darken = 0.5) %>%
  plot_map()

resized_matrix <- resize_matrix(elev_matrix, width = 215, height = 215)

resized_matrix %>%
  sphere_shade(texture = "imhof4") %>%
  add_water(watermap, color = "imhof4") %>%
  # add_shadow(raymat, max_darken = 0.5) %>%
  # add_shadow(ambmat, max_darken = 0.5) %>%
  plot_map()

render_polygons(buildings,
                extent = attr(montereybay,"extent"), data_column_top = "ALAND",
                scale_data = 300/(2.6E9), color="chartreuse4",
                parallel=TRUE)


buildings <- st_read("/Users/matthewlaw/OneDrive - The University of Liverpool/Dissertation/maps/buildingpart_selection.gpkg")

buildings %>% st_geometry %>% plot
elev_img %>% plot

# ggplot 

gg <- ggplot(buildings, aes(color = numberOfFloorsAboveGround)) +
  scale_color_viridis_c(trans = "log10") +
  geom_sf() +
  theme_void() +
  theme(legend.position = "none")

plot_gg(gg, width = 5, height = 4, height_aes = fill, scale = 300, multicore = TRUE, windowsize = c(1000, 800))
# render_camera(fov = 70, zoom = 0.5, theta = 130, phi = 35)
render_camera(zoom = 0.2, theta = 130, phi = 35)

#### examples

montereybay %>%
  sphere_shade(texture = "desert") %>%
  add_shadow(ray_shade(montereybay,zscale=50)) %>%
  plot_3d(montereybay,water=TRUE, windowsize=c(1000,800), watercolor="dodgerblue")
render_camera(theta=-60,  phi=60, zoom = 0.85, fov=30)

#We will apply a negative buffer to create space between adjacent polygons:
mont_county_buff = sf::st_simplify(sf::st_buffer(monterey_counties_sf,-0.003), dTolerance=0.001)

render_polygons(mont_county_buff,
                extent = attr(montereybay,"extent"), data_column_top = "ALAND",
                scale_data = 300/(2.6E9), color="chartreuse4",
                parallel=TRUE)
render_highquality(clamp_value=10,sample_method="stratified")




#Plot the counties around Monterey Bay, CA
#Only run these examples if the `magick` package is installed.
if (length(find.package("magick", quiet = TRUE)) > 0) {
  # \donttest{
  generate_polygon_overlay(monterey_counties_sf, palette = rainbow, 
                           extent = attr(montereybay,"extent"), heightmap = montereybay) %>%
    plot_map() 
  
  #These counties include the water, so we'll plot bathymetry data over the polygon
  #data to only include parts of the polygon that fall on land.
  water_palette = colorRampPalette(c("darkblue", "dodgerblue", "lightblue"))(200)
  bathy_hs = height_shade(montereybay, texture = water_palette)
  
  generate_polygon_overlay(monterey_counties_sf, palette = rainbow, 
                           extent = attr(montereybay,"extent"), heightmap = montereybay) %>%
    add_overlay(generate_altitude_overlay(bathy_hs, montereybay, start_transition = 0)) %>%
    plot_map()
  
  #Add a semi-transparent hillshade and change the palette, and remove the polygon lines
  montereybay %>%
    sphere_shade(texture = "bw") %>%
    add_overlay(generate_polygon_overlay(monterey_counties_sf, 
                                         palette = terrain.colors, linewidth=NA,
                                         extent = attr(montereybay,"extent"), heightmap = montereybay),
                alphalayer=0.7) %>%
    add_overlay(generate_altitude_overlay(bathy_hs, montereybay, start_transition = 0)) %>%
    add_shadow(ray_shade(montereybay,zscale=50),0) %>%
    plot_map()
  
  #Map one of the variables in the sf object and use an explicitly defined color palette
  county_palette = c("087" = "red",    "053" = "blue",   "081" = "green", 
                     "069" = "yellow", "085" = "orange", "099" = "purple") 
  montereybay %>%
    sphere_shade(texture = "bw") %>%
    add_shadow(ray_shade(montereybay,zscale=50),0) %>%
    add_overlay(generate_polygon_overlay(monterey_counties_sf, linecolor="white", linewidth=3,
                                         palette = county_palette, data_column_fill = "COUNTYFP",
                                         extent = attr(montereybay,"extent"), heightmap = montereybay),
                alphalayer=0.7) %>%
    add_overlay(generate_altitude_overlay(bathy_hs, montereybay, start_transition = 0)) %>%
    add_shadow(ray_shade(montereybay,zscale=50),0.5) %>%
    plot_map()
  # }
}
