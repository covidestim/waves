#Load library
library(sf)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tmap)

pop_data<-read.csv("data-products/geo-hexes/meta_population/metapop.csv")
pop_data$meta30m_pop = rowSums(pop_data[,4:9])
pop_data$bias <- pop_data$population - pop_data$meta30m_pop 
pop_data$bias_abs <- abs(pop_data$bias)

#Load shapefile
grid_pop <- st_read("data-products/geo-hexes/meta_population/hexid-population_new.shp") |> 
  st_transform(crs = 26915)
grid_pop$metapop_30m = pop_data$meta30m_pop

st_write(grid_pop |> 
           select(hexid,metapop_30m, geometry),
         dsn = "data-products/geo-hexes/meta_population/hexgrid_meta30m_population.geojson",
         delete_dsn = T)

grid_pop$bias <- pop_data$bias
grid_pop$bias_abs <- pop_data$bias_abs
grid_pop$bias_sd <- scale(grid_pop$bias)
grid_pop <- grid_pop |> 
  mutate(lat = st_coordinates(st_centroid(geometry))[,"X"],
         lon = st_coordinates(st_centroid(geometry))[,"Y"])

hist <- ggplot()+
  geom_histogram(data = grid_pop,
                 aes(x = log10(bias),
                     group = hexid),
                 bins = 100)+
  theme_minimal()+
  scale_x_continuous(name = "Bias [CBG - Meta30m], \n (in log10)",
                     labels = scales::label_math(expr = 10^.x),
                     breaks = c(-2:5))+
  scale_y_continuous(name = "Number of hexes affected")
hist

hist_lat <- ggplot()+
  geom_histogram(data = grid_pop,
                 aes(x = log10(bias),
                     fill = lat,
                     group = hexid),
                 bins = 100)+
  theme_minimal()+
  scale_x_continuous(name = "Bias [CBG - Meta30m], \n (in log10)",
                     labels = scales::label_math(expr = 10^.x),
                     breaks = c(-2:5))+
  scale_y_continuous(name = "Number of hexes affected")+
  khroma::scale_fill_acton(name = "Latitude")
hist_lat

hist_lon <- ggplot()+
  geom_histogram(data = grid_pop,
                 aes(x = log10(bias),
                     fill = lon,
                     group = hexid),
                 bins = 100)+
  theme_minimal()+
  scale_x_continuous(name = "Bias [CBG - Meta30m], \n (in log10)",
                     labels = scales::label_math(expr = 10^.x),
                     breaks = c(-2:5))+
  scale_y_continuous(name = "Number of hexes affected")+
  khroma::scale_fill_acton(name = "Longitude")
hist_lon

(hist | (hist_lat / hist_lon))

ggsave(filename = "img/extra_figures/histogram_bias.png",
       width = 16,
       height = 9,
       dpi = 200)

hexid_plt <- ggplot(grid_pop) +
  geom_sf(aes(fill = log(population))) +
  khroma::scale_fill_iridescent(name = "log (Pop)")+
  theme_minimal()+
  labs(title = "With CBGs")
hexid_plt

meta_30m_plt <- ggplot(grid_pop) +
  geom_sf(aes(fill = log(metapop_30m))) +
  khroma::scale_fill_iridescent(name = "log (Pop)")+
  theme_minimal()+
  labs(title = "With Meta 30m")
meta_30m_plt

library(patchwork)
(hexid_plt | meta_30m_plt)

ggsave(filename = "img/extra_figures/hexgrid_pop_bias.png",
       width = 16,
       height = 9,
       dpi = 200)

