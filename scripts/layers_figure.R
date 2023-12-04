rm(list = ls())
gc()

library(tidyverse)
library(ggnewscale)
library(vroom)

## Loading data
hexes <- st_read("data-products/geo-hexes/hexes.shp") 

hexes_obversation <- vroom::vroom("data-products/geo-hexes/hexid-observations.csv") |> 
  filter(date == max(date))

observation <- vroom::vroom("data-products/covidestim-observations.csv") |> 
  filter(date == max(date))

us_counties <- tigris::counties(cb = T) |> 
  st_transform(crs = st_crs(hexes)) |> 
  # tigris::shift_geometry() |> 
  filter(!STATE_NAME %in% c("Alaska", 
                            "Hawaii", 
                            "Puerto Rico", 
                            "Guam", 
                            "Commonwealth of the Northern Mariana Islands",
                            "American Samoa",
                            "United States Virgin Islands"))

## Aux functions
rotate_data <- function(data, 
                        shear_cos_x = 2,
                        shear_sin_x = 1.2,
                        shear_cos_y = 0,
                        shear_sin_y = 1,
                        x_add = 0, y_add = 0) {
  
  shear_matrix <- function(){ matrix(c(shear_cos_x, 
                                       shear_sin_x, 
                                       shear_cos_y, 
                                       shear_sin_y), 
                                     2, 
                                     2) }
  
  rotate_matrix <- function(x){ 
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
  }
  data %>% 
    dplyr::mutate(
      geometry = .$geometry * shear_matrix() * rotate_matrix(pi/20) + 
        c(x_add, y_add)
    )
}

rotate_data_geom <- function(data, x_add = 0, y_add = 0) {
  shear_matrix <- function(){ matrix(c(2, 1.2, 0, 1), 2, 2) }
  
  rotate_matrix <- function(x) { 
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
  }
  data %>% 
    dplyr::mutate(
      geom = .$geom * shear_matrix() * rotate_matrix(pi/20) + c(x_add, y_add)
    )
}

## Joined dataset

observation_joined <- us_counties |> 
  right_join(observation,
             by = c("GEOID" = "fips"))

ggplot()+
  geom_sf(data = observation_joined |> 
            rotate_data(shear_cos_x = 1.2,
                        shear_sin_x = 1),
          aes(fill = infectionsPC), 
          color = NA)+
  scale_fill_viridis_c(option = "rocket",
                       direction = -1,
                       na.value = "darkblue",
                       breaks = seq(0,400, 50),
                       limits = c(0,400),
                       oob = scales::squish,
                       guide = metR::guide_colorstrip(title.position = "top",
                                                      title.hjust = 0.5,
                                                      barwidth = grid::unit(12, "cm")))+
  theme_void()+
  theme(legend.position = "none")

hexes_joined <- hexes |> 
  right_join(hexes_obversation |> 
               mutate(hexid = as.character(hexid))) 

ggplot()+
  geom_sf(data = hexes_joined |> 
            rotate_data(shear_cos_x = 1.2,
                        shear_sin_x = 1),
          aes(fill = infectionsPC), 
          color = NA)+
  scale_fill_viridis_c(option = "rocket",
                       direction = -1,
                       na.value = "darkblue",
                       breaks = seq(0,400, 50),
                       limits = c(0,400),
                       oob = scales::squish,
                       guide = metR::guide_colorstrip(title.position = "top",
                                                      title.hjust = 0.5,
                                                      barwidth = grid::unit(12, "cm")))+
  theme_void()+
  theme(legend.position = "none")

# annotate parameters
x = -20
y = 45
color = 'gray40'
shear_cos_x = 1.2
shear_sin_x = 1

## Plotting

temp1 <- ggplot() +
  
  # Covidestim obsevartions on counties polygons
  geom_sf(data = observation_joined %>% 
            rotate_data(shear_cos_x = shear_cos_x,
                        shear_sin_x = shear_sin_x), 
          aes(fill = infectionsPC),
          color=NA, 
          show.legend = FALSE) +
  scale_fill_viridis_c(option = "rocket",
                       direction = -1,
                       na.value = "darkblue",
                       breaks = seq(0,400, 50),
                       limits = c(0,400),
                       oob = scales::squish,
                       guide = metR::guide_colorstrip(title.position = "top",
                                                      title.hjust = 0.5,
                                                      barwidth = grid::unit(12, "cm")))+
  annotate("text",
           label='Infections per capita',
           x=x,
           y=y,
           hjust = 0,
           color=color) +
  # labs(caption = "image by @rafalpx")+
  theme(legend.position = "none")
temp1


temp2 <- temp1 +
  
  # Counties
  new_scale_fill() + 
  new_scale_color() +
  geom_sf(data = us_counties %>% 
            rotate_data(shear_cos_x = shear_cos_x,
                        shear_sin_x = shear_sin_x,
                        y_add = 10), 
          fill="white", 
          color="darkblue", 
          show.legend = FALSE) +
  annotate("text", 
           label='Counties polygons', 
           x=x, 
           y=y+10, 
           hjust = 0, 
           color=color) +
  
  # Hexes
  new_scale_fill() + 
  new_scale_color() +
  geom_sf(data = hexes %>% 
            rotate_data(shear_cos_x = shear_cos_x,
                        shear_sin_y = shear_sin_x,
                        y_add = 20), 
          color='darkred', 
          fill="white")+
  annotate("text", 
           label='Hex grid', 
           x=x, 
           y=y+20, 
           hjust = 0, 
           color=color) +
  
  # Hexes observation
  new_scale_fill() + 
  new_scale_color() +
  geom_sf(data = hexes_joined %>% 
            rotate_data(shear_cos_x = shear_cos_x,
                        shear_sin_y = shear_sin_x,
                        y_add = 30), 
          aes(fill = infectionsPC),
          color=NA)+
  scale_fill_viridis_c(option = "mako",
                       direction = -1,
                       na.value = "darkblue",
                       breaks = seq(0,400, 50),
                       limits = c(0,400),
                       oob = scales::squish,
                       guide = metR::guide_colorstrip(title.position = "top",
                                                      title.hjust = 0.5,
                                                      barwidth = grid::unit(12, "cm")))+
  annotate("text", 
           label='Infections per capita \n (on hexgrid)', 
           x=x, 
           y=y+30, 
           hjust = 0, 
           color=color)+
  theme_void() +
  scale_x_continuous(limits = c(-105, -10))+
  theme(legend.position = "none")
temp2


ggsave(filename = "img/layered_plot.png",
       plot = temp2,
       width = 16,
       height = 9, 
       dpi = 100)
