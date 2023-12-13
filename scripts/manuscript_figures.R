rm(list = ls())
gc()

library(tidyverse)
library(sf)
library(MoMAColors)

## Reading hex grid
hexes <- st_read("data-products/geo-hexes/hexes.shp") 

## Reading observation with hexes id
hexes_obversation <- vroom::vroom("data-products/geo-hexes/hexid-observations.csv")

## Reading observation
covidestim_observation <- vroom::vroom("data-products/covidestim-observations.csv")

## US Counties
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

## Fig.1 Patterns of synchronization
fig1_data <- us_counties |> 
  left_join(covidestim_observation,
            by = c("GEOID" = "fips"))

breaks_plt <- seq(0,600, 100)
labels_plt <- c(seq(0,500, 100), '600+')
limits_plt <- c(0,600)

fig1a <- ggplot()+
  geom_sf(data = us_counties,
          fill = "gray80",
          color = NA)+
  geom_sf(data = fig1_data  |> 
            filter(date == as.Date("2021-09-01") - 42),
          aes(fill = infectionsPC),
          color = NA)+
  scale_fill_viridis_c(option = "rocket",
                       direction = -1,
                       # na.value = "darkblue",
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
                       oob = scales::squish,
                       guide = metR::guide_colorstrip(title = "Estimated infection/100k/day",
                                                      title.position = "top",
                                                      title.hjust = 0.5,
                                                      barwidth = grid::unit(12, "cm")))+
  theme_void() +
  theme(legend.position = "top")+
  labs(subtitle = as.Date("2021-09-01") - 42)
fig1a

fig1b <- ggplot()+
  geom_sf(data = us_counties,
          fill = "gray80",
          color = NA)+
  geom_sf(data = fig1_data  |> 
            filter(date == as.Date("2021-09-01") - 21),
          aes(fill = infectionsPC),
          color = NA)+
  scale_fill_viridis_c(option = "rocket",
                       direction = -1,
                       # na.value = "darkblue",
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
                       oob = scales::squish,
                       guide = metR::guide_colorstrip(title = "Estimated infection/100k/day",
                                                      title.position = "top",
                                                      title.hjust = 0.5,
                                                      barwidth = grid::unit(12, "cm")))+
  theme_void() +
  theme(legend.position = "top")+
  labs(subtitle = as.Date("2021-09-01") - 21)
fig1b

fig1c <- ggplot()+
  geom_sf(data = us_counties,
          fill = "gray80",
          color = NA)+
  geom_sf(data = fig1_data  |> 
            filter(date == as.Date("2021-09-01")),
          aes(fill = infectionsPC),
          color = NA)+
  scale_fill_viridis_c(option = "rocket",
                       direction = -1,
                       # na.value = "darkblue",
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
                       oob = scales::squish,
                       guide = metR::guide_colorstrip(title = "Estimated infection/100k/day",
                                                      title.position = "top",
                                                      title.hjust = 0.5,
                                                      barwidth = grid::unit(12, "cm")))+
  theme_void() +
  theme(legend.position = "top")+
  labs(subtitle = as.Date("2021-09-01"))
fig1c

fig1d <- covidestim_observation |> 
  reframe(infectionsPC = sum(infectionsPC, na.rm = T),
          .by = "date") |> 
  ggplot(aes(x = date, y = infectionsPC))+
  geom_line()+
  theme_minimal()+
  scale_x_date(name = "Date", 
               date_breaks = "3 months", 
               date_labels = "%b %y'")+
  scale_y_continuous(name = "Estimated infection/100k/day",
                     labels = scales::label_comma())+
  geom_vline(xintercept = as.Date("2021-09-01") - 42, 
             color = "gray80")+
  annotate("text", 
           x = as.Date("2021-09-01") - 45, 
           y = 700000,
           label = "B")+
  geom_vline(xintercept = as.Date("2021-09-01") - 21,
             color = "gray80")+
  annotate("text", 
           x = as.Date("2021-09-01") - 24, 
           y = 700000,
           label = "C")+
  geom_vline(xintercept = as.Date("2021-09-01"),
             color = "gray80")+
  annotate("text", 
           x = as.Date("2021-09-01") - 3, 
           y = 700000,
           label = "D")+
  theme(axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 14))
fig1d

library(patchwork)
fig1 <- (fig1d/(fig1a | fig1b | fig1c))+
  plot_layout(guides = 'collect')+
  plot_annotation(tag_levels = 'A')&
  theme(legend.position = "bottom")
fig1

ggsave(filename = "img/fig1.png", 
       plot = fig1, 
       width = 16, 
       height = 9, 
       dpi = 100)

## Fig2.
## Loading data
hexes_obversation_to_fig2 <- vroom::vroom("data-products/geo-hexes/hexid-observations.csv") |> 
  filter(date == max(date))

observation_to_fig2 <- vroom::vroom("data-products/covidestim-observations.csv") |> 
  filter(date == max(date))

## Fig2A - Layered depiction on transforming estimated infections on counties polygon on hexgrid
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

## Joined datasets
observation_joined <- us_counties |> 
  right_join(observation_to_fig2,
             by = c("GEOID" = "fips"))

hexes_joined <- hexes |> 
  right_join(hexes_obversation_to_fig2 |> 
               mutate(hexid = as.character(hexid)))

# annotate parameters
x = -20
y = 45
color = 'gray40'
shear_cos_x = 1.2
shear_sin_x = 1

## Plotting
fig2a <- ggplot() +
  
  # Covidestim obsevartions on counties polygons
  geom_sf(data = hexes_joined %>% 
            rotate_data(shear_cos_x = shear_cos_x,
                        shear_sin_x = shear_sin_x), 
          aes(fill = infectionsPC),
          color=NA, 
          show.legend = FALSE) +
  scale_fill_viridis_c(option = "rocket",
                       direction = -1,
                       # na.value = "darkblue",
                       breaks = seq(0,400, 50),
                       limits = c(0,400),
                       oob = scales::squish,
                       guide = metR::guide_colorstrip(title.position = "top",
                                                      title.hjust = 0.5,
                                                      barwidth = grid::unit(12, "cm")))+
  annotate("text",
           label='Infections per capita \n (on hexgrid)',
           x=x,
           y=y,
           hjust = 0,
           color=color) +
  theme_void()+
  theme(legend.position = "none")
fig2a

fig2a <- fig2a +
  
  # Counties
  new_scale_fill() + 
  new_scale_color() +
  geom_sf(data = hexes %>% 
            rotate_data(shear_cos_x = shear_cos_x,
                        shear_sin_x = shear_sin_x,
                        y_add = 10), 
          fill="white", 
          color="gray50", 
          show.legend = FALSE) +
  annotate("text", 
           label='Hexes grid', 
           x=x, 
           y=y+10, 
           hjust = 0, 
           color=color) +
  
  # Hexes
  new_scale_fill() + 
  new_scale_color() +
  geom_sf(data = us_counties %>% 
            rotate_data(shear_cos_x = shear_cos_x,
                        shear_sin_y = shear_sin_x,
                        y_add = 20), 
          color='gray50', 
          fill="white")+
  annotate("text", 
           label='Counties polygon', 
           x=x, 
           y=y+20, 
           hjust = 0, 
           color=color) +
  
  # Hexes observation
  new_scale_fill() + 
  new_scale_color() +
  geom_sf(data = observation_joined %>% 
            rotate_data(shear_cos_x = shear_cos_x,
                        shear_sin_y = shear_sin_x,
                        y_add = 30), 
          aes(fill = infectionsPC),
          color=NA)+
  scale_fill_viridis_c(option = "rocket",
                       direction = -1,
                       # na.value = "darkblue",
                       breaks = seq(0,400, 50),
                       limits = c(0,400),
                       oob = scales::squish,
                       guide = metR::guide_colorstrip(title.position = "top",
                                                      title.hjust = 0.5,
                                                      barwidth = grid::unit(12, "cm")))+
  annotate("text", 
           label='Infections per capita', 
           x=x, 
           y=y+30, 
           hjust = 0, 
           color=color)+
  theme_void() +
  scale_x_continuous(limits = c(-105, -10))+
  theme(legend.position = "none")
fig2a

ggsave(filename = "img/fig2a.png",
       plot = fig2a,
       width = 16,
       height = 9, 
       dpi = 100)

## Scatterplot to alphas
## Reading the weekly model output
alphas_week <- vroom::vroom("data-products/geo-hexes/mixedmodel/alphas_weekly_regardless_rt-reformat.csv")

alphas_week <- alphas_week |> 
  ## Writing the hexbin code as a 4-digit number
  mutate(hex_i = sprintf("%04d", i),
         hex_j = sprintf("%04d", j)) |> 
  ## Creating a 8-digit hexbin code to identifying uniquely them
  mutate(i_to_j = str_c(hex_i, hex_j),
         j_to_i = str_c(hex_j, hex_i))

## Plotting
alphas_i_to_j_week <- alphas_week |> 
  group_by(i_to_j, date_week) |> 
  summarise(raw = alpha,
            normalized = alpha_normalized) |>
  mutate(standarlized = (raw - mean(raw, na.rm = T))/sd(raw, na.rm = T)) |> 
  pivot_longer(cols = raw:standarlized,
               names_to = "type",
               values_to = "values")

alphas_j_to_i_week <- alphas_week |> 
  group_by(j_to_i, date_week) |> 
  summarise(raw = alpha,
            normalized = alpha_normalized) |>
  mutate(standarlized = (raw - mean(raw, na.rm = T))/sd(raw, na.rm = T)) |> 
  pivot_longer(cols = raw:standarlized,
               names_to = "type",
               values_to = "values")

joined_ij_week <- inner_join(alphas_i_to_j_week, 
                             alphas_j_to_i_week, 
                             by = c("i_to_j" = "j_to_i", 
                                    "date_week", "type"), 
                             suffix = c(".itoj", ".jtoi"))

fig2b <- joined_ij_week |> 
  filter(type == "raw") |> 
  ggplot(aes(x = values.itoj, 
             y = values.jtoi))+
  geom_point(alpha = 0.01)+
  theme_minimal()+
  labs(x = expression(alpha["i,j"]),
       y = expression(alpha["j,i"]))+
  lims(x = c(-50,60), y = c(-50, 60))+
  theme(legend.position = "none", 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 14))
fig2b

ggsave(filename = "img/fig2b.png",
       plot = fig2b,
       width = 16,
       height = 9, 
       dpi = 100)

# scatterplot_weekly <- joined_ij_week |> 
#   ggplot(aes(x = values.itoj, 
#              y = values.jtoi, 
#              col = type))+
#   geom_point(alpha = 0.01)+
#   theme_minimal()+
#   labs(title = "Weekly model",
#        x = expression(alpha~"i to j"),
#        y = expression(alpha~"j to i"))+
#   theme(axis.title = element_text(size = 14))+
#   facet_wrap(type~.)+
#   theme(legend.position = "none")
# scatterplot_weekly
# 
# ggsave(filename = "img/scatterplot_facetted_alphas_weekly.png",
#        plot = scatterplot_weekly,
#        width = 16,
#        height = 9, 
#        dpi = 100)

# hist_weekly <- joined_ij_week |> 
#   pivot_longer(cols = values.itoj:values.jtoi,
#                names_to = "relation",
#                values_to = "values") |> 
#   mutate(relation = str_remove(relation, "values.")) |> 
#   ggplot(aes(x = values, fill = relation))+
#   geom_histogram(bins = 1000)+
#   facet_wrap(type~., scales = "free")+
#   theme_minimal()
# hist_weekly
# 
# ggsave(filename = "img/histogram_alphas_weekly.png",
#        plot = hist_weekly,
#        width = 16,
#        height = 9, 
#        dpi = 100)

fig2 <- (fig2a | fig2b)+
  plot_layout(widths = c(2,1))+
  plot_annotation(tag_levels = 'A')
fig2

ggsave(filename = "img/fig2.png",
       plot = fig2, 
       width = 16, 
       height = 9, 
       dpi = 100)
