rm(list = ls())
gc()

library(tidyverse)
library(sf)
# library(MoMAColors)

## Reading hex grid
hexes <- st_read("data-products/geo-hexes/hexes.shp") 

## Reading observation with hexes id
hexes_obversation <- vroom::vroom("data-products/geo-hexes/hexid-observations.csv")

## Reading observation
covidestim_observation <- vroom::vroom("data-products/covidestim-observations.csv")

omicronera_observation <- vroom::vroom("data-products/omicronera_observation.csv.xz")

## Population
population <- tidycensus::get_decennial(geography = 'county', 
                                        variables = 'P2_001N',
                                        geometry = F,
                                        year = 2020) |> 
  mutate(fips = GEOID,
         population = value) |> 
  select(fips, population)

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

## One dataset for all
covidestim_week <- covidestim_observation |> 
  mutate(date_week = lubridate::floor_date(date, unit = "week", week_start = "Thursday")) |> 
  reframe(cases = sum(cases, na.rm = T),
          Rt = round(mean(Rt, na.rm = T), 3),
          infections = round(sum(infections, na.rm = T), 2),
          infectionsPC = round(sum(infectionsPC, na.rm = T),2),
          .by = c("fips", "date_week"))

omicronera_week <- omicronera_observation |> 
  rename(date_week = date) |> 
  reframe(cases = sum(cases, na.rm = T),
          Rt = round(mean(Rt, na.rm = T), 3),
          infections = round(sum(infections, na.rm = T), 2),
          infectionsPC = round(sum(infectionsPC, na.rm = T),2),
          .by = c("fips", "date_week"))

covidestim <- rbind(covidestim_week, 
                    omicronera_week) |> 
  left_join(population) |> 
  mutate(infectionsPC_new = round((infections/population)*1e5, 2))

## Fig.1 Patterns of synchronization
fig1_data <- us_counties |> 
  left_join(covidestim,
            by = c("GEOID" = "fips"))

dates <- unique(covidestim$date_week)

## Date of maximum infections per capita at national level
date_peak_preomicron <- as.Date("2021-08-26")
date_peak_omicronera <- as.Date("2022-01-20")

fig1a <- covidestim |> 
  reframe(infectionsPC = sum(infectionsPC, na.rm = T)/7,
          .by = "date_week") |> 
  ggplot(aes(x = date_week, y = infectionsPC))+
  geom_line()+
  theme_minimal()+
  scale_x_date(name = "Date", 
               date_breaks = "4 months", 
               date_labels = "%b %y'")+
  scale_y_continuous(name = "Estimated infection/day",
                     labels = scales::label_comma())+
  ## Pre-Omicron model
  geom_vline(xintercept = date_peak_preomicron - 42, 
             color = "gray80")+
  annotate("text", 
           x = date_peak_preomicron - 45, 
           y = 1e6,
           label = "B",
           size = 6)+
  geom_vline(xintercept = date_peak_preomicron - 21,
             color = "gray80")+
  annotate("text", 
           x = date_peak_preomicron - 24, 
           y = 1e6,
           label = "C",
           size = 6)+
  geom_vline(xintercept = date_peak_preomicron,
             color = "gray80")+
  annotate("text", 
           x = date_peak_preomicron - 3, 
           y = 1e6,
           label = "D",
           size = 6)+
  ## Omicron era model
  geom_vline(xintercept = date_peak_omicronera - 42, 
             color = "gray80")+
  annotate("text", 
           x = date_peak_omicronera - 45, 
           y = 4e6,
           label = "E",
           size = 6)+
  geom_vline(xintercept = date_peak_omicronera - 21,
             color = "gray80")+
  annotate("text", 
           x = date_peak_omicronera - 24, 
           y = 4e6,
           label = "F",
           size = 6)+
  geom_vline(xintercept = date_peak_omicronera,
             color = "gray80")+
  annotate("text", 
           x = date_peak_omicronera - 3, 
           y = 4e6,
           label = "G",
           size = 6)+
  theme(axis.text = element_text(size = 14),
        # axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 14))
fig1a

ggsave(filename = "img/extra_figures/fig1a.png",
       plot = fig1a,
       width = 16,
       height = 9, 
       dpi = 100)

## Alpha peak snapshots

## Breakdowns of each peaks
breaks_plt <- seq(0,1000, 100)
labels_plt <- c(seq(0,900, 100), '1000+')
limits_plt <- c(0,1000)

fig1b <- ggplot()+
  geom_sf(data = us_counties,
          fill = "gray80",
          color = NA)+
  geom_sf(data = fig1_data |> 
            filter(date_week == date_peak_preomicron - 42),
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
  labs(title = date_peak_preomicron - 42)
fig1b

ggsave(filename = "img/extra_figures/fig1b.png",
       plot = fig1b,
       width = 16,
       height = 9, 
       dpi = 100)

fig1c <- ggplot()+
  geom_sf(data = us_counties,
          fill = "gray80",
          color = NA)+
  geom_sf(data = fig1_data  |> 
            filter(date_week == date_peak_preomicron - 21),
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
  labs(title = date_peak_preomicron - 21)
fig1c

ggsave(filename = "img/extra_figures/fig1c.png",
       plot = fig1c,
       width = 16,
       height = 9, 
       dpi = 100)

fig1d <- ggplot()+
  geom_sf(data = us_counties,
          fill = "gray80",
          color = NA)+
  geom_sf(data = fig1_data  |> 
            filter(date_week == date_peak_preomicron),
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
  labs(title = date_peak_preomicron)
fig1d

ggsave(filename = "img/extra_figures/fig1d.png",
       plot = fig1d,
       width = 16,
       height = 9, 
       dpi = 100)

## Omicron BA.1 peak snapshots

## Breakdowns of each peaks
breaks_plt <- seq(0,5000, 500)
labels_plt <- c(seq(0,4900, 500), '5000+')
limits_plt <- c(0,5000)

fig1e <- ggplot()+
  geom_sf(data = us_counties,
          fill = "gray80",
          color = NA)+
  geom_sf(data = fig1_data |> 
            filter(date_week == date_peak_omicronera - 42),
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
  labs(title = date_peak_omicronera - 42)
fig1e

ggsave(filename = "img/extra_figures/fig1e.png",
       plot = fig1e,
       width = 16,
       height = 9, 
       dpi = 100)

fig1f <- ggplot()+
  geom_sf(data = us_counties,
          fill = "gray80",
          color = NA)+
  geom_sf(data = fig1_data  |> 
            filter(date_week == date_peak_omicronera - 21),
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
  labs(title = date_peak_omicronera - 21)
fig1f

fig1g <- ggplot()+
  geom_sf(data = us_counties,
          fill = "gray80",
          color = NA)+
  geom_sf(data = fig1_data  |> 
            filter(date_week == date_peak_omicronera),
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
  labs(title = date_peak_omicronera)
fig1g

library(patchwork)
fig1 <- (fig1a/(((fig1b | fig1c | fig1d)+
                   plot_layout(guides = 'collect'))/
                  ((fig1e | fig1f | fig1g)+
                     plot_layout(guides = 'collect'))))+
  plot_layout(heights = c(1,3))+
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
  geom_sf(data = hexes_joined, 
          aes(fill = infectionsPC),
          color=NA, 
          show.legend = FALSE) +
  geom_sf(data = us_counties,
          color = "gray80",
          fill = NA)+
  scale_fill_viridis_c(option = "rocket",
                       direction = -1,
                       # na.value = "darkblue",
                       breaks = seq(0,400, 50),
                       limits = c(0,400),
                       oob = scales::squish,
                       guide = metR::guide_colorstrip(title.position = "top",
                                                      title.hjust = 0.5,
                                                      barwidth = grid::unit(12, "cm")))+
  # annotate("text",
  #          label='Infections per capita \n (on hexgrid)',
  #          x=x,
  #          y=y,
  #          hjust = 0,
  #          color=color) +
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
## function to build the joined alphas correctly
joined_alphas <- \(alphas, date_col){
  alphas <- alphas |> 
    ## Writing the hexbin code as a 4-digit number
    dplyr::mutate(hex_i = sprintf("%04d", i),
           hex_j = sprintf("%04d", j)) |> 
    ## Creating a 8-digit hexbin code to identifying uniquely them
    dplyr::mutate(i_to_j = str_c(hex_i, hex_j),
           j_to_i = str_c(hex_j, hex_i))
  
  ## Plotting
  alphas_i_to_j <- alphas |> 
    dplyr::group_by(i_to_j, 
             {{ date_col }}) |> 
    dplyr::summarise(raw = alpha,
              normalized = alpha_normalized) 
  # |>
  #   mutate(standarlized = (raw - mean(raw, na.rm = T))/sd(raw, na.rm = T)) |> 
  #   pivot_longer(cols = raw:standarlized,
  #                names_to = "type",
  #                values_to = "values")
  
  alphas_j_to_i <- alphas |> 
    dplyr::group_by(j_to_i, 
             {{ date_col }}) |> 
    dplyr::summarise(raw = alpha,
              normalized = alpha_normalized)
  # |>
  #   mutate(standarlized = (raw - mean(raw, na.rm = T))/sd(raw, na.rm = T)) |> 
  #   pivot_longer(cols = raw:standarlized,
  #                names_to = "type",
  #                values_to = "values")
  
  joined_ij <- dplyr::inner_join(alphas_i_to_j, 
                          alphas_j_to_i, 
                          by = c("i_to_j" = "j_to_i", "date_week"), 
                          suffix = c(".itoj", ".jtoi"))
  return(joined_ij)
}

## Alphas dataset
## Pre-Omicron
### Main model
alphas_week_po_main <- vroom::vroom("data-products/geo-hexes/mixedmodel/alphas_weekly_regardless_rt-reformat_preomicron_main.csv")
### SAI model
alphas_week_po_sa1 <- vroom::vroom("data-products/geo-hexes/mixedmodel/alphas_weekly_regardless_rt-reformat_preomicron_SAI.csv")
### SAII model
alphas_week_po_sa2 <- vroom::vroom("data-products/geo-hexes/mixedmodel/alphas_weekly_regardless_rt-reformat_preomicron_SAII.csv")

## Joined data.frames
## Main
joined_ij_week_po_main <- joined_alphas(alphas = alphas_week_po_main, 
                                date_col = date_week)
## SAI
joined_ij_week_po_sa1 <- joined_alphas(alphas = alphas_week_po_sa1, 
                                        date_col = date_week)

## SAII
joined_ij_week_po_sa2 <- joined_alphas(alphas = alphas_week_po_sa2, 
                                       date_col = date_week)

fig2b <- joined_ij_week_po_main |> 
  ggplot(aes(x = raw.itoj, 
             y = raw.jtoi))+
  geom_point(alpha = 0.01)+
  theme_minimal()+
  labs(x = expression(alpha["i,j"]),
       y = expression(alpha["j,i"]), 
       title = "Pre-Omicron Main Model")+
  # lims(x = c(-50,60), y = c(-50, 60))+
  theme(legend.position = "none", 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 14))
fig2b

ggsave(filename = "img/fig2b_preomicron_main.png",
       plot = fig2b,
       width = 16,
       height = 9, 
       dpi = 100)

fig2c <- joined_ij_week_po_sa1 |> 
  ggplot(aes(x = raw.itoj, 
             y = raw.jtoi))+
  geom_point(alpha = 0.01)+
  theme_minimal()+
  labs(x = expression(alpha["i,j"]),
       y = expression(alpha["j,i"]), 
       title = "Pre-Omicron SAI Model")+
  # lims(x = c(-50,60), y = c(-50, 60))+
  theme(legend.position = "none", 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 14))
fig2c

ggsave(filename = "img/fig2c_preomicron_SAI.png",
       plot = fig2c,
       width = 16,
       height = 9, 
       dpi = 100)

fig2d <- joined_ij_week_po_sa2 |> 
  ggplot(aes(x = raw.itoj, 
             y = raw.jtoi))+
  geom_point(alpha = 0.01)+
  theme_minimal()+
  labs(x = expression(alpha["i,j"]),
       y = expression(alpha["j,i"]), 
       title = "Pre-Omicron SAII Model")+
  # lims(x = c(-50,60), y = c(-50, 60))+
  theme(legend.position = "none", 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 14))
fig2d

ggsave(filename = "img/fig2d_preomicron_SAII.png",
       plot = fig2d,
       width = 16,
       height = 9, 
       dpi = 100)

## Omicron-era
### Main model
alphas_week_oe_main <- vroom::vroom("data-products/geo-hexes/mixedmodel/alphas_weekly_regardless_rt-reformat_omicronera_main.csv")
### SAI model
alphas_week_oe_sa1 <- vroom::vroom("data-products/geo-hexes/mixedmodel/alphas_weekly_regardless_rt-reformat_omicronera_SAI.csv")
### SAII model
alphas_week_oe_sa2 <- vroom::vroom("data-products/geo-hexes/mixedmodel/alphas_weekly_regardless_rt-reformat_omicronera_SAII.csv")

## Joined data.frames
## Main
joined_ij_week_oe_main <- joined_alphas(alphas = alphas_week_oe_main, 
                                        date_col = date_week)
## SAI
joined_ij_week_oe_sa1 <- joined_alphas(alphas = alphas_week_oe_sa1, 
                                       date_col = date_week)

## SAII
joined_ij_week_oe_sa2 <- joined_alphas(alphas = alphas_week_oe_sa2, 
                                       date_col = date_week)

fig2e <- joined_ij_week_oe_main |> 
  ggplot(aes(x = raw.itoj, 
             y = raw.jtoi))+
  geom_point(alpha = 0.01)+
  theme_minimal()+
  labs(x = expression(alpha["i,j"]),
       y = expression(alpha["j,i"]), 
       title = "Omicron-era Main Model")+
  # lims(x = c(-50,60), y = c(-50, 60))+
  theme(legend.position = "none", 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 14))
fig2e

ggsave(filename = "img/fig2e_omicronera_main.png",
       plot = fig2e,
       width = 16,
       height = 9, 
       dpi = 100)

fig2f <- joined_ij_week_oe_sa1 |> 
  ggplot(aes(x = raw.itoj, 
             y = raw.jtoi))+
  geom_point(alpha = 0.01)+
  theme_minimal()+
  labs(x = expression(alpha["i,j"]),
       y = expression(alpha["j,i"]), 
       title = "Omicron-era SAI Model")+
  # lims(x = c(-50,60), y = c(-50, 60))+
  theme(legend.position = "none", 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 14))
fig2f

ggsave(filename = "img/fig2f_omicronera_SAI.png",
       plot = fig2f,
       width = 16,
       height = 9, 
       dpi = 100)

fig2g <- joined_ij_week_oe_sa2 |> 
  ggplot(aes(x = raw.itoj, 
             y = raw.jtoi))+
  geom_point(alpha = 0.01)+
  theme_minimal()+
  labs(x = expression(alpha["i,j"]),
       y = expression(alpha["j,i"]), 
       title = "Omicron-era SAII Model")+
  # lims(x = c(-50,60), y = c(-50, 60))+
  theme(legend.position = "none", 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 14))
fig2g

ggsave(filename = "img/fig2g_omicronera_SAII.png",
       plot = fig2g,
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
