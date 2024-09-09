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
preomicron_week <- covidestim_observation |> 
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

covidestim <- rbind(preomicron_week, 
                    omicronera_week) |> 
  left_join(population) |> 
  mutate(infectionsPC_new = round((infections/population)*1e5, 2),
         date_week = as.Date(date_week, "%Y-%m-%d"))

## Fig.1 Patterns of synchronization
fig1_data <- us_counties |> 
  left_join(covidestim,
            by = c("GEOID" = "fips"))

dates <- unique(covidestim$date_week)

## Date of maximum infections per capita at national level
sum_infections <- covidestim |> 
  reframe(infectionsPC = sum(infectionsPC, na.rm = T)/7,
          .by = "date_week") |> 
  arrange(desc(date_week)) |> 
  filter(date_week <= "2021-12-01")

## Peaks
## Ten most dates with infectionsPC
head(sum_infections[order(rank(-sum_infections$infectionsPC)),],20)

## Peaks dates
date_peak_preomicron <- as.Date("2021-08-26")
date_peak_omicronera <- as.Date("2022-01-20")

fig1a_data <- covidestim |> 
  reframe(infectionsPC = sum(infectionsPC, na.rm = T)/7,
          .by = "date_week") |> 
  arrange(desc(date_week))

fig1a <- ggplot()+
  geom_line(data = fig1a_data,
            aes(x = date_week, 
                y = infectionsPC))+
  theme_minimal()+
  scale_x_date(name = "Date",
               date_breaks = "4 months",
               date_labels = "%b %y'")+
  scale_y_continuous(name = "Estimated infection/day",
                     labels = scales::label_comma())+
  # fig1a
  ## Delta wave marks
  annotate("rect",
           xmin = date_peak_preomicron - 70,
           xmax = date_peak_preomicron + 70,
           ymin = 0, ymax = Inf,
           fill = "grey50",alpha = 0.2)+
  annotate("text",
           x = c(date_peak_preomicron-63,
                 date_peak_preomicron-45, 
                 date_peak_preomicron-24, 
                 date_peak_preomicron),
           y = rep(4.05e6, 4),
           label = LETTERS[2:5],
           size = 5)+
  annotate("segment", 
           y = rep(0,4),
           yend = rep(4e6,4),
           x = c(date_peak_preomicron-63,
                 date_peak_preomicron-45, 
                 date_peak_preomicron-24, 
                 date_peak_preomicron),
           xend = c(date_peak_preomicron-63,
                    date_peak_preomicron-45, 
                    date_peak_preomicron-24, 
                    date_peak_preomicron),
           color = "grey50",
           linetype = "dashed")+
  ## Omicron wave marks
  annotate("rect",
           xmin = date_peak_omicronera - 70,
           xmax = date_peak_omicronera + 70,
           ymin = 0, ymax = Inf,
           fill = "grey50",alpha = 0.2)+
  annotate("text",
           x = c(date_peak_omicronera-63,
                 date_peak_omicronera-45, 
                 date_peak_omicronera-24, 
                 date_peak_omicronera),
           y = rep(4.05e6, 4),
           label = LETTERS[6:9],
           size = 5)+
  annotate("segment", 
           y = rep(0,4),
           yend = rep(4e6,4),
           x = c(date_peak_omicronera-63,
                 date_peak_omicronera-45, 
                 date_peak_omicronera-24, 
                 date_peak_omicronera),
           xend = c(date_peak_omicronera-63,
                    date_peak_omicronera-45, 
                    date_peak_omicronera-24, 
                    date_peak_omicronera),
           color = "grey50",
           linetype = "dashed")
fig1a

ggsave(filename = "img/extra_figures/fig1a.png",
       plot = fig1a,
       width = 16,
       height =9, 
       dpi = 100)

# Hacky workaround for demo purposes
scale_x_date2 <- function(..., rescaler) {
  x <- scale_x_date(...)
  x$rescaler <- rescaler
  x
}

# The default `to` argument of the `rescale()` function is `c(0, 1)`.
# Here, we reverse that.
invert_scale <- function(x, to = c(1, 0), from = range(x)) {
  scales::rescale(x, to, from)
}

fig1a_vertical <- fig1a+
  coord_flip()+ 
  scale_x_date2(rescaler = invert_scale,
                name = "Date",
                date_breaks = "4 months",
                date_labels = "%b %y'")
fig1a_vertical

ggsave(filename = "img/extra_figures/fig1a_veritcal.png",
       plot = fig1a_vertical,
       width = 9,
       height =16, 
       dpi = 100)

## Transforming CRS
us_counties <- us_counties |> 
  st_transform(crs = 'ESRI:102009')

fig1_data <- fig1_data |> 
  st_transform(crs = 'ESRI:102009')

## Delta peak snapshots

## Breakdowns of each peaks
breaks_plt <- seq(0,1500, 150)
labels_plt <- c(seq(0,1350, 150), '1,500+')
limits_plt <- c(0,1500)
color_option <- "magma"

fig1b <- ggplot()+
  geom_sf(data = us_counties,
          fill = "gray80",
          color = NA)+
  geom_sf(data = fig1_data |> 
            filter(date_week == date_peak_preomicron - 63),
          aes(fill = infectionsPC),
          color = NA)+
  scale_fill_viridis_c(option = color_option,
                       direction = -1,
                       # na.value = "darkblue",
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
                       oob = scales::squish,
                       guide = guide_colorsteps(title = "Estimated Infection/100k/day"))+
  theme_void() +
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(3, "cm"))+
  labs(title = date_peak_preomicron - 63)
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
  geom_sf(data = fig1_data |> 
            filter(date_week == date_peak_preomicron - 42),
          aes(fill = infectionsPC),
          color = NA)+
  scale_fill_viridis_c(option = color_option,
                       direction = -1,
                       # na.value = "darkblue",
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
                       oob = scales::squish,
                       guide = guide_colorsteps(title = "Estimated Infection/100k/day"))+
  theme_void() +
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(3, "cm"))+
  labs(title = date_peak_preomicron - 42)
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
            filter(date_week == date_peak_preomicron - 21),
          aes(fill = infectionsPC),
          color = NA)+
  scale_fill_viridis_c(option = color_option,
                       direction = -1,
                       # na.value = "darkblue",
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
                       oob = scales::squish,
                       guide = guide_colorsteps(title = "Estimated Infection/100k/day"))+
  theme_void() +
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(3, "cm"))+
  labs(title = date_peak_preomicron - 21)
fig1d

ggsave(filename = "img/extra_figures/fig1d.png",
       plot = fig1d,
       width = 16,
       height = 9, 
       dpi = 100)

fig1e <- ggplot()+
  geom_sf(data = us_counties,
          fill = "gray80",
          color = NA)+
  geom_sf(data = fig1_data  |> 
            filter(date_week == date_peak_preomicron),
          aes(fill = infectionsPC),
          color = NA)+
  scale_fill_viridis_c(option = color_option,
                       direction = -1,
                       # na.value = "darkblue",
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
                       oob = scales::squish,
                       guide = guide_colorsteps(title = "Estimated Infection/100k/day"))+
  theme_void() +
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(3, "cm"))+
  labs(title = date_peak_preomicron,
       subtitle = "Delta wave peak")
fig1e

ggsave(filename = "img/extra_figures/fig1e.png",
       plot = fig1e,
       width = 16,
       height = 9, 
       dpi = 100)

## Omicron BA.1 peak snapshots

## Breakdowns of each peaks
breaks_plt <- seq(0,5000, 500)
labels_plt <- c(seq(0,4900, 500), '5000+')
limits_plt <- c(0,5000)

fig1f <- ggplot()+
  geom_sf(data = us_counties,
          fill = "gray80",
          color = NA)+
  geom_sf(data = fig1_data |> 
            filter(date_week == date_peak_omicronera - 63),
          aes(fill = infectionsPC),
          color = NA)+
  scale_fill_viridis_c(option = color_option,
                       direction = -1,
                       # na.value = "darkblue",
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
                       oob = scales::squish,
                       guide = guide_colorsteps(title = "Estimated Infection/100k/day"))+
  theme_void() +
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(3, "cm"))+
  labs(title = date_peak_omicronera - 63)
fig1f

ggsave(filename = "img/extra_figures/fig1f.png",
       plot = fig1f,
       width = 16,
       height = 9, 
       dpi = 100)

fig1g <- ggplot()+
  geom_sf(data = us_counties,
          fill = "gray80",
          color = NA)+
  geom_sf(data = fig1_data |> 
            filter(date_week == date_peak_omicronera - 42),
          aes(fill = infectionsPC),
          color = NA)+
  scale_fill_viridis_c(option = color_option,
                       direction = -1,
                       # na.value = "darkblue",
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
                       oob = scales::squish,
                       guide = guide_colorsteps(title = "Estimated Infection/100k/day"))+
  theme_void() +
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(3, "cm"))+
  labs(title = date_peak_omicronera - 42)
fig1g

ggsave(filename = "img/extra_figures/fig1g.png",
       plot = fig1g,
       width = 16,
       height = 9, 
       dpi = 100)

fig1h <- ggplot()+
  geom_sf(data = us_counties,
          fill = "gray80",
          color = NA)+
  geom_sf(data = fig1_data  |> 
            filter(date_week == date_peak_omicronera - 21),
          aes(fill = infectionsPC),
          color = NA)+
  scale_fill_viridis_c(option = color_option,
                       direction = -1,
                       # na.value = "darkblue",
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
                       oob = scales::squish,
                       guide = guide_colorsteps(title = "Estimated Infection/100k/day"))+
  theme_void() +
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(3, "cm"))+
  labs(title = date_peak_omicronera - 21)
fig1h

ggsave(filename = "img/extra_figures/fig1h.png",
       plot = fig1h,
       width = 16,
       height = 9, 
       dpi = 100)

fig1i <- ggplot()+
  geom_sf(data = us_counties,
          fill = "gray80",
          color = NA)+
  geom_sf(data = fig1_data  |> 
            filter(date_week == date_peak_omicronera),
          aes(fill = infectionsPC),
          color = NA)+
  scale_fill_viridis_c(option = color_option,
                       direction = -1,
                       # na.value = "darkblue",
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
                       oob = scales::squish,
                       guide = guide_colorsteps(title = "Estimated Infection/100k/day"))+
  theme_void() +
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(3, "cm"))+
  labs(title = date_peak_omicronera,
       subtitle = "Omicron BA.1 wave peak")
fig1i

ggsave(filename = "img/extra_figures/fig1i.png",
       plot = fig1i,
       width = 16,
       height = 9, 
       dpi = 100)

library(patchwork)

fig1_delta <- ((fig1b | fig1c | fig1d | fig1e))+
  plot_layout(guides = 'collect')+
  plot_annotation(tag_levels = list(LETTERS[2:5]))&
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(hjust = 0.5, angle = 90),
        legend.title.position = "left", 
        legend.key.height = grid::unit(1, "cm"),
        legend.key.width = grid::unit(1, "cm"))
fig1_delta

fig1_omicron <- ((fig1f | fig1g | fig1h | fig1i))+
  plot_layout(guides = 'collect')+
  plot_annotation(tag_levels = list(LETTERS[6:9]))&
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(hjust = 0.5, angle = 90),
        legend.title.position = "left", 
        legend.key.height = grid::unit(1, "cm"),
        legend.key.width = grid::unit(1, "cm"))
fig1_omicron

fig1 <- (fig1a/(fig1_delta/fig1_omicron))
fig1

ggsave(filename = "img/fig1.png", 
       plot = fig1, 
       width = 16, 
       height = 9, 
       dpi = 100)

ggsave(filename = "img/fig1.pdf", 
       plot = fig1, 
       width = 16, 
       height = 9, 
       dpi = 100)


fig1_data_vertical <- fig1_data  |> 
  filter(date_week %in% c(date_peak_preomicron, 
                          date_peak_preomicron - 21,
                          date_peak_preomicron - 42,
                          date_peak_preomicron - 63,
                          date_peak_omicronera, 
                          date_peak_omicronera - 21,
                          date_peak_omicronera - 42,
                          date_peak_omicronera - 63))

fig1bg_vertical <- ggplot()+
  geom_sf(data = fig1_data_vertical,
          aes(fill = infectionsPC),
          color = NA)+
  scale_fill_viridis_c(option = color_option,
                       direction = -1,
                       # na.value = "darkblue",
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
                       oob = scales::squish,
                       guide = metR::guide_colorstrip(title = "Estimated infection/100k",
                                                      title.position = "right",
                                                      title.hjust = 0.5,
                                                      reverse = T,
                                                      barheight = grid::unit(12, "cm")))+
  facet_wrap(.~date_week, 
             ncol = 1, 
             strip.position = "right")+
  theme_void() +
  theme(legend.position = "right", 
        legend.title = element_text(angle = -90),
        strip.text = element_text(angle = 90))
fig1bg_vertical

fig1_vertical <- ((fig1a_vertical)|
                    (fig1bg_vertical))
fig1_vertical

ggsave(filename = "img/fig1_vertical.png",
       plot = fig1_vertical,
       height = 16, 
       width = 9,
       dpi = 100)

ggsave(filename = "img/fig1_vertical.pdf",
       plot = fig1_vertical,
       height = 16, 
       width = 9,
       dpi = 100)

## Figure2

# Source: https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code
excludes = c(
  "02", "60", "03", "81", "07", "64",
  "14", "66", "84", "86", "67", "89",
  "68", "71", "76", "69", "70", "95",
  "43", "72", "74", "78", "79", "15", "11"
)

cbgpop <- sf::st_read("data-products/geo-hexes/cbg_population.shp")

hexpop <- sf::st_read("data-products/geo-hexes/hexid-population.shp")

us_states <- tigris::states(cb = T) |> 
  dplyr::filter(!STATEFP %in% excludes) |> 
  tigris::shift_geometry()

new_england_states <- us_states |> 
  dplyr::filter(GEOID %in% c("09","23","25","33","44","50"))

new_england_counties <- tigris::counties(state = c("09","23","25","33","44","50"))

ct_counties <- tigris::counties(state = 09)

cbgpop <- sf::st_transform(cbgpop, crs = 26915)
us_states <- sf::st_transform(us_states, crs = 26915)

low <- "thistle1"
high <- "deeppink3"

us_cbg <- ggplot() + 
  geom_sf(cbgpop,
          mapping = aes(fill=population, color=population)) +
  geom_sf(us_states,
          mapping=aes(),
          color = "deeppink4",
          fill = "transparent")+
  theme_minimal() + 
  scale_fill_gradient(low = low, high = high, 
                      name = "Population", 
                      labels = scales::label_comma(),
                      breaks = scales::breaks_pretty(),
                      guide = metR::guide_colorstrip(title = "Population",
                                                     title.position = "top",
                                                     title.hjust = 0.5,
                                                     barwidth = grid::unit(12, "cm")))+
  scale_color_gradient(low = low, high = high)+
  guides(color = "none")+
  theme(legend.position = "bottom")
us_cbg

new_england <- ggplot() + 
  geom_sf(cbgpop |> 
            filter(substr(GEOID,1,2) %in% c("09","23","25","33","44","50")), 
          mapping = aes(fill=population, color=population)) + 
  geom_sf(new_england_states,
          mapping=aes(),
          color = "deeppink4",
          fill = "transparent")+
  theme_minimal() + 
  scale_fill_gradient(low = low, high = high, 
                      name = "Population", 
                      label = scales::label_comma(),
                      breaks = scales::breaks_pretty(n = 5),
                      guide = metR::guide_colorstrip(title = "Population",
                                                     title.position = "top",
                                                     title.hjust = 0.5,
                                                     barwidth = grid::unit(12, "cm")))+
  scale_color_gradient(low = low, high = high)+
  guides(color = "none")+
  theme(legend.position = "bottom")
new_england

ct <- ggplot() + 
  geom_sf(cbgpop |> 
            filter(substr(GEOID,1,2) == "09"), 
          mapping = aes(fill=population, 
                        color=population)) + 
  geom_sf(ct_counties,
          mapping=aes(),
          color = "deeppink4",
          fill = "transparent")+
  theme_minimal() + 
  scale_fill_gradient(low = low, high = high, 
                      name = "Population",  
                      label = scales::label_comma(),
                      breaks = scales::breaks_pretty(n = 5),
                      guide = metR::guide_colorstrip(title = "Population",
                                                     title.position = "top",
                                                     title.hjust = 0.5,
                                                     barwidth = grid::unit(12, "cm")))+
  scale_color_gradient(low = low, high = high)+
  guides(color = "none")+
  theme(legend.position = "bottom")
ct

library(patchwork)
pop_zoom <- (us_cbg | (new_england / ct))+
  plot_layout(guides = 'collect',
              widths = c(3,1))+
  plot_annotation(tag_levels = 'A')&
  guides(color = "none")&
  theme(legend.position = "bottom")
pop_zoom

ggsave(plot = pop_zoom,
       file = "img/extra_figures/figS2a.png",
       width = 16,
       height = 9,
       dpi = 100
)

ggsave(plot = pop_zoom,
       file = "img/extra_figures/figS2a.pdf",
       width = 16,
       height = 9,
       dpi = 100
)

## Hexgrid plots
# high <- "deeppink1"
# low <- "thistle1"
palette <- "Mako"

us_hex_plt <- ggplot() + 
  geom_sf(hexpop |> 
            filter(!is.na(population)), 
          mapping=aes(fill = log10(population+1),
                      color = log10(population+1))) +
  geom_sf(us_states,
          mapping=aes(),
          color = "deeppink1",
          fill = "transparent")+
  colorspace::scale_fill_continuous_sequential(name = "Population",
                                               rev = F,
                                               breaks = seq(1,6,1),
                                               labels = scales::label_math(),
                                               # guide = guide_colorsteps(),
                                               palette = palette)+
  colorspace::scale_color_continuous_sequential(palette = palette,
                                                rev = F)+
  theme_minimal()+
  guides(color = "none")+
  theme(legend.position = "bottom", 
        legend.key.width = grid::unit(2, "cm"),
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 6))
us_hex_plt

ggsave(plot = us_hex_plt,
       filename = "img/extra_figures/fig2a.png",
       width = 16,
       height = 9,
       dpi = 100)

hexes_to_county <- vroom::vroom("data-products/geo-hexes/hexid-fips-map.csv")

hexpop <- hexpop |> 
  left_join(hexes_to_county |> select(hexid, fips)) |> 
  mutate(STATEFP = str_sub(fips, 1, 2))

new_england_hexpop <- hexpop |> 
  filter(STATEFP %in% c("09","23","25","33","44","50"))

new_england_hex_plt <- ggplot()+
  geom_sf(new_england_hexpop |> 
            filter(!is.na(population)),
          mapping=aes(fill= log10(population+1),
                      color = log10(population+1)))+ 
  geom_sf(new_england_states,
          mapping=aes(),
          color = "deeppink1",
          fill = "transparent")+
  colorspace::scale_fill_continuous_sequential(name = "Population",
                                               rev = F,
                                               breaks = seq(1,6,1),
                                               labels = scales::label_math(),
                                               # guide = guide_colorsteps(),
                                               palette = palette)+
  colorspace::scale_color_continuous_sequential(palette = palette,
                                                rev = F)+
  theme_minimal()+
  guides(color = "none")+
  theme(legend.position = "bottom", 
        legend.key.width = grid::unit(2, "cm"),
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 6))
new_england_hex_plt

ggsave(plot = new_england_hex_plt,
       filename = "img/extra_figures/fig2b.png",
       width = 16, 
       height = 9, 
       dpi = 100)

ct_hexpop <- hexpop |> 
  filter(STATEFP %in% c("09"))

ct_hex_plt <- ggplot()+
  geom_sf(ct_hexpop |> 
            filter(!is.na(population)),
          mapping=aes(fill = log10(population+1),
                      color = log10(population+1)))+ 
  geom_sf(ct_counties,
          mapping=aes(),
          color = "deeppink1",
          fill = "transparent")+
  colorspace::scale_fill_continuous_sequential(name = "Population",
                                               rev = F,
                                               breaks = seq(1,6,1),
                                               labels = scales::label_math(),
                                               # guide = guide_colorsteps(),
                                               palette = palette)+
  colorspace::scale_color_continuous_sequential(palette = palette,
                                                rev = F)+
  theme_minimal()+
  guides(color = "none")+
  theme(legend.position = "none",
        axis.text = element_text(size = 6))
ct_hex_plt

ggsave(filename = "img/extra_figures/fig2c.png",
       plot = ct_hex_plt,
       width = 16,
       height = 9, 
       dpi = 100)

library(patchwork)
p1 <- (new_england_hex_plt / ct_hex_plt)+
  plot_layout(widths = c(1,1),
              guides = "collect",
              heights = c(1,1))&
  theme(legend.position = "none")
p1

ggsave(filename = "img/extra_figures/fig2c.png",
       plot = p1, 
       width = 16, 
       height = 9,
       dpi = 100)

ggsave(filename = "img/extra_figures/fig2c.pdf",
       plot = p1, 
       width = 16, 
       height = 9,
       dpi = 100)

hexpop_zoom <- (us_hex_plt | p1&
                  theme(legend.position = "none"))+
  plot_layout(widths = c(3,1),
              heights = c(3,1))
hexpop_zoom

ggsave(plot = hexpop_zoom, 
       filename = "img/extra_figures/fig2b.png",
       width = 16,
       height = 9,
       dpi = 100)

ggsave(plot = hexpop_zoom, 
       filename = "img/extra_figures/fig2b.pdf",
       width = 16,
       height = 9,
       dpi = 100)

## 
hexes <- sf::st_read("data-products/geo-hexes/hexes.shp")

## Pre-Omicron
hexgrid_preomicron <- vroom::vroom("data-products/geo-hexes/hexid-observations_preomicron.csv") |>
  mutate(hexid = as.character(hexid),
         date = as.Date(date)) |>
  select(-geometry) |>
  mutate(infectionsPC = (infections/population)*1e5) |>
  filter(infectionsPC >= 1) |>
  left_join(hexes, by = "hexid") |>
  sf::st_as_sf()

# ## Omicron-era
hexgrid_omicronera <- vroom::vroom("data-products/geo-hexes/hexid-observations_omicronera.csv") |>
  mutate(hexid = as.character(hexid),
         date = as.Date(date)) |>
  # select(-geometry) |>
  # mutate(infectionsPC = (infections/population)*1e5) |>
  filter(infectionsPC >= 1) |>
  left_join(hexes, by = "hexid") |>
  sf::st_as_sf()

## Hexes joining with the county information
hexgrid_infections <- hexgrid_preomicron |> 
  filter(date == as.Date("2020-11-26")-21) |> ## Alpha peak
  left_join(hexes_to_county |> select(hexid, fips) |> mutate(hexid = as.character(hexid))) |> 
  mutate(STATEFP = str_sub(fips, 1, 2)) |> 
  st_transform(crs = 'ESRI:102009')

hexes <- hexes |> 
  filter(as.integer(hexid) < 7662) |> 
  left_join(hexes_to_county |> 
              select(hexid, fips) |> 
              mutate(hexid = as.character(hexid))) |> 
  mutate(STATEFP = str_sub(fips, 1, 2)) |> 
  st_transform(crs = 'ESRI:102009')

breaks_plt <- seq(1,1001, 100)
labels_plt <- c("1>", seq(100,900, 100), '1,000+')
limits_plt <- c(0,1000)
color_option <- "magma"

### Pre-Omicron
us_hex_infections <- ggplot() + 
  geom_sf(data = hexes,
          fill = 'transparent')+
  geom_sf(hexgrid_infections |> 
            filter(!is.na(infectionsPC)), 
          mapping=aes(fill = infectionsPC, 
                      color = infectionsPC)) +
  geom_sf(us_states,
          mapping=aes(),
          color = "deeppink2",
          fill = "transparent")+
  scale_fill_viridis_c(option = color_option,
                       name = "Estimated Infections/100k",
                       na.value = "grey80",
                       direction = -1,
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
                       oob = scales::squish)+
  scale_color_viridis_c(option = color_option,
                        name = "Estimated Infections/100k",
                        na.value = "grey80",
                        direction = -1,
                        breaks = breaks_plt,
                        labels = labels_plt,
                        limits = limits_plt,
                        oob = scales::squish)+
  theme_minimal()+
  guides(color = "none",
         fill = guide_coloursteps(title.position = "top",
                                  title.vjust = 0.5))+
  theme(legend.position = "bottom", 
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(3, "cm"),
        axis.text = element_text(size = 6))
us_hex_infections

ggsave(filename = "img/extra_figures/fig2e.png", 
       plot = us_hex_infections,
       width = 16,
       height = 9, 
       dpi = 100)

## Omicron-era
# us_hex_infections <- ggplot() + 
#   geom_sf(data = hexes,
#           fill = 'transparent')+
#   geom_sf(hexgrid_infections |> 
#             filter(!is.na(infectionsPC)), 
#           mapping=aes(fill = infectionsPC, 
#                       color = infectionsPC)) +
#   geom_sf(us_states,
#           mapping=aes(),
#           color = "deeppink3",
#           fill = "transparent")+
#   scale_fill_viridis_c(option = color_option,
#                        name = "Infections PC",
#                        # direction = -1,
#                        breaks = seq(1, 5001, 500),
#                        labels = c(">1", seq(500, 4500, 500), "5,000+"),
#                        limits = c(1,5001),
#                        oob = scales::squish,
#                        guide = metR::guide_colorstrip(title.position = "top",
#                                                       title.hjust = 0.5,
#                                                       barwidth = grid::unit(5, "in")))+
#   scale_color_viridis_c(option = color_option,
#                         name = "Infections PC",
#                         # direction = -1,
#                         breaks = seq(1, 5001, 500),
#                         labels = c(">1", seq(500, 4500, 500), "5,000+"),
#                         limits = c(1,5001),
#                         oob = scales::squish,
#                         guide = metR::guide_colorstrip(title.position = "top",
#                                                        title.hjust = 0.5,
#                                                        barwidth = grid::unit(5, "in")))+
#   theme_minimal()+
#   guides(color = "none")+
#   theme(legend.position = "bottom")
# us_hex_infections

new_england_grid_infections <- hexgrid_infections |> 
  filter(STATEFP %in% c("09","23","25","33","44","50"))

new_england_hexes <- hexes |> 
  filter(STATEFP %in% c("09","23","25","33","44","50"))

new_england_hex_infections <- ggplot() + 
  geom_sf(data = new_england_hexes,
          fill = 'transparent')+
  geom_sf(new_england_grid_infections |> 
            filter(!is.na(infectionsPC)), 
          mapping=aes(fill = infectionsPC, 
                      color = infectionsPC)) +
  geom_sf(new_england_states,
          mapping=aes(),
          color = "deeppink2",
          fill = "transparent")+
  scale_fill_viridis_c(option = color_option,
                       name = "Estimated Infections/100k",
                       na.value = "grey80",
                       direction = -1,
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
                       oob = scales::squish)+
  scale_color_viridis_c(option = color_option,
                        name = "Estimated Infections/100k",
                        na.value = "grey80",
                        direction = -1,
                        breaks = breaks_plt,
                        labels = labels_plt,
                        limits = limits_plt,
                        oob = scales::squish)+
  theme_minimal()+
  guides(color = "none",
         fill = guide_coloursteps(title.position = "top",
                                  title.vjust = 0.5))+
  theme(legend.position = "bottom", 
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(3, "cm"),
        axis.text = element_text(size = 6))
new_england_hex_infections

ct_grid_infection <- hexgrid_infections |> 
  filter(STATEFP == "09")

ct_hex_infections <-  ggplot() + 
  geom_sf(ct_grid_infection |> 
            filter(!is.na(infectionsPC)), 
          mapping=aes(fill = infectionsPC, 
                      color = infectionsPC)) +
  geom_sf(ct_counties,
          mapping=aes(),
          color = "deeppink3",
          fill = "transparent")+
  scale_fill_viridis_c(option = color_option,
                       name = "Estimated Infections/100k",
                       na.value = "grey80",
                       direction = -1,
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
                       oob = scales::squish)+
  scale_color_viridis_c(option = color_option,
                        name = "Estimated Infections/100k",
                        na.value = "grey80",
                        direction = -1,
                        breaks = breaks_plt,
                        labels = labels_plt,
                        limits = limits_plt,
                        oob = scales::squish)+
  theme_minimal()+
  guides(color = "none",
         fill = guide_coloursteps(title.position = "top",
                                  title.vjust = 0.5))+
  theme(legend.position = "bottom", 
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(3, "cm"),
        axis.text = element_text(size = 6))
ct_hex_infections

library(patchwork)
hex_infections <- (us_hex_infections | (new_england_hex_infections / ct_hex_infections))+
  plot_layout(widths = c(4,1,1),
              heights = c(4,1,1))+
  plot_layout(guides = 'collect')&
  theme(legend.position = "bottom", 
        axis.text = element_text(size = 6))
hex_infections

ggsave(plot = hex_infections,
       filename = "img/extra_figures/fig2c.pdf",
       width = 16,
       height = 9,
       dpi = 100)

ggsave(plot = hex_infections,
       filename = "img/extra_figures/fig2c.png",
       width = 16,
       height = 9,
       dpi = 100)

## Patchwork all together
fig2 <- (hexpop_zoom / hex_infections)+
  plot_annotation(tag_level = 'A')+
  plot_layout(widths = c(3,1))&
  theme(plot.tag = element_text(size = 18),
        axis.text = element_text(size = 4))
fig2

ggsave(plot = fig2,
       file = "img/fig2.pdf",
       width = 9,
       height = 16,
       dpi = 100
)


## Figure 5


# ## Scatterplot to alphas
# ## Reading the weekly model output
# ## function to build the joined alphas correctly
# joined_alphas <- \(alphas, date_col){
#   alphas <- alphas |> 
#     ## Writing the hexbin code as a 4-digit number
#     dplyr::mutate(hex_i = sprintf("%04d", i),
#                   hex_j = sprintf("%04d", j)) |> 
#     ## Creating a 8-digit hexbin code to identifying uniquely them
#     dplyr::mutate(i_to_j = str_c(hex_i, hex_j),
#                   j_to_i = str_c(hex_j, hex_i))
#   
#   ## Plotting
#   alphas_i_to_j <- alphas |>
#     filter(alpha > 0,
#            .by = c("i_to_j", "date_week")) |> 
#     dplyr::group_by(i_to_j, 
#                     {{ date_col }}) |> 
#     dplyr::summarise(raw = alpha,
#                      normalized = alpha_normalized) |>
#     mutate(standarlized = (raw - mean(raw, na.rm = T))/sd(raw, na.rm = T)) 
#   # |>
#   #   pivot_longer(cols = raw:standarlized,
#   #                names_to = "type",
#   #                values_to = "values")
#   
#   alphas_j_to_i <- alphas |> 
#     filter(alpha > 0,
#            .by = c("j_to_i", "date_week")) |> 
#     dplyr::group_by(j_to_i, 
#                     {{ date_col }}) |> 
#     dplyr::summarise(raw = alpha,
#                      normalized = alpha_normalized)|>
#     mutate(standarlized = (raw - mean(raw, na.rm = T))/sd(raw, na.rm = T)) 
#   # |>
#   #   pivot_longer(cols = raw:standarlized,
#   #                names_to = "type",
#   #                values_to = "values")
#   
#   joined_ij <- dplyr::inner_join(alphas_i_to_j, 
#                                  alphas_j_to_i, 
#                                  by = c("i_to_j" = "j_to_i", "date_week"), 
#                                  suffix = c(".itoj", ".jtoi"))
#   return(joined_ij)
# }
# 
# ## Alphas dataset
# ## Pre-Omicron
# ### Main model
# alphas_week_po_main <- vroom::vroom("data-products/geo-hexes/mixedmodel/alphas_weekly_reformat_preomicron_mainNEW.csv")
# ### SAI model
# alphas_week_po_sa1 <- vroom::vroom("data-products/geo-hexes/mixedmodel/alphas_weekly_reformat_preomicron_SAINEW.csv")
# ### SAII model
# alphas_week_po_sa2 <- vroom::vroom("data-products/geo-hexes/mixedmodel/alphas_weekly_reformat_preomicron_SAIINEW.csv")
# 
# ## Joined data.frames
# ## Main
# joined_ij_week_po_main <- joined_alphas(alphas = alphas_week_po_main, 
#                                         date_col = date_week)
# ## SAI
# joined_ij_week_po_sa1 <- joined_alphas(alphas = alphas_week_po_sa1, 
#                                        date_col = date_week)
# 
# ## SAII
# joined_ij_week_po_sa2 <- joined_alphas(alphas = alphas_week_po_sa2, 
#                                        date_col = date_week)
# 
# fig3c <- joined_ij_week_po_main |> 
#   ggplot(aes(x = raw.itoj, 
#              y = raw.jtoi))+
#   geom_point(alpha = 0.01)+
#   theme_minimal()+
#   labs(x = expression(alpha["i,j"]),
#        y = expression(alpha["j,i"]))+
#   theme(legend.position = "none", 
#         axis.title = element_text(size = 18), 
#         axis.text = element_text(size = 14))
# fig3c
# 
# ggsave(filename = "img/extra_figures/fig3c_preomicron_main.png",
#        plot = fig3c,
#        width = 16,
#        height = 9, 
#        dpi = 300)
# 
# ggsave(filename = "img/extra_figures/fig3c_preomicron_main.pdf",
#        plot = fig3c,
#        width = 16,
#        height = 9, 
#        dpi = 100)
# 
# figS3d <- joined_ij_week_po_sa1 |> 
#   ggplot(aes(x = values.itoj, 
#              y = values.jtoi))+
#   geom_point(alpha = 0.01)+
#   theme_minimal()+
#   labs(x = expression(alpha["i,j"]),
#        y = expression(alpha["j,i"]), 
#        title = "Pre-Omicron SAI Model")+
#   # lims(x = c(-50,60), y = c(-50, 60))+
#   theme(legend.position = "none", 
#         axis.title = element_text(size = 14), 
#         axis.text = element_text(size = 14))
# figS3d
# 
# ggsave(filename = "img/extra_figures/figS3d_preomicron_SAI.png",
#        plot = figS3d,
#        width = 16,
#        height = 9, 
#        dpi = 100)
# 
# figS3e <- joined_ij_week_po_sa2 |> 
#   ggplot(aes(x = raw.itoj, 
#              y = raw.jtoi))+
#   geom_point(alpha = 0.01)+
#   theme_minimal()+
#   labs(x = expression(alpha["i,j"]),
#        y = expression(alpha["j,i"]), 
#        title = "Pre-Omicron SAII Model")+
#   # lims(x = c(-50,60), y = c(-50, 60))+
#   theme(legend.position = "none", 
#         axis.title = element_text(size = 14), 
#         axis.text = element_text(size = 14))
# figS3e
# 
# ggsave(filename = "img/figS3e_preomicron_SAII.png",
#        plot = figS3e,
#        width = 16,
#        height = 9, 
#        dpi = 100)
# 
# ## Omicron-era
# ### Main model
# alphas_week_oe_main <- vroom::vroom("data-products/geo-hexes/mixedmodel/alphas_weekly_reformat_omicronera_mainNEW.csv")
# ### SAI model
# alphas_week_oe_sa1 <- vroom::vroom("data-products/geo-hexes/mixedmodel/alphas_weekly_reformat_omicronera_SAINEW.csv")
# ### SAII model
# alphas_week_oe_sa2 <- vroom::vroom("data-products/geo-hexes/mixedmodel/alphas_weekly_reformat_omicronera_SAIINEW.csv")
# 
# ## Joined data.frames
# ## Main
# joined_ij_week_oe_main <- joined_alphas(alphas = alphas_week_oe_main, 
#                                         date_col = date_week)
# ## SAI
# joined_ij_week_oe_sa1 <- joined_alphas(alphas = alphas_week_oe_sa1, 
#                                        date_col = date_week)
# 
# ## SAII
# joined_ij_week_oe_sa2 <- joined_alphas(alphas = alphas_week_oe_sa2, 
#                                        date_col = date_week)
# 
# fig3d <- joined_ij_week_oe_main |> 
#   ggplot(aes(x = raw.itoj, 
#              y = raw.jtoi))+
#   geom_point(alpha = 0.01)+
#   theme_minimal()+
#   labs(x = expression(alpha["i,j"]),
#        y = expression(alpha["j,i"]), 
#        title = "Omicron-era Main Model")+
#   # lims(x = c(-50,60), y = c(-50, 60))+
#   theme(legend.position = "none", 
#         axis.title = element_text(size = 18), 
#         axis.text = element_text(size = 14))
# fig3d
# 
# ggsave(filename = "img/extra_figures/fig3d_omicronera_main.png",
#        plot = fig3d,
#        width = 16,
#        height = 9, 
#        dpi = 100)
# 
# ggsave(filename = "img/extra_figures/fig3d_omicronera_main.pdf",
#        plot = fig3d,
#        width = 16,
#        height = 9, 
#        dpi = 100)
# 
# fig2f <- joined_ij_week_oe_sa1 |> 
#   ggplot(aes(x = raw.itoj, 
#              y = raw.jtoi))+
#   geom_point(alpha = 0.01)+
#   theme_minimal()+
#   labs(x = expression(alpha["i,j"]),
#        y = expression(alpha["j,i"]), 
#        title = "Omicron-era SAI Model")+
#   # lims(x = c(-50,60), y = c(-50, 60))+
#   theme(legend.position = "none", 
#         axis.title = element_text(size = 14), 
#         axis.text = element_text(size = 14))
# fig2f
# 
# ggsave(filename = "img/fig2f_omicronera_SAI.png",
#        plot = fig2f,
#        width = 16,
#        height = 9, 
#        dpi = 100)
# 
# fig2g <- joined_ij_week_oe_sa2 |> 
#   ggplot(aes(x = raw.itoj, 
#              y = raw.jtoi))+
#   geom_point(alpha = 0.01)+
#   theme_minimal()+
#   labs(x = expression(alpha["i,j"]),
#        y = expression(alpha["j,i"]), 
#        title = "Omicron-era SAII Model")+
#   # lims(x = c(-50,60), y = c(-50, 60))+
#   theme(legend.position = "none", 
#         axis.title = element_text(size = 14), 
#         axis.text = element_text(size = 14))
# fig2g
# 
# ggsave(filename = "img/fig2g_omicronera_SAII.png",
#        plot = fig2g,
#        width = 16,
#        height = 9, 
#        dpi = 100)
# 
# # scatterplot_weekly <- joined_ij_week |> 
# #   ggplot(aes(x = values.itoj, 
# #              y = values.jtoi, 
# #              col = type))+
# #   geom_point(alpha = 0.01)+
# #   theme_minimal()+
# #   labs(title = "Weekly model",
# #        x = expression(alpha~"i to j"),
# #        y = expression(alpha~"j to i"))+
# #   theme(axis.title = element_text(size = 14))+
# #   facet_wrap(type~.)+
# #   theme(legend.position = "none")
# # scatterplot_weekly
# # 
# # ggsave(filename = "img/scatterplot_facetted_alphas_weekly.png",
# #        plot = scatterplot_weekly,
# #        width = 16,
# #        height = 9, 
# #        dpi = 100)
# 
# # hist_weekly <- joined_ij_week |> 
# #   pivot_longer(cols = values.itoj:values.jtoi,
# #                names_to = "relation",
# #                values_to = "values") |> 
# #   mutate(relation = str_remove(relation, "values.")) |> 
# #   ggplot(aes(x = values, fill = relation))+
# #   geom_histogram(bins = 1000)+
# #   facet_wrap(type~., scales = "free")+
# #   theme_minimal()
# # hist_weekly
# # 
# # ggsave(filename = "img/histogram_alphas_weekly.png",
# #        plot = hist_weekly,
# #        width = 16,
# #        height = 9, 
# #        dpi = 100)
# 
# fig2 <- (fig2a | fig2b)+
#   plot_layout(widths = c(2,1))+
#   plot_annotation(tag_levels = 'A')
# fig2
# 
# ggsave(filename = "img/fig2.png",
#        plot = fig2, 
#        width = 16, 
#        height = 9, 
#        dpi = 100)
# 
# # ## Fig2.
# # ## Loading data
# # hexes_obversation_to_fig2 <- vroom::vroom("data-products/geo-hexes/hexid-observations.csv") |> 
# #   filter(date == max(date))
# # 
# # observation_to_fig2 <- vroom::vroom("data-products/covidestim-observations.csv") |> 
# #   filter(date == max(date))
# # 
# ## Fig2A - Layered depiction on transforming estimated infections on counties polygon on hexgrid
# ## Aux functions
# rotate_data <- function(data,
#                         shear_cos_x = 2,
#                         shear_sin_x = 1.2,
#                         shear_cos_y = 0,
#                         shear_sin_y = 1,
#                         x_add = 0, y_add = 0) {
#   
#   shear_matrix <- function(){ matrix(c(shear_cos_x,
#                                        shear_sin_x,
#                                        shear_cos_y,
#                                        shear_sin_y),
#                                      2,
#                                      2) }
#   
#   rotate_matrix <- function(x){
#     matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2)
#   }
#   data %>%
#     dplyr::mutate(
#       geometry = .$geometry * shear_matrix() * rotate_matrix(pi/20) +
#         c(x_add, y_add)
#     )
# }
# 
# rotate_data_geom <- function(data, x_add = 0, y_add = 0) {
#   shear_matrix <- function(){ matrix(c(2, 1.2, 0, 1), 2, 2) }
#   
#   rotate_matrix <- function(x) {
#     matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2)
#   }
#   data %>%
#     dplyr::mutate(
#       geom = .$geom * shear_matrix() * rotate_matrix(pi/20) + c(x_add, y_add)
#     )
# }
# # 
# # ## Joined datasets
# # observation_joined <- us_counties |> 
# #   right_join(observation_to_fig2,
# #              by = c("GEOID" = "fips"))
# # 
# # hexes_joined <- hexes |> 
# #   right_join(hexes_obversation_to_fig2 |> 
# #                mutate(hexid = as.character(hexid)))
# # 
# # # annotate parameters
# # x = -20
# # y = 45
# # color = 'gray40'
# # shear_cos_x = 1.2
# # shear_sin_x = 1
# # 
# # ## Plotting
# # fig2a <- ggplot() +
# #   
# #   # Covidestim obsevartions on counties polygons
# #   geom_sf(data = hexes_joined, 
# #           aes(fill = infectionsPC),
# #           color=NA, 
# #           show.legend = FALSE) +
# #   geom_sf(data = us_counties,
# #           color = "gray80",
# #           fill = NA)+
# #   scale_fill_viridis_c(option = color_option,
# #                        direction = -1,
# #                        # na.value = "darkblue",
# #                        breaks = seq(0,400, 50),
# #                        limits = c(0,400),
# #                        oob = scales::squish,
# #                        guide = metR::guide_colorstrip(title.position = "top",
# #                                                       title.hjust = 0.5,
# #                                                       barwidth = grid::unit(12, "cm")))+
# #   # annotate("text",
# #   #          label='Infections per capita \n (on hexgrid)',
# #   #          x=x,
# #   #          y=y,
# #   #          hjust = 0,
# #   #          color=color) +
# #   theme_void()+
# #   theme(legend.position = "none")
# # fig2a
# # 
# # fig2a <- fig2a +
# #   
# #   # Counties
# #   new_scale_fill() + 
# #   new_scale_color() +
# #   geom_sf(data = hexes %>% 
# #             rotate_data(shear_cos_x = shear_cos_x,
# #                         shear_sin_x = shear_sin_x,
# #                         y_add = 10), 
# #           fill="white", 
# #           color="gray50", 
# #           show.legend = FALSE) +
# #   annotate("text", 
# #            label='Hexes grid', 
# #            x=x, 
# #            y=y+10, 
# #            hjust = 0, 
# #            color=color) +
# #   
# #   # Hexes
# #   new_scale_fill() + 
# #   new_scale_color() +
# #   geom_sf(data = us_counties %>% 
# #             rotate_data(shear_cos_x = shear_cos_x,
# #                         shear_sin_y = shear_sin_x,
# #                         y_add = 20), 
# #           color='gray50', 
# #           fill="white")+
# #   annotate("text", 
# #            label='Counties polygon', 
# #            x=x, 
# #            y=y+20, 
# #            hjust = 0, 
# #            color=color) +
# #   
# #   # Hexes observation
# #   new_scale_fill() + 
# #   new_scale_color() +
# #   geom_sf(data = observation_joined %>% 
# #             rotate_data(shear_cos_x = shear_cos_x,
# #                         shear_sin_y = shear_sin_x,
# #                         y_add = 30), 
# #           aes(fill = infectionsPC),
# #           color=NA)+
# #   scale_fill_viridis_c(option = color_option,
# #                        direction = -1,
# #                        # na.value = "darkblue",
# #                        breaks = seq(0,400, 50),
# #                        limits = c(0,400),
# #                        oob = scales::squish,
# #                        guide = metR::guide_colorstrip(title.position = "top",
# #                                                       title.hjust = 0.5,
# #                                                       barwidth = grid::unit(12, "cm")))+
# #   annotate("text", 
# #            label='Infections per capita', 
# #            x=x, 
# #            y=y+30, 
# #            hjust = 0, 
# #            color=color)+
# #   theme_void() +
# #   scale_x_continuous(limits = c(-105, -10))+
# #   theme(legend.position = "none")
# # fig2a
# # 
# # ggsave(filename = "img/fig2a.png",
# #        plot = fig2a,
# #        width = 16,
# #        height = 9, 
# #        dpi = 100)
