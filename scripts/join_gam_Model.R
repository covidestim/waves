## Cleaning
rm(list = ls())
gc()

## Loadiung libraries
library(tidyverse)
library(sf)

### Reload hexgrid 
hexes <- st_read("Data/data-products/geo-hexes/hexgrid_1100_km.shp") |>
  filter(
    # Taking out the isolated hex at Keywest
    as.integer(hexid) != 6644) %>%
  # INLA requires the id to only be 1:N, where N is the total
  # number of observations; because of this we need to rename the
  # hexids to be continuous.
  mutate(hexid = ifelse(as.numeric(hexid) < 6645, as.numeric(hexid),
                        as.numeric(hexid) - 1),
         hexid = as.character(hexid))

### Loading GAM model outputs
load("Data/data-products/gam-model/alpha_wave_fits_Nov-24-2025.rData")
load("Data/data-products/gam-model/delta_wave_fits_Nov-24-2025.rData")

## Old code that the function is based
# alphaPreds500DF <- as.data.frame(alphaPreds500) 
# alphaPreds500DF$hexid <- rownames(alphaPreds500)
# CAR_df_test <-  alphaPreds500DF |> 
#   reshape2::melt(id.vars=c("hexid"), value.name = "mean", variable = "date") %>% 
#   right_join(hexes) |>
#   st_as_sf()

### Joining each gam run with the hexgrid
make_it_long <- function(hexgrid, gam){
  gam <- as.data.frame(gam)
  gam$hexid <- rownames(gam)
  gam_long <- gam |> 
    reshape2::melt(id.vars=c("hexid"), value.name = "mean", variable = "date") %>% 
    mutate(date = as.Date(date)) |> 
    right_join(hexes) |>
    st_as_sf()
  return(gam_long)
}

## 100
wave1_pred_100 <- make_it_long(hexgrid = hexes, gam = alphaPreds100)
wave2_pred_100 <- make_it_long(hexgrid = hexes, gam = deltaPreds100)

joined_waves_100 <- rbind(wave1_pred_100 |> st_drop_geometry() |> mutate(wave = "1st wave"),
                          wave2_pred_100 |> st_drop_geometry() |> mutate(wave = "2nd wave"))

vroom::vroom_write(joined_waves_100, file = "Data/data-products/gam-model/gam_joined_waves_100.csv")

## 200
wave1_pred_200 <- make_it_long(hexgrid = hexes, gam = alphaPreds200)
wave2_pred_200 <- make_it_long(hexgrid = hexes, gam = deltaPreds200)

joined_waves_200 <- rbind(wave1_pred_200 |> st_drop_geometry() |> mutate(wave = "1st wave"),
                          wave2_pred_200 |> st_drop_geometry() |> mutate(wave = "2nd wave"))

vroom::vroom_write(joined_waves_200, file = "Data/data-products/gam-model/gam_joined_waves_200.csv")

## 500
wave1_pred_500 <- make_it_long(hexgrid = hexes, gam = alphaPreds500)
wave2_pred_500 <- make_it_long(hexgrid = hexes, gam = deltaPreds500)

joined_waves_500 <- rbind(wave1_pred_500 |> st_drop_geometry() |> mutate(wave = "1st wave"),
                          wave2_pred_500 |> st_drop_geometry() |> mutate(wave = "2nd wave"))

vroom::vroom_write(joined_waves_500, file = "Data/data-products/gam-model/gam_joined_waves_500.csv")

## 10000
wave1_pred_1000 <- make_it_long(hexgrid = hexes, gam = alphaPreds1000)
wave2_pred_1000 <- make_it_long(hexgrid = hexes, gam = deltaPreds1000)

joined_waves_1000 <- rbind(wave1_pred_1000 |> st_drop_geometry() |> mutate(wave = "1st wave"),
                          wave2_pred_1000 |> st_drop_geometry() |> mutate(wave = "2nd wave"))

vroom::vroom_write(joined_waves_1000, file = "Data/data-products/gam-model/gam_joined_waves_1000.csv")

## Joining all
joined_waves_all <- rbind(joined_waves_100 |> mutate(smooth = 100),
                          joined_waves_200 |> mutate(smooth = 200),
                          joined_waves_500 |> mutate(smooth = 500),
                          joined_waves_1000 |> mutate(smooth = 1000))

## Selecting threshold for defining being within a wave or not
# figS2a <- 
  ggplot(data = joined_waves_all,
                 aes(x = mean, fill = smooth, group = smooth), alpha = 0.10)+
  # geom_histogram(bins = 10000)+
  geom_density()+
  theme_minimal()+
  # xlim(c(0,300))+
  labs(x = expression("GAM mean estimates " *theta ~ "(Ai)"),
       y = "Frequency")+
  scale_y_continuous(labels = scales::label_comma())
# figS2a

ecdf_mean <- ecdf(joined_waves_all$mean)
# ecdf_upper <- ecdf(CAR_df_preomicron$`0.975quant`)
# ecdf_lower <- ecdf(CAR_df_preomicron$`0.025quant`)
# ecdf_median <- ecdf(CAR_df_preomicron$`0.5quant`)

# figS2b <- 
  ggplot(data = joined_waves_all,
                 aes(x = mean, y = ecdf_mean(mean), color = smooth, group = smooth))+
  geom_line()+
  # geom_ribbon(aes(x = mean, 
  #                 ymin = ecdf_lower(`0.025quant`), 
  #                 ymax = ecdf_upper(`0.975quant`)))+
  geom_vline(xintercept = 184, linetype = "dashed")+
  theme_minimal()+
  labs(x = expression("GAM mean estimates " *theta ~ "(Ai)"),
       y = "Percentile")
# figS2b


## Test plotting
color_option <- "magma"
na_color <- "grey70"
## Breakdowns of each peaks
breaks_plt <- c(0,seq(184,500, 25))
labels_plt <- c("184< ",seq(185,360, 25), ' >360')
limits_plt <- c(0,800)

# ## Figure2
# # Source: https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code
excludes = c(
  "02", "60", "03", "81", "07", "64",
  "14", "66", "84", "86", "67", "89",
  "68", "71", "76", "69", "70", "95",
  "43", "72", "74", "78", "79", "15", "11"
)
# 
us_states <- tigris::states(cb = T) |>
  dplyr::filter(!STATEFP %in% excludes) #|>
  # tigris::shift_geometry()

##### Define the peaks of interest
alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-04")

ggplot() +
  geom_sf(data = wave1_pred_500 |> 
            filter(date == date_i),
          aes(fill = mean), color = NA)+
  scale_fill_viridis_b(option = color_option,
                       # name = "Estimated Infections/1000/week",
                       direction = -1,
                       na.value = na_color,
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
  )+
  # scale_fill_viridis_c(option = color_option,
  #                      name = "Estimated Infections/100k/day", direction = -1)+
  geom_sf(data = us_states, aes(geometry = geometry), fill = NA) +
  theme_void()+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "in"))+
  # facet_wrap(.~date, nrow = 8)+
  labs(title = "Wave 1 - Alpha Peak")

ggplot() +
  geom_sf(data = wave2_pred_100 |> 
            filter(!is.na(date)),
          aes(fill = mean), color = NA)+
  scale_fill_viridis_b(option = color_option,
                       # name = "Estimated Infections/1000/week",
                       direction = -1,
                       na.value = na_color,
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
  )+
  # scale_fill_viridis_c(option = color_option, 
  #                      name = "Estimated Infections/100k/day", direction = -1)+
  geom_sf(data = us_states, aes(geometry = geometry), fill = NA) +
  theme_void()+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "in"))+
  facet_wrap(.~date, nrow = 8)+
  labs(title = "Wave 2 - 64 dates")

