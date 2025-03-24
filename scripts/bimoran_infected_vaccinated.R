rm(list = ls())
gc()

## packages
library(sf)
library(rgeoda)
library(tidyverse)

## Loading data
hexes <- sf::st_read("data-products/geo-hexes/hexes.shp") |> 
  filter(as.integer(hexid) < 7662)

## US States border lines
us_states <- tigris::states(cb = T, resolution = "500k") |> 
  st_transform(crs = 26915) |> 
  filter(!NAME %in% c("Alaska",
                      "Hawaii",
                      "Puerto Rico", 
                      "Guam", 
                      "Commonwealth of the Northern Mariana Islands",
                      "American Samoa",
                      "United States Virgin Islands"))

## Peak dates
alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-08")

## hexgrids
dataset <- "preomicron"

## Pre-Omicron
CAR_df_preomicron <- vroom::vroom(paste0("data-products/tsa_",
                                         dataset, 
                                         ".csv")) |> 
  mutate(hexid = as.character(hexid))

## hexgrids
dataset <- "immunity"

## Pre-Omicron
CAR_df_immunity <- vroom::vroom(paste0("data-products/tsa_",
                                       dataset, 
                                       ".csv")) |> 
  mutate(hexid = as.character(hexid))

## Querry neighbors
guerry <- hexes |> 
  filter(hexid !=6545)

## Queen weights
queen_w <- queen_weights(guerry)

## Alpha
alpha_trend <- CAR_df_preomicron |>
  filter(hexid != 6545) |>  ## excluding the hex which generate NULL weights
  filter(date %in% seq.Date(from = (alpha_peak-128),
                            to = (alpha_peak),
                            length.out = 128)) |>
  mutate(days = (alpha_peak - date),
         trend_alpha = mean,
         wave = "alpha") |>
  select(date, days, hexid, trend_alpha)

alpha_immune <- CAR_df_immunity |>
  filter(hexid != 6545) |>  ## excluding the hex which generate NULL weights
  filter(date %in% c(alpha_peak, alpha_peak-21, alpha_peak-42, 
                     alpha_peak-63, alpha_peak-84, alpha_peak-105,
                     alpha_peak-126)) |>
  mutate(days = (alpha_peak - date),
         immune_alpha = mean,
         wave = "alpha")

alpha_trend_immune <- full_join(alpha_trend, alpha_immune) |> 
  select(-date)

joined_alpha <- list()

for (i in c(0,21,42,63,84,105,126)) {
  
  immune <- alpha_trend_immune |> 
    filter(days == i) |> 
    select(starts_with("immune_"), hexid)
  
  trend <- alpha_trend_immune |> 
    filter(days == 0) |> 
    select(starts_with("trend_"), hexid)
  
  df <- left_join(immune, trend)
  
  bimoran_I <- function(data, var1, var2){
    
    ## Calculating the local bivariate Moran
    lisa_bivariate <- local_bimoran(queen_w,
                                    data |> 
                                      select({{ var1 }}, 
                                             {{ var2 }}))
    
    df <- data
    
    hexid <- data |> 
      pull(var = hexid)
    
    lisa_labels <- lisa_labels(lisa_bivariate)
    lisa_colors <- lisa_colors(lisa_bivariate)
    
    ## Retrieving interesting measure from LISA
    data$local_bimoran <- lisa_values(lisa_bivariate)
    data$pvalues <- lisa_pvalues(lisa_bivariate)
    data$clusters <- lisa_clusters(lisa_bivariate)
    data$labels <- sapply(data$clusters, 
                          function(x){return(lisa_labels[[x+1]])})
    data$colors <- sapply(data$clusters, 
                          function(x){return(lisa_colors[[x+1]])})
    
    p_labels <- c("Not significant", "p <= 0.05", "p <= 0.01", "p <= 0.001")
    
    data$plabels <- sapply(data$pvalues, function(x){
      if (x <= 0.001) return(p_labels[4])
      else if (x <= 0.01) return(p_labels[3])
      else if (x <= 0.05) return (p_labels[2])
      else return(p_labels[1])})
    
    data <- data |> 
      # rename_with(~ paste(., var1, sep = "_")) |> 
      mutate(hexid = as.character(hexid))
    
    return(left_join(df, data))
  }
  
  ## Over the mean, no immunity components separate
  df <- bimoran_I(df, "immune_alpha", "trend_alpha")
  
  ## Over each of the immunity components separately
  # df <- bimoran_I(df, "total", "trend_alpha")
  # df <- bimoran_I(df, "infected", "trend_alpha")
  # df <- bimoran_I(df, "vaccinated", "trend_alpha")
  
  df$day1 <- 0
  df$day2 <- i
  
  i <- ifelse(i == 0, 1, i)
  
  joined_alpha[[i]] <- df
}

joined_alpha <- bind_rows(joined_alpha) |>
  mutate(hexid = as.character(hexid)) |>
  left_join(hexes) |>
  st_as_sf() |> 
  st_transform(crs = 26915)

## Delta
delta_trend <- CAR_df_preomicron |>
  filter(hexid != 6545) |>  ## excluding the hex which generate NULL weights
  filter(date %in% seq.Date(from = (delta_peak-128),
                            to = (delta_peak),
                            length.out = 128)) |>
  mutate(days = (delta_peak - date),
         trend_delta = mean,
         wave = "delta") |>
  select(date, days, hexid, trend_delta)

delta_immune <- CAR_df_immunity |>
  filter(hexid != 6545) |>  ## excluding the hex which generate NULL weights
  filter(date %in% c(delta_peak, delta_peak-21, delta_peak-42, 
                     delta_peak-63, delta_peak-84, delta_peak-105,
                     delta_peak-126)) |>
  mutate(days = (delta_peak - date),
         immune_delta = mean,
         wave = "delta")
# |>
#   rename(immune_delta_total = mean_total,
#          immune_delta_infected = mean_infected,
#          immune_delta_vaccinated = mean_vaccinated) |> 
#   select(date, days, hexid, starts_with("immune_delta_"))

delta_trend_immune <- full_join(delta_trend, delta_immune) |> 
  select(-date)

joined_delta <- list()

for (i in c(0,21,42,63,84,105,126)) {
  
  immune <- delta_trend_immune |> 
    filter(days == i) |> 
    select(starts_with("immune_"), hexid)
  
  trend <- delta_trend_immune |> 
    filter(days == 0) |> 
    select(trend_delta, hexid)
  
  df <- left_join(immune, trend)
  
  bimoran_I <- function(data, var1, var2){
    
    ## Calculating the local bivariate Moran
    lisa_bivariate <- local_bimoran(queen_w,
                                    data |> 
                                      select({{ var1 }}, 
                                             {{ var2 }}))
    
    df <- data
    
    hexid <- data |> 
      pull(var = hexid)
    
    lisa_labels <- lisa_labels(lisa_bivariate)
    lisa_colors <- lisa_colors(lisa_bivariate)
    
    ## Retrieving interesting measure from LISA
    data$local_bimoran <- lisa_values(lisa_bivariate)
    data$pvalues <- lisa_pvalues(lisa_bivariate)
    data$clusters <- lisa_clusters(lisa_bivariate)
    data$labels <- sapply(data$clusters, 
                        function(x){return(lisa_labels[[x+1]])})
    data$colors <- sapply(data$clusters, 
                        function(x){return(lisa_colors[[x+1]])})
    
    p_labels <- c("Not significant", "p <= 0.05", "p <= 0.01", "p <= 0.001")
    
    data$plabels <- sapply(data$pvalues, function(x){
      if (x <= 0.001) return(p_labels[4])
      else if (x <= 0.01) return(p_labels[3])
      else if (x <= 0.05) return (p_labels[2])
      else return(p_labels[1])})
    
    data <- data |> 
      # rename_with(~ paste(., var1, sep = "_")) |> 
      mutate(hexid = as.character(hexid))
      
    return(left_join(df, data))
  }
  
  ## Over the mean, no immunity components separate
  df <- bimoran_I(df, "immune_delta", "trend_delta")
  
  ## Over each of the immunity components separately
  # df <- bimoran_I(df, "total", "trend_delta")
  # df <- bimoran_I(df, "infected", "trend_delta")
  # df <- bimoran_I(df, "vaccinated", "trend_delta")
  
  df$day1 <- 0
  df$day2 <- i
  
  i <- ifelse(i == 0, 1, i)
  
  joined_delta[[i]] <- df
}

joined_delta <- bind_rows(joined_delta) |>
  mutate(hexid = as.character(hexid)) |>
  left_join(hexes) |>
  st_as_sf() |> 
  st_transform(crs = 26915)

fig5b <- joined_alpha |> 
  filter(day2 == 126) |> 
  ggplot()+
  geom_sf(mapping = aes(fill = labels,
                        color = labels))+
  # geom_sf(data = us_states,
  #         color = "deeppink4",
  #         fill = "transparent")+
  scale_fill_manual(name = "Moran's I",
                    values = c("#FF0000","#f4ada8", "#a7adf9", "#0000FF", "#eeeeee"))+
  scale_color_manual(name = "Moran's I",
                     values = c("#FF0000","#f4ada8", "#a7adf9", "#0000FF", "#eeeeee"))+
  theme_void(base_size = 12)+
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  labs(title = "TSA[Immunity] vs TSA[Infections]")
fig5b

fig5c <- joined_delta |> 
  filter(day2 == 63) |> 
  ggplot()+
  geom_sf(mapping = aes(fill = labels,
                        color = labels))+
  # geom_sf(data = us_states,
  #         color = "deeppink4",
  #         fill = "transparent")+
  scale_fill_manual(name = "Moran's I",
                    values = c("#FF0000","#f4ada8", "#a7adf9", "#0000FF", "#eeeeee"))+
  scale_color_manual(name = "Moran's I",
                     values = c("#FF0000","#f4ada8", "#a7adf9", "#0000FF", "#eeeeee"))+
  theme_void(base_size = 12)+
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  labs(title = "TSA[Immunity_infection] vs TSA[Infections]",
       subtitle = "July 7th, 2021")
fig5c

fig5c <- joined_dataframe |> 
  filter(day2 == 63) |> 
  ggplot()+
  geom_sf(mapping = aes(fill = labels_infected,
                        color = labels_infected))+
  # geom_sf(data = us_states,
  #         color = "deeppink4",
  #         fill = "transparent")+
  scale_fill_manual(name = "Moran's I",
                    values = c("#FF0000","#f4ada8", "#a7adf9", "#0000FF", "#eeeeee"))+
  scale_color_manual(name = "Moran's I",
                     values = c("#FF0000","#f4ada8", "#a7adf9", "#0000FF", "#eeeeee"))+
  theme_void(base_size = 12)+
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  labs(title = "TSA[Immunity_vaccinated] vs TSA[Infections]",
       subtitle = "July 7th, 2021")
fig5c

library(patchwork)
patchwork <- (fig5b | fig5c)+
  plot_layout(guides = "collect")&
  theme(legend.position = "bottom")
patchwork
