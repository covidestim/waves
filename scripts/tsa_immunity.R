rm(list = ls())
gc()

library(tidyverse)
library(sf)
library(spdep)
library(spatialreg)

## 
hexes <- sf::st_read("data-products/geo-hexes/hexes.shp") |> 
  filter(as.integer(hexid) < 7662)

hexes_nb <- poly2nb(hexes, queen = T, snap = 0)
# hexes_nb2 <- st_touches(st_geometry(hexes))

# wt_B <- nb2mat(neighbours = hexes_nb, style = "B", zero.policy = T)
wt_W <- nb2mat(neighbours = hexes_nb, style = "W", zero.policy = T)

# listB <- nb2listw(neighbours = hexes_nb, style = "B", zero.policy = T)
listW <- nb2listw(neighbours = hexes_nb, style = "W", zero.policy = T)

## Weight list as a Sparse C Matrix
# B <- as(listB, "CsparseMatrix")
W <- as(listW, "CsparseMatrix")

## Loading centroids shapefile
hexes_centroids <- as.data.frame(st_coordinates(st_cast(st_centroid(hexes), "MULTIPOINT"))) |> 
  rename(hexid = L1) |> 
  mutate(hexid = as.character(hexid)) |> 
  left_join(hexes) |> 
  mutate(geometry = st_centroid(geometry)) |> 
  st_as_sf()

## Immunity
hexgrid_immune <- vroom::vroom("data-products/geo-hexes/hexid-immunity.csv") |> 
  mutate(hexid = as.character(hexid))

hexgrid_immune <- hexgrid_immune |> 
  mutate(immunePC = (immune.count/population)*1e5) |> 
  left_join(hexes) |>
  st_as_sf() |> 
  sf::st_transform(crs = 26915)

ggplot() + 
  geom_sf(hexgrid_immune |> 
            filter(date == delta_peak), 
                   mapping = aes(fill = immunePC))+
  theme_minimal() +
  scale_fill_gradient(low = "thistle1", high = "deeppink4", na.value = "green")+
  theme(legend.position = "bottom", 
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(2, "cm"))+
  guides(fill = guide_colorbar(nbin = 10))

## Population hexes
hex_population <- vroom::vroom("data-products/geo-hexes/hexid-population.csv")

## Immunity dataset expanded
hex_spacetime <- expand.grid(hexid = unique(hexes$hexid),
                             date = seq.Date(from = min(hexgrid_immune$date),
                                             to = max(hexgrid_immune$date), 
                                             by = "day")) |> 
  left_join(hex_population |> 
              mutate(hexid = as.character(hexid))) |> 
  ## just join the desired columns, we have the geometry
  left_join(hexgrid_immune) |>  
  # st_as_sf()|> 
  mutate(Time = as.numeric(date - min(date)) + 1,
         ID = as.numeric(hexid),
         immunePC = (immune.count/population)*1e5)

## Peak dates
alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-08")

## Filtering to 128 before peaks dates
hex_spacetime <- hex_spacetime |> 
  filter(date %in% c(seq.Date(from = (alpha_peak - 128),
                            to = alpha_peak,
                            length.out = 128),
                     seq.Date(from = (delta_peak - 128),
                              to = delta_peak,
                              length.out = 128)))

# ## Rerun setting up
# ## Pre-Omicron
# CAR_df_rerun <- vroom::vroom("data-products/tsa_preomicron.csv") 
# 
# ## Omicron-era
# # CAR_df_rerun <- vroom::vroom("data-products/tsa_omicronera.csv") 
# 
# ## Returning the data.frame into a list format
# CAR_list_rerun <- CAR_df_rerun |> 
#   group_split(date) |> 
#   as.list()

## Reruns accouting
## 499:628 ok!
## 398:498 ok!
## 297:397 ok!
## 196:296 ok!
## 95:195 ok!
## 1:94 ok!

weeks <- sort(unique(hex_spacetime$date))

## List of CAR models per dates
CAR_list <- list()

## Flag if it is a rerun
is.rerun <- FALSE

## This should be run in batches, as it may take long time and not be suit for memory size.
## The fitting strategy is to run the model for all dates and rerun over the problematic dates, when the model didn't fit well
## One very common issue is returning a bad run from INLA, 
## which only the iid part fits to the model and not the Besag model.
## The Newton-Raphson method can not converge, the method is used to some numerical integration on the hyperparameters. 
## Another alternative is to log-transform the data before imputing to the INLA model, 
## Hävard Rue suggested this on a help desk response at INLA email list. 
## Yet another alternative is to set the prior parameterization to the model that can afford huge variance on the data ## Omicron dataset case. We can set the shape and rate parameters of log-Gamma prior to have mean = 1 and variance = 1.

## CAR INLA equivalent model
library(INLA)
compute_list <- control.compute(hyperpar = T, 
                                return.marginals = T, 
                                return.marginals.predictor = T,
                                dic = T, 
                                cpo = T, 
                                waic = T)

predictor_list <- control.predictor(compute = T)

for (i in 1:length(weeks)) {
  
  ## Checking if it is a rerun
  if(is.rerun){
    ## Dates to rerun
    if(is.na(dates_to_rerun[[i]])){next} 
    
    ## Making counter, if the model has as a median of sd for fitted values that it is less than 10,
    ## after 10 iterations means the date is a not super variable date, so the model is write
    counter <- 0
    while (exp(median(CAR_list[[i]]$sd) < 10 && counter <= 10)) {
      hex_spacetime_sp <- hex_spacetime |> 
        filter(date == weeks[[i]])
      
      cat("Initializing model for week:", as.character(weeks[i]),"  \n", sep = " ")
      
      ## Handling errors even if after successive runs
      tryCatch({
        ## Setting seeds to reproducibility
        set.seed(.Random.seed)
        
        CAR_model <- inla(formula = as.formula(immune.count ~ 1+
                                                 f(ID, 
                                                   model = "bym2",
                                                   graph = wt_W)),
                          data = as.data.frame(hex_spacetime_sp),
                          # offset = log(population),
                          family = "poisson",
                          verbose = T,
                          control.compute = compute_list,
                          control.predictor = predictor_list)}, 
        error = {
          ## Setting seeds to reproducibility
          set.seed(.Random.seed)
          
          CAR_model <- inla(formula = as.formula(immune.count ~ 1+
                                                   f(ID, 
                                                     model = "bym2",
                                                     graph = wt_W)),
                            data = as.data.frame(hex_spacetime_sp),
                            # offset = log(population),
                            family = "nbinomial",
                            verbose = T,
                            control.compute = compute_list,
                            control.predictor = predictor_list)})
      
      CAR_list[[i]] <- cbind(hex_spacetime_sp, 
                             CAR_model$summary.fitted.values |> 
                               rownames_to_column(var = "INLApred"))
      
      counter = counter + 1
    }
  }else{
    hex_spacetime_sp <- hex_spacetime |> 
      filter(date == weeks[[i]])
    
    cat("Fitting a first try model \n")
    
    ## Setting seeds to reproducibility
    set.seed(.Random.seed)
    
    counter <- 1
    cat("Initializing model for week:", as.character(weeks[i]),"  \n", sep = " ")
    
    CAR_model <- inla(formula = as.formula(log10(immune.count+1) ~ 1+
                                             f(ID, 
                                               model = "bym2",
                                               graph = wt_W)),
                      data = as.data.frame(hex_spacetime_sp),
                      # offset = log(population),
                      family = "gaussian",
                      verbose = T,
                      control.compute = compute_list,
                      control.predictor = predictor_list)
    
    CAR_list[[i]] <- cbind(hex_spacetime_sp, 
                           CAR_model$summary.fitted.values |> 
                             rownames_to_column(var = "INLApred")
    )
  }
  
  rm(hex_spacetime_sp, CAR_model)
  
  cat("Finished CAR model for week ", as.character(weeks[i]),"! \n", 
      "And the counter went until: ", counter, "\n", sep = "")
}

## List to rerun if the model is not well fitted to the data
dates_to_rerun <- lapply(CAR_list, function(x){ifelse(median(10^(x$sd))<10, x$date, NA)})

## Rerun flag, set this to TRUE is the dates to rerun are a lot to get better model estimates
is.rerun <- TRUE

## setting the hexid column on the same class
CAR_list <- lapply(CAR_list, function(x){x <- x |> mutate(hexid = as.integer(hexid))})

## Turning into a df
CAR_df <- bind_rows(CAR_list)

## Saving the df, remember to change the name if the dataset is "preomicron" or "omicronera". The pattern nomenclature to files are tsa_preomicron.csv or tsa_omicronera.csv
dataset <- "immunity"

vroom::vroom_write(x = CAR_df, 
                   file = paste0("data-products/tsa_", 
                                 dataset,
                                 ".csv"))

#


## Prototyping codes to check model convergence and plots

## Breakdowns of each peaks
breaks_plt <- c(0,seq(150,300, 20))
labels_plt <- c("150< ",seq(150,280, 20), ' >300')
limits_plt <- c(0,350)
color_option <- "magma"
na_color <- "grey70"

ggplot(data = hexes |> 
         cbind(predCAR_B = CAR_df |> filter(date == alpha_peak-63) |> pull(var = mean)) |> 
         st_transform(crs = 26915)) +
  geom_sf(aes(fill = 10^(predCAR_B)))+
  # scale_fill_viridis_b(option = color_option,
  #                      name = "Estimated Infections/100k/week",
  #                      direction = -1,
  #                      breaks = breaks_plt,
  #                      labels = labels_plt,
  #                      limits = limits_plt,
  # )+
  scale_fill_viridis_b(option = "magma",
                       n.breaks = 10,
                       labels = scales::label_comma(),
                       direction = -1)+
  theme_minimal()+
  theme(legend.position = "right",
        legend.title.position = "left",
        legend.title = element_text(angle = 90, hjust = 0.5),
        legend.direction = "vertical",
        legend.key.height = grid::unit(1, "cm"))+
  guides(fill = guide_bins(title = "Trend surface estimated immune per million",
                           title.position = "left",
                           title.vjust = 0.5))+
  labs(title = "Trend Surface over immunity estimates",
       subtitle = "CAR model with BYM2 implementation",
       caption = "*Proportional weights means weights ranging from 1 to 0")

