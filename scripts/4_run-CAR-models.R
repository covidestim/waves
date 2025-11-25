gc()
rm(list = ls())
###############################################################################
##### Load in the required packages                                       #####
##############################################################################|
library(tidyverse)
library(sf)
library(spdep)
library(spatialreg)
library(Matrix)
library(INLA)
library(foreach)
library(doParallel)
library(dplyr)
library(tibble)

###############################################################################
##### Load in the required data files                                     #####
##############################################################################|

##### Hexgrid |
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

## Certifying the correct number of unique hex; 7516
length(unique(na.omit(hexes$hexid)))
##############################################################################|
## Pre-Omicron infections allocated to hexgrid
hexgrid_preomicron <- vroom::vroom("Data/data-products/geo-hexes/hexid-observations_preomicron_intersection_hexgrid1100km.csv") %>% 
  mutate(date = as.Date(date)) |>
  filter(
    ## Taking out the isolated hex at Keywest
    as.integer(hexid) != 6644) %>%
  # INLA requires the id to only be 1:N, where N is the total
  # number of observations; because of this we need to rename the 
  # hexids to be continuous. 
  mutate(hexid = ifelse(as.numeric(hexid) < 6645, as.numeric(hexid), 
                        as.numeric(hexid) - 1),
         hexid = as.character(hexid)) #%>% 
#filter(populationTotal > 0) 

# hexgrid_preomicronGEOM <- hexgrid_preomicron %>% filter(date==delta_peak) %>%
#                           left_join(hexes, by="hexid")
# 
# ggplot() + geom_sf(data=hexgrid_preomicronGEOM %>% filter(infectionsPC > 0),
#                    aes(geometry=geometry, fill=infectionsPC))
# 
# alphaPeakPop <- hexgrid_preomicron %>% filter(date==alpha_peak) %>% 
#                 select(population) 
# 
# deltaPeakPop <- hexgrid_preomicron %>% filter(date==delta_peak) %>% 
#                 select(population) 
# 
# identical(alphaPeakPop, deltaPeakPop)
# 
# dim(alphaPeakPop)
# dim(deltaPeakPop)

## Certifying the correct number of unique hex; 7332
length(unique(na.omit(hexgrid_preomicron$hexid)))

### Filter out hexagons with zero population from the hexgrid before 
### next steps. 
hexes <- hexes %>% 
  filter(hexid %in% hexgrid_preomicron$hexid) 

## Certifying the correct number of unique hex; 7332
length(unique(na.omit(hexes$hexid)))

##############################################################################|
## Pre-Omicron expanded dataset
##### Reformat the data to give INLA a complete dataframe 
##### Gives full hexgrid and date for smoothing 
hex_spacetime <- expand.grid(hexid = unique(hexes$hexid),
                             date = seq.Date(from = min(hexgrid_preomicron$date),
                                             to = max(hexgrid_preomicron$date), 
                                             by = "day")) |> 
  left_join(hexgrid_preomicron |> 
              st_drop_geometry() |> 
              select(hexid, date, infections, infectionsPC)) |>  
  mutate(Time = as.numeric(date - min(date)) + 1, 
         infectionsPC = infectionsPC*1e5) %>% 
  group_by(date) %>% 
  mutate(id = 1:n())

## Certifying the correct number of unique hex; 7332
length(unique((hex_spacetime$hexid)))

###############################################################################
##### Identify neighboring hexes                                          #####
##############################################################################|
## Neighbors
hexes_nb <- spdep::poly2nb(hexes, queen = TRUE, row.names = hexes$id)

# head(st_centroid(hexes))
# ggplot() + geom_sf(data=(st_centroid(hexes)), size=.5) + 
#   geom_sf(data=hexes, fill= NA)

#### Create and save the graph file needed for INLA
nb2INLA("Data/data-products/hexes_adjmat.graph", nb = hexes_nb)
#### Assign this graph code to object
hexes_graph <- INLA::inla.read.graph("Data/data-products/hexes_adjmat.graph")

###############################################################################
##### Clean up environment before entering model setup and run            #####
##############################################################################|
rm(hexes)
rm(hexgrid_preomicron)
rm(hexes_nb)

## CAR INLA equivalent model

compute_list <- control.compute(hyperpar = T, 
                                return.marginals = T, 
                                return.marginals.predictor = T,
                                config = T,
                                # openmp.strategy = "huge",
                                dic = T, 
                                cpo = T, 
                                waic = T)

predictor_list <- control.predictor(compute = T)

diag.eps = 1e-3

###############################################################################
##### Set which days to run the model for                                 #####
##############################################################################|
##### Define the peaks of interest
alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-04")

days <- c(seq.Date(from = alpha_peak-63, to = alpha_peak, by = "day"),
          seq.Date(from = delta_peak-63, to = delta_peak, by = "day"))

##### Create an empty list to keep model output
  
  CAR_list <- list()
  CAR_diag_list <- list()
  
  # Dates for testing the algorithm in development
  # test_dates <- c(c(alpha_peak-63,
  #                   alpha_peak-54,
  #                   alpha_peak-45,
  #                   alpha_peak-39,
  #                   alpha_peak-30,
  #                   alpha_peak-24,
  #                   alpha_peak-11,
  #                   alpha_peak),
  #                 c(delta_peak-63,
  #                   delta_peak-54,
  #                   delta_peak-45,
  #                   delta_peak-39,
  #                   delta_peak-30,
  #                   delta_peak-24,
  #                   delta_peak-11,
  #                   delta_peak))

###############################################################################
##### Run the model 
##############################################################################|
for (i in 1:length(days)) {

  current_date <- days[i]
  cat("Starting model for date: ", as.character(current_date),"!\n")
  
  hex_week <- hex_spacetime %>% 
    filter(date == current_date)
  
  #### Setup for the while loop 
  best_model <- NULL # holds the model fit
  ## counter for while
  counter <- 0
  while_condition <- TRUE
  initial_value <- 0.1
  while ((while_condition) & counter <= 8) {  
    
    hyper_smooth_bym2 <- list(
      phi = list(prior = "pc", param = c(.5, 2/3),
                 initial=initial_value), 
      prec = list(prior = "pc.prec", param = c(0.5/3, 0.01)
      )
    )
    
    cat("Attempt to fit the model number: ", counter, "!\n")
    ### sometimes a bad date can break the INLA model, this tryCatch
    ### is to avoid escaping the loop due to one date error. 
    tryCatch({
      seed <- sample(1:1e6,1) #386860
      set.seed(seed)
      # set.seed(.Random.seed) ## change the seed for reruns 
      ### Call the BYM2 model with INLA
      best_model <- inla(
        as.formula(infectionsPC ~ 1 + 
                     f(id, ### either need to update data above or id to hexid
                       model = "bym2", 
                       graph = hexes_graph,
                       scale.model = TRUE, 
                       # diagonal = diag.eps,
                       constr = TRUE, ## sum to zero constraint 
                       hyper = hyper_smooth_bym2
                     )),
        data = as.data.frame(hex_week),
        family = "gaussian", #likelihood
        control.inla = control.inla(strategy = "gaussian", int.strategy = "grid"), #strategy to sample the hyperparameter space
        control.mode = control.mode(restart = TRUE),  
        control.compute = compute_list,
        control.predictor = predictor_list,
        # control.fixed = list(prec.intercept = 0.1),
        num.threads = 1#,  # Prevent internal threading conflicts (needs to be 1 for parallelized code)
        # verbose = T
      )
      
    }, error = function(e) {
      
    })
    
    counter <- counter + 1
    initial_value <- initial_value + 0.1 
    # Update while condition
    tryCatch({
      if(best_model$internal.summary.hyperpar[2,"mean"] < -7 |
         best_model$internal.summary.hyperpar[2,"mean"] > -4) {
        while_condition <- TRUE 
      } else {
        while_condition <- FALSE
      }
    }, error = function(e) {
      
    })
  } # end while loop
  cat("Finished CAR model for week ", as.character(current_date),"! \n")
  
  CAR_list[[i]] <- cbind(hex_week, 
                         best_model$summary.fitted.values |> 
                           rownames_to_column(var = "INLApred"))
  
  CAR_diag_list[[i]] <- list(seed, 
                             best_model$summary.hyperpar, 
                             best_model$mlik)
  ##### Clean up memory before next run
  rm(best_model, hex_week)
  gc()
}

###############################################################################
##### Reformat the CAR list into a dataframe and saving it. 

## Turning into a df
CAR_df <- bind_rows(CAR_list) 

## Saving the df, remember to change the name if the dataset is "preomicron" or "omicronera". The pattern nomenclature to files are tsa_preomicron.csv or tsa_omicronera.csv
dataset <- "wave2_adaptStart"

vroom::vroom_write(x = CAR_df,
                   file = paste0("Data/data-products/car_",
                                 dataset,
                                 ".csv"))

## Saving as list object
saveRDS(CAR_list,
        file = paste0("Data/data-products/car_list_", dataset, ".rds"),
        version=2)

# save(list = CAR_list, 
#      file = "Data/data-products/car_list.RDS", 
#      compress = "xz", 
#      compression_level = 9)
# CAR_list <- readRDS("Data/data-products/car_list_Wave1.RDS")
###############################################################################
##### CHECK WHETHER THE MODEL FIT with some plots #####
###############################################################################

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


CAR_df_test <- CAR_df |> 
  # filter(date %in% test_dates) |> 
  right_join(hexes) |>
  st_as_sf()

color_option <- "magma"
na_color <- "grey70"
## Breakdowns of each peaks
breaks_plt <- c(0,seq(25,275, 25))
labels_plt <- c("25< ",seq(25,250, 25), ' >250')
limits_plt <- c(0,300)

ggplot() +
  geom_sf(data = CAR_df_test |> 
            filter(!is.na(date)),
          aes(fill = mean), color = NA)+
  scale_fill_viridis_b(option = color_option,
                       # name = "Estimated Infections/1000/week",
                       direction = -1,
                       na.value = na_color,
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt
  )+
  # scale_fill_viridis_c(option = color_option, 
  #                      name = "Estimated Infections/100k/day", direction = -1)+
  geom_sf(data = us_states, aes(geometry = geometry), fill = NA) +
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "in"))+
  facet_wrap(.~date, nrow= 7)
