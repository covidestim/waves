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
                               as.numeric(hexid) - 1))

## Hexgrid pop
## New hexgrid with Meta 30m population
hexgrid_pop <- st_read("Data/data-products/geo-hexes/hexgrid_1100_km_meta_pop.shp") |> 
  filter(## Taking out the isolated hex at Keywest
         as.integer(hexid) != 6644) |>
  rename(population = meta_pop) %>%  
  mutate(logpopulation = log(population),
         # INLA requires the id to only be 1:N, where N is the total
         # number of observations; because of this we need to rename the 
         # hexids to be continuous. 
         hexid = ifelse(as.numeric(hexid) < 6645, as.numeric(hexid), as.numeric(hexid) - 1)) %>% 
  filter(population != 0)

# hexesFilt <- hexes %>% filter(hexid %in% hexgrid_pop$hexid)

# hexgrid_pop_old <- st_read("~/Desktop/untitled folder 3/data-products/geo-hexes/meta_pop_new/hex_pop_meta_new.shp")#
# 
# 
# ggplot(hexgrid_pop) + geom_sf(aes(fill=population)) + 
#   scale_fill_viridis_c(option = color_option, limits = c(0,5*1e6),
#                        name = "Population", direction = -1)
#   
# ggplot(hexgrid_pop_old) + geom_sf(aes(fill=meta_pop)) +
#   scale_fill_viridis_c(option = color_option, limits = c(0,5*1e6),
#                        name = "Population", direction = -1)
### Check which hexes have population zero
# ggplot() + geom_sf(data=hexgrid_pop) + 
#   geom_sf(data = hexgrid_pop %>% filter(population ==0), fill = "red")

# hexgrid_pop <- hexgrid_pop %>% filter(population > 0)

### Check for equal crs 
st_crs(hexes) == st_crs(hexgrid_pop)

###############################################################################
##### Identify neighboring hexes                                          #####
##############################################################################|
## Neighbors
hexes_nb <- spdep::poly2nb(hexes, queen = TRUE, row.names = hexes$hexid)

## Check Plot
# plot(st_geometry(hexes))
# plot(hexes_nb, st_coordinates(st_centroid(hexes)), add=TRUE, col="blue")

#### Create and save the graph file needed for INLA
nb2INLA("Data/data-products/hexes_adjmat.graph", nb = hexes_nb)
#### Assign this graph code to object
hexes_graph <- INLA::inla.read.graph("Data/data-products/hexes_adjmat.graph")

##############################################################################|
#### Recoding the adjacency matrix to numeric matrix
#### Do not use unless needing to troubleshoot model convergence. 
# wt_B <- nb2mat(neighbours = hexes_nb, style = "B", zero.policy = T)
# wt_W <- nb2mat(neighbours = hexes_nb, style = "W", zero.policy = T)
# 
# listB <- nb2listw(neighbours = hexes_nb, style = "B", zero.policy = T)
# listW <- nb2listw(neighbours = hexes_nb, style = "W", zero.policy = T)
# 
# ## Weight list as a Sparse C Matrix
# B <- as(listB, "CsparseMatrix")
# W <- as(listW, "CsparseMatrix")
##############################################################################|
##############################################################################|

## Certifying the correct number of unique hex; 7516
length(unique(na.omit(hexes$hexid)))

## Pre-Omicron 
hexgrid_preomicron <- vroom::vroom("Data/data-products/geo-hexes/hexid-observations_preomicron_hexgrid1100km.csv") |>
  mutate(hexid = as.character(hexid),
         date = as.Date(date)) |>
  filter(
         ## Taking out the isolated hex at Keywest
         as.integer(hexid) != 6644) %>%
  # INLA requires the id to only be 1:N, where N is the total
  # number of observations; because of this we need to rename the 
  # hexids to be continuous. 
  mutate(hexid = ifelse(as.numeric(hexid) < 6645, as.numeric(hexid), 
                        as.numeric(hexid) - 1))

## Certifying the correct number of unique hex; 7516
length(unique(na.omit(hexgrid_preomicron$hexid)))

## Pre-Omicron expanded dataset
##### Reformat the data to give INLA a complete dataframe 
##### Gives full hexgrid and date for smoothing 
hex_spacetime <- expand.grid(hexid = unique(hexes$hexid),
                             date = seq.Date(from = min(hexgrid_preomicron$date),
                                             to = max(hexgrid_preomicron$date), 
                                             by = "day")) |> 
                  left_join(hexgrid_pop |>
                              # mutate(hexid = as.character(hexid)) |>
                              st_drop_geometry()) |>
                  left_join(hexgrid_preomicron |> 
                              st_drop_geometry() |> 
                              # rename(date = weekdate) |> 
                              select(hexid, date, infections, infectionsPC)) |>  
                  mutate(Time = as.numeric(date - min(date)) + 1, 
                         infectionsPC = infectionsPC*1e5)

## Certifying the correct number of unique hex; 7516
length(unique((hex_spacetime$hexid)))

###############################################################################
##### Clean up environment before entering model setup and run            #####
##############################################################################|
rm(hexes)
rm(hexgrid_preomicron)
rm(hexgrid_pop)
rm(hexes_nb)

## Returning the data.frame into a list format
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

## Flag if it is a rerun
is.rerun <- FALSE

## This should be run in batches, as it may take long time and not be suit for memory size.
## The fitting strategy is to run the model for all dates and rerun over the problematic dates, when the model didn't fit well
## One very common issue is returning a bad run from INLA, 
## which only the iid part fits to the model and not the Besag model.
## The Newton-Raphson method could not converged, the method is used to some numerical integration on the hyperparameters. 
## Another alternative is to log-transform the data before imputing to the INLA model, 
## Hävard Rue suggested this on a help desk response at INLA email list. 
## Yet another alternative is to set the prior parameterization to the model that can afford huge variance on the data ## Omicron dataset case. We can set the shape and rate parameters of log-Gamma prior to have mean = 1 and variance = 1.

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

# # Set up parallel backend
# cl <- makeCluster(detectCores() - 2)
# registerDoParallel(cl)

##### Priors for BYM model
### Strict smoothing prior
hyper_smooth <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.2, 0.01),  # P(SD > 0.2) = 1% (STRONG smoothing)
    initial = 5            # Start with high precision (1/exp(4) ≈ 0.018 SD)
  )
)

### Priors for BYM2 model ###
### Strict smoothing prior ### 
hyper_smooth_bym2 <- list(
  phi = list(prior = "pc", param = c(0.95, 0.5)),  # 50% prob ϕ > 0.95
  # phi = list(prior = "logitbeta", param = c(0.69, 0.69)),  # 50% prob ϕ > 0.95
  prec = list(prior = "pc.prec", param = c(0.2, 0.01))
)

diag.eps = 1e-3

###############################################################################
### Old code to parallelize the model. 
# # Preprocess adjacency matrix on all workers
# clusterEvalQ(cl, {
#   library(INLA)
#   library(dplyr)
#   library(tibble)
#   library(base)
#   library(stats)
#   hex_graph <- inla.read.graph("data-products/hexes_adjmat.graph")
# })
# 
# # Export required objects
# clusterExport(cl, c("hex_spacetime", "hyper_smooth", "hyper_smooth_bym2", "compute_list", "predictor_list"))

# CAR_list <- foreach(i = 1:length(days),
#                     .combine = c,
#                     .multicombine = TRUE) %dopar% {
###############################################################################
##### Set which days to run the model for                                 #####
##############################################################################|
# days <- sort(unique(na.omit(hex_spacetime$date)))

##### Define the peaks of interest
alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-04")

days <- c(seq.Date(from = alpha_peak-63, to = alpha_peak, by = "day"),
           seq.Date(from = delta_peak-63, to = delta_peak, by = "day"))

###############################################################################
##### Check if this is an initial run or a rerun.                         #####
##############################################################################|
##### If rerun flag is set to true, it signals a secondary model run to try 
##### to fit the dates that failed on the initial model run. 

is.rerun <- FALSE

if (is.rerun == TRUE){
  ### Reload the adjacency matrix
  hex_graph <- inla.read.graph("Data/data-products/hexes_adjmat.graph")
  ### Reload the initial results from the previous model run
  CAR_df <- vroom::vroom("Data/data-products/car_hexgrid1100km_run_preomicron_daily.csv")
  ### Reformating into a list as required by INLA
  CAR_list <- CAR_df |> dplyr::group_split(date)
  ## setting the hexid column on the same class
  ## guarantee that all the hexids are integers
  CAR_list <- lapply(CAR_list, function(x){x <- x |> mutate(hexid = as.integer(hexid))})
  ## List to rerun if the model is not well fitted to the data
  ## Using the sd to determine if the model has failed -- check the other repository for 
  ## more robust error detection. 
  dates_to_rerun <- sapply(CAR_list, function(x){ifelse(sd(x$sd)<0.0025 || sd(x$sd)>0.010, x$date, NA)})
  dates_to_rerun
  length(na.omit(dates_to_rerun))
  
  sd_values <- data.frame(days = days, 
                          sd = sapply(CAR_list, function(x){sd(x$sd)}),
                          upper = sapply(CAR_list, function(x){max(x$sd)}),
                          lower = sapply(CAR_list, function(x){sd(x$sd)})) |> 
    mutate(id = row_number())
  
  ggplot(data = sd_values, 
         aes(x = days, y = sd, 
             label = id,
             ymin = lower, ymax = upper,
             color = if_else(sd <= 0.01 & sd >= 0.0025, "firebrick", "steelblue"),
         ))+
    geom_label()+
    geom_pointrange()+
    theme_minimal()
  ## Alpha wave convergence analysis
  ## Alpha Peak Movie
  j <- which(days == alpha_peak)
  
  sd_values_alpha <- sd_values |> 
    filter(days %in% days[seq(j-90,j+60, 2)])
  
  ggplot(data = sd_values_alpha, 
         aes(x = days, y = sd, 
             label = id,
             ymin = lower, ymax = upper,
             color = if_else(sd <= 0.01 & sd >= 0.0025, "blue", "red"),
         ))+
    geom_label()+
    geom_pointrange()+
    theme_minimal()
  
  ## Delta wave convergence analysis
  ## Delta Peak Movie
  j <- which(days == delta_peak)
  
  sd_values_delta <- sd_values |> 
    filter(days %in% days[seq(j-90,j+60, 2)])
  
  ggplot(data = sd_values_delta, 
         aes(x = days, y = sd, 
             label = id,
             # ymin = lower, ymax = upper,
             color = if_else(sd <= 0.01 & sd >= 0.0025, "blue", "red"),
         ))+
    geom_label()+
    # geom_pointrange()+
    theme_minimal()
} else if (is.rerun == FALSE){
  
##### Create an empty list to keep model output

CAR_list <- list()

test_dates <- c(c(alpha_peak-63,
                  alpha_peak-45,
                  alpha_peak-24,
                  alpha_peak),
                c(delta_peak-63,
                  delta_peak-45,
                  delta_peak-24,
                  delta_peak))
} 

# cat("Will rerun for :", length_dates_to_rerun, "dates! \n")
# days <- test_dates

###############################################################################
##### Run the model 
##############################################################################|
for (i in 1:length(days)) {
# for (i in 1:4) {
  #### If there is nothing to rerun go to the next date
  #### might need to fix for first run - check. 
  if (is.rerun == TRUE){
    if(is.na(dates_to_rerun[i]))next
  }
  current_date <- days[i]
  cat("Starting model for date: ", as.character(current_date),"!\n")
  
  hex_week <- hex_spacetime %>% 
    filter(date == current_date)
  
  #### Setup for the while loop 
  best_model <- NULL # holds the model fit
  ## counter for while
  counter <- 0
  while_condition <- TRUE
  
  ### Going to run the model 10 times for bad dates. 
  while ((while_condition) & counter <= 10) {  # Flipped condition
    
    cat("Attempt to fit the model number: ", counter, "!\n")
    ### sometimes a bad date can break the INLA model, this tryCatch
    ### is to avoid escaping the loop due to one date error. 
    tryCatch({
      set.seed(.Random.seed) ## change the seed for reruns 
      ### Call the BYM2 model with INLA
      best_model <- inla(
        as.formula(infectionsPC ~ 1 + 
                     f(hexid, ### either need to update data above or id to hexid
                       model = "bym2", 
                       graph = hexes_graph,
                       scale.model = TRUE, 
                       # diagonal = diag.eps,
                       constr = TRUE, ## sum to zero constraint 
                       hyper = hyper_smooth_bym2)),
        data = as.data.frame(hex_week),
        family = "gaussian", #likelihood
        control.inla = control.inla(strategy = "gaussian", int.strategy = "eb"), #strategy to sample the hyperparameter space
        control.mode = control.mode(restart = TRUE),  
        control.compute = compute_list,
        control.predictor = predictor_list,
        # control.fixed = list(prec.intercept = 0.1),
        num.threads = 2,  # Prevent internal threading conflicts (needs to be 1 for parallelized code)
        # verbose = T
      )
      
    }, error = function(e) {
      set.seed(.Random.seed)
      
      best_model <- inla(
        as.formula(infectionsPC ~ 1 + 
                     f(hexid, 
                       model = "bym2",
                       graph = hexes_graph,
                       # diagonal = diag.eps,
                       scale.model = TRUE,
                       constr = TRUE,
                       hyper = hyper_smooth_bym2)),
        data = as.data.frame(hex_week),
        family = "gaussian",
        control.inla = control.inla(strategy = "gaussian", int.strategy = "eb"),
        control.mode = control.mode(restart = TRUE),
        control.compute = compute_list,
        control.predictor = predictor_list,
        # control.fixed = list(prec.intercept = 0.1),
        num.threads = 2,  # Prevent internal threading conflicts
        # verbose = T
      )
    })
    
    counter <- counter + 1
    # Update while condition
    if(i >= 2){
      ## Create a vector of size 2 that will check for big drop in the median value for the sd
      vec_1 <- data.frame(
        upper_sd  = range(CAR_list[[i-1]]$sd)[2],
        median_sd = median(CAR_list[[i-1]]$sd))
      
      vec_2 <- data.frame(
        upper_sd  = range(best_model$summary.fitted.values$sd)[2],
        median_sd = median(best_model$summary.fitted.values$sd))
      
      ### We cannot check the "after" until we have the full dataset. 
      ### We will revisit this after running one peak to see if necessary. 
      
      # vec_3 <- data.frame(
      #   upper_sd  = range(CAR_list[[i+1]]$sd)[2],
      #   median_sd = median(CAR_list[[i+1]]$sd))
      
      vec <- rbind(vec_1, vec_2) #, vec_3)
      
      ### These check for "bad" runs for certain dates. If 
      ### either are true, rerun that date.  
      # Position 2 is the rerun position
      # 1) big drop in median vs. either neighbor
      drop_prev <- (vec$median_sd[1] - vec$median_sd[2]) > 3
      # drop_next <- (vec$median_sd[3] - vec$median_sd[2]) > 3
      median_drop <- drop_prev #|| drop_next
      
      # 2) big spike in upper vs. both neighbors
      spike_prev <- (vec$upper_sd[2] - vec$upper_sd[1]) > 3
      # spike_next <- (vec$upper_sd[2] - vec$upper_sd[3]) > 3
      upper_spike <- spike_prev #&& spike_next
      
      while_condition <- median_drop || upper_spike
      
    } else {
      while_condition <- FALSE}
  }
  
  cat("Finished CAR model for week ", as.character(current_date),"! \n")
  
  CAR_list[[i]] <- cbind(hex_week, 
                         best_model$summary.fitted.values |> 
                         rownames_to_column(var = "INLApred"))
  ##### Clean up memory before next run
  # rm(best_model, hex_week)
  gc()
}

###############################################################################
##### Reformat the CAR list into a dataframe and saving it. 
# stopCluster(cl)

## Turning into a df
CAR_df <- bind_rows(CAR_list)

## Saving the df, remember to change the name if the dataset is "preomicron" or "omicronera". The pattern nomenclature to files are tsa_preomicron.csv or tsa_omicronera.csv
dataset <- "hexgrid1100km_run_preomicron_daily_"

vroom::vroom_write(x = CAR_df, 
                   file = paste0("Data/data-products/car_", 
                                 dataset,
                                 ".csv"))

## Saving as list object
saveRDS(CAR_list, 
     file = "Data/data-products/car_list.RDS")
# save(list = CAR_list, 
#      file = "Data/data-products/car_list.RDS", 
#      compress = "xz", 
#      compression_level = 9)
# CAR_list <- readRDS("Data/data-products/car_list_Wave1_testDates.RDS")
###############################################################################
##### CHECK WHETHER THE MODEL FIT with some plots #####
###############################################################################
# fitted_values <- inla.tmarginal(exp, CAR_model$marginals.fitted.values)


## Breakdowns of each peaks
# breaks_plt1 <- seq(0,500, 50)
# labels_plt1 <- c("<50",seq(50,450, 50), '>500')
# limits_plt1 <- c(0,500)
# color_option <- "magma"

# CAR_model <- best_model
family <- "gaussian"
model <- "besag2"

## Figure2
# Source: https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code
excludes = c(
  "02", "60", "03", "81", "07", "64",
  "14", "66", "84", "86", "67", "89",
  "68", "71", "76", "69", "70", "95",
  "43", "72", "74", "78", "79", "15", "11"
)

us_states <- tigris::states(cb = T) |> 
  dplyr::filter(!STATEFP %in% excludes) #|> 
  # tigris::shift_geometry()

### Reload hexgrid 
hexes <- st_read("Data/data-products/geo-hexes/hexgrid_1100_km.shp") |> 
  filter(
    # Taking out the isolated hex at Keywest
    as.integer(hexid) != 6644) %>%
  # INLA requires the id to only be 1:N, where N is the total
  # number of observations; because of this we need to rename the 
  # hexids to be continuous. 
  mutate(hexid = ifelse(as.numeric(hexid) < 6645, as.numeric(hexid), 
                        as.numeric(hexid) - 1))


CAR_df_test <- CAR_df |> 
  filter(date %in% test_dates) |> 
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
                       limits = limits_plt,
  )+
  # scale_fill_viridis_c(option = color_option, 
  #                      name = "Estimated Infections/100k/day", direction = -1)+
  geom_sf(data = us_states, aes(geometry = geometry), fill = NA) +
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "in"))+
  facet_wrap(.~date, nrow = 2)


ggplot() +
  geom_sf(data = CAR_df_test |> 
            filter(date == alpha_peak),
          aes(fill = mean), color = NA)+
  # scale_fill_viridis_b(option = color_option,
  #                      # name = "Estimated Infections/1000/week",
  #                      direction = -1,
  #                      na.value = na_color,
  #                      breaks = breaks_plt,
  #                      labels = labels_plt,
  #                      limits = limits_plt,
  # )+
  scale_fill_viridis_c(option = color_option,
                       name = "Estimated Infections/100k/day", direction = -1)+
  geom_sf(data = us_states, aes(geometry = geometry), fill = NA) +
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "in"))
