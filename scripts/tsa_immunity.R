rm(list = ls())
gc()

library(tidyverse)
library(sf)
library(spdep)
library(spatialreg)

setwd("~/Desktop/repos/Waves/waves/")

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
  select(-immunePC) |> 
  mutate(across(starts_with("immune."), ~ (.x/population)*1e5, .names = "{.col}PC")) |> 
  # mutate(across(ends_with("PC"), ~ if_else(.x < 0, 0, .x))) |> 
  left_join(hexes) |>
  st_as_sf() |> 
  sf::st_transform(crs = 26915)

## Peak dates
alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-08")

# ggplot() + 
#   geom_sf(hexgrid_immune |> 
#             filter(date == delta_peak), 
#           mapping = aes(fill = immune.infectedPC))+
#   theme_minimal() +
#   scale_fill_gradient(low = "thistle1", high = "deeppink4", na.value = "green")+
#   theme(legend.position = "bottom", 
#         legend.title.position = "top",
#         legend.title = element_text(hjust = 0.5),
#         legend.key.width = grid::unit(2, "cm"))+
#   guides(fill = guide_colorbar(nbin = 10))

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
  ## Regularizing the columns of interest
  mutate(Time = as.numeric(date - min(date)) + 1,
         ID = as.numeric(hexid),
         ID2 = as.numeric(hexid)) 

## Filtering to 128 before peaks dates
hex_spacetime <- hex_spacetime |> 
  filter(date %in% c(delta_peak, delta_peak-21, delta_peak-42, 
                     delta_peak-63, delta_peak-84, delta_peak-105,
                     delta_peak-126))

weeks <- sort(unique(hex_spacetime$date))

## List of CAR models per dates
CAR_list <- list()

## Flag if it is a rerun
# is.rerun <- FALSE

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

for (i in 1:length(weeks)) {
  
  hex_spacetime_sp <- hex_spacetime |> 
    filter(date == weeks[[i]]) 
  # |> 
  # mutate(immunePC = immune) |> 
  # mutate(mu.z = if_else(immunePC == 0, 0, 1),
  #        mu.o = if_else(immunePC == 0, NA, immunePC)) |> 
  # select(hexid, immunePC, ID)
  
  # #Unique index for patient from 1 to n.patients
  # unique.id <- unique(hex_spacetime_sp$hexid)
  # n.id <- length(unique.id)
  # idx <- 1:n.id
  
  # #Unique indices for long. and survival data
  # idx <- match(hex_spacetime_sp$hexid, unique.id)
  
  # #Indices for random intercept
  # ## A vector with indexes and another NAs
  # ID <- c(idx, rep(NA, n.id))
  # ## A of NAs and all the indexes
  # ID2 <- c(rep(NA, n.id), idx)
  
  # mu.z <- c(as.integer(hex_spacetime_sp$mu.z), rep(NA, n.id))
  # mu.o <- c(rep(NA, n.id), hex_spacetime_sp$mu.o)
  # Y.joint <- list(mu.z, mu.o)
  
  # #Covariates
  # covariates <- data.frame(
  #   #Intercepts (as factor with 2 levels)
  #   inter = as.factor(c(rep("binom", n.id), rep("poisson", n.id))),
  #   #Ocurrence
  #   mu.o = mu.o,
  #   #Number of Occurence
  #   mu.z = mu.z 
  # )
  
  # #Random effects
  # r.effects <- list(
  #   # hexid ID
  #   ID = ID,
  #   # hexid ID2
  #   ID2 = ID2
  # )
  
  # ## Joint.data
  # joint.data <- c(covariates, r.effects)
  # joint.data$Y <- Y.joint
  
  cat("Fitting a first try model \n")
  
  ## Setting seeds to reproducibility
  set.seed(.Random.seed)
  
  counter <- 1
  cat("Initializing model for week:", as.character(weeks[i]),"  \n", sep = " ")
  
  cat("Fitting a model over total immune estimates \n")
  
  compute_list <- control.compute(hyperpar = T, 
                                  return.marginals = T, 
                                  return.marginals.predictor = T,
                                  dic = T, 
                                  cpo = T, 
                                  waic = T)
  
  predictor_list <- control.predictor(compute = T, link = 1)
  
  # family_list <- control.family(link = "log10")
  data_inla <- as.data.frame(hex_spacetime_sp |>
                               ## Making sure the components are counts
                               mutate(across(ends_with(".count"), 
                                             ~round(if_else(.x < 0, 0, .x), 0)))
                             )
  
  CAR_model <- inla(formula = as.formula(immune.count ~ f(ID, 
                                                      model = "bym2",
                                                      graph = wt_W)),
                    data = data_inla,
                    offset = log(population),
                    family = "nbinomial",
                    verbose = T,
                    control.compute = compute_list,
                    control.predictor = predictor_list)
  
  cat("Fitting a model over infected immune estimates \n")
  
  CAR_model.infected <- inla(formula = as.formula(immune.infected.count ~ 
                                                    f(ID, 
                                                      model = "bym2",
                                                      graph = wt_W)),
                             data = data_inla,
                             offset = log(population),
                             family = "nbinomial",
                             verbose = T,
                             control.compute = compute_list,
                             control.predictor = predictor_list)
  
  cat("Fitting a model over vacccinated immune estimates \n")
  
  CAR_model.vaccinated <- inla(formula = as.formula(immune.vaccinated.count ~ 
                                                      f(ID, 
                                                        model = "bym2",
                                                        graph = wt_W)),
                               data = data_inla,
                               offset = log(population),
                               family = "nbinomial",
                               verbose = T,
                               control.compute = compute_list,
                               control.predictor = predictor_list)
  
  CAR_list[[i]] <- cbind(hex_spacetime_sp, 
                         CAR_model$summary.fitted.values |> 
                           select(mean, sd) |> 
                           rename_with(~ paste(., "total", sep = "_")),
                         CAR_model.infected$summary.fitted.values |> 
                           select(mean, sd) |> 
                           rename_with(~ paste(., "infected", sep = "_")),
                         CAR_model.vaccinated$summary.fitted.values |> 
                           select(mean, sd) |> 
                           rename_with(~ paste(., "vaccinated", sep = "_"))
  )
  # }
  
  rm(hex_spacetime_sp, CAR_model, CAR_model.infected, CAR_model.vaccinated)
  
  cat("Finished CAR model for week ", as.character(weeks[i]),"! \n", 
      "And the counter went until: ", counter, "\n", sep = "")
}

# ## List to rerun if the model is not well fitted to the data
# dates_to_rerun <- lapply(CAR_list, function(x){ifelse(median(10^(x$sd))<10, x$date, NA)})
# 
# ## Rerun flag, set this to TRUE is the dates to rerun are a lot to get better model estimates
# is.rerun <- TRUE
# 
# ## setting the hexid column on the same class
# CAR_list <- lapply(CAR_list, function(x){x <- x |> mutate(hexid = as.integer(hexid))})

## Turning into a df
CAR_df <- bind_rows(CAR_list)

## Saving the df, remember to change the name if the dataset is "preomicron" or "omicronera". The pattern nomenclature to files are tsa_preomicron.csv or tsa_omicronera.csv

dataset <- "immunity_infected_vaccinated"

vroom::vroom_write(x = CAR_df, 
                   file = paste0("data-products/tsa_", 
                                 dataset,
                                 ".csv"))

#


# ## Prototyping codes to check model convergence and plots
# 
# ## Breakdowns of each peaks
# breaks_plt <- pretty(CAR_model$summary.fitted.values$mean, n = 5)
# # labels_plt <- c("150< ",seq(150,280, 20), ' >300')
# # limits_plt <- c(0,350)
# color_option <- "magma"
# na_color <- "grey70"
# 
# total <- ggplot(data = hexes |> 
#          cbind(predCAR_B = CAR_model$summary.fitted.values |> 
#                  # filter(date == alpha_peak-63) |> 
#                  pull(var = mean)) |> 
#          st_transform(crs = 26915)) +
#   geom_sf(aes(fill = predCAR_B,
#               color = predCAR_B))+
#   scale_fill_viridis_b(option = color_option,
#                        # name = "Estimated Infections/100k/week",
#                        direction = -1,
#                        # breaks = pretty(CAR_model.vaccinated$summary.fitted.values$mean,
#                        #                 n = 5),
#                        labels = scales::label_comma())+
#   scale_color_viridis_b(option = color_option,
#                         # name = "Estimated Infections/100k/week",
#                         direction = -1,
#                         # breaks = pretty(CAR_model.vaccinated$summary.fitted.values$mean,
#                         #                 n = 5),
#                         labels = scales::label_comma())+
#   theme_void()+
#   theme(legend.position = "right",
#         legend.title.position = "left",
#         legend.title = element_text(angle = 90, hjust = 0.5),
#         legend.direction = "vertical",
#         legend.key.height = grid::unit(1, "cm"))+
#   guides(fill = guide_bins(title = "TSA on immunity",
#                            title.position = "left",
#                            title.vjust = 0.5),
#          color = "none")+
#   labs(title = "total")
# total
# 
# infection <- ggplot(data = hexes |> 
#          cbind(predCAR_B = CAR_model.infected$summary.fitted.values |> 
#                  # filter(date == alpha_peak-63) |> 
#                  pull(var = mean)) |> 
#          st_transform(crs = 26915)) +
#   geom_sf(aes(fill = predCAR_B, 
#               color = predCAR_B))+
#   scale_fill_viridis_b(option = color_option,
#                        # name = "Estimated Infections/100k/week",
#                        direction = -1,
#                        breaks = pretty(CAR_model.vaccinated$summary.fitted.values$mean,
#                                        n = 5),
#                        labels = scales::label_comma())+
#   scale_color_viridis_b(option = color_option,
#                         # name = "Estimated Infections/100k/week",
#                         direction = -1,
#                         breaks = pretty(CAR_model.vaccinated$summary.fitted.values$mean,
#                                         n = 5),
#                         labels = scales::label_comma())+
#   theme_void()+
#   theme(legend.position = "right",
#         legend.title.position = "left",
#         legend.title = element_text(angle = 90, hjust = 0.5),
#         legend.direction = "vertical",
#         legend.key.height = grid::unit(1, "cm"))+
#   guides(fill = guide_bins(title = "TSA on immunity",
#                            title.position = "left",
#                            title.vjust = 0.5),
#          color = "none")+
#   labs(title = "Infected")
# infection
# 
# vaccination <- ggplot(data = hexes |> 
#          cbind(predCAR_B = CAR_model.vaccinated$summary.fitted.values |> 
#                  # filter(date == alpha_peak-63) |> 
#                  pull(var = mean)) |> 
#          st_transform(crs = 26915)) +
#   geom_sf(aes(fill = predCAR_B,
#               color = predCAR_B))+
#   # geom_sf(data = us_states,
#   #         fill = "transparent",
#   #         color = "deeppink4")+
#   scale_fill_viridis_b(option = color_option,
#                        # name = "Estimated Infections/100k/week",
#                        direction = -1,
#                        breaks = pretty(CAR_model.vaccinated$summary.fitted.values$mean,
#                                        n = 5),
#                        labels = scales::label_comma())+
#   scale_color_viridis_b(option = color_option,
#                        # name = "Estimated Infections/100k/week",
#                        direction = -1,
#                        breaks = pretty(CAR_model.vaccinated$summary.fitted.values$mean,
#                                        n = 5),
#                        labels = scales::label_comma())+
#   theme_void()+
#   theme(legend.position = "right",
#         legend.title.position = "left",
#         legend.title = element_text(angle = 90, hjust = 0.5),
#         legend.direction = "vertical",
#         legend.key.height = grid::unit(1, "cm"))+
#   guides(fill = guide_bins(title = "TSA on immunity",
#                            title.position = "left",
#                            title.vjust = 0.5),
#          color = "none")+
#   labs(title = "Vaccinated")
# vaccination
# 
# library(patchwork)
# p <- (total | infection | vaccination)+
#   plot_layout(guides = "keep")
# p
