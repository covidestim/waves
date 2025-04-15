rm(list = ls())
gc()

library(tidyverse)
library(sf)
library(spdep)
library(spatialreg)
library(Matrix)
library(INLA)

## 
hexes <- sf::st_read("data-products/geo-hexes/hexes.shp") |> 
  filter(as.integer(hexid) < 7662)

## Loading centroids shapefile
hexes_centroids <- as.data.frame(st_coordinates(st_cast(st_centroid(hexes), "MULTIPOINT"))) |> 
  rename(hexid = L1) |> 
  mutate(hexid = as.character(hexid)) |> 
  left_join(hexes) |> 
  mutate(geometry = st_centroid(geometry)) |> 
  st_as_sf()

## Neighbors
hexes_nb <- poly2nb(hexes, row.names = hexes$hexid)
# hexes_nb2 <- st_touches(st_geometry(hexes))

nb2INLA("data-products/hexes_adjmat.graph", nb = hexes_nb)
hexes_graph <- inla.read.graph("data-products/hexes_adjmat.graph")

wt_B <- nb2mat(neighbours = hexes_nb, style = "B", zero.policy = T)
wt_W <- nb2mat(neighbours = hexes_nb, style = "W", zero.policy = T)
# 
listB <- nb2listw(neighbours = hexes_nb, style = "B", zero.policy = T)
listW <- nb2listw(neighbours = hexes_nb, style = "W", zero.policy = T)
# 
# ## Weight list as a Sparse C Matrix
B <- as(listB, "CsparseMatrix")
W <- as(listW, "CsparseMatrix")

## Pre-Omicron
hexgrid_preomicron <- vroom::vroom("data-products/geo-hexes/hexid-observations_preomicron_meta30m.csv") |>
  mutate(hexid = as.character(hexid),
         date = as.Date(date)) |>
  select(-geometry) |>
  mutate(infectionsPC = (infections/population)*1e5) |>
  filter(infectionsPC >= 1) |>
  left_join(hexes, by = "hexid")

# ## Omicron-era
# hexgrid_omicronera <- vroom::vroom("data-products/geo-hexes/hexid-observations_omicronera.csv") |>
#   mutate(hexid = as.character(hexid),
#          date = as.Date(date)) |>
#   # select(-geometry) |>
#   # mutate(infectionsPC = (infections/population)*1e5) |>
#   filter(infectionsPC >= 1) |>
#   left_join(hexes, by = "hexid")

## Population hexes
# hex_population <- sf::st_read("data-products/geo-hexes/meta_population/hexid-population_new.shp")

## Pre-Omicron expanded dataset
hex_spacetime <- expand.grid(hexid = as.character(unique(hexes$hexid)),
                             date = seq.Date(from = min(hexgrid_preomicron$date),
                                             to = max(hexgrid_preomicron$date), 
                                             by = "day")) |> 
  # left_join(hex_population |> 
  #             mutate(hexid = as.character(hexid)) |> 
  #             st_drop_geometry()) |> 
  left_join(hexgrid_preomicron |> 
              st_drop_geometry() |> 
              select(hexid, date, population, infections)) |>  ## just join the desired columns, we have the geometry
  # st_as_sf()|> 
  mutate(Time = as.numeric(date - min(date)) + 1,
         ID = as.numeric(hexid),
         # infections = as.integer(replace_na(infections, replace = 0)),
         infectionsPC = (infections/population)*1e5,
         logpopulation = log(population))

# # ## Omicro-era dataset
# hex_spacetime <- expand.grid(hexid = unique(hexes$hexid),
#                              date = seq.Date(from = min(hexgrid_omicronera$date),
#                                              to = max(hexgrid_omicronera$date), 
#                                              by = "day")) |>
#   left_join(hex_population |>
#               mutate(hexid = as.factor(hexid))) |>
#   left_join(hexgrid_omicronera |>
#               select(-geometry)) |>  ## just join the desired columns, we have the geometry
#   # st_as_sf()|>
#   # dividing by 7 to make it an average of the week
#   dplyr::mutate(across(starts_with("infections"), ~.x/7))

# hex_spacetime <- hex_spacetime |> 
#   select(hexid, date, infectionsPC) |> 
#   pivot_wider(names_from = hexid, values_from = infectionsPC) |> 
#   ## Stretching the time series to a daily basis, as the data is the sum of week infections estimates
#   dplyr::mutate(across(where(is.numeric), ~zoo::na.approx(.x, maxgap = 6))) |> 
#   ## Going back to longer format
#   pivot_longer(cols = -date, names_to = "hexid", values_to = "infectionsPC") |> 
#   mutate(Time = as.numeric(date - min(date)) + 1,
#          ID = as.numeric(hexid))

## Rerun setting up
## Pre-Omicron
# CAR_df_rerun <- vroom::vroom("data-products/tsa_preomicron.csv") 

## Omicron-era
# CAR_df_rerun <- vroom::vroom("data-products/tsa_omicronera.csv") 

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

weeks <- sort(unique(hex_spacetime$date))

## List of CAR models per dates
CAR_list <- list()

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
library(INLA)
compute_list <- control.compute(hyperpar = T, 
                                return.marginals = T, 
                                return.marginals.predictor = T,
                                dic = T, 
                                cpo = T, 
                                waic = T)

predictor_list <- control.predictor(compute = T)

inla_list <- control.inla(cmin = 0, strategy = "adaptative")

# fixed effects priors
# control.fixed1 = list(mean.intercept=0, # prior mean for intercept
#                       prec.intercept=0.1, # prior precision for intercept (wide sd bc. n binomial; intercept likely v small)
#                       mean=0, # prior mean for fixed effects
#                       prec=0.5)  # prior precision for (scaled) fixed effects; slightly tighter

hyper.bym2 = list(theta1 = list(prior="pc.prec", param=c(1, 0.01)),
                  theta2 = list(prior="pc", param=c(0.5, 0.75)))

# form_base <- as.formula(y ~ 1+
#                           # offset(logpopulation)+
#                           f(ID, 
#                             model = "bym2", 
#                             graph = W,
#                             # replicate = Time,
#                             hyper = hyper.bym2
#                           )
# )

alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-08")

hex_spacetime_sp <- hex_spacetime |>
  filter(date == delta_peak) |>
  mutate(y = infectionsPC)

# Q <- INLA:::inla.pc.bym.Q(graph = hexes_graph)
# Q.scale <- INLA:::inla.scale.model.bym(Q, adjust.for.con.comp = TRUE)
# Q.as.C <- as(Q.scale, "CsparseMatrix")
# 
# log.prior = INLA:::inla.pc.bym.phi(Q = Q.scale, rankdef = 1, 
#                                    u = 0.5, alpha = 2/3) 
# phis = 1/(1+exp(-seq(-10, 10, len=10000)))
# plot(phis, exp(log.prior(phis)), type="l", 
#      lwd = 2, bty = "l", ylim = c(0,10), 
#      xlab = expression(phi), ylab = "Density")

form <- formula(y ~ 1 + 
                  f(ID,
                    model = 'bym2', 
                    graph = B, 
                    hyper = hyper.bym2))

CAR_model <-inla(formula = form,
                 data = hex_spacetime_sp,
                 family = "gaussian",
                 # control.fixed = control.fixed(prec = 0.1),
                 control.predictor= predictor_list,
                 control.compute= compute_list,
                 num.threads = 3,
                 control.inla=control.inla(cmin = 0, strategy = "adaptative"),
                 # # fixing Q factorisation issue https://groups.google.com/g/r-inla-discussion-group/c/hDboQsJ1Mls
                 verbose=TRUE)

# fitted_values <- inla.tmarginal(exp, CAR_model$marginals.fitted.values)
color_option <- "magma"
na_color <- "grey70"
## Breakdowns of each peaks
breaks_plt <- c(0,seq(150,300, 20))
labels_plt <- c("150< ",seq(150,280, 20), ' >300')
limits_plt <- c(0,350)

ggplot(data = hexes|>
         dplyr::mutate(cases = hex_spacetime_sp$y,
                       cases_fitted = CAR_model$summary.fitted.values$mean) |> 
         st_transform(crs = 26915)) +
  geom_sf(aes(fill = log(cases)))+
  # scale_fill_viridis_b(option = color_option,
  #                      name = "Estimated Infections/100k/week",
  #                      direction = -1,
  #                      breaks = breaks_plt,
  #                      labels = labels_plt,
  #                      limits = limits_plt
  # )+
  # scale_fill_viridis_b(option = "magma",
  #                      n.breaks = 10,
  #                      labels = scales::label_comma(),
  #                      direction = -1)+
theme_minimal()+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  guides(fill = guide_bins(title = "Infections per capita/100k",
                           title.position = "top",
                           title.vjust = 0.5))+
  labs(title = "Prediction from CAR model with proportional* weights",
       subtitle = "CAR model with BYM2 implementation",
       caption = "*Proportional weights means weights ranging from 1 to 0")

CAR_hyper_list <- list()

for (i in 1:length(weeks)) {
  
  ## filtering the dataset to a specific date
  hex_spacetime_sp <- hex_spacetime |> 
    filter(date == weeks[i]) |> 
    mutate(infections = as.integer(infections))
  
  cat("Fitting a first try model \n")
  
  # counter <- 1
  cat("Initializing model for week:", as.character(weeks[i]),"  \n", sep = " ")
  
  counter <- 1
  sd_values <- 0
  while (median(sd_values) < 10 && counter < 10) {
    ## Handling errors even if after successive runs
    tryCatch({
      ## Setting random seeds to avoid problems of convergence for bad seeds
      set.seed(.Random.seed)
      
      CAR_model <- inla(formula = as.formula(infectionsPC ~ 1+
                                               # offset(log(population))+
                                               f(ID, 
                                                 model = "bym2", 
                                                 graph = W,
                                                 scale.model = TRUE,
                                                 constr = TRUE,
                                                 hyper = hyper.bym2)
      ),
      data = as.data.frame(hex_spacetime_sp),
      family = "gaussian",
      # control.fixed=control.fixed1,
      control.predictor=predictor_list,
      control.compute=compute_list,
      # control.inla=list(cmin=0, strategy='adaptive'),
      # fixing Q factorisation issue https://groups.google.com/g/r-inla-discussion-group/c/hDboQsJ1Mls
      verbose=TRUE)},
      error = {
        ## Setting random seeds to avoid problems of convergence for bad seeds
        set.seed(.Random.seed)
        
        CAR_model <- inla(formula = as.formula(infectionsPC ~ 1+
                                                 f(ID,
                                                   model = "bym2",
                                                   graph = W)),
                          data = as.data.frame(hex_spacetime_sp),
                          # offset = log(population),
                          family = "gaussian",
                          verbose = T,
                          control.compute = compute_list,
                          control.predictor = predictor_list,
                          
                          control.inla=list(cmin=0, strategy='adaptive'),
                          #fixing Q factorisation issue https://groups.google.com/g/r-inla-discussion-group/c/hDboQsJ1Mls
        )})
    
    # sd_values <- CAR_model$summary.fitted.values$sd
    ## Uncomment if you wanna skip the while loop
    sd_values <- 11
    
    ## The last resort, brute force
    if(counter == 10 && median(sd_values) < 10){
      ## Last resort
      CAR_model <- inla(formula = as.formula(log(infectionsPC) ~ 1+
                                               f(ID,
                                                 model = "bym2",
                                                 graph = W)),
                        data = as.data.frame(hex_spacetime_sp),
                        # offset = log(population),
                        family = "gaussian",
                        verbose = T,
                        control.compute = compute_list,
                        control.predictor = predictor_list)
    }
    
    cat("Attempt ", counter, "! \n", sep = "")
    counter = counter + 1
  }
  # }
  
  CAR_list[[i]] <- cbind(hex_spacetime_sp, 
                         CAR_model$summary.fitted.values |> 
                           rownames_to_column(var = "INLApred")
  )
  
  ## To keep the phi hyperparameter and check how it varied over time
  CAR_hyper_list[[i]] <- cbind(phi = CAR_model$summary.hyperpar$mean,
                               date = as.Date(weeks[i], "%Y-%m-%d"))
  
  # rm(hex_spacetime_sp, CAR_model)
  
  cat("Finished BYM2 model for week ", as.character(weeks[i]),"! \n", 
      "With ", counter, " Attempts! \n",sep = "")
}

## List to rerun if the model is not well fitted to the data
dates_to_rerun <- lapply(CAR_list, function(x){ifelse(median(x$sd)<10, x$date, NA)})

## Rerun flag, set this to TRUE is the dates to rerun are a lot to get better model estimates
is.rerun <- TRUE

## setting the hexid column on the same class
CAR_list <- lapply(CAR_list, function(x){x <- x |> mutate(hexid = as.integer(hexid))})

## Turning into a df
CAR_df <- bind_rows(CAR_list)

## Saving the df, remember to change the name if the dataset is "preomicron" or "omicronera". The pattern nomenclature to files are tsa_preomicron.csv or tsa_omicronera.csv
dataset <- "new_rerun_preomicron"

vroom::vroom_write(x = CAR_df, 
                   file = paste0("data-products/tsa_", 
                                 dataset,
                                 ".csv"))

#


## Prototyping codes to check model convergence and plots

# Use 4 cores to process marginals in parallel
library("parallel")
options(mc.cores = 4)
# Transform marginals and compute posterior mean
#marginals: List of `marginals.fitted.values`from inla model
tmarg <- function(marginals) {
  post.means <- mclapply(marginals, function (marg) {
    # Transform post. marginals
    aux <- inla.tmarginal(exp, marg)
    # Compute posterior mean
    inla.emarginal(function(x) x, aux)
  })
  
  return(as.vector(unlist(post.means)))
}

# fitted_values <- inla.tmarginal(exp, CAR_model$marginals.fitted.values)
color_option <- "magma"
na_color <- "grey70"
## Breakdowns of each peaks
breaks_plt <- c(0,seq(150,300, 20))
labels_plt <- c("150< ",seq(150,280, 20), ' >300')
limits_plt <- c(0,350)

ggplot(data = hex_population |>
         dplyr::mutate(cases_fitted = CAR_model$summary.fitted.values$mean,
                       logpopulation = log(population),
                       incidence_fitted = exp(log(cases_fitted) - logpopulation)*100000,
                       log_incidence = log(incidence_fitted+1))|> 
         st_transform(crs = 26915)) +
  geom_sf(aes(fill = cases_fitted))+
  scale_fill_viridis_b(option = color_option,
                       name = "Estimated Infections/100k/week",
                       direction = -1,
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt
  )+
  # scale_fill_viridis_b(option = "magma",
  #                      n.breaks = 10,
  #                      labels = scales::label_comma(),
  #                      direction = -1)+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  guides(fill = guide_bins(title = "Infections per capita/100k",
                           title.position = "top",
                           title.vjust = 0.5))+
  labs(title = "Prediction from CAR model with proportional* weights",
       subtitle = "CAR model with BYM2 implementation",
       caption = "*Proportional weights means weights ranging from 1 to 0")

hist(CAR_model$summary.fitted.values$mean)
