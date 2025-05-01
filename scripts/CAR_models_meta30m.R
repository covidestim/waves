rm(list = ls())
gc()

library(tidyverse)
library(sf)
library(spdep)
library(spatialreg)
library(Matrix)

## 
hexes <- sf::st_read("data-products/geo-hexes/hexes.shp") |> 
  filter(as.integer(hexid) < 7662,
         ## Taking out the isolated hex at Keywest
         as.integer(hexid) != 6545) |> 
  st_transform(crs = 4326) |> 
  mutate(hexid = as.character(1:n()))

## Loading centroids shapefile
hexes_centroids <- as.data.frame(st_coordinates(st_cast(st_centroid(hexes), "MULTIPOINT"))) |> 
  rename(hexid = L1) |> 
  mutate(hexid = as.character(hexid)) |> 
  left_join(hexes) |> 
  mutate(geometry = st_centroid(geometry)) |> 
  st_as_sf()

## Neighbors
hexes_nb <- poly2nb(hexes, queen = TRUE, snap = 0, row.names = hexes$hexid)
# hexes_nb2 <- st_touches(st_geometry(hexes))

nb2INLA("data-products/hexes_adjmat.graph", nb = hexes_nb)
hexes_graph <- INLA::inla.read.graph("data-products/hexes_adjmat.graph")

# wt_B <- nb2mat(neighbours = hexes_nb, style = "B", zero.policy = T)
wt_W <- nb2mat(neighbours = hexes_nb, style = "W", zero.policy = T)
# 
# listB <- nb2listw(neighbours = hexes_nb, style = "B", zero.policy = T)
listW <- nb2listw(neighbours = hexes_nb, style = "W", zero.policy = T)
# 
# ## Weight list as a Sparse C Matrix
# B <- as(listB, "CsparseMatrix")
W <- as(listW, "CsparseMatrix")

## Pre-Omicron
hexgrid_preomicron <- vroom::vroom("data-products/geo-hexes/hexid-observations_preomicron_meta30m.csv") |>
  mutate(hexid = as.character(hexid),
         date = as.Date(date)) |>
  select(-geometry) |>
  mutate(infectionsPC = (infections/population)*1e5) |>
  filter(infectionsPC >= 1) |>
  left_join(hexes, by = "hexid") |>
  sf::st_as_sf()

# ## Omicron-era
# hexgrid_omicronera <- vroom::vroom("data-products/geo-hexes/hexid-observations_omicronera.csv") |>
#   mutate(hexid = as.character(hexid),
#          date = as.Date(date)) |>
#   # select(-geometry) |>
#   # mutate(infectionsPC = (infections/population)*1e5) |>
#   filter(infectionsPC >= 1) |>
#   left_join(hexes, by = "hexid")

## Population hexes
hex_population <- sf::st_read("data-products/geo-hexes/meta_population/hexid-population_new.shp")

## Pre-Omicron expanded dataset
hex_spacetime <- expand.grid(hexid = as.character(unique(hexes$hexid)),
                             date = seq.Date(from = min(hexgrid_preomicron$date),
                                             to = max(hexgrid_preomicron$date), 
                                             by = "day")) |> 
  left_join(hex_population |>
              mutate(hexid = as.character(hexid)) |>
              st_drop_geometry()) |>
  left_join(hexgrid_preomicron |> 
              st_drop_geometry() |> 
              select(hexid, date, infections)) |>  ## just join the desired columns, we have the geometry
  # st_as_sf()|> 
  mutate(Time = as.numeric(date - min(date)) + 1,
         ID = as.numeric(hexid),
         infections = case_when(infections >= population ~ population,
                                infections < population ~ infections,
                                infections > 1 ~ infections,
                                infections <= 1 ~ NA),
         # infections = as.integer(replace_na(infections, replace = 0)),
         infectionsPC = (infections/population)*1e5,
         logpopulation = log10(population+1))

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

hyper.bym2 = list(phi = list(prior="pc.prec", param=c(1, 0.01)),
                  theta = list(prior="pc", param=c(0.5, 0.75)))

## Vector list to keep model result
CAR_list <- list()

CAR_hyper_list <- list()

for (i in 1:length(weeks)) {
  
  ## filtering the dataset to a specific date
  hex_spacetime_sp <- hex_spacetime |> 
    filter(date == weeks[i]) 
  # |> 
  #   mutate(infections = as.integer(infections))|>
  #   mutate(y = (as.integer(infections)/population)*1e5)
  
  cat("Fitting a first try model \n")
  
  # counter <- 1
  cat("Initializing model for week:", as.character(weeks[i]),"  \n", sep = " ")
  
  ## Handling errors even if after successive runs
  set.seed(.Random.seed)

  skip_to_next <- FALSE
  
  # Note that print(b) fails since b doesn't exist
  
  tryCatch(CAR_model <- inla(formula = as.formula(infectionsPC ~ 1+
                                                    f(ID,
                                                      model = "bym2",
                                                      scale.model = TRUE,
                                                      constr = TRUE,
                                                      graph = W)),
                             data = as.data.frame(hex_spacetime_sp),
                             family = "gaussian",
                             verbose = T,
                             control.inla=list(cmin=0, strategy='adaptive'),
                             control.compute = compute_list,
                             control.predictor = predictor_list), 
           error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }  
  
  CAR_list[[i]] <- cbind(hex_spacetime_sp, 
                         CAR_model$summary.fitted.values |> 
                           rownames_to_column(var = "INLApred")
  )
  
  ## To keep the phi hyperparameter and check how it varied over time
  CAR_hyper_list[[i]] <- cbind(phi = CAR_model$summary.hyperpar$mean,
                               date = as.Date(weeks[i], "%Y-%m-%d"))
  
  rm(hex_spacetime_sp, CAR_model)
  
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
dataset <- "meta30m_run_preomicron"

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
  mutate(infectionsPC = as.double(infectionsPC),
         infections = as.integer(infections),
         y = as.integer(infections),
         logpopulation = as.integer(log(population+1)))

# form <- formula(y ~ 1 + 
#                   f(ID,
#                     model = 'bym2', 
#                     graph = "data-products/hexes_adjmat.graph",
#                     hyper = hyper.bym2))

set.seed(.Random.seed)

model <- "bym2"
family <- "gaussian"

CAR_model <- inla(formula = as.formula(infectionsPC ~ 1+
                                         # offset(logpopulation)+
                                         f(ID,
                                           model = model,
                                           scale.model = TRUE,
                                           constr = TRUE,
                                           # adjust.for.con.comp = TRUE,
                                           graph = W)),
                  # control.family = control.family(variant = 1),
                  data = as.data.frame(hex_spacetime_sp),
                  family = family,
                  verbose = T,
                  # control.inla=list(cmin=0, strategy='adaptive'),
                  control.compute = compute_list,
                  control.predictor = predictor_list)

# fitted_values <- inla.tmarginal(exp, CAR_model$marginals.fitted.values)
# color_option <- "magma"
# na_color <- "grey70"
# ## Breakdowns of each peaks
# breaks_plt <- c(0,seq(500,5e3, 500))
# labels_plt <- c("500<",seq(500, 4.5e3, 500), ">5,000")
# limits_plt <- c(0,5e3)

# # Source: https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code
# excludes = c(
#   "02", "60", "03", "81", "07", "64",
#   "14", "66", "84", "86", "67", "89",
#   "68", "71", "76", "69", "70", "95",
#   "43", "72", "74", "78", "79", "15", "11"
# )
# 
# ## US States
# us_states <- tigris::states(cb = T) |> 
#   dplyr::filter(!STATEFP %in% excludes) |> 
#   tigris::shift_geometry() |> 
#   st_transform(crs = 26915)

model <- "besag2"

X <- splines::bs(hex_spacetime_sp$population, df = 3)
X2 <- splines::bs(hex_spacetime_sp$population*hex_spacetime_sp$population, df = 3)

CAR_list2 <- list()

weeks <- na.omit(unique(hex_spacetime$date))

for (i in 579:589) {
  
  ## filtering the dataset to a specific date
  hex_spacetime_sp <- hex_spacetime |> 
    filter(date == weeks[i]) 
  
  counter <- 1
  sd_values <- 0
  while (median(sd_values) < 10 && counter < 10) {
    tryCatch({
      set.seed(.Random.seed)
      
      CAR_model <- inla(formula = as.formula(infectionsPC ~ 1+
                                               # X+
                                               f(ID, 
                                                 model = model, 
                                                 graph = "data-products/hexes_adjmat",
                                                 scale.model = TRUE,
                                                 constr = TRUE)
                                             ),
                        data = as.data.frame(hex_spacetime_sp),
                        family = "gaussian",
                        verbose = T,
                        # control.inla=list(cmin=0, strategy='adaptive'),
                        control.compute = compute_list,
                        control.predictor = predictor_list) 
    },
      error = function(e) { 
        set.seed(.Random.seed)
        
        CAR_model <- inla(formula = as.formula(infectionsPC ~ 1+
                                                 # X+
                                                 f(ID,
                                                   model = model,
                                                   scale.model = TRUE,
                                                   adjust.for.con.comp = TRUE,
                                                   constr = TRUE,
                                                   # prior = "pc",
                                                   # param = "c(u, alpha)",
                                                   graph = "data-products/hexes_adjmat.graph")),
                          data = as.data.frame(hex_spacetime_sp),
                          family = "gaussian",
                          verbose = T,
                          # control.inla=list(cmin=0, strategy='adaptive'),
                          control.compute = compute_list,
                          control.predictor = predictor_list)
      })
    sd_values <- CAR_model$summary.fitted.values$sd
    cat("Attempt ", counter, "! \n", sep = "")
    counter = counter + 1
  }
  
  
  CAR_list2[[i-578]] <- cbind(hex_spacetime_sp, 
                         CAR_model$summary.fitted.values |> 
                           rownames_to_column(var = "INLApred")
  )
  
}

# fitted_values <- inla.tmarginal(exp, CAR_model$marginals.fitted.values)
color_option <- "magma"
na_color <- "grey70"
## Breakdowns of each peaks
breaks_plt <- c(0,seq(150,500, 50))
labels_plt <- c("150< ",seq(150,450, 50), ' >500')
limits_plt <- c(0,500)

ggplot() +
  geom_sf(data = hexes|>
            dplyr::mutate(infections = hex_spacetime_sp$infections,
                          infectionsPC = hex_spacetime_sp$infectionsPC,
                          population = hex_spacetime_sp$population,
                          logpopulation = hex_spacetime_sp$logpopulation) |>
            dplyr::mutate(cases_fitted = CAR_model$summary.fitted.values$mean,
                          incidence_fitted = exp(log(cases_fitted) - logpopulation)*1e5,
                          log_incidence = log10(incidence_fitted)) |> 
            # filter(infections > 1) |> 
            st_transform(crs = 26915),
          aes(fill = cases_fitted))+
  scale_fill_viridis_b(option = color_option,
                       name = "Estimated Infections",
                       direction = -1,
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt
  )+
  # khroma::scale_fill_batlow(
  #   name = "Estimated Infections/day",
  #   reverse = T,
  #   # breaks = seq(0,7,1),
  #   labels = scales::label_math(),
  #   # limits = limits_plt
  # )+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  guides(fill = guide_bins(title = "Infections per capita/100k",
                           title.position = "top",
                           title.vjust = 0.5))+
  # labs(title = "InfectionsPC", subtitle = delta_peak)
  labs(subtitle = paste0("CAR model with ", model," implementation and ", family, " likelihood"),
       caption = "*Proportional weights means weights ranging from 1 to 0")

