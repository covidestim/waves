gc()
rm(list = ls())

library(tidyverse)
library(sf)
library(spdep)
library(spatialreg)
library(Matrix)

## 
hexes <- sf::st_read("data-products/geo-hexes/hexes.shp") |> 
  filter(as.integer(hexid) < 7662,
         ## Taking out the isolated hex at Keywest
         as.integer(hexid) != 6545) 

## Hexgrid pop
## New hexgrid with Meta 30m population
hexgrid_pop <- st_read("data-products/geo-hexes/meta_population/hexgrid_meta30m_population.geojson") |> 
  filter(as.integer(hexid) < 7662,
         ## Taking out the isolated hex at Keywest
         as.integer(hexid) != 6545) |> 
  rename(population = metapop_30m) |> 
  filter(population > 0)

## Only keep hexes with a less than 10 cumulative infections per capita
hexgrid_preomicron_cum <- vroom::vroom("data-products/geo-hexes/hexid-observations_preomicron_meta30m.csv") |> 
  # hexObservationsAllSF |>
  st_drop_geometry() |>
  mutate(hexid = as.character(hexid),
         # ## This is wrong! Check back when we do infections allocation
         # infections = case_when(infections >= population ~ population,
         #                        population == 0 ~ 0,
         #                        TRUE ~ infections)
         ) |>
  group_by(hexid) |> 
  summarise(cum_infections = sum(infections, na.rm = T)) |> 
  right_join(hexgrid_pop |>  
               mutate(hexid = as.character(hexid)) |> 
               select(hexid, population))  |> 
  mutate(cum_infectionsPC = cum_infections/population) |> 
  sf::st_as_sf() |> 
  dplyr::mutate(logpopulation = log10(population),
                cum_incidence = exp(log10(cum_infections) - logpopulation),
                log_incidence = log10(cum_incidence+1))

## Filter for only the hexes we are keep to the analysis
hexid_to_keep <- hexgrid_preomicron_cum |> 
  filter(population > 0) |> 
  filter(cum_infectionsPC<= 10)

vroom::vroom_write(x = hexid_to_keep, file = "data-sources/hexid_to_keep.csv")
write_sf(obj = hexid_to_keep,
         dsn = "data-products/geo-hexes/hexid_to_keep.geojson",
         delete_dsn = T,
         delete_layer = T)

## Hex population
hexpop <- st_read("data-products/geo-hexes/meta_population/hexgrid_meta30m_population.geojson") |> 
  filter(as.integer(hexid) < 7662,
         ## Taking out the isolated hex at Keywest
         as.integer(hexid) != 6545,
         as.character(hexid) %in% as.character(hexid_to_keep$hexid)) |> 
  rename(population = metapop_30m)

## Certifying the correct number of unique hex; 7352
length(unique(na.omit(hexpop$hexid)))

hexes <- sf::st_read("data-products/geo-hexes/hexes.shp") |> 
  filter(as.integer(hexid) < 7662,
         ## Taking out the isolated hex at Keywest
         as.integer(hexid) != 6545,
         as.character(hexid) %in% as.character(hexid_to_keep$hexid))

## Neighbors
hexes_nb <- spdep::poly2nb(hexes, queen = TRUE, row.names = hexes$hexid)
# hexes_nb2 <- st_touches(st_geometry(hexes))

## Check Plot
# plot(st_geometry(hexes))
# plot(hexes_nb, st_coordinates(st_centroid(hexes)), add=TRUE, col="blue")

nb2INLA("data-products/hexes_adjmat.graph", nb = hexes_nb)
hexes_graph <- INLA::inla.read.graph("data-products/hexes_adjmat.graph")

# wt_B <- nb2mat(neighbours = hexes_nb, style = "B", zero.policy = T)
# wt_W <- nb2mat(neighbours = hexes_nb, style = "W", zero.policy = T)
# 
# listB <- nb2listw(neighbours = hexes_nb, style = "B", zero.policy = T)
# listW <- nb2listw(neighbours = hexes_nb, style = "W", zero.policy = T)
# 
# ## Weight list as a Sparse C Matrix
# B <- as(listB, "CsparseMatrix")
# W <- as(listW, "CsparseMatrix")

## Population hexes
hex_population <- sf::st_read("data-products/geo-hexes/meta_population/hexgrid_meta30m_population.geojson") |> 
  filter(as.integer(hexid) < 7662,
         ## Taking out the isolated hex at Keywest
         as.integer(hexid) != 6545,
         as.character(hexid) %in% as.character(hexid_to_keep$hexid)) |> 
  rename(population = metapop_30m) |> 
  mutate(logpopulation = log(population))

## Certifying the correct number of unique hex; 7352
length(unique(na.omit(hexes$hexid)))

## Pre-Omicron
hexgrid_preomicron <- vroom::vroom("data-products/geo-hexes/hexid-observations_preomicron_meta30m.csv") |>
  mutate(hexid = as.character(hexid),
         date = as.Date(date)) |>
  select(-geometry) |>
  mutate(infectionsPC = (infections/population)*1e5) |>
  filter(as.integer(hexid) < 7662,
         ## Taking out the isolated hex at Keywest
         as.integer(hexid) != 6545,
         hexid %in% hexid_to_keep$hexid) |> 
  left_join(hexes, by = "hexid") |>
  sf::st_as_sf()

## Certifying the correct number of unique hex; 7352
length(unique(na.omit(hexgrid_preomicron$hexid)))

## Function to push all dates to the end of the epidemiological week
# end.of.epiweek <- function(x, end = 6) {
#   offset <- (end - 4) %% 7
#   num.x <- as.numeric(x)
#   return(x - (num.x %% 7) + offset + ifelse(num.x %% 7 > offset, 7, 0))
# }
# 
# hexgrid_preomicron_week <- hexgrid_preomicron |> 
#   st_drop_geometry() |> 
#   mutate(weekdate = end.of.epiweek(date)) |> 
#   group_by(weekdate, hexid) |> 
#   summarise(infections = sum(infections, na.rm = T)) |> 
#   left_join(hex_population |> 
#               st_drop_geometry() |> 
#               select(hexid, population) |> 
#               mutate(hexid = as.character(hexid))) |> 
#   mutate(infectionsPC = (infections/population)*1e5)
# 
# vroom::vroom_write(x = hexgrid_preomicron_week,
#                    file = "data-products/geo-hexes/hexgrid_meta30m_week.csv")

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
              # rename(date = weekdate) |> 
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

## Certifying the correct number of unique hex; 7352
length(unique(na.omit(hex_spacetime$hexid)))

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
library(INLA)
compute_list <- control.compute(hyperpar = T, 
                                return.marginals = T, 
                                return.marginals.predictor = T,
                                config = T,
                                openmp.strategy = "huge",
                                dic = T, 
                                cpo = T, 
                                waic = T)

predictor_list <- control.predictor(compute = T)

library(foreach)
library(doParallel)
library(dplyr)
library(tibble)

# # Set up parallel backend
# cl <- makeCluster(detectCores() - 2)
# registerDoParallel(cl)

hyper_smooth <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.2, 0.01),  # P(SD > 0.2) = 1% (STRONG smoothing)
    initial = 5            # Start with high precision (1/exp(4) ≈ 0.018 SD)
  )
)

hyper_smooth_bym2 <- list(
  phi = list(prior = "pc", param = c(0.95, 0.5)),  # 50% prob ϕ > 0.95
  prec = list(prior = "pc.prec", param = c(0.2, 0.01))
)

diag.eps = 1e-3

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

weeks <- sort(unique(na.omit(hex_spacetime$date)))

hex_graph <- inla.read.graph("data-products/hexes_adjmat.graph")

# CAR_list <- foreach(i = 1:length(weeks),
#                     .combine = c,
#                     .multicombine = TRUE) %dopar% {

CAR_df <- vroom::vroom("data-products/tsa_meta30m_run_preomicron_daily.csv")

CAR_list <- CAR_df |> 
  dplyr::group_split(date)

## Rerun flag, set this to TRUE is the dates to rerun are a lot to get better model estimates
is.rerun <- TRUE

## setting the hexid column on the same class
CAR_list <- lapply(CAR_list, function(x){x <- x |> mutate(hexid = as.integer(hexid))})

## List to rerun if the model is not well fitted to the data
dates_to_rerun <- sapply(CAR_list, function(x){ifelse(sd(x$sd)<0.0025 || sd(x$sd)>0.010, x$date, NA)})
dates_to_rerun
length(na.omit(dates_to_rerun))

sd_values <- data.frame(weeks = weeks, 
                        sd = sapply(CAR_list, function(x){sd(x$sd)}),
                        upper = sapply(CAR_list, function(x){max(x$sd)}),
                        lower = sapply(CAR_list, function(x){sd(x$sd)})) |> 
  mutate(id = row_number())

ggplot(data = sd_values, 
       aes(x = weeks, y = sd, 
           label = id,
           # ymin = lower, ymax = upper,
           color = if_else(sd <= 0.01 & sd >= 0.0025, "firebrick", "steelblue"),
       ))+
  geom_label()+
  # geom_pointrange()+
  theme_minimal()

## Alpha wave convergence analysis
## Alpha Peak Movie
j <- which(weeks == alpha_peak)

sd_values_alpha <- sd_values |> 
  filter(weeks %in% weeks[seq(j-90,j+60, 2)])

ggplot(data = sd_values_alpha, 
       aes(x = weeks, y = sd, 
           label = id,
           # ymin = lower, ymax = upper,
           color = if_else(sd <= 0.01 & sd >= 0.0025, "blue", "red"),
       ))+
  geom_label()+
  # geom_pointrange()+
  theme_minimal()

## Alpha wave convergence analysis
## Alpha Peak Movie
j <- which(weeks == delta_peak)

sd_values_delta <- sd_values |> 
  filter(weeks %in% weeks[seq(j-90,j+60, 2)])

ggplot(data = sd_values_delta, 
       aes(x = weeks, y = sd, 
           label = id,
           # ymin = lower, ymax = upper,
           color = if_else(sd <= 0.01 & sd >= 0.0025, "blue", "red"),
       ))+
  geom_label()+
  # geom_pointrange()+
  theme_minimal()

## Uncomment when is not a rerun
CAR_list <- list()

alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-04")

test_dates <- c(c(alpha_peak-63,
                  alpha_peak-45,
                  alpha_peak-24,
                  alpha_peak),
                c(delta_peak-63,
                  delta_peak-45,
                  delta_peak-24,
                  delta_peak))

## This fitting takes very long, it is process of prune fitting. Where it starts fitting something and narrows down to a best fit, which parameteres are defined at the while loop

for (i in 1:length(test_dates)) {
  
  ## To clean unused variables here
  # gc()
  
  # if(is.na(dates_to_rerun[i]))next
  
  current_date <- test_dates[i]
  hex_week <- hex_spacetime %>% 
    filter(date == current_date)
  
  best_model <- NULL
  counter <- 0
  sd_values <- c(0.0001, 0.03)  # Start with high value
  
  ## Parameters for the Hessian negative testing, to avoid INLA solver get stuck at negative eigenvalues
  # hess.min <- -1
  # h.value <- 0.01
  # h.trials <- 0.001
  # trials <- 0
  # while (hess.min <= 0 & trials < 50){
    
  while ((sd(sd_values)<0.0025 || sd(sd_values) > 0.010) & counter <= 10) {  # Flipped condition
    tryCatch({
      set.seed(.Random.seed)
      
      best_model <- inla(
        as.formula(infectionsPC ~ 1 + 
                     f(ID, 
                       model = "besag2",
                       graph = "data-products/hexes_adjmat.graph",
                       scale.model = TRUE,
                       diagonal = diag.eps,
                       constr = TRUE,
                       hyper = hyper_smooth)),
        data = as.data.frame(hex_week),
        family = "gaussian",
        control.inla = control.inla(strategy = "gaussian", int.strategy = "eb"),
        # control.inla = control.inla(strategy = "gaussian", h = h.value, int.strategy = "eb"),
        control.mode = control.mode(restart = TRUE),
        control.compute = compute_list,
        control.predictor = predictor_list,
        control.fixed = list(prec.intercept = 0.1),
        num.threads = 6,  # Prevent internal threading conflicts
        # verbose = T
      )
      
      sd_values <- best_model$summary.fitted.values$sd
      
    }, error = function(e) {
      set.seed(.Random.seed)
      
      best_model <- inla(
        as.formula(infectionsPC ~ 1 + 
                     f(ID, 
                       model = "besag2",
                       graph = hex_graph,
                       diagonal = diag.eps,
                       scale.model = TRUE,
                       constr = TRUE,
                       hyper = hyper_smooth)),
        data = as.data.frame(hex_week),
        family = "gaussian",
        control.inla = control.inla(strategy = "gaussian", int.strategy = "eb"),
        # control.inla = control.inla(strategy = "gaussian", h = h.value, int.strategy = "eb"),
        control.mode = control.mode(restart = TRUE),
        control.compute = compute_list,
        control.predictor = predictor_list,
        control.fixed = list(prec.intercept = 0.1),
        num.threads = 6,  # Prevent internal threading conflicts
        # verbose = T
      )
      sd_values <- best_model$summary.fitted.values$sd
    })
    
    counter <- counter + 1
    # hess.start <- which(best_model$logfile == 'Eigenvalues of the Hessian')
    # hess.min <- min(as.numeric(best_model$logfile[(hess.start+1):(hess.start+3)]))
    # h.value <- h.trials + 0.001
    # h.trials <- h.value
    # trials <- trials + 1
  }
  
  cat("Finished CAR model for week ", as.character(current_date),"! \n")
  
  CAR_list[[i]] <-
    # list(
    cbind(hex_week, 
          best_model$summary.fitted.values |> 
            rownames_to_column(var = "INLApred"))
  
  gc()
  rm(best_model, hex_week)
  # )
}

# stopCluster(cl)

## Turning into a df
CAR_df <- bind_rows(CAR_list)

## Saving the df, remember to change the name if the dataset is "preomicron" or "omicronera". The pattern nomenclature to files are tsa_preomicron.csv or tsa_omicronera.csv
dataset <- "meta30m_run_preomicron_daily"

vroom::vroom_write(x = CAR_df, 
                   file = paste0("data-products/tsa_", 
                                 dataset,
                                 ".csv"))

## Saving as list object
save(list = CAR_list, 
     file = "data-products/CAR_list_meta30m.RDS", 
     compress = "xz", 
     compression_level = 9)

#

# fitted_values <- inla.tmarginal(exp, CAR_model$marginals.fitted.values)
color_option <- "magma"
na_color <- "grey70"
## Breakdowns of each peaks
breaks_plt <- c(0,seq(250,2500, 250))
labels_plt <- c("250< ",seq(250,2250, 250), ' >2,500')
limits_plt <- c(0,2500)

## Breakdowns of each peaks
breaks_plt1 <- seq(0,500, 50)
labels_plt1 <- c("<50",seq(50,450, 50), '>500')
limits_plt1 <- c(0,500)
color_option <- "magma"

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
  dplyr::filter(!STATEFP %in% excludes) |> 
  tigris::shift_geometry()

i=325

CAR_df <- vroom::vroom("data-products/tsa_meta30m_run_preomicron_daily.csv")

hexes <- sf::st_read("data-products/geo-hexes/hexes.shp") |> 
  filter(as.integer(hexid) < 7662,
         ## Taking out the isolated hex at Keywest
         as.integer(hexid) != 6545) |> 
  mutate(hexid = as.character(1:n())) |> 
  filter(hexid %in% as.character(unique(CAR_df$hexid))) 

CAR_df_test <- CAR_df |> 
  mutate(hexid = as.character(hexid)) |> 
  filter(date %in% test_dates) |> 
  right_join(hexes) |> 
  st_as_sf()

ggplot() +
  geom_sf(data = CAR_df_test |> 
            filter(!is.na(date)),
          aes(fill = mean))+
  scale_fill_viridis_c(option = color_option, name = "Estimated Infections/100k/day", direction = -1)+
  # scale_fill_viridis_b(option = color_option, 
  #                      name = "Estimated Infections/100k/day", 
  #                      direction = -1,
  #                      breaks = breaks_plt,
  #                      labels = labels_plt,
  #                      limits = limits_plt)+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "in"))+
  facet_wrap(.~date, nrow = 2)
# +
  # guides(fill = guide_bins(title = "Infections per capita/100k",
  #                          # labels = scales::label_math(expr = 10^., format = "force"),
  #                          title.position = "top",
  #                          title.vjust = 0.5))+
  # labs(title = "InfectionsPC", subtitle = delta_peak)+
  # labs(subtitle = paste0("CAR model with ", model," implementation and ", family, " likelihood"),
       # caption = "*Proportional weights means weights ranging from 1 to 0")

