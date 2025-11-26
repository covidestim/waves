setwd("~/Library/CloudStorage/GoogleDrive-nick.menzies@gmail.com/My Drive/Harvard/wavesForNick/")

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
library(here)

###############################################################################
##### Load in the required data files                                     #####
##############################################################################|

##### Hexgrid |
hexes <- st_read("geo-hexes/hexgrid_1100_km.shp") |> 
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
hexgrid_preomicron <- vroom::vroom("geo-hexes/hexid-observations_preomicron_intersection_hexgrid1100km.csv") %>% 
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

##NM add hex centroid coordinates
hex_spacetime$Y <- hex_spacetime$X <- NA

coords <- st_coordinates(st_centroid(hexes))

for( i in unique(hex_spacetime$hexid)){
  hex_spacetime$X[hex_spacetime$hexid==i] <- coords[hexes$hexid==i,"X"]
  hex_spacetime$Y[hex_spacetime$hexid==i] <- coords[hexes$hexid==i,"Y"]
cat("\r",i,"    "); flush.console() }
  
### Return a simple features collection with point geometries for each hexid 
head(st_centroid(hexes))
### Plot to confirm
ggplot() + geom_sf(data=(st_centroid(hexes)), size=.5) +   geom_sf(data=hexes, fill= NA)

###############################################################################
##### Identify neighboring hexes                                          #####
##############################################################################|
## Neighbors
# hexes_nb <- spdep::poly2nb(hexes, queen = TRUE, row.names = hexes$id)

## Check Plot
# plot(st_geometry(hexes))
# plot(hexes_nb, st_coordinates(st_centroid(hexes)), add=F, col="blue",cex=.1)

#### Create and save the graph file needed for INLA
# nb2INLA("hexes_adjmat.graph", nb = hexes_nb)
#### Assign this graph code to object
# hexes_graph <- INLA::inla.read.graph("/hexes_adjmat.graph")

###############################################################################
##### Clean up environment before entering model setup and run            #####
##############################################################################|
rm(hexes)
rm(hexgrid_preomicron)
rm(hexes_nb)

# ## This should be run in batches, as it may take long time and not be suit for memory size.
# ## The fitting strategy is to run the model for all dates and rerun over the problematic dates, when the model didn't fit well
# ## One very common issue is returning a bad run from INLA, 
# ## which only the iid part fits to the model and not the Besag model.
# ## The Newton-Raphson method could not converged, the method is used to some numerical integration on the hyperparameters. 
# ## Another alternative is to log-transform the data before imputing to the INLA model, 
# ## Hävard Rue suggested this on a help desk response at INLA email list. 
# ## Yet another alternative is to set the prior parameterization to the model that can afford huge variance on the data ## Omicron dataset case. We can set the shape and rate parameters of log-Gamma prior to have mean = 1 and variance = 1.
# 
# ## CAR INLA equivalent model
# 
# compute_list <- control.compute(hyperpar = T, 
#                                 return.marginals = T, 
#                                 return.marginals.predictor = T,
#                                 config = T,
#                                 # openmp.strategy = "huge",
#                                 dic = T, 
#                                 cpo = T, 
#                                 waic = T)
# 
# predictor_list <- control.predictor(compute = T)
# 
# # # Set up parallel backend
# # cl <- makeCluster(detectCores() - 2)
# # registerDoParallel(cl)
# 
# ##### Priors for BYM model
# ### Strict smoothing prior
# ### Not used. 
# # hyper_smooth <- list(
# #   prec = list(
# #     prior = "pc.prec",
# #     param = c(0.2, 0.01),  # P(SD > 0.2) = 1% (STRONG smoothing)
# #     initial = 5            # Start with high precision (1/exp(4) ≈ 0.018 SD)
# #   )
# # )
# 
# ### Priors for BYM2 model ###
# ### Strict smoothing prior ### 
# hyper_smooth_bym2 <- list(
#   ### prior on proportion of variance that is due to spatially structured 
#   ### vs. not spatially structured factors
#   phi = list(prior = "pc", param = c(.95, .5),
#              initial=.8),  ## model is sensitive to starting values
#   ### Prior for precision -- how much smoothing 
#   prec = list(prior = "pc.prec", param = c(0.5 / 0.31, 0.01) # Moraga recommended
#   # prec = list(prior = "pc.prec", param = c(0.2, 0.01), #Rafa parameterization
#   )
# )
# 
# diag.eps = 1e-3

###############################################################################
##### Set which days to run the model for                                 #####
##############################################################################|
# days <- sort(unique(na.omit(hex_spacetime$date)))

##### Define the peaks of interest
alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-04")

# days <- c(seq.Date(from = alpha_peak-63, to = alpha_peak, by = "day"))#,
# days <- (seq.Date(from = delta_peak-63, to = delta_peak, by = "day"))
  
  ##### Create an empty list to keep model output
  #   CAR_list <- list()
  # CAR_diag_list <- list()
  
  
  
  ##NM
  library(mgcv)

  ###### alpha wave   ######   ######   ######   ######   ######   ###### 
alpha_dates <- alpha_peak-(63:0)

####### Fit  models with k = 100
alphaFits100 <- list()
for(dy in 1:64){
  alphaFits100[[dy]] <- gam(infectionsPC~s(X,Y,bs="tp",k=100),
                            data=hex_spacetime[hex_spacetime$date==alpha_dates[dy],])
  cat("\r",dy,"    "); flush.console() }

## Tabulate predictions
alphaPreds100 <- matrix(NA,length(unique(hex_spacetime$hexid)),64)
rownames(alphaPreds100) <- unique(hex_spacetime$hexid)
colnames(alphaPreds100) <- as.character(alpha_dates)

for(dy in 1:64){
  alphaPreds100[!is.na(hex_spacetime$infectionsPC[hex_spacetime$date==alpha_dates[dy]]),dy] <- 
    alphaFits100[[dy]]$fitted.values
}

## Set <0 to 0
alphaPreds100[alphaPreds100<0] <- 0

colnames(hex_spacetime)
####### Fit  models with k = 100
alphaFits100b <- list()
hex_spacetime$hexid_f <- as.factor(hex_spacetime$hexid)
for(dy in 1:64){
  alphaFits100b[[dy]] <- gam(infectionsPC~s(X,Y,bs="tp",k=100)+s(hexid_f,bs="re"),
                            data=hex_spacetime[hex_spacetime$date==alpha_dates[dy],])
  cat("\r",dy,"    "); flush.console() }


## Tabulate predictions
alphaPreds100 <- matrix(NA,length(unique(hex_spacetime$hexid)),64)
rownames(alphaPreds100) <- unique(hex_spacetime$hexid)
colnames(alphaPreds100) <- as.character(alpha_dates)

for(dy in 1:64){
  alphaPreds100[!is.na(hex_spacetime$infectionsPC[hex_spacetime$date==alpha_dates[dy]]),dy] <- 
    alphaFits100[[dy]]$fitted.values
}

## Set <0 to 0
alphaPreds100[alphaPreds100<0] <- 0

  ####### Fit  models with k = 200

  alphaFits200 <- list()
  for(dy in 1:64){
    alphaFits200[[dy]] <- gam(infectionsPC~s(X,Y,bs="tp",k=200),
                data=hex_spacetime[hex_spacetime$date==alpha_dates[dy],])
    cat("\r",dy,"    "); flush.console() }
  
  ## Tabulate predictions
  alphaPreds200 <- matrix(NA,length(unique(hex_spacetime$hexid)),64)
  rownames(alphaPreds200) <- unique(hex_spacetime$hexid)
  colnames(alphaPreds200) <- as.character(alpha_dates)
  
  for(dy in 1:64){
    alphaPreds200[!is.na(hex_spacetime$infectionsPC[hex_spacetime$date==alpha_dates[dy]]),dy] <- 
      alphaFits200[[dy]]$fitted.values
  }

  ## Set <0 to 0
  alphaPreds200[alphaPreds200<0] <- 0
  
  ####### Fit  models with k = 500
  alphaFits500 <- list()
  
  for(dy in 1:64){
    alphaFits500[[dy]] <- gam(infectionsPC~s(X,Y,bs="tp",k=500),
                              data=hex_spacetime[hex_spacetime$date==alpha_dates[dy],])
    cat("\r",dy,"    "); flush.console() }
  
  ## Tabulate predictions
  alphaPreds500 <- matrix(NA,length(unique(hex_spacetime$hexid)),64)
  rownames(alphaPreds500) <- unique(hex_spacetime$hexid)
  colnames(alphaPreds500) <- as.character(alpha_dates)
  
  for(dy in 1:64){
    alphaPreds500[!is.na(hex_spacetime$infectionsPC[hex_spacetime$date==alpha_dates[dy]]),dy] <- 
      alphaFits500[[dy]]$fitted.values
  }
  
  ## Set <0 to 0
  alphaPreds500[alphaPreds500<0] <- 0
  
  ####### Fit  models with k = 500
  alphaFits1000 <- list()
  
  for(dy in 1:64){
    alphaFits1000[[dy]] <- gam(infectionsPC~s(X,Y,bs="tp",k=1000),
                              data=hex_spacetime[hex_spacetime$date==alpha_dates[dy],])
    cat("\r",dy,"    "); flush.console() }
  
  ## Tabulate predictions
  alphaPreds1000 <- matrix(NA,length(unique(hex_spacetime$hexid)),64)
  rownames(alphaPreds1000) <- unique(hex_spacetime$hexid)
  colnames(alphaPreds1000) <- as.character(alpha_dates)
  
  for(dy in 1:64){
    alphaPreds1000[!is.na(hex_spacetime$infectionsPC[hex_spacetime$date==alpha_dates[dy]]),dy] <- 
      alphaFits1000[[dy]]$fitted.values
  }
  
  ## Set <0 to 0
  alphaPreds1000[alphaPreds1000<0] <- 0
  
  ####### Get coordinates
  coords2 <- data.frame(hexid = unique(hex_spacetime$hexid),X=NA,Y=NA)
  for(i in 1:nrow(coords2)){
    coords2$X[i] <- hex_spacetime$X[hex_spacetime$hexid==coords2$hexid[i]][1]
    coords2$Y[i] <- hex_spacetime$Y[hex_spacetime$hexid==coords2$hexid[i]][1]
  }
  
  ####### Plot
  ## Color levels
  mx = max(alphaPreds100,alphaPreds200,alphaPreds500,alphaPreds1000,na.rm=T)
  cuts <- seq(0,mx,length.out=31)
  alphaPreds100_lvls <- floor(alphaPreds100/(mx+1e-9)*30)+1
  alphaPreds200_lvls <- floor(alphaPreds200/(mx+1e-9)*30)+1
  alphaPreds500_lvls <- floor(alphaPreds500/(mx+1e-9)*30)+1
  alphaPreds1000_lvls <- floor(alphaPreds1000/(mx+1e-9)*30)+1
  
  # k=100
  library(RColorBrewer)
  cls <-  colorRampPalette(brewer.pal(11, "RdYlBu"))(30)[30:1]
  
  pdfnam <- "Fig_alpha_wave_k100_Nov-23-2025.pdf"
  pdf(file=pdfnam,width=9, height=7.25) 
  par(mfrow=c(7,5),mar=c(0,0,0,0),oma=c(1,1,1,1))
  
  for(dy in 1:64){
    plot(NA,NA,xlim=range(coords2$X),ylim=range(coords2$Y),axes=F,xlab=NA,ylab=NA)
    points(coords2$X,coords2$Y,col=cls[alphaPreds100_lvls[,dy]],pch=16,cex=0.28)
    text(-2.6*1e6,350000,alpha_dates[dy],pos=4,cex=0.9)
    box(lwd=0.3)
  }
  
  dev.off();system(paste("open", pdfnam))
  
    # k=200
  library(RColorBrewer)
  cls <-  colorRampPalette(brewer.pal(11, "RdYlBu"))(30)[30:1]
 
  pdfnam <- "Fig_alpha_wave_k200_Nov-23-2025.pdf"
  pdf(file=pdfnam,width=9, height=7.25) 
  par(mfrow=c(7,5),mar=c(0,0,0,0),oma=c(1,1,1,1))
  
  for(dy in 1:64){
  plot(NA,NA,xlim=range(coords2$X),ylim=range(coords2$Y),axes=F,xlab=NA,ylab=NA)
  points(coords2$X,coords2$Y,col=cls[alphaPreds200_lvls[,dy]],pch=16,cex=0.28)
  text(-2.6*1e6,350000,alpha_dates[dy],pos=4,cex=0.9)
  box(lwd=0.3)
  }
  
  dev.off();system(paste("open", pdfnam))
  
  # k=500
  pdfnam <- "Fig_alpha_wave_k500_Nov-23-2025.pdf"
  pdf(file=pdfnam,width=9, height=7.25) 
  par(mfrow=c(7,5),mar=c(0,0,0,0),oma=c(1,1,1,1))
  
  for(dy in 1:64){
    plot(NA,NA,xlim=range(coords2$X),ylim=range(coords2$Y),axes=F,xlab=NA,ylab=NA)
    points(coords2$X,coords2$Y,col=cls[alphaPreds500_lvls[,dy]],pch=16,cex=0.28)
    text(-2.6*1e6,350000,alpha_dates[dy],pos=4,cex=0.9)
    box(lwd=0.3)
  }
  
  dev.off();system(paste("open", pdfnam))
  
  
  # k=1000
  pdfnam <- "Fig_alpha_wave_k1000_Nov-23-2025.pdf"
  pdf(file=pdfnam,width=9, height=7.25) 
  par(mfrow=c(7,5),mar=c(0,0,0,0),oma=c(1,1,1,1))
  
  for(dy in 1:64){
    plot(NA,NA,xlim=range(coords2$X),ylim=range(coords2$Y),axes=F,xlab=NA,ylab=NA)
    points(coords2$X,coords2$Y,col=cls[alphaPreds1000_lvls[,dy]],pch=16,cex=0.28)
    text(-2.6*1e6,350000,alpha_dates[dy],pos=4,cex=0.9)
    box(lwd=0.3)
  }
  
  dev.off();system(paste("open", pdfnam))
  
  
  
  # save
  save(alphaPreds200,alphaPreds500,alphaPreds1000,coords2,file="alpha_wave_fits_Nov-23-2025.rData")
 #  load("alpha_wave_fits_Nov-23-2025.rData") # alphaPreds200,alphaPreds500,coords2
  
  ###### delta wave   ######   ######   ######   ######   ######   ###### 
  ####### Fit  models with k = 200
  delta_dates <- delta_peak-(63:0)
  deltaFits200 <- list()
  
  for(dy in 1:64){
    deltaFits200[[dy]] <- gam(infectionsPC~s(X,Y,bs="tp",k=200),
                              data=hex_spacetime[hex_spacetime$date==delta_dates[dy],])
    cat("\r",dy,"    "); flush.console() }
  
  ## Tabulate predictions
  deltaPreds200 <- matrix(NA,length(unique(hex_spacetime$hexid)),64)
  rownames(deltaPreds200) <- unique(hex_spacetime$hexid)
  colnames(deltaPreds200) <- as.character(delta_dates)
  
  for(dy in 1:64){
    deltaPreds200[!is.na(hex_spacetime$infectionsPC[hex_spacetime$date==delta_dates[dy]]),dy] <- 
      deltaFits200[[dy]]$fitted.values
  }
  
  ## Set <0 to 0
  deltaPreds200[deltaPreds200<0] <- 0
  
  ####### Fit  models with k = 500
  deltaFits500 <- list()
  
  for(dy in 1:64){
    deltaFits500[[dy]] <- gam(infectionsPC~s(X,Y,bs="tp",k=500),
                              data=hex_spacetime[hex_spacetime$date==delta_dates[dy],])
    cat("\r",dy,"    "); flush.console() }
  
  ## Tabulate predictions
  deltaPreds500 <- matrix(NA,length(unique(hex_spacetime$hexid)),64)
  rownames(deltaPreds500) <- unique(hex_spacetime$hexid)
  colnames(deltaPreds500) <- as.character(delta_dates)
  
  for(dy in 1:64){
    deltaPreds500[!is.na(hex_spacetime$infectionsPC[hex_spacetime$date==delta_dates[dy]]),dy] <- 
      deltaFits500[[dy]]$fitted.values
  }
  
  ## Set <0 to 0
  deltaPreds500[deltaPreds500<0] <- 0
  
  ####### Fit  models with k = 1000
  deltaFits1000 <- list()
  
  for(dy in 1:64){
    deltaFits1000[[dy]] <- gam(infectionsPC~s(X,Y,bs="tp",k=1000),
                              data=hex_spacetime[hex_spacetime$date==delta_dates[dy],])
    cat("\r",dy,"    "); flush.console() }
  
  ## Tabulate predictions
  deltaPreds1000 <- matrix(NA,length(unique(hex_spacetime$hexid)),64)
  rownames(deltaPreds1000) <- unique(hex_spacetime$hexid)
  colnames(deltaPreds1000) <- as.character(delta_dates)
  
  for(dy in 1:64){
    deltaPreds1000[!is.na(hex_spacetime$infectionsPC[hex_spacetime$date==delta_dates[dy]]),dy] <- 
      deltaFits1000[[dy]]$fitted.values
  }
  
  ## Set <0 to 0
  deltaPreds1000[deltaPreds1000<0] <- 0
  
  ####### Plot
  ## Color levels
  mx = max(deltaPreds200,deltaPreds500,deltaPreds1000,na.rm=T)
  cuts <- seq(0,mx,length.out=31)
  deltaPreds200_lvls <- floor(deltaPreds200/(mx+1e-9)*30)+1
  deltaPreds500_lvls <- floor(deltaPreds500/(mx+1e-9)*30)+1
  deltaPreds1000_lvls <- floor(deltaPreds1000/(mx+1e-9)*30)+1
  
  # k=200
  library(RColorBrewer)
  cls <-  colorRampPalette(brewer.pal(11, "RdYlBu"))(30)[30:1]
  
  pdfnam <- "Fig_delta_wave_k200_Nov-23-2025.pdf"
  pdf(file=pdfnam,width=9, height=7.25) 
  par(mfrow=c(7,5),mar=c(0,0,0,0),oma=c(1,1,1,1))
  
  for(dy in 1:64){
    plot(NA,NA,xlim=range(coords2$X),ylim=range(coords2$Y),axes=F,xlab=NA,ylab=NA)
    points(coords2$X,coords2$Y,col=cls[deltaPreds200_lvls[,dy]],pch=16,cex=0.28)
    text(-2.6*1e6,350000,delta_dates[dy],pos=4,cex=0.9)
    box(lwd=0.3)
  }
  
  dev.off();system(paste("open", pdfnam))
  
  # k=500
  pdfnam <- "Fig_delta_wave_k500_Nov-23-2025.pdf"
  pdf(file=pdfnam,width=9, height=7.25) 
  par(mfrow=c(7,5),mar=c(0,0,0,0),oma=c(1,1,1,1))
  
  for(dy in 1:64){
    plot(NA,NA,xlim=range(coords2$X),ylim=range(coords2$Y),axes=F,xlab=NA,ylab=NA)
    points(coords2$X,coords2$Y,col=cls[deltaPreds500_lvls[,dy]],pch=16,cex=0.28)
    text(-2.6*1e6,350000,delta_dates[dy],pos=4,cex=0.9)
    box(lwd=0.3)
  }
  
  dev.off();system(paste("open", pdfnam))
  
  # k=500
  pdfnam <- "Fig_delta_wave_k1000_Nov-23-2025.pdf"
  pdf(file=pdfnam,width=9, height=7.25) 
  par(mfrow=c(7,5),mar=c(0,0,0,0),oma=c(1,1,1,1))
  
  for(dy in 1:64){
    plot(NA,NA,xlim=range(coords2$X),ylim=range(coords2$Y),axes=F,xlab=NA,ylab=NA)
    points(coords2$X,coords2$Y,col=cls[deltaPreds1000_lvls[,dy]],pch=16,cex=0.28)
    text(-2.6*1e6,350000,delta_dates[dy],pos=4,cex=0.9)
    box(lwd=0.3)
  }
  
  dev.off();system(paste("open", pdfnam))
  
  # save
  save(deltaPreds200,deltaPreds500,deltaPreds1000,coords2,file="delta_wave_fits_Nov-23-2025.rData")
  # load("delta_wave_fits_Nov-16-2025.rData")
  
###############################################################################
##### Run the model 
# ##############################################################################|
# for (i in 1:length(days)) {
#   # for (i in which(days %in% bad.runs[4])) {
#   #### If there is nothing to rerun go to the next date
#   #### might need to fix for first run - check. 
# 
#   current_date <- days[i]
#   cat("Starting model for date: ", as.character(current_date),"!\n")
#   
#   hex_week <- hex_spacetime %>% 
#     filter(date == current_date)
#   
#   #### Setup for the while loop 
#   best_model <- NULL # holds the model fit
#   ## counter for while
#   counter <- 0
#   while_condition <- TRUE
#   
#   ### Going to run the model 10 times for bad dates. 
#   while ((while_condition) & counter <= 3) {  # Flipped condition
#     
#     cat("Attempt to fit the model number: ", counter, "!\n")
#     ### sometimes a bad date can break the INLA model, this tryCatch
#     ### is to avoid escaping the loop due to one date error. 
#     tryCatch({
#       seed <- sample(1:1e6,1) #386860
#       set.seed(seed)
#       # set.seed(.Random.seed) ## change the seed for reruns 
#       ### Call the BYM2 model with INLA
#       best_model <- inla(
#         as.formula(infectionsPC ~ 1 + 
#                      f(id, ### either need to update data above or id to hexid
#                        model = "bym2", 
#                        graph = hexes_graph,
#                        scale.model = TRUE, 
#                        # diagonal = diag.eps,
#                        constr = TRUE, ## sum to zero constraint 
#                        hyper = hyper_smooth_bym2
#                      )),
#         data = as.data.frame(hex_week),
#         family = "gaussian", #likelihood
#         control.inla = control.inla(strategy = "gaussian", int.strategy = "eb"), #strategy to sample the hyperparameter space
#         control.mode = control.mode(restart = TRUE),  
#         control.compute = compute_list,
#         control.predictor = predictor_list,
#         # control.fixed = list(prec.intercept = 0.1),
#         num.threads = 1#,  # Prevent internal threading conflicts (needs to be 1 for parallelized code)
#         # verbose = T
#       )
#       
#     }, error = function(e) {
#       seed <- sample(1:1e6,1) #386860
#       set.seed(seed)
#       # set.seed(.Random.seed)
#       
#       best_model <- inla(
#         as.formula(infectionsPC ~ 1 + 
#                      f(id, 
#                        model = "bym2",
#                        graph = hexes_graph,
#                        # diagonal = diag.eps,
#                        scale.model = TRUE,
#                        constr = TRUE,
#                        hyper = hyper_smooth_bym2
#                      )),
#         data = as.data.frame(hex_week),
#         family = "gaussian",
#         control.inla = control.inla(strategy = "gaussian", int.strategy = "eb"),
#         control.mode = control.mode(restart = TRUE),
#         control.compute = compute_list,
#         control.predictor = predictor_list,
#         # control.fixed = list(prec.intercept = 0.1),
#         num.threads = 1#,  # Prevent internal threading conflicts
#         # verbose = T
#       )
#     })
#     
#     counter <- counter + 1
#     # Update while condition
#     if(i >= 2){
#       ## Create a vector of size 2 that will check for big drop in the median value for the sd
#       vec_1 <- data.frame(
#         upper_sd  = range(CAR_list[[i-1]]$sd)[2],
#         median_sd = median(CAR_list[[i-1]]$sd))
#       
#       vec_2 <- data.frame(
#         upper_sd  = range(best_model$summary.fitted.values$sd)[2],
#         median_sd = median(best_model$summary.fitted.values$sd))
#       
#       ### We cannot check the "after" until we have the full dataset. 
#       ### We will revisit this after running one peak to see if necessary. 
#       
#       # vec_3 <- data.frame(
#       #   upper_sd  = range(CAR_list[[i+1]]$sd)[2],
#       #   median_sd = median(CAR_list[[i+1]]$sd))
#       
#       vec <- rbind(vec_1, vec_2) #, vec_3)
#       
#       ### These check for "bad" runs for certain dates. If 
#       ### either are true, rerun that date.  
#       # Position 2 is the rerun position
#       # 1) big drop in median vs. either neighbor
#       drop_prev <- (vec$median_sd[1] - vec$median_sd[2]) > 3
#       # drop_next <- (vec$median_sd[3] - vec$median_sd[2]) > 3
#       median_drop <- drop_prev #|| drop_next
#       
#       # 2) big spike in upper vs. both neighbors
#       spike_prev <- (vec$upper_sd[2] - vec$upper_sd[1]) > 3
#       # spike_next <- (vec$upper_sd[2] - vec$upper_sd[3]) > 3
#       upper_spike <- spike_prev #&& spike_next
#       
#       while_condition <- median_drop || upper_spike
#       
#     } else {
#       while_condition <- FALSE}
#   }
#   
#   cat("Finished CAR model for week ", as.character(current_date),"! \n")
#   
#   CAR_list[[i]] <- cbind(hex_week, 
#                          best_model$summary.fitted.values |> 
#                            rownames_to_column(var = "INLApred"))
#   
#   CAR_diag_list[[i]] <- list(seed, 
#                              best_model$summary.hyperpar)
#   ##### Clean up memory before next run
#   # rm(best_model, hex_week)
#   gc()
# }

###############################################################################
##### Reformat the CAR list into a dataframe and saving it. 
# stopCluster(cl)

## Turning into a df
## last good day 2020-10-13
#CAR_df <- bind_rows(CAR_list)
# CAR_df <- bind_rows(CAR_list[-length(CAR_list)])

## Saving the df, remember to change the name if the dataset is "preomicron" or "omicronera". The pattern nomenclature to files are tsa_preomicron.csv or tsa_omicronera.csv
# dataset <- "hexgrid1100km_run_preomicron_daily_wave1"

# vroom::vroom_write(x = CAR_df,
#                    file = paste0("Data/data-products/car_",
#                                  dataset,
#                                  ".csv"))
# 
# ## Saving as list object
# saveRDS(CAR_list,
# file = "Data/data-products/car_list_wave2.RDS", version=2)
# save(list = CAR_list, 
#      file = "Data/data-products/car_list.RDS", 
#      compress = "xz", 
#      compression_level = 9)
# CAR_list <- readRDS("Data/data-products/car_list_Wave1.RDS")
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
# family <- "gaussian"
# model <- "besag2"
# 
# ## Figure2
# # Source: https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code
# excludes = c(
#   "02", "60", "03", "81", "07", "64",
#   "14", "66", "84", "86", "67", "89",
#   "68", "71", "76", "69", "70", "95",
#   "43", "72", "74", "78", "79", "15", "11"
# )
# 
# us_states <- tigris::states(cb = T) |> 
#   dplyr::filter(!STATEFP %in% excludes) #|> 
#   # tigris::shift_geometry()

### Reload hexgrid 
# hexes <- st_read("Data/data-products/geo-hexes/hexgrid_1100_km.shp") |> 
#   filter(
#     # Taking out the isolated hex at Keywest
#     as.integer(hexid) != 6644) %>%
#   # INLA requires the id to only be 1:N, where N is the total
#   # number of observations; because of this we need to rename the 
#   # hexids to be continuous. 
#   mutate(hexid = ifelse(as.numeric(hexid) < 6645, as.numeric(hexid), 
#                         as.numeric(hexid) - 1), 
#          hexid = as.character(hexid))
# 
# 
# CAR_df_test <- CAR_df |> 
#   # filter(date %in% test_dates) |> 
#   right_join(hexes) |>
#   st_as_sf()
# 
# color_option <- "magma"
# na_color <- "grey70"
# ## Breakdowns of each peaks
# breaks_plt <- c(0,seq(25,275, 25))
# labels_plt <- c("25< ",seq(25,250, 25), ' >250')
# limits_plt <- c(0,300)
# 
# ggplot() +
#   geom_sf(data = CAR_df_test |> 
#             filter(!is.na(date)),
#           aes(fill = mean), color = NA)+
#   scale_fill_viridis_b(option = color_option,
#                        # name = "Estimated Infections/1000/week",
#                        direction = -1,
#                        na.value = na_color,
#                        breaks = breaks_plt,
#                        labels = labels_plt,
#                        limits = limits_plt
#   )+
#   # scale_fill_viridis_c(option = color_option, 
#   #                      name = "Estimated Infections/100k/day", direction = -1)+
#   geom_sf(data = us_states, aes(geometry = geometry), fill = NA) +
#   theme_minimal()+
#   theme(legend.position = "bottom",
#         legend.title.position = "top",
#         legend.key.width = grid::unit(1, "in"))+
#   facet_wrap(.~date, nrow = 7)
