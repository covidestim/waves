### NEED TO UPDATE LINES 66, 67 WITH MEDIAN VALUES BEFORE RUNNING 
###############################################################################
###########################        SETUP        ###############################
###############################################################################
setwd("~/waves")

### load packages
library(dplyr)
library(magick)
library(sf)
library(ggplot2)
library(here)
library(viridis)
library(patchwork)
library(ggridges)

### define the date of the first and second peaks
first_peak <- as.Date("2020-11-19")
second_peak <- as.Date("2021-09-04")

### Read in the stable hex grid
hexesSHP <- st_read("Data/data-products/geo-hexes/hexgrid_1100_km.shp") |> 
            filter(
              # Taking out the isolated hex at Keywest
              as.integer(hexid) != 6644) %>%
            # INLA requires the id to only be 1:N, where N is the total
            # number of observations; because of this we need to rename the 
            # hexids to be continuous. 
            mutate(hexid = ifelse(as.numeric(hexid) < 6645, as.numeric(hexid), 
                                  as.numeric(hexid) - 1),
                   hexid = as.integer(hexid))

### Read in the first peak speeds
distanceToFrontier_firstWave <- readRDS(here("Data/data-products/wavefronts/distanceToFrontier_firstWave.rds"))

### Read in the second peak speeds 
distanceToFrontier_secondWave <- readRDS(here("Data/data-products/wavefronts/distanceToFrontier_secondWave.rds"))


### Read in the first wave bounds 
boundList_w1 <- readRDS(here("Data/data-products/wavefronts/boundaryData_firstWave.rds"))

### Read in the second wave bounds
boundList_w2 <- readRDS(here("Data/data-products/wavefronts/boundaryData_secondWave.rds"))


### Overall means and median speeds for each wave
### Wave 1 
mean(bind_rows(distanceToFrontier_firstWave)$distToFront/1e3) #km/day
median(bind_rows(distanceToFrontier_firstWave)$distToFront/1e3) #km/day
quantile(bind_rows(distanceToFrontier_firstWave)$distToFront/1e3, probs=c(0.25, 0.75))

### Wave 2
mean(bind_rows(distanceToFrontier_secondWave)$distToFront/1e3) #km/day
median(bind_rows(distanceToFrontier_secondWave)$distToFront/1e3) #km/day
quantile(bind_rows(distanceToFrontier_secondWave)$distToFront/1e3, probs=c(0.25, 0.75))


### set a variable for the number of rows to use to create empty dataframes
dim.rows <- length(distanceToFrontier_firstWave) + 
  length(distanceToFrontier_secondWave)

### empty dataframes for various summary measures across wave speeds
medianSpeed <- data.frame("wave" = c(rep("first wave", length(distanceToFrontier_firstWave)), 
                                     rep("second wave", length(distanceToFrontier_secondWave))),
                          "speed" = rep(NA, dim.rows),
                          "days before" = rep(63:1,2), 
                          "quantile" = "50th", 
                          check.names = FALSE)

firstQuantSpeed <- data.frame("wave" = c(rep("first wave", length(distanceToFrontier_firstWave)), 
                                         rep("second wave", length(distanceToFrontier_secondWave))),
                              "speed" = rep(NA, dim.rows),
                              "days before" = rep(63:1,2), 
                              "quantile" = "25th", 
                              check.names = FALSE)

thirdQuantSpeed <- data.frame("wave" = c(rep("first wave", length(distanceToFrontier_firstWave)), 
                                         rep("second wave", length(distanceToFrontier_secondWave))),
                              "speed" = rep(NA, dim.rows),
                              "days before" = rep(63:1,2), 
                              "quantile" = "75th", 
                              check.names = FALSE)

meanSpeed <- data.frame("wave" = c(rep("first wave", length(distanceToFrontier_firstWave)), 
                                     rep("second wave", length(distanceToFrontier_secondWave))),
                          "speed" = rep(NA, dim.rows),
                          "days before" = rep(63:1,2), 
                          "weeks before" = rep(9:1, each=7),
                          check.names = FALSE)

### Calculate median wavefront speed at each time
for (i in 1:dim.rows){
  print(i)
  if (i <= length(distanceToFrontier_firstWave)){
    medianSpeed[i, "speed"] <- median(distanceToFrontier_firstWave[[i]]$distToFront, na.rm = TRUE) / 1000
    firstQuantSpeed[i, "speed"] <- quantile(distanceToFrontier_firstWave[[i]]$distToFront, na.rm = TRUE, probs=c(0.25)) / 1000
    thirdQuantSpeed[i, "speed"] <- quantile(distanceToFrontier_firstWave[[i]]$distToFront, na.rm = TRUE, probs=c(0.75)) / 1000
    meanSpeed[i, "speed"] <- mean(distanceToFrontier_firstWave[[i]]$distToFront, na.rm = TRUE) / 1000
  } else {
    medianSpeed[i, "speed"] <- median(distanceToFrontier_secondWave[[i-length(distanceToFrontier_firstWave)]]$distToFront, na.rm = TRUE) /1000
    firstQuantSpeed[i, "speed"] <- quantile(distanceToFrontier_secondWave[[i-length(distanceToFrontier_firstWave)]]$distToFront, na.rm = TRUE, probs=c(0.25)) / 1000
    thirdQuantSpeed[i, "speed"] <- quantile(distanceToFrontier_secondWave[[i-length(distanceToFrontier_firstWave)]]$distToFront, na.rm = TRUE, probs=c(0.75)) / 1000
    meanSpeed[i, "speed"] <- mean(distanceToFrontier_secondWave[[i-length(distanceToFrontier_firstWave)]]$distToFront, na.rm = TRUE) /1000
}}

### After the wave saturates the United States, the speed was encoded as "Inf" 
### Replace these with NA in each of the various dataframes 
medianSpeed <- medianSpeed %>% mutate(speed = ifelse(is.infinite(speed), NA, speed))
meanSpeed <- meanSpeed %>% mutate(speed = ifelse(is.infinite(speed), NA, speed))
firstQuantSpeed <- firstQuantSpeed %>% mutate(speed = ifelse(is.infinite(speed), NA, speed))
thirdQuantSpeed <- thirdQuantSpeed %>% mutate(speed = ifelse(is.infinite(speed), NA, speed))

wave1char <- readRDS(here("Data/data-products/wave1-lengtharea.rds"))
wave2char <- readRDS(here("Data/data-products/wave2-lengtharea.rds"))

###############################################################################
###########################      WAVE 1      ##################################
###############################################################################
### Create a dataframe of all the daily speeds of wave 1 
allWave1 <- distanceToFrontier_firstWave[[1]]
for (i in 2:length(distanceToFrontier_firstWave)){
  allWave1 <- rbind(allWave1, distanceToFrontier_firstWave[[i]])
}
### After the wave saturates the United States, the speed was encoded as "Inf" 
### Replace these with NA in each of the various dataframes 
# allWave1 <- allWave1 %>% as.data.frame() %>%
#   mutate(distToFrontNew = ifelse(is.infinite(distToFront), NA, distToFront))

### Create dataframe of dates and weeks 
dates <- as.Date(unique(unlist(allWave1$date)))
week <- rep(9:1, each=7)
weekDF <- as.data.frame(cbind(dates, week))
weekDF$dates <- as.Date(weekDF$dates)
weekDF <- weekDF %>% rename(date=dates)

### Join these so we can compute daily averages for each week
allWave1 <- left_join(allWave1, weekDF)

### daily mean for each day ### 
dlyMean1 <- allWave1 %>% as.data.frame() %>%
  st_drop_geometry() %>%
  reframe(dailyMean = mean(distToFront, na.rm=TRUE)/1000, .by=c("date"))

### daily median for each day ### 
dlyMedian1 <- allWave1 %>% as.data.frame() %>%
  st_drop_geometry() %>%
  reframe(dailyMedian = median(distToFront, na.rm=TRUE)/1000, .by=c("date"))

### difference between daily mean and median
# ggplot() +
#   geom_point(data=dlyMean1, aes(y=date, x=dailyMean), color="skyblue") +
#   geom_point(data=dlyMedian1, aes(y=date, x=dailyMedian)) + theme_minimal() +
#   scale_x_log10()

### daily mean for each week ### 
wkMean1 <- allWave1 %>%  as.data.frame() %>%
  st_drop_geometry() %>%
  reframe(weeklyMean = mean(distToFront, na.rm=TRUE)/1000, .by=c("week")) %>%
  mutate(wave = "1")
###Overall mean
mean(allWave1$distToFront/1000, na.rm=T)
### Range of the daily mean
range(dlyMean1$dailyMean)
quantile(dlyMean1$dailyMean)
mean(dlyMean1$dailyMean, na.rm=T)
mean(wkMean1$weeklyMean, na.rm=T)

###############################################################################
###########################      WAVE 2      ##################################
###############################################################################
### Create a dataframe of all the daily speeds of wave 2
allWave2 <- distanceToFrontier_secondWave[[1]]
for (i in 2:length(distanceToFrontier_secondWave)){
  allWave2 <- rbind(allWave2, distanceToFrontier_secondWave[[i]])
}

# allWave2 <- allWave2 %>% 
#   mutate(distToFront = ifelse(is.infinite(distToFront), NA, distToFront))

### Create dataframe of dates and weeks 
dates <- as.Date(unique(unlist(allWave2$date)))
week <- rep(9:1, each=7)[1:(length(dates))]
weekDF <- as.data.frame(cbind(dates, week))
weekDF$dates <- as.Date(weekDF$dates)
weekDF <- weekDF %>% rename(date=dates)

### Join these so we can compute daily averages for each week
allWave2 <- left_join(allWave2, weekDF)

### daily mean for each day ### 
dlyMean2 <- allWave2 %>% as.data.frame() %>%
  st_drop_geometry() %>%
  reframe(dailyMean = mean(distToFront, na.rm=TRUE)/1000, .by=c("date"))

### daily median for each day ###
dlyMedian2 <- allWave2 %>% as.data.frame() %>%
  st_drop_geometry() %>%
  reframe(dailyMedian = median(distToFront, na.rm=TRUE)/1000, .by=c("date"))

### difference between daily mean and median
# ggplot() + 
#   geom_point(data=dlyMean2, aes(y=date, x=dailyMean), color="skyblue") + 
#   geom_point(data=dlyMedian2, aes(y=date, x=dailyMedian)) + theme_minimal() + 
#   scale_x_log10()

### daily mean for each week ### 
wkMean2 <- allWave2 %>% as.data.frame() %>%
  st_drop_geometry() %>%
  reframe(weeklyMean = mean(distToFront, na.rm=TRUE)/1000, .by=c("week")) %>%
  mutate(wave = "2")

mean(allWave2$distToFront/1000, na.rm=T)
quantile(allWave2$distToFront/1000)
range(dlyMean2$dailyMean)
mean(dlyMean2$dailyMean, na.rm=T)
mean(wkMean2$weeklyMean, na.rm=T)

###########  BY WEEK PLOT  ########### ########### ########### ########### 
wkMeans <- rbind(wkMean1, wkMean2) %>% as.data.frame()

ggplot() + 
  geom_point(data=wkMeans, aes(x=week, y=weeklyMean, color = wave), size=2) + 
  geom_line(data=wkMeans, aes(x=week, y=weeklyMean, color = wave)) + 
  scale_x_continuous(trans = "reverse", breaks = seq(9,1,-1)) +
  theme_minimal() + 
  xlab("weeks before peak") + ylab("Mean daily speed per week (km/day)") + 
  theme(legend.title = element_text("Wave no."), 
        legend.position = "bottom")


###########  BY WAVE SECTIONS  ########### ########### ########### ########### 
allWave1 <- allWave1 %>% mutate(section = case_when(date < "2020-10-08" ~ "2020-09-17 -\n 2020-10-07", 
                                        date >= "2020-10-08" & date < "2020-10-29" ~ "2020-10-08 -\n 2020-10-28", 
                                        date >= "2020-10-29" & date <  "2020-11-19"~ "2020-10-29 -\n 2020-11-19",
                                        date >= "2020-11-19" ~  "2020-11-19"))

allWave2 <- allWave2 %>% mutate(section = case_when(date < "2021-07-24" ~ "2021-07-03 -\n 2021-07-23", 
                                                    date >= "2021-07-24" & date < "2021-08-14" ~ "2021-07-24 -\n 2021-08-13", 
                                                    date >= "2021-08-14" & date <  "2021-09-04"~ "2021-08-14 -\n 2021-09-03",
                                                    date >= "2021-09-04" ~ "2021-09-04"))
sectMean1 <- allWave1 %>% as.data.frame() %>%
  st_drop_geometry() %>%
  reframe(sectMean = mean(distToFront, na.rm=TRUE)/1000, .by=c("section"))

sectMedian1 <- allWave1 %>% as.data.frame() %>%
  st_drop_geometry() %>%
  reframe(sectMedian = median(distToFront, na.rm=TRUE)/1000, .by=c("section"))

sectIQR_low1 <- allWave1 %>% as.data.frame() %>%
  st_drop_geometry() %>%
  reframe(sectMedian = quantile(distToFront, na.rm=TRUE, probs=0.25)/1000, .by=c("section"))

sectIQR_high1 <- allWave1 %>% as.data.frame() %>%
  st_drop_geometry() %>%
  reframe(sectMedian = quantile(distToFront, na.rm=TRUE, probs=0.75)/1000, .by=c("section"))

sectMean2 <- allWave2 %>% as.data.frame() %>%
  st_drop_geometry() %>%
  reframe(sectMean = mean(distToFront, na.rm=TRUE)/1000, .by=c("section"))

sectMedian2 <- allWave2 %>% as.data.frame() %>%
  st_drop_geometry() %>%
  reframe(sectMedian = median(distToFront, na.rm=TRUE)/1000, .by=c("section"))

sectIQR_low2 <- allWave2 %>% as.data.frame() %>%
  st_drop_geometry() %>%
  reframe(sectMedian = quantile(distToFront, na.rm=TRUE, probs=0.25)/1000, .by=c("section"))

sectIQR_high2 <- allWave2 %>% as.data.frame() %>%
  st_drop_geometry() %>%
  reframe(sectMedian = quantile(distToFront, na.rm=TRUE, probs=0.75)/1000, .by=c("section"))


allWave <- rbind(allWave1, allWave2)

###########  BY WAVE SECTIONS  ########### ########### ########### ########### 
w1<-ggplot(allWave1, aes(x = distToFront/1000, y = section, group=section, fill = factor(..quantile..))) + 
  stat_density_ridges(quantiles = c(0.25,0.5,0.75)
                      , quantile_lines =TRUE
                      , geom="density_ridges_gradient") + 
  scale_fill_viridis(discrete = TRUE
                     , name = "Quantile"
                     , option = "magma") +
  theme_ridges() + 
  scale_x_continuous(trans=scales::pseudo_log_trans(base = 10)) +
  xlab("wavefront speed (km/day)") + ggtitle("Wave 1") + ylab("") 

w2<-ggplot(allWave2, aes(x = distToFront/1000, y = section, group=section, fill = factor(..quantile..))) + 
  stat_density_ridges(quantiles = c(0.25,0.5,0.75)
                      , quantile_lines =TRUE
                      , geom="density_ridges_gradient") + 
  scale_fill_viridis(discrete = TRUE
                     , name = "Quantile"
                     , option = "magma") +
  theme_ridges() + 
  scale_x_continuous(trans=scales::pseudo_log_trans(base = 10)) +
  
  xlab("wavefront speed (km/day)")  + ggtitle("Wave 2") + ylab("") + expand_limits(x=1500)


(w1+w2) + plot_layout(nrow=2,guides = "collect")

###############################################################################
### Max speed 
###############################################################################

firstWaveMax <- allWave1 %>% st_drop_geometry() %>% 
                as.data.frame() %>% reframe(maxSpeed=max(distToFront)/1000, .by=date) %>%
                select(maxSpeed) %>% unlist() %>% as.vector()
secondWaveMax <- allWave2 %>% st_drop_geometry() %>% 
                 as.data.frame() %>% reframe(maxSpeed=max(distToFront)/1000, .by=date) %>%
                 select(maxSpeed) %>% unlist() %>% as.vector()

firstWaveMaxMedian <- median(firstWaveMax)
secondWaveMaxMedian <- median(secondWaveMax)

### Filter the boundary based on the median max speed
secondWaveBoundLengthMax <- firstWaveBoundLengthMax <- rep(NA, length(distanceToFrontier_firstWave))
secondWaveBoundLength <- firstWaveBoundLength <- rep(NA, length(distanceToFrontier_firstWave))

for (i in 1:length(distanceToFrontier_firstWave)){
  hexesFirst <- distanceToFrontier_firstWave[[i]] %>% filter(distToFront >= firstWaveMaxMedian) %>% select(geometry)
  firstWaveBoundLengthMax[i] <- sum(st_length(st_filter(boundList_w1[[i]][["boundary"]] %>% 
                                                          select(hexid, date, infectionsPC, mean, geometry), hexesFirst)))/1000
  hexesSecond <- distanceToFrontier_secondWave[[i]] %>% filter(distToFront >= secondWaveMaxMedian) %>% select(geometry)
  secondWaveBoundLengthMax[i] <- sum(st_length(st_filter(boundList_w2[[i]][["boundary"]], hexesSecond)))/1000
}

allWave1char <- data.frame(wave1char, 
                           medianSpeed[1:63,"speed"], 
                           meanSpeed[1:63,"speed"], 
                           firstWaveBoundLengthMax)

allWave2char <- data.frame(wave2char, 
                           medianSpeed[64:126,"speed"], 
                           meanSpeed[64:126,"speed"], 
                           secondWaveBoundLengthMax)

colnames(allWave2char)[4:6] <- 
colnames(allWave1char)[4:6] <- c("median.speed", 
                                 "mean.speed", 
                                 "length greater than max")

write.csv(allWave1char, here("Data/data-products/wavefronts/wave1Characteristics.csv"))
write.csv(allWave2char, here("Data/data-products/wavefronts/wave2Characteristics.csv"))

# max(firstWaveBoundLengthMax); 63-which(firstWaveBoundLengthMax == max(firstWaveBoundLengthMax))
# max(secondWaveBoundLengthMax); 63-which(secondWaveBoundLengthMax == max(secondWaveBoundLengthMax))

### Min speed 
firstWaveMin <- allWave1 %>% st_drop_geometry() %>% 
  as.data.frame() %>% reframe(minSpeed=min(distToFront)/1000, .by=date) %>%
  select(minSpeed) %>% unlist() %>% as.vector()
secondWaveMin <- allWave2 %>% st_drop_geometry() %>% 
  as.data.frame() %>% reframe(minSpeed=max(distToFront)/1000, .by=date) %>%
  select(minSpeed) %>% unlist() %>% as.vector()
