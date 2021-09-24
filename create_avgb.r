# create_avgb
# modification of GBYT_empirical_approach_uncertainty
# to create input file for GBYT_limiter
# also use bootstraps from Miller et al. directly in calculating avgb

# set working directory to source file location to begin

library(dplyr)
library(tidyr)
library(ggplot2)

dat.file <- "survey_catch_per_tow.csv"
cv.file <- "survey_cv.csv"
const.file <- "survey_constants.csv"

chain2rockhopper.conv <- 0.31   # chain sweep to rockhopper sweep calibration

set.seed(4)                     # so can get repeatable results

#---------------------------------------------------------------------------
# get data for surveys biomass in kg/tow and total fishery catch in metric tons
raw.dat <- read.csv(dat.file)
names(raw.dat)

const.dat <- read.csv(const.file)
names(const.dat)

cv.dat <- read.csv(cv.file)
names(cv.dat)

# create data structures
survey.kg.tow <- raw.dat %>%
  gather(Survey, kg.tow, 2:4) %>%
  mutate(Survey = factor(Survey))

survey.cv <- gather(cv.dat, Survey, CV, 2:4)

const.dat <- mutate(const.dat, 
                    footprint.door = Door_Width * Tow_Length / 1000,
                    footprint.wing = Wing_Width * Tow_Length / 1000,
                    expand.door = Survey.Area / footprint.door,
                    expand.wing = Survey.Area / footprint.wing)

NMFS <- filter(const.dat, Survey == "NMFS")
Spring <- mutate(NMFS, Survey = "Spring")
Fall <- mutate(NMFS, Survey = "Fall")
const3 <- rbind(filter(const.dat, Survey =="DFO"), Spring, Fall) %>%
  mutate(Survey = factor(Survey))

#---------------------------------------------------------------------
# function to create estimates of biomass from catch per tow, area swept and total, and calibrations
# based on single values so can be computed many times easily
calc_B <- function(survey.kg.tow, areas, surveyq, chainsweep){
  res <- left_join(survey.kg.tow, areas, by="Survey") %>%
    mutate(msab = kg.tow * (Survey.Area / tow.area) / 1000) %>%
    mutate(biomass = msab / (surveyq * chainsweep)) %>%
    group_by(Year) %>%
    summarise(Avg = mean(biomass, na.rm = TRUE), .groups="keep")
 return(res)
}
  
#---------------------------------------------------------------------
# calculate point estimates
door.areas <- mutate(const3, tow.area = footprint.door) %>%
  select(Survey, Survey.Area, tow.area)
wing.areas <- mutate(const3, tow.area = footprint.wing) %>%
  select(Survey, Survey.Area, tow.area)

pt.ests.door <- calc_B(survey.kg.tow, door.areas, chain2rockhopper.conv, 1.0)
pt.ests.wing <- calc_B(survey.kg.tow, wing.areas, chain2rockhopper.conv, 1.0)

#---------------------------------------------------------------------
#---------------------------------------------------------------------
# create Monte Carlo data for the inputs
niter <- 1000

sd.vals <- sqrt(log(1 + survey.cv$CV^2))
n.sd <- length(sd.vals)
rand.kg.tow.mult <- matrix(NA, nrow=niter, ncol=n.sd)
for(i in 1:n.sd){
  rand.kg.tow.mult[,i] <- exp(rnorm(niter) * sd.vals[i])
}
apply(rand.kg.tow.mult, 2, mean)
summary(rand.kg.tow.mult)

chain2rockhopper.conv.cv <- 0.065
c2rsd <- sqrt(log(1 + chain2rockhopper.conv.cv^2))
c2r.conv.rand <- chain2rockhopper.conv * exp(rnorm(niter) * c2rsd)
summary(c2r.conv.rand)

survey.area.mult.min <- 0.95
survey.area.mult.max <- 1.0
tow.footprint.mult.min <- 1.0  # 1 means only wingspread
tow.footprint.mult.max <- 1.2  # 1.2 means 20% additional footprint due to herding
chainsweep.min <- 0.9
chainsweep.max <- 1.0

survey.area.mult.rand <- runif(niter, survey.area.mult.min, survey.area.mult.max)
tow.footprint.mult.rand <- runif(niter, tow.footprint.mult.min, tow.footprint.mult.max)
chainsweep.rand <- runif(niter, chainsweep.min, chainsweep.max)

# loop through Monte Carlo draw calculating pop B and catch advice
n.y <- length(raw.dat$Year)

#--------------------------------------------------
# function to compute average biomass with uncertainty
compute_average_biomass <- function(flag){
  average.biomass.res <- matrix(NA, nrow=niter, ncol=n.y)
  res.iter <- matrix(NA, nrow=niter, ncol=5)

  for (iter in 1:niter){
    survey.area.mult   <- ifelse(flag[2], 1.0, survey.area.mult.rand[iter])
    tow.footprint.mult <- ifelse(flag[3], 1.0,  tow.footprint.mult.rand[iter])
    c2r.conv.use       <- ifelse(flag[4], chain2rockhopper.conv, c2r.conv.rand[iter])
    chainsweep.use     <- ifelse(flag[5], 1.0, chainsweep.rand[iter])
    
    kg.tow.use <- survey.kg.tow
    if (flag[1] == FALSE){
      kg.tow.use <- mutate(survey.kg.tow,  kg.tow = kg.tow * rand.kg.tow.mult[iter,])
    }
    
    area.use <- mutate(wing.areas, 
                       Survey.Area = Survey.Area * survey.area.mult,
                       tow.area = tow.area * tow.footprint.mult)
    
    rand.B <- calc_B(kg.tow.use, area.use, c2r.conv.use, chainsweep.use)
    
    average.biomass.res[iter,] <- rand.B$Avg
    
    vals <- c(survey.area.mult,
              tow.footprint.mult,
              c2r.conv.use,
              chainsweep.use)
    res.iter[iter,] <- c(vals, average.biomass.res[iter,length(average.biomass.res[iter,])])
  }
  
  colnames(average.biomass.res) <- raw.dat$Year
  ab.res <- data.frame(average.biomass.res) 
  ab.res <- gather(ab.res, Year, AverageBiomass) %>%
    mutate(Year = substring(Year, 2, 5))
  
  colnames(res.iter) <- c("survey.area.mult",
                          "tow.footprint.mult",
                          "c2r.conv",
                          "chainsweep",
                          "catch.advice")
  my.res <- list("ab.res" = ab.res,
                 "res.iter" = res.iter)
  return(my.res)
}
#--------------------------------------------------

# flags are TRUE (base value) or FALSE (random)
flag <- rep(TRUE, 5)
flag[1] <- FALSE  # kg.tow.flag
# only dealing with kg.tow uncertainty, change to FALSE to include other factors
#flag[2] <- FALSE  # survey.area.flag 
#flag[3] <- FALSE  # tow.footprint.flag
#flag[4] <- FALSE  # chain2rockhopper.conv.flag
#flag[5] <- FALSE  # chainsweep.flag

avgb <- compute_average_biomass(flag)

myavgb <- data.frame(Year = avgb$ab.res$Year,
                     avgb = avgb$ab.res$AverageBiomass)

#write.csv(myavgb, file="avgb.csv", row.names = FALSE)

# now get DFO results from above and combine with Miller et al. bootstraps
dfo.kg.tow <- survey.kg.tow %>%
  filter(Survey == "DFO")
dfo.vals <- tibble(Year = integer(),
                   boot = integer(),
                   DFO = double())
for (iter in 1:1000){
  kg.tow.use <- mutate(dfo.kg.tow,  kg.tow = kg.tow * rand.kg.tow.mult[iter,1:11]) # first 11 columns are DFO, next 11 are Spring, last 11 are Fall
  res <- left_join(kg.tow.use, wing.areas, by="Survey") %>%
    mutate(msab = kg.tow * (Survey.Area / tow.area) / 1000) %>%
    mutate(biomass = msab / (chain2rockhopper.conv * 1.0)) 
  thisvals <- tibble(Year = dfo.kg.tow$Year,
                     boot = iter,
                     DFO = res$biomass)
  dfo.vals <- rbind(dfo.vals, thisvals)
}
dfo.vals
checkdfo <- dfo.vals %>%
  group_by(Year) %>%
  summarize(meanval = mean(DFO), sd = sd(DFO), medianval = median(DFO))
checkdfo

# get the Miller et al. bootstrap values, remember to shift Fall
miller <- read.csv("Miller_etal_boot_data.csv")
miller
spring.vals <- miller %>%
  rename(Year = year, Spring = spring) %>%
  select(Year, boot, Spring)
spring.vals
fall.vals <- miller %>%
  mutate(year = year + 1) %>% # here is the lagging for Fall
  rename(Year = year, Fall = fall) %>%
  select(Year, boot, Fall)
fall.vals

xx <- left_join(dfo.vals, spring.vals, by = c("Year", "boot")) %>%
  left_join(fall.vals, by = c("Year", "boot")) %>%
  rowwise() %>%
  mutate(avgb = mean(c(DFO, Spring, Fall), na.rm = TRUE)) %>%
  arrange(Year, boot)

# save file with both original and Miller avgb values
myavgb <- myavgb %>%
  mutate(Source = "original")
Milleravgb <- xx %>%
  mutate(Source = "Miller") %>%
  select(Year, avgb, Source)
bothavgb <- rbind(myavgb, Milleravgb)
write.csv(bothavgb, file="avgb.csv", row.names = FALSE)

# compare time series
ggplot(bothavgb, aes(x=factor(Year), y=avgb, fill=Source)) +
  geom_violin() +
  theme_bw()
ggplot(filter(bothavgb, as.integer(as.character(Year))>=2014), aes(x=factor(Year), y=avgb, fill=Source)) +
  geom_violin() +
  theme_bw()
