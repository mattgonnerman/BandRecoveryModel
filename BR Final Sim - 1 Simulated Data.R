###Code is based on BR Model - 5f Simulated Data - JtoA at Cap2

#Load Packages
require(dplyr)
require(sf)


###########################
### Data Specifications ###
###########################
# Set dimensions of study area and region boundaries, Study area = AxB square 
A = 100
B = 100

# C*D = Total # Regions 
C = 5
D = 5

#Capture Locations Locations
nbandsites <- 3*50 # Number of Banding Capture Sites, must be multiple of 3 due to high/medium/low sampling code
n.band.years <- sample(3:8,1) #banding seasons
nbandind <- n.band.years * 500 #Number of individuals banded, #Assumed 1:1 adult to juvenile

# Define Telemetry Data
ntelemsites <- 3*15
n.years.telem <- 3
n.occasions.wsr <- 52*n.years.telem # Maximum Exposure Length
ntelemind <- n.years.telem*sample(c(50,75,100), 1)    # Annual number of newly marked individuals
visit.rate <- .95 #Probability of finding a bird in a given week, assuming it hasn't been found dead yet

#Amount of spatial correlation in weekly survival
# Range = lower means more correlated over longer distances
wsr.sc <- .2 #weekly survival rate
psill.wsr <- .02 
#nuggest is half of psill so local variation is half of the total variation observed

# Harvest Rate Regression Coefficients
hr <- sample(seq(.10,.30, .01), 1)  #set weekly survival rate for simulation
hr.b0 <- log(hr/(1-hr))
hr.b.adult <- 1
hr.b.year <- rep(0, n.band.years)
for(i in 2:n.band.years){
  hr.b.year[i] <- rnorm(1, 0, .05)
}
hr.error <- .00

# Weekly Survival Rate Regression Coefficients
wsr <- 0.975 #set weekly survival rate for simulation
wsr.b0 <- log(wsr/(1-wsr))
wsr.b.adult <- .5
wsr.error <- 0.00

#Determines the distance between spatial knots. Number of knots will be A/nknot X A/nknot
nknot <- 10

# Set Parameters for simulating Region Specific Abundance over time
n.years.totharv <- 5 + 10 #Number of additional years of total harvest data (will be added on to banding years), 5 is burn in
hr.TH.year <- rep(0, n.years.totharv)
for(i in 1:n.years.totharv){
  hr.TH.year[i] <- rnorm(1, 0, .05)
}
max.N.J.1 <- 1000
max.N.A.1 <- 1000
min.N.J.1 <- 25
min.N.A.1 <- 25
mean.R <- 1.5 #average R around which WMD specific will be sampled
sd.R <- .1

#Used for to specify how to determine starting values
simrun <- "Y"

######################################################################
### Create Study Area/Regions/Capture Sites with Spatial Variation ###
######################################################################
# Create SF grid for study area regions
SA.points.df <- data.frame(x = rep(1:A, B),
                       y = rep(1:B, each = A))
SA.points.sf <- st_as_sf(SA.points.df, coords = c("x","y"))
SA.grid <- st_as_sf(st_make_grid(SA.points.sf, n = c(C,D))) %>%
  mutate(RegionID = 1:(C*D))

### Generate spatial variation SPP surfaces for survival and harvest rate
# Create gaussian random field for w(c)
require(gstat)
# Create field = study area size
Field = expand.grid(1:A, 1:B)
names(Field) = c('x','y')

### Define the spatial structure of the variation in harvest rate and survival 
# https://cran.r-project.org/web/packages/gstat/gstat.pdf
# http://santiago.begueria.es/2010/10/generating-spatially-correlated-random-fields-with-r/
# https://www.aspexit.com/en/simulating-spatial-datasets-with-known-spatial-variability/

## HARVEST RATE
# Set the parameters of the semi-variogram
Psill = psill.hr ## Partial sill = Magnitude of variation, value that variogram levels out at 
Range= hr.sc  ## Maximal distance of autocorrelation, where variogram levels out
Nugget= nugget.hr  ## Small-scale variations

# Set the semi-variogram model
Beta = 0   ## mean yield (tons/ha) in the field
HR_modelling=gstat(formula=z~1, ## We assume that there is a constant trend in the data
                   locations=~x+y,
                   dummy=T,    ## Logical value to set to True for unconditional simulation
                   beta=Beta,  ## Necessity to set the average value over the field
                   model=vgm(psill=Psill,
                             range=Range ,
                             nugget=Nugget,
                             model='Gau'), ## Spherical semi-variogram model
                   nmax=40) ## number of nearest observations used for each new prediction
# Simulate the spatial structure within the field
HR_gaussian_field <- predict(HR_modelling, newdata=Field, nsim=1) %>% ## nsim : Nombre de simulations
  rename(HR.SPP = sim1) %>%
  mutate(HR.base = exp(hr.b0+HR.SPP)/(1+exp(hr.b0+HR.SPP)))
HR_spvar.points <- st_as_sf(HR_gaussian_field, coords = c("x","y")) 
# #Visualize the spatial variation
# require(ggplot2)
# ggplot()+  ## Initialize the ggplot layer
#   geom_point(data=HR_gaussian_field,aes(x=x,y=y,col=HR.base))+ ## plot the observations as points with a colored yield gradient
#   scale_colour_gradient(low="red",high="green") +   ## Set the colors of the gradient
#   geom_sf(data = bandsiteselect) +
#   geom_sf(data = st_cast(SA.grid,"LINESTRING"))


## WEEKLY SURVIVAL RATE
# Set the parameters of the semi-variogram
Psill = psill.wsr ## Partial sill = Magnitude of variation, value that variogram levels out at 
Range = A*wsr.sc  ## Maximal distance of autocorrelation, where variogram levels out
Nugget = psill.wsr  ## Small-scale variations
# Set the semi-variogram model
Beta = 0   ## mean yield (tons/ha) in the field
WSR_modelling=gstat(formula=z~1, ## We assume that there is a constant trend in the data
                   locations=~x+y,
                   dummy=T,    ## Logical value to set to True for unconditional simulation
                   beta=Beta,  ## Necessity to set the average value over the field
                   model=vgm(psill=Psill,
                             range=Range ,
                             nugget=Nugget,
                             model='Gau'), ## Spherical semi-variogram model
                   nmax=40) ## number of nearest observations used for each new prediction
## Simulate the spatial structure within the field
WSR_gaussian_field <- predict(WSR_modelling, newdata=Field, nsim=1) %>% ## nsim : Nombre de simulations
  rename(WSR.SPP = sim1) %>%
  mutate(WSR.base = exp(wsr.b0+WSR.SPP)/(1+exp(wsr.b0+WSR.SPP)))
WSR_spvar.points <- st_as_sf(WSR_gaussian_field, coords = c("x","y")) 
# #Visualize the spatial variation
# require(ggplot2)
# ggplot()+  ## Initialize the ggplot layer
#   geom_point(data=WSR_gaussian_field,aes(x=x,y=y,col=WSR.base))+ ## plot the observations as points with a colored yield gradient
#   scale_colour_gradient(low="red",high="green")   ## Set the colors of the gradient


# Create Banding Sites
bandsiteselect <- st_as_sf(st_sample(SA.grid, nbandsites, by_polygon = T)) %>%
  mutate(SiteID = 1:nbandsites)

# bandsiteselect<- sample(1:nrow(SA.points.df), nbandsites, replace = F)
# BSites.points.df <- SA.points.df[bandsiteselect,]
# BSites.points.sf <- st_as_sf(BSites.points.df, coords = c("x","y")) 
# BSites.spvar.sf <- st_join(BSites.points.sf, HR_spvar.points, left = T)
BSites.spvar.sf <- st_join(bandsiteselect, HR_spvar.points, join = st_nearest_feature, left = T)
BSites.spvar.sf <- st_join(BSites.spvar.sf, WSR_spvar.points, join = st_nearest_feature, left = T)

# Create Telemetry Sites
telemsiteselect <- st_as_sf(st_sample(BSites.spvar.sf, ntelemsites))
telemsiteselect <- st_cast(telemsiteselect, "POINT")

# telemsiteselect<- sample(bandsiteselect, ntelemsites, replace = F)
# TSites.points.df <- SA.points.df[telemsiteselect,]
# TSites.points.sf <- st_as_sf(TSites.points.df, coords = c("x","y"))
# TSites.spvar.sf <- st_join(TSites.points.sf, BSites.spvar.sf, left = T)
TSites.spvar.sf <- st_join(telemsiteselect, BSites.spvar.sf, left = T)


#Simulate total harvest rates by summing N within each region and dividing by h with error
#Randomize N within each region
#Could use gaussian field?

#Generate spatial knots for model


###################################
### Simulate Band Recovery Data ###
###################################
#Define Individuals at Capture
#3 levels for number individuals caught at capture site
probcap <- c(rep((3*.15/nbandsites), nbandsites/3), #Low
             rep((3*.35/nbandsites), nbandsites/3), #Med
             rep((3*.50/nbandsites), nbandsites/3)) #High
#Randomly assign individuals to Age, Year, and Capture Site
hr.ind.cap <- data.frame( Ind.ID = 1:nbandind) %>%
  mutate(Adult = sample(0:1, nbandind, replace = T)) %>% 
  mutate(CapYear = sample(1:n.band.years, nbandind, replace = T)) %>%
  mutate(CapSite = c(sample(1:nbandsites, nbandind, replace = T, prob = probcap)))
#Retrieve associated spatial variation in HR
for(i in 1:nbandind){
  hr.ind.cap$HR.SPP[i] <- BSites.spvar.sf$HR.SPP[hr.ind.cap$CapSite[i]]
  hr.ind.cap$WSR.SPP[i] <- BSites.spvar.sf$WSR.SPP[hr.ind.cap$CapSite[i]]
}



# Calculate Indiviudal Harvest Rate and Survival
hr.ind.coef <- hr.ind.cap %>%
  mutate(WSR.Error = rnorm(nbandind, 0, wsr.error)) %>%
  mutate(HR.Error = rnorm(nbandind, 0, hr.error)) %>%
  mutate(WSRRegressionA = wsr.b0 + wsr.b.adult + WSR.SPP + WSR.Error) %>%
  mutate(WSRA = exp(WSRRegressionA)/(1+exp(WSRRegressionA))) %>%
  mutate(WSRRegressionJ = wsr.b0 + WSR.SPP + WSR.Error) %>%
  mutate(WSRJ = exp(WSRRegressionJ)/(1+exp(WSRRegressionJ))) %>%
  mutate(SA_C2H = WSRA^11) %>%
  mutate(SA_H2C = WSRA^36) %>%
  mutate(SJ_C2H = WSRJ^11) %>%
  mutate(SJ_H2C = WSRJ^36) %>%
  mutate(HRRegressionA = hr.b0 + hr.b.adult + HR.SPP + HR.Error) %>%
  mutate(HRRegressionJ = hr.b0 + HR.SPP + HR.Error)

br.year.df <- as.data.frame(matrix(hr.b.year, nrow = nbandind, ncol = n.band.years, byrow = T))
hr.ind.betas <- cbind(hr.ind.cap, br.year.df)
  
BR.br <- matrix(NA, ncol = n.band.years, nrow = nbandind, byrow = F)
BR.br.J <- matrix(NA, ncol = n.band.years, nrow = nbandind, byrow = F)
for(i in 1:n.band.years){
  BR.br[,i] <- exp(hr.ind.coef$HRRegressionA + hr.b.year[i])/(1+exp(hr.ind.coef$HRRegressionA + hr.b.year[i]))
  BR.br.J[,i] <- exp(hr.ind.coef$HRRegressionJ + hr.b.year[i])/(1+exp(hr.ind.coef$HRRegressionJ + hr.b.year[i]))
}

for(i in 1:nbandind){
  if(hr.ind.coef$Adult[i] == 0){
    for(j in 1:hr.ind.coef$CapYear[i]){
      BR.br[i,j] <- BR.br.J[i,j]
    }
  }
}


S_H2C.br <- matrix(hr.ind.coef$SA_H2C, ncol = n.band.years, nrow = nbandind, byrow = F)
### Comment out if all individuals are A after 1st hunting season
for(i in 1:nrow(S_H2C.br)){
  if(hr.ind.cap$Adult[i] == 0){
    S_H2C.br[i,1:hr.ind.cap$CapYear[i]] <- hr.ind.coef$SJ_H2C[i]
  }
}

S_C2H.br <- matrix(hr.ind.coef$SA_C2H, ncol = n.band.years, nrow = nbandind, byrow = F)
for(i in 1:nrow(S_C2H.br)){
  if(hr.ind.cap$Adult[i] == 0){
    S_C2H.br[i,1:hr.ind.cap$CapYear[i]] <- hr.ind.coef$SJ_C2H[i]
  }
}

yearofcap <- hr.ind.coef$CapYear

# Define function to simulate Dead-Recovery data
simul.br <- function(S_H2C.br, S_C2H.br, BR.br, nbandind, yearofcap){
  n.occasions <- 2*dim(S_H2C.br)[2]
  true.S.br <- matrix(NA, ncol = n.occasions, nrow = nbandind)
  EH <- matrix(0, ncol = n.occasions, nrow = nbandind)
  
  #Set initial live status
  for(i in 1:nbandind){
    true.S.br[i, (2*yearofcap[i])-1] <- 1    # record true live at the capture occasion
    EH[i, (2*yearofcap[i])-1] <- 1    # record live encounter at the capture occasion
    
    # Fill the EH matrix
    for (t in (yearofcap[i]*2):n.occasions){
      ts1 <- rbinom(1, 1, S_C2H.br[i,ceiling(t/2)]) #does bird survive from Capture season to harvest
      harvest <- rbinom(1,1, BR.br[i,ceiling(t/2)]) #is bird Harvested
      ts2 <- rbinom(1, 1, S_H2C.br[i,floor(t/2)]) #does bird survive from harvest to capture season
      
      if(t %% 2 == 0){ #If a hunting occassion
        if(true.S.br[i,t-1] == 0){ # Bird dead at beginning of previous last occasion
          true.S.br[i,t] == 0
          break
        }
        
        if(true.S.br[i,t-1] == 1){ #Bird Alive at beginning of previous occasion
          if(ts1 == 0){ #Bird dies before start of hunting season
            true.S.br[i,t] <- 0 #record death
            break
          }
          if(ts1 == 1){ #Bird survives from capture to start of hunting season
            if(harvest==1){ #Bird harvested
              true.S.br[i,t] <- 0 #record death
              EH[i,t] <- 1 # Record Harvest
              break
            }
            if(harvest == 0){ #Bird not harvested
              true.S.br[i,t] <- 1 #record survival
            }
          }
        }
      }
      
      if(t %% 2 != 0){ #if a capture occassion
        if(true.S.br[i,t-1] == 0){ # Bird dead at beginning of previous last occasion
          true.S.br[i,t] == 0
          break
        }
        
        if(true.S.br[i,t-1] == 1){ #Bird Alive at beginning of previous occasion
          if(ts2 == 0){ #Bird dies before capture season
            true.S.br[i,t] <- 0 #record death
            break
          }
          if(ts2 == 1){ #Bird survives to hunting seasons
            true.S.br[i,t] <- 1 #record survival
          }
        }
      }
    }
  }
  
  EH.list.br <- list(EH, true.S.br)
  
  return(EH.list.br)
  
}

# Execute function
EH_list_br <- simul.br(S_H2C.br, S_C2H.br, BR.br, nbandind, yearofcap)
EH_raw <- EH_list_br[[1]]

#Create vector with occasion of marking
get.first <- function(x) min(which(x!=0))
f <- apply(EH_raw, 1, get.first)

#Matrix of Ages for JAGS
#Because of how the model is setup, need a separate survival and harvest rate matrix
#Need to have a column for each week observed and have the Age change depending on when they were caught. 
br_adult_hr <- matrix(1, nrow = nbandind, ncol = 2*n.band.years)
for(i in 1:nrow(br_adult_hr)){
  if(hr.ind.cap$Adult[i] == 0){
    br_adult_hr[i,1:(2*hr.ind.cap$CapYear[i])] <- 0
  }
}

br_adult_s <- matrix(1, nrow = nbandind, ncol = 2*n.band.years)
for(i in 1:nrow(br_adult_s)){
  if(hr.ind.cap$Adult[i] == 0){
    # br_adult_s[i,1:((2*hr.ind.cap$CapYear[i])-1)] <- 0
    br_adult_s[i,1:((2*hr.ind.cap$CapYear[i]))] <- 0
  }
}

#Matrix for Year of Harvest for JAGS
br_year <- rep(1:n.band.years,each = 2)

#Define z (latent state = true survival before harvest)
#Define function to create a matrix with information about known latent state z
known.state.mr <- function(eh){
  state <- matrix(NA, nrow = dim(eh)[1], ncol = dim(eh)[2])
  rec <- which(rowSums(eh)==2)
  for(i in 1:length(rec)){
    n1 <- min(which(eh[rec[i],]==1))
    n2 <- max(which(eh[rec[i],]==1))
    state[rec[i], n1:n2] <- 1
    state[rec[i], n1] <- NA
    if(n2 < dim(eh)[2]){
      state[rec[i], (n2+1):dim(eh)[2]] <- 0
    }
    
  }
  return(state)
}

#Define function to create a matrix of INITIAL values for latent state z
mr.init.z <- function(eh){
  ch <- matrix(NA, nrow = dim(eh)[1], ncol = dim(eh)[2])
  rec <- which(rowSums(eh)==1)
  for(i in 1:length(rec)){
    n1 <- which(eh[rec[i],]==1)
    ch[rec[i], n1:dim(eh)[2]] <- 0
    ch[rec[i],n1] <- NA
  }
  return(ch)
}

#Assume all birds were caught same number of weeks before first hunting season. Can add some variation here eventually
weeks2harv <- rep(11, nbandind) 

#Create vector of which Region each turkey was captured in
telemsite.region <- st_drop_geometry(st_join(BSites.spvar.sf, SA.grid, join = st_within)) %>%
  rename(CapSite = SiteID)
br.ind.wmd <- merge(hr.ind.cap, telemsite.region, by = "CapSite", all.x = T) %>% arrange(Ind.ID)
br_wmd <- br.ind.wmd$RegionID

#Create matrix to designate Season (S2W)
br_s2w <- matrix(ncol = ncol(br_adult_hr), nrow = nrow(br_adult_hr))
br_w2s <- matrix(ncol = ncol(br_adult_hr), nrow = nrow(br_adult_hr))
odd <- seq(1, 2*n.band.years, 2)
even <- seq(2, 2*n.band.years, 2)
br_s2w[,odd] <- 0
br_s2w[,even] <- 1
br_w2s[,odd] <- 1
br_w2s[,even] <- 0

#vector to designate capture
odd <- seq(1, 2*n.band.years, 2)
br_season <- c()
for(t in 1:(2*n.band.years)){
  if(t %in% odd){br_season[t] <- 1}else{br_season[t] <- 0}
}



#######################################################################################################
#########################################
### Simulate Weekly Surival Rate Data ###
#########################################
# Define Individuals at capture
#3 levels for number individuals caught at capture site
probcap <- c(rep((3*.15/ntelemsites), ntelemsites/3), #Low
             rep((3*.35/ntelemsites), ntelemsites/3), #Med
             rep((3*.50/ntelemsites), ntelemsites/3)) #High

WSR.ind.cap <- data.frame(ID = 1:ntelemind) %>%
  # mutate(Sex = sample(0:1, ntelemind, replace = T, c(0.25, .75))) %>%
  mutate(Adult = sample(0:1, ntelemind, replace = T)) %>%
  mutate(CapYear = sample(1:n.years.telem, ntelemind, replace = T)) %>%
  mutate(CapSite = sample(TSites.spvar.sf$SiteID, ntelemind, replace = T, prob = probcap))


#Retrieve associated spatial variation in HR
for(i in 1:ntelemind){
  WSR.ind.cap$WSR.SPP[i] <- BSites.spvar.sf$WSR.SPP[WSR.ind.cap$CapSite[i]]
}

# Calculate WSR
WSR.ind.coef <- WSR.ind.cap %>%
  mutate(ProcessError = rnorm(ntelemind,0, wsr.error)) %>%
  mutate(RegressionJ = wsr.b0 + WSR.SPP + ProcessError) %>%
  mutate(RegressionA = wsr.b0 + wsr.b.adult + WSR.SPP +ProcessError) %>%
  # mutate(RegressionJ = wsr.b0 + wsr.b.f*Sex + WSR.SPP + ProcessError) %>%
  # mutate(RegressionA = wsr.b0 + wsr.b.f*Sex + wsr.b.adult + WSR.SPP +ProcessError) %>%
  mutate(WSRJ = exp(RegressionJ)/(1+exp(RegressionJ))) %>%
  mutate(WSRA = exp(RegressionA)/(1+exp(RegressionA)))

# Define matrices with survival and location probabilities
# Visit.wsr <- matrix(visit.rate, ncol = n.occasions.wsr, nrow = ntelemind)
WSR.wsr <- matrix(WSR.ind.coef$WSRA, ncol = n.occasions.wsr, nrow = ntelemind, byrow = F)
#Juveniles transition to Adults
for(i in 1:ntelemind) {
  if(WSR.ind.cap$Adult[i] == 0){
    WSR.wsr[i,1:52] <- WSR.ind.coef$WSRJ[i]
  }
}

# # This code is to deal with the periods that should be during the hunting season. 
# # For now just have them be the same as the other
# WSR.wsr[,rep(12:16, n.years.telem) + rep((52*(0:(n.years.telem-1))), each = 5)] <- 1

# Define function to simulate mark-recovery data
simul.wsr <- function(WSR.wsr, Visit.wsr, ntelemind){
  n.occasions <- 1+dim(WSR.wsr)[2]
  true.S <- matrix(NA, ncol = n.occasions, nrow = ntelemind)
  EH <- matrix(NA, ncol = n.occasions, nrow = ntelemind)
  
  #Set initial live status
  true.S[, 1] <- 1    # record true live at the capture occasion
  EH[, 1] <- 1    # record live encounter at the capture occasion
  
  # Fill the EH matrix
  for (i in 1:ntelemind){
    for (t in 2:n.occasions){
      ts <- rbinom(1, 1, WSR.wsr[i,t-1]) #does bird survive from last occasion
      find <- rbinom(1,1, visit.rate) #is bird found this occasion
      
      if(true.S[i,t-1] == 1){ #Was bird alive at last occasion
        if(ts == 1){ #Bird Survived to current occasion
          true.S[i,t] <- 1
          if(find == 1){ #Bird is Found
            EH[i,t] <- 1
          }else{ #Bird is not Found
            EH[i,t] <- NA
          }
        }else{ #Bird dies between t-1 and t
          true.S[i,t] <- 0
          if(find == 1){ #Bird is Found
            EH[i,t] <- 0
            break
          }else{ #Bird is not Found
            EH[i,t] <- NA
          }
        }
      }else{ #Bird dead at previous occasion, not found yet
        true.S[i,t] <- 0
        if(find == 1){#Bird not found in previous occasion
          EH[i,t] <- 0
          break
        }else{
          EH[i,t] <- NA
        }
      }
    }
  }
  
  EH.list.wsr <- list(EH, true.S)
  
  return(EH.list.wsr)
  
}

# Execute function
EH.wsr.list <- simul.wsr(WSR.wsr, Visit.wsr, ntelemind)
EH.wsr.1 <- EH.wsr.list[[1]]
EH.wsr <- EH.wsr.1[,-c(1)]

#Age Matrix for JAGS
#Need to have a column for each week observed and have the Age change depending on when they were caught. 
WSR.adult1 <- matrix(1, nrow = ntelemind, ncol = n.occasions.wsr+1)
WSR.adult1[,1] <- WSR.ind.cap$Adult
for(i in 1:nrow(WSR.adult1)){
  if(WSR.adult1[i,1] != 1){
    WSR.adult1[i,1:53] <- 0
  }
}
WSR.adult <- WSR.adult1
WSR.adult[is.na(EH.wsr.1)] <- NA
WSR.adult <- WSR.adult[,-c(1)]
wsr_adult1 <- as.vector(t(WSR.adult))
wsr_adult <- wsr_adult1[!is.na(wsr_adult1)]

# wsr_adult <- ifelse(wsr_adult == 1, 0, 1) #This is because you stupidly coded the simulation so 1 is JUV but in the model 1 is Adult


# #Sex Vector for JAGS
# WSR.sex1 <- matrix(NA, nrow = ntelemind, ncol = n.occasions.wsr+1)
# for(i in 1:nrow(WSR.sex1)){
#   WSR.sex1[i,] <- WSR.ind.cap$Sex[i]
# }
# WSR.sex <- WSR.sex1
# WSR.sex[is.na(EH.wsr.1)] <- NA
# WSR.sex <- WSR.sex[,-c(1)]
# wsr_sex1 <- as.vector(t(WSR.sex))
# wsr_sex <- wsr_sex1[!is.na(wsr_sex1)]


#Create vector of which Region each turkey was captured in
telemsite.region <- st_drop_geometry(st_join(BSites.spvar.sf, SA.grid, join = st_within)) %>%
  rename(CapSite = SiteID)
wsr.ind.wmd <- merge(telemsite.region, WSR.ind.cap, by = "CapSite", all.y = T) %>% arrange(ID)
WSR.wmd <- matrix(NA, nrow = ntelemind, ncol = n.occasions.wsr+1)
for(i in 1:nrow(WSR.wmd)){
  WSR.wmd[i,] <- wsr.ind.wmd$RegionID[i]
}
WSR.wmd[is.na(EH.wsr.1)] <- NA
WSR.wmd <- WSR.wmd[,-c(1)]
wsr_wmd1 <- as.vector(t(WSR.wmd))
wsr_wmd <- wsr_wmd1[!is.na(wsr_wmd1)]


#Vectorize encounter histories
ntelemind <- nrow(EH.wsr)
exposure.wsr <- ncol(EH.wsr)
succ1.wsr <- as.vector(t(EH.wsr))
succ <- succ1.wsr[!is.na(succ1.wsr)]
ID.wsr <- matrix(1:ntelemind, nrow = ntelemind, ncol = exposure.wsr)
ID.wsr <- as.vector(t(ID.wsr))
ID.wsr <- ID.wsr[!is.na(succ1.wsr)]      # ID marker

#Interval length between known fates
visit_ind <- matrix(NA, ncol = ncol(EH.wsr.1), nrow = nrow(EH.wsr.1))
get.last <- function(x) max(which(!is.na(x)))

for(i in 1:ntelemind){
  for(j in 2:(exposure.wsr+1)){
    if(!is.na(EH.wsr.1[i,j])){
      visit_ind[i,j] <- j - get.last(EH.wsr.1[i,1:(j-1)])
    }
  }
}

# Interval length between visits
interval.wsr <- as.vector(t(visit_ind))
interval <- interval.wsr[!is.na(interval.wsr)]

#Vector designating which season a visit was in, S2W (1) vs W2S (0)
time.cov <- EH.wsr
W2Sweeks <- rep(1:11, n.years.telem) + rep((52*(0:(n.years.telem-1))), each = 11)
harvestweeks <- rep(12:16, n.years.telem) + rep((52*(0:(n.years.telem-1))), each = 5)
for(i in 1:ncol(time.cov)){
  time.cov[,i] <- ifelse(i %in% W2Sweeks, 1, 
                         ifelse(i %in% harvestweeks, 0, 2))
}
time.cov[is.na(EH.wsr)] <- NA
wsr_time1 <- as.vector(t(time.cov))
wsr_time <- wsr_time1[!is.na(wsr_time1)]

wsr_S2W <- ifelse(wsr_time == 2, 1, 0)
wsr_W2S <- ifelse(wsr_time == 1, 1, 0)


#######################################################
### Create Spatial Knots and Prepare Inputs for SPP ###
#######################################################
require(raster)
#Load WMD Boundaried and Capture locations, convert to sf objects
cap_sf <- BSites.spvar.sf
wmd_sf <- SA.grid
wmd_centroid <- st_centroid(st_geometry(wmd_sf))

#Make initial grid and then subset to only those within WMDs
spatialknots1 <- st_make_grid(wmd_sf, square = T, what = "centers", n = A/nknot) #create a grid
spatialknots2 <- c(spatialknots1, wmd_centroid)
spatialknots3 <- st_join(st_sf(spatialknots2), st_sf(wmd_sf), join = st_within)
knotscoords.df <- st_coordinates(spatialknots3) #extract coordinates
spatialknots <- cbind(knotscoords.df, spatialknots3) %>% #bind with coordinates
  filter(!is.na(RegionID)) #remove knots not in a wmd
spatialknots$Knot_ID <- 1:length(spatialknots$X) #Add a reference ID

#To double check
# require(ggplot2)
# ggplot(wmd_sf) +
#   geom_sf() +
#   geom_sf(data = spatialknots) +
#   geom_sf_label(data = spatialknots, aes(label = Knot_ID))
# ggplot(wmd_sf) +
#   geom_sf() +
#   geom_sf_label(aes(label = IDENTIFIER))

knots.xy <- st_coordinates(spatialknots); names(knots.xy) <- c("x","y")
cap.xy <- st_coordinates(cap_sf)

# knot to site distances (in meters)
KnotLocalDis.mat <- proxy::dist(x=cap.xy, 
                                y=knots.xy, method = "euclidean") *1000
# knot to knot distances (in meters)
KnotDis.mat <- proxy::dist(x=knots.xy, y=knots.xy, method = "euclidean")*1000

#vector of capture site ID numbers for the predictive process to reference in the regression
ind.cap.site <- hr.ind.coef$CapSite

#Need a vector saying which WMD a knot is in
WhichWMD.df <- as.data.frame(spatialknots) %>%
  dplyr::select(Knot_ID, WMD_IN = RegionID) %>%
  group_by(WMD_IN) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(values_from = Knot_ID, names_from = row, names_prefix = 'Knot')
WMD.matrix <- as.matrix(WhichWMD.df[2:ncol(WhichWMD.df)])

CountWMD.df <- as.data.frame(spatialknots) %>%
  dplyr::select(Knot_ID, WMD_IN = RegionID) %>%
  group_by(WMD_IN) %>%
  summarize(Total = n()) %>%
  arrange(match(WMD_IN, WhichWMD.df$WMD_IN))
WMD.vec <- CountWMD.df$Total
WMD.id <- WhichWMD.df$WMD_IN

#Vector of all Regions that are sampled
sampledwmd <- sort(unique(c(wsr_wmd, br_wmd)))

##########################################
### Simulate Total Harvest Information ###
##########################################
#Get Region specific averages of season specific survival and harvest rate
#Total number of years of Total harvest data
n.years.TH <- n.band.years + n.years.totharv
#Year specific beta for harvest rate 
hr.TH <- c(hr.TH.year, hr.b.year)

Region_Mean_HR <- st_drop_geometry(st_join(HR_spvar.points, SA.grid, st_intersects, left = T)) %>%
  group_by(RegionID) %>%
  summarize(HR.SPP = mean(HR.SPP)) %>%
  mutate(REG.HR.J.noyear = hr.b0 + HR.SPP) %>%
  mutate(REG.HR.A.noyear = hr.b0 + hr.b.adult + HR.SPP)
SS.HR.A <- matrix(NA, ncol = n.years.TH, nrow = nrow(Region_Mean_HR), byrow = F)
SS.HR.J <- matrix(NA, ncol = n.years.TH, nrow = nrow(Region_Mean_HR) , byrow = F)



for(i in 1:nrow(Region_Mean_HR)){
  for(j in 1:n.years.TH){
    SS.HR.A[i,j] <- exp(Region_Mean_HR$REG.HR.A.noyear[i] + hr.TH[j])/(1+exp(Region_Mean_HR$REG.HR.A.noyear[i] + hr.TH[j]))
    SS.HR.J[i,j] <- exp(Region_Mean_HR$REG.HR.J.noyear[i] + hr.TH[j])/(1+exp(Region_Mean_HR$REG.HR.J.noyear[i] + hr.TH[j]))
  }
}

# Region_Mean_HR$Mean.HR.A <- rowMeans(SS.HR.A)
# Region_Mean_HR$Mean.HR.J <- rowMeans(SS.HR.J)

Region_Mean_WSR <- st_drop_geometry(st_join(WSR_spvar.points, SA.grid, st_intersects, left = T)) %>%
  group_by(RegionID) %>%
  summarize(WSR.SPP = mean(WSR.SPP)) %>%
  mutate(REG.WSR.J = wsr.b0 + WSR.SPP) %>%
  mutate(REG.WSR.A = wsr.b0 + wsr.b.adult + WSR.SPP) %>%
  mutate(Mean.WSR.J = exp(REG.WSR.J)/(1+exp(REG.WSR.J))) %>%
  mutate(Mean.WSR.A = exp(REG.WSR.A)/(1+exp(REG.WSR.A))) %>%
  mutate(NonHarv.S.A = (Mean.WSR.A^47)) %>%
  mutate(NonHarv.S.J = (Mean.WSR.J^36)*(Mean.WSR.A^11))

# Region_Means <- merge(Region_Mean_HR, Region_Mean_WSR, by = "RegionID") %>%
#   mutate(Annual.S.A = (1-Mean.HR.A)*(Mean.WSR.A^47)) %>%
#   mutate(Annual.S.J = (1-Mean.HR.J)*(Mean.WSR.A^47))

#Create Total Harvest Matrices
N.A <- matrix(NA, nrow = C*D, ncol = n.years.TH)
N.J <- matrix(NA, nrow = C*D, ncol = n.years.TH)
totharv.A <- matrix(NA, nrow = C*D, ncol = n.years.TH)
totharv.J <- matrix(NA, nrow = C*D, ncol = n.years.TH)
N.A[,1] <- sample(min.N.A.1:max.N.A.1, C*D, replace = T)
N.J[,1] <- sample(min.N.J.1:max.N.J.1, C*D, replace = T)


r.vector <- rnorm(n.years.TH-1, mean.R, sd.R)
K <- as.data.frame(matrix(NA, nrow = C*D, ncol = n.years.TH-1))
for(i in 1:(C*D)){
  totharv.A[i,1] <- rbinom(1, N.A[i,1], SS.HR.A[i,1])
  totharv.J[i,1] <- rbinom(1, N.J[i,1], SS.HR.J[i,1])
  
  for(j in 2:n.years.TH){
    N.A[i,j] <- rbinom(1, (N.A[i,j-1] - totharv.A[i,j-1]), Region_Mean_WSR$NonHarv.S.A[i]) + 
      rbinom(1, (N.J[i,j-1] - totharv.J[i,j-1]), Region_Mean_WSR$NonHarv.S.J[i])
    K[i,j-1] <- exp((1-(N.A[i,j-1]/5000)))
    N.J[i,j] <- rpois(1,r.vector[j-1]*K[i, j-1]*N.A[i,j-1])
    
    totharv.A[i,j] <- rbinom(1, N.A[i,j], SS.HR.A[i,j])
    totharv.J[i,j] <- rbinom(1, N.J[i,j], SS.HR.J[i,j])
  }
}

totharv.A <- totharv.A[,6:ncol(totharv.A)]
totharv.J <- totharv.J[,6:ncol(totharv.J)]

#Vector for which years in the total harvest data we estimated HR from the BR model
yearsHRavail <- (n.years.totharv+1):n.years.TH
HRnotavail <- 1:n.years.totharv

yearsHRavail-max(HRnotavail)

# require(ggplot2)
# require(tidyr)
# N.A.long <- as.data.frame(N.A) %>%
#   mutate(WMD = as.factor(1:25)) %>%
#   pivot_longer(cols = 1:(ncol(N.A)-1)) %>%
#   mutate(Year = as.numeric(sub(".", "", name))) %>%
#   mutate(WMD = as.factor(WMD))
# ggplot(data = N.A.long, aes(x =Year, y = value, group = WMD)) +
#   geom_line(aes(color = WMD), size = 1.3)
# # ggsave("N.A.example.jpeg", width = 12, height = 8, units = "in")
# N.J.long <- as.data.frame(N.J) %>%
#   mutate(WMD = as.factor(1:25)) %>%
#   pivot_longer(cols = 1:(ncol(N.J)-1)) %>%
#   mutate(Year = as.numeric(sub(".", "", name))) %>%
#   mutate(WMD = as.factor(WMD))
# ggplot(data = N.J.long, aes(x =Year, y = value, group = WMD)) +
#   geom_line(aes(color = WMD), size = 1.3)
# # ggsave("N.J.example.jpeg", width = 12, height = 8, units = "in")