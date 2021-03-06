require(magrittr)
require(coda)
require(R2jags)
require(dplyr)
require(reshape2)
require(lubridate)
require(sf)
require(rgdal)

#Set Working Directory
# setwd(choose.dir())


trap.raw <- read.csv("Trapping - Data.csv")
harvest.raw <- read.csv("Harvests - Harvested Birds.csv")
telem.raw <- read.csv("Telemetry_Data - Telemetry.csv")

######################################################
### Create Adult Weekly Survival Encounter History ###
######################################################
### 1) BOTH SEXES
#Simplify databases for construction of EH
#Simplify telemetry database to just BirdID, date of "visit" and Fate at "visit"
telemetry_eh <- telem.raw %>% dplyr::select(BirdID = AlumBand, Date, Fate) %>% 
  mutate(BirdID = as.character(BirdID)) %>% filter(!is.na(Fate))


#Simplify trap database to just BirdID, date of capture, add Fate(L) for that date
#filter to remove birds without transmitters
trap_eh <- trap.raw %>% dplyr::select(BirdID = AlumBand, Date) %>% mutate(BirdID = as.character(BirdID)) %>%
  mutate(Fate = "L") %>% filter(BirdID %in% unique(telemetry_eh$BirdID))

#Combine two databases into one so all visits are together
visits_eh <- rbind(telemetry_eh, trap_eh)

#Convert "Date" from MM/DD/YY to a standardized Julian Date across years
#Want to standarize date into a numeric value across years.
#JulDate will represent the total number of days since Jan1 2018
visits_eh$Date <- as.POSIXlt(visits_eh$Date, format="%m/%d/%Y")
for (i in 1:length(visits_eh$Date)){
  if(visits_eh$Date$year[i] >120){
    visits_eh$JulDate[i] <- visits_eh$Date$yday[i] + (((visits_eh$Date$year[i] - 118)*365)+1)
  }else{visits_eh$JulDate[i] <- visits_eh$Date$yday[i] + ((visits_eh$Date$year[i] - 118)*365)
  }
}
visits_eh$Date <- as.POSIXct(visits_eh$Date, format="%m/%d/%Y")

####Worth checking to make sure no NA, giant numbers, or negatives.
summary(visits_eh)

#Create a column that says what month (as a number) the visit was in, will be helpful for later
visits_eh$Month <- format(visits_eh$Date, "%m")

#Create a Week column that is based on the first week of the first trapping event and populate it
#A loop to populate the "Week" column with appropriate values based on "Date"
visits_eh$Week_refJan12018 <- NA
week1 <- 1
day1 <- 29 #Manually chosen, this is the julian date for the first day of the week of the first trapping event
for (i in 1:length(visits_eh$Week_refJan12018)){
  repeat{
    if(is.na(visits_eh$Week_refJan12018[i])){
      if(visits_eh$JulDate[i] >= day1 & visits_eh$JulDate[i] <= (day1 + 6)){
        visits_eh$Week_refJan12018[i] <- week1
        week1 <- 1
        day1 <- 29
      } else {
        week1 <- (week1 + 1)
        day1 <- (day1 + 7)
      }
    }else{
      break
    }
  }
}

#Create a Week column that is standardized to first week for each bird
for(i in 1:length(visits_eh$Week_refJan12018)){
  visits_eh$Week[i] <- 1+visits_eh$Week_refJan12018[i]-min(visits_eh$Week_refJan12018[visits_eh$BirdID == visits_eh$BirdID[i]])
} 

#Check for non standard entries for Fate and fix
unique(visits_eh$Fate)

visits_eh$Fate[visits_eh$Fate == "l"] <- "L"
visits_eh$Fate[visits_eh$Fate == "d"] <- "D"
visits_eh$Fate[visits_eh$Fate == ""] <- NA
visits_eh$Fate[visits_eh$Fate == " "] <- NA
visits_eh$Fate[visits_eh$Fate == "MIA"] <- NA
visits_eh$Fate[visits_eh$Fate == "?"] <- NA

visits_eh <- visits_eh %>% arrange(BirdID, Date)

#Need to make sure you only have one D for each bird. 
#This returns the number of D for each bird in database
#Mannually changing database to assess each situation
#Usually left first D as the known D and moved the subsequent Fates to Notes
checkD <- visits_eh %>% 
  filter(Fate == "D") %>% 
  group_by(BirdID) %>% 
  dplyr::summarize(Total = n()) %>% 
  filter( Total > 1)
# View(checkD)

#Check for occassions where there is a death recorded but bird was later alive
#Check and change manually in database
deathdate <- visits_eh %>%
  filter(Fate == "D") %>%
  dplyr::select(BirdID, DeathDate = Date)
alivedate <- visits_eh %>%
  filter(Fate == "L") %>%
  group_by(BirdID) %>%
  top_n(1, Date) %>%
  ungroup() %>%
  dplyr::select(BirdID, LastAlive = Date)
comparedates <- merge(deathdate, alivedate, all.x = T, by = "BirdID") %>%
  filter(LastAlive > DeathDate)
comparedates

#remove any NA values for fate, then Remove extra visits within a week
visits_eh_filter <- visits_eh %>% arrange(BirdID, Week, Date) %>% filter(!is.na(Fate)) %>% 
  mutate(Extra = ifelse(BirdID == lead(BirdID), ifelse(Week != lead(Week), 0, 1), 0)) %>% #Create a column to id rows with same BirdID and Week and Next row
  filter(Extra == 0) %>% dplyr::select(-Extra) %>% #Remove Duplicates then remove the extra column
  mutate(Fate = ifelse(Fate == "L", 1, 0)) #L and D to 1 and 0

#Remove birds that died in first two weeks
for(i in 1:length(visits_eh_filter$BirdID)){
  if(max(visits_eh_filter$Week[visits_eh_filter$BirdID == visits_eh_filter$BirdID[i]])-min(visits_eh_filter$Week[visits_eh_filter$BirdID == visits_eh_filter$BirdID[i]]) > 2){
    visits_eh_filter$Cens[i] <- 0
  }else{
    visits_eh_filter$Cens[i] <- 1
  }
}

visits_eh_cens <- visits_eh_filter %>% filter(Cens == 0)

#Transform into EH format
weeklysurvival_eh <- visits_eh_cens %>% dcast(BirdID ~ Week, value.var = "Fate")

#For harvested birds, truncate encounter history (Change day of harvest from 0 to NA)
#Commented code is to change the week before harvest to a 1 since we know its alive, not working for some reason and not super important to invest time in solving right now
harvestedradios <- unique(harvest.raw$Alum.Band.ID)
weeklysurvival_eh_harvest <- weeklysurvival_eh
for(i in 1:nrow(weeklysurvival_eh_harvest)){
  if(weeklysurvival_eh_harvest$BirdID[i] %in% harvestedradios){
    weeklysurvival_eh_harvest[i,which(weeklysurvival_eh_harvest[i,] == 0)] <- NA
    # if(is.na(weeklysurvival_eh_harvest[i,(which(weeklysurvival_eh_harvest[i,] == 0)-1)])){
    #   weeklysurvival_eh_harvest[i,(which(weeklysurvival_eh_harvest[i,] == 0)-1)] <- 1
    # }
  }
}


#Final EH format
weeklysurvival_eh_nocap <- weeklysurvival_eh_harvest[,-c(1)]
eh_matrix <- data.matrix(weeklysurvival_eh_nocap)

### Vectorize encounter histories ###
# Translate visitation history to exposure length history
eh_matrix_edit <- eh_matrix[,-c(1)]
n.ind <- nrow(eh_matrix)
exposure <- ncol(eh_matrix)-1

#Vectorize encounter histories
succ1 <- as.vector(t(eh_matrix_edit))
succ <- succ1[!is.na(succ1)]
ID <- matrix(1:n.ind, nrow = n.ind, ncol = exposure)
ID <- as.vector(t(ID))
ID <- ID[!is.na(succ1)]      # ID marker

# Interval length between visits
visit_ind <- matrix(NA, ncol = ncol(eh_matrix), nrow = nrow(eh_matrix))
get.last <- function(x) max(which(!is.na(x)))

for(i in 1:n.ind){
  for(j in 2:(exposure+1)){
    if(is.na(eh_matrix[i,j]) == FALSE){
      visit_ind[i,j] <- j - get.last(eh_matrix[i,1:(j-1)])
    }
  }
}

interval <- as.vector(t(visit_ind))
interval <- interval[!is.na(interval)]


##############################################
### Create Band Recovery Encounter History ###
##############################################
#Create a df with each bird's trap information
#additional columns for Kelsey_Region have been added
first.encounter <- trap.raw %>% dplyr::select(BirdID = AlumBand, AltID1 = Rivet.Band, AltID2 = Pat.Tag, Date, Sex, WMD, Region, Recapture, Age)  %>%
  filter(Recapture == "N") %>%
  mutate(Kelsey_Region = ifelse(WMD %in% c(15,16,17,20:26), "High", ifelse(WMD %in% c(6,12,13,14,18,19,27,28), "Med", "Low"))) %>%
  mutate(Kelsey_Region_factor = ifelse(Kelsey_Region == "High", 1 , ifelse(Kelsey_Region == "Med", 2, 1))) %>%
  filter(!is.na(BirdID)) %>%
  mutate(BirdID = as.character(BirdID)) %>%
  mutate(AltID1 = as.character(AltID1)) %>%
  mutate(AltID2 = as.character(AltID2)) %>%
  mutate(Date = as.Date(Date, '%m/%d/%Y')) %>%
  mutate(Year = format(Date, "%Y")) %>%
  mutate(Season = "Winter") %>%
  mutate(Column = paste(Year, Season, sep = "_")) %>%
  mutate(BirdID = ifelse(is.na(BirdID), AltID, BirdID)) %>%
  arrange(BirdID, Date) %>%
  distinct(BirdID, .keep_all = T) %>%
  filter(Sex=="M") %>% #Just males
  mutate(BirdID = as.numeric(BirdID))

#Create
dead.recovery <- harvest.raw %>% dplyr::select(BirdID = Alum.Band.ID, AltID1 = Rivet.ID, AltID2 = Pat.Tag.ID, Season, Year, Sex) %>%
  mutate(BirdID = as.character(BirdID)) %>%
  mutate(AltID1 = as.character(AltID1)) %>%
  mutate(AltID2 = as.character(AltID2)) %>%
  mutate(Column = paste(Year, Season, sep = "_")) %>%
  filter(!is.na(BirdID)) %>%
  filter(Sex=="Male") %>% #Just males
  filter(Season!="Fall") #Only spring harvests

missingbands <- dead.recovery %>% filter(BirdID == "Missing")
missingbands

for(i in 1:length(dead.recovery$BirdID)){
  if(dead.recovery$BirdID[i] == "Missing" | is.na(dead.recovery$BirdID[i])){
    if(is.na(dead.recovery$AltID1[i]) | dead.recovery$AltID1[i] == "Missing"){
      dead.recovery$BirdID[i] <- first.encounter$BirdID[first.encounter$AltID2 == dead.recovery$AltID2[i]]
    }else{
      dead.recovery$BirdID[i] <- first.encounter$BirdID[first.encounter$AltID1 == dead.recovery$AltID1[i]]
    }
  }
}

dead.recovery.eh <- dead.recovery %>% mutate(BirdID = as.numeric(BirdID)) %>%
  dplyr::select(BirdID, DeathSeason = Column)
first.encounter.eh1 <- first.encounter %>%
  dplyr::select(BirdID, CapSeason = Column, Age)
first.encounter.eh <- first.encounter.eh1 %>% dplyr::select(-Age)

combo.br <- merge(first.encounter.eh, dead.recovery.eh, all.x = T, by = "BirdID")

EH_raw <- data.frame(matrix(0, ncol = (2*(1+max(as.numeric(dead.recovery$Year)) - min(as.numeric(first.encounter$Year)))), nrow = length(first.encounter$BirdID)))
colnames(EH_raw) <- paste(rep(min(first.encounter$Year):max(dead.recovery$Year), each = 2), c("Winter", "Spring"), sep = "_" )
rownames(EH_raw) <- combo.br$BirdID

for(i in 1:length(combo.br$BirdID)){
  row_ = as.character(combo.br$BirdID[i])
  col_ = combo.br$CapSeason[i]
  EH_raw[row_, col_] <- 1
}

for(i in 1:length(combo.br$BirdID)){
  if(!is.na(combo.br$DeathSeason[i])){
    row_ = as.character(combo.br$BirdID[i])
    col_ = combo.br$DeathSeason[i]
    EH_raw[row_, col_] <- 1
  }
}

#Create vector with occasion of marking
get.first <- function(x) min(which(x!=0))
f <- apply(EH_raw, 1, get.first)

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

##########################################
### Prepare Additional Inputs for JAGS ###
##########################################
#How to do WMD/Region/Whatever - categorical covariates in JAGS
#http://mikemeredith.net/blog/2017/Categories_in_JAGS.htm

###WSR###
#Sex
trap.cov <- trap.raw %>% dplyr::select(BirdID = AlumBand, Sex, Recapture) %>%
  filter(Recapture == "N") %>% dplyr::select(-Recapture)
sex.cov <- merge(weeklysurvival_eh, trap.cov, by = "BirdID", all.x = T) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate(Sex = ifelse(Sex == "F", 1, 0))

for(i in 1:nrow(sex.cov)){
  for(j in 2:(ncol(sex.cov)-1)){
    if(!is.na(sex.cov[i,j])){
      sex.cov[i,j] <- sex.cov[i,ncol(sex.cov)]
    }
  }
}
sex.cov <- sex.cov %>% dplyr::select(-BirdID, -'1', -Sex)
wsr_sex1 <- as.vector(t(sex.cov))
wsr_sex <- wsr_sex1[!is.na(wsr_sex1)]


#WMD
trap.cov <- trap.raw %>% dplyr::select(BirdID = AlumBand, WMD, Recapture) %>%
  filter(Recapture == "N") %>% dplyr::select(-Recapture)
wmd.cov <- merge(weeklysurvival_eh, trap.cov, by = "BirdID", all.x = T)
for(i in 1:nrow(wmd.cov)){
  for(j in 2:(ncol(wmd.cov)-1)){
    if(!is.na(wmd.cov[i,j])){
      wmd.cov[i,j] <- wmd.cov[i,ncol(wmd.cov)]
    }
  }
}
wmd.cov <- wmd.cov %>% dplyr::select(-BirdID, -'1', -WMD)
wsr_wmd1 <- as.vector(t(wmd.cov))
wsr_wmd <- wsr_wmd1[!is.na(wsr_wmd1)]

#To create month, steal from the Adult Survival Code, should be mostly transferable
#Transform into EH format
#month.cov <- visits_eh_cens %>% dcast(BirdID ~ Week, value.var = "Month")
#month.cov.nocap <- month.cov[,-c(1,2)]
#month.cov.matrix <- data.matrix(month.cov.nocap)

#month.cov.vector <- as.vector(t(month.cov.matrix))
#month.cov.vector1 <- month.cov.vector[!is.na(month.cov.vector)]

#Age
## Age ##
#Need to have a column for each week and have the Age change depending on when they were caught. 
trap.cov <- trap.raw %>% dplyr::select(BirdID = AlumBand, Age, Date, Recapture) %>%
  filter(Recapture == "N") %>% dplyr::select(-Recapture) %>%
  mutate(JDayCap = yday(as.Date(Date, format = "%m/%d/%Y"))) %>%
  mutate(JDayCap = ifelse(JDayCap > 120, JDayCap - 365, JDayCap))
weeklysurvival.covs <- merge(weeklysurvival_eh, trap.cov, by = "BirdID", all.x = T)
max.week <- ncol(eh_matrix)
WSR.age <- weeklysurvival.covs %>%
  dplyr::select(BirdID, JDayCap, Adult1 = Age) %>%
  mutate(Adult1 = as.character(Adult1))
WSR.age[,4:(max.week + 2)] <- NA
oldnames <- colnames(WSR.age[,4:(2 + max.week)])
newnames <- paste("Adult", c(2:max.week), sep = "")
WSR.age <- WSR.age %>%
  rename_at(vars(oldnames), ~newnames)

for(i in 1:nrow(WSR.age)){
  if(WSR.age$Adult1[i] == "A"){
    WSR.age[i,3:(max.week+1)] <- "A"
  }else{
    for(j in 4:ncol(WSR.age)){
      if(WSR.age[i,2] + (j-3)*7 > 213){
        WSR.age[i, j] <- "A"
      }else{
        WSR.age[i, j] <- "J"
      }
    }
  }
}
WSR.age[,ncol(WSR.age)] <- "A"

for(i in 1:nrow(WSR.age)){
  for(j in 3:ncol(WSR.age)){
    if(WSR.age[i,j] == "A"){
      WSR.age[i,j] <- 1
    }else{
      WSR.age[i,j] <- 0
    }
  }
}

wsr_age.df <- as.matrix(sapply(WSR.age[3:ncol(WSR.age)], as.numeric))
wsr_age.df <- wsr_age.df[,2:ncol(wsr_age.df)]

for(i in 1:ncol(wsr_age.df)){
  for(j in 1:nrow(wsr_age.df)){
    if(is.na(eh_matrix_edit[j,i])){
      wsr_age.df[j,i] <- NA
    }
  }
}

wsr_age1 <- as.vector(t(wsr_age.df))
wsr_age <- wsr_age1[!is.na(wsr_age1)]

#S2W vs W2S
trap.cov <- trap.raw %>% dplyr::select(BirdID = AlumBand, Date, Recapture) %>%
  filter(Recapture == "N") %>% dplyr::select(-Recapture) %>%
  mutate(JDayCap = yday(as.Date(Date, format = "%m/%d/%Y"))) 
time.cov <- merge(weeklysurvival_eh, trap.cov, by = "BirdID", all.x = T)
for(i in 1:nrow(time.cov)){
  for(j in 2:(ncol(time.cov)-2)){
    if(!is.na(time.cov[i,j])){
      time.cov[i,j] <- time.cov$JDayCap[i]+(j*7)
      if(time.cov[i,j] > 365){
        time.cov[i,j] <- time.cov[i,j] - 365
      }
      if(time.cov[i,j] > 41 & time.cov[i,j] < 158){
        time.cov[i,j] <- 0
      }else{
        time.cov[i,j] <- 1
      }
    }
  }
}
time.cov <- time.cov %>% dplyr::select(-BirdID, -'1', -Date, -JDayCap)
wsr_time1 <- as.vector(t(time.cov))
wsr_time <- wsr_time1[!is.na(wsr_time1)]

### Band Recovery ###
#Age
br.age.raw <- first.encounter.eh1 %>%
  mutate(BirdID = as.character(BirdID)) %>%
  mutate(Age = ifelse(Age == "J", 0, 1))
br.age <- EH_raw

for(i in 1:nrow(br.age)){
  j <- min(which(br.age[br.age.raw$BirdID[i],] == 1))
  br.age[br.age.raw$BirdID[i],(j+1):ncol(br.age)] <- 1
  br.age[br.age.raw$BirdID[i],1:(j+1)] <- br.age.raw$Age[i]
}

br_age <- as.matrix(br.age)


#Year
br_2019 <- matrix(ncol = ncol(br_age), nrow =nrow(br_age))
br_2019[,c(1,2,5,6)] <- 0
br_2019[,c(3,4)] <- 1

br_2020 <- matrix(ncol = ncol(br_age), nrow =nrow(br_age))
br_2020[,1:4] <- 0
br_2020[,5:6] <- 1

#S2W
br_s2w <- matrix(ncol = ncol(br_age), nrow = nrow(br_age))
br_s2w[,c(1,3,5)] <- 0
br_s2w[,c(2,4,6)] <- 1

#Road and Human population density
spat_cov_raw <- read.csv("Spatial_Covariates_zstand.csv") # z-standardized
spat_cov_raw_org <- read.csv("Spatial_Covariates.csv") #not z-stand

traplocations <- trap.raw %>% dplyr::select(BirdID = AlumBand, AltID1 = Rivet.Band, Date, Location, WMD, Region, Recapture,Sex)  %>%
  filter(Recapture == "N") %>%
  filter(!is.na(BirdID)) %>%
  mutate(BirdID = as.character(BirdID)) %>%
  mutate(AltID1 = as.character(AltID1)) %>%
  mutate(Date = as.Date(Date, '%m/%d/%Y')) %>%
  mutate(BirdID = ifelse(is.na(BirdID), AltID, BirdID)) %>%
  arrange(BirdID, Date) %>%
  distinct(BirdID, .keep_all = T) %>%
  filter(Sex=="M") %>% #Just males
  mutate(BirdID = as.numeric(BirdID)) %>%
  dplyr::select(-Sex, -Date, -Recapture, -AltID1)
spatial_covariates <- merge(traplocations, spat_cov_raw, by = "Location", all.x = T)
spatial_covariates_org <- merge(traplocations, spat_cov_raw_org, by = "Location", all.x = T)

#Need to make sure these are in right order.
correctorder <- data.frame(BirdID = as.numeric(rownames(EH_raw)))
spatial_covariates <- merge(correctorder, spatial_covariates, by = "BirdID")
spatial_covariates_org <- merge(correctorder, spatial_covariates_org, by = "BirdID")

br_road <- spatial_covariates_org$Road_Density
# br_road <- spatial_covariates$Road_Density #z stand
# br_dev <- spatial_covariates$Developed
# br_hpop <- spatial_covariates$Pop_per_KM2
br_wmd <- spatial_covariates_org$WMD
br_road_log <- log(spatial_covariates_org$Road_Density) #natural log of road density


### WMD Specific Road Density
wmd_roaddens.raw <- read.csv("WMDRoadDensity.csv")
wmd_roaddens <- wmd_roaddens.raw$MEAN


### Prepare WMD Specific Total Harvest numbers
ifw.harvest.2018.raw <- read.csv("IFW_Turkey_Harvest_2018.csv")
ifw.harvest.2019.raw <- read.csv("IFW_Turkey_Harvest_2019.csv")

ifw.harvest.2018.J <- ifw.harvest.2018.raw %>%
  mutate(KillDate = as.Date(KillDate, format = "%m/%d/%Y")) %>% #To check that all spring harvests
  filter(SexAge %in% c("Jake")) %>%
  group_by(WMD) %>%
  summarize(J_2018 = n())
ifw.harvest.2018.A <- ifw.harvest.2018.raw %>%
  mutate(KillDate = as.Date(KillDate, format = "%m/%d/%Y")) %>% #To check that all spring harvests
  filter(SexAge %in% c("Tom")) %>%
  group_by(WMD) %>%
  summarize(A_2018 = n())
ifw.harvest.2019.J <- ifw.harvest.2019.raw %>%
  mutate(KillDate = as.Date(KillDate, format = "%m/%d/%Y")) %>% #To check that all spring harvests
  rename(WMD = KillWMD) %>%
  filter(SexAge == "Juvenile Male (Jake)")%>%
  group_by(WMD) %>%
  summarize(J_2019 = n())
ifw.harvest.2019.A <- ifw.harvest.2019.raw %>%
  mutate(KillDate = as.Date(KillDate, format = "%m/%d/%Y")) %>% #To check that all spring harvests
  rename(WMD = KillWMD) %>%
  filter(SexAge == "Adult Male (Tom)")%>%
  group_by(WMD) %>%
  summarize(A_2019 = n())


totalharvest.df <- data.frame(WMD = wmd_roaddens.raw$WMD)
totalharvest.df <- merge(totalharvest.df, ifw.harvest.2018.J, by = "WMD", all.x = T)
totalharvest.df <- merge(totalharvest.df, ifw.harvest.2018.A, by = "WMD", all.x = T)
totalharvest.df <- merge(totalharvest.df, ifw.harvest.2019.J, by = "WMD", all.x = T)
totalharvest.df <- merge(totalharvest.df, ifw.harvest.2019.A, by = "WMD", all.x = T)
totalharvest.df[is.na(totalharvest.df)] <- 0

TH_J2018 <- totalharvest.df$J_2018
TH_A2018 <- totalharvest.df$A_2018
TH_J2019 <- totalharvest.df$J_2019
TH_A2019 <- totalharvest.df$A_2019


sampledwmd <- unique(c(wsr_wmd, br_wmd))

#Pre2018 harvest data
pre2018.raw <- read.csv("Pre2018Harvest.csv") %>%
  filter(!is.na(mRegID))
pre2018.reduced <- pre2018.raw %>%
  dplyr::select(Year, WMD, SexAge) %>%
  mutate(Age = as.factor(ifelse(SexAge == 1, "A" ,
                                ifelse(SexAge == 3, "J",
                                       NA)))) %>%
  mutate(Sex = as.factor(ifelse(SexAge %in% c(1,3), "M", NA))) %>%
  filter(!is.na(Age)) %>%
  filter(!is.na(Year))
#Had to drop unknowns (9s)

pre2018.summary <- pre2018.reduced %>%
  group_by(Year, WMD, Age) %>%
  summarize(Total = n())

require(tidyr)
pre2018.pivot <- pre2018.summary %>% pivot_wider(names_from = c(Age, Year), values_from = Total) %>%
  arrange(WMD)

allyearsharvest <- merge(totalharvest.df, pre2018.pivot, by = "WMD") 
allyearsharvest <- allyearsharvest[,order(colnames(allyearsharvest))]
allyearsharvest[is.na(allyearsharvest)] <- 0

allyearharv.A <- as.matrix(allyearsharvest[,1:((ncol(allyearsharvest)-1)/2)])
allyearharv.J <- as.matrix(allyearsharvest[,((ncol(allyearsharvest)+1)/2):(ncol(allyearsharvest)-1)])

#Change years for SSPop here
#-9 for 2010 to present
#-5 for 2014 to present
totharv.A <- allyearharv.A[,(ncol(allyearharv.A)-5):ncol(allyearharv.A)] 
totharv.J <- allyearharv.J[,(ncol(allyearharv.J)-5):ncol(allyearharv.J)]


### Need number of weeks from capture to first hunting season for each banded bird
capdate.df <- trap.raw %>% filter(Recapture != "Y") %>%
  dplyr::select(BirdID = AlumBand, CapDate = Date) %>%
  mutate(CapDate = as.POSIXct(CapDate, format = "%m/%d/%Y")) %>%
  mutate(JCap = yday(CapDate)) %>%
  mutate(JHarv = ifelse(year(CapDate) == 2018, 118, 
                        ifelse(year(CapDate) == 2019, 117, 123)))%>%
  mutate(Weeks2Harv = floor((JHarv-JCap)/7)) %>%
  mutate(Weeks2Harv = ifelse(Weeks2Harv < 0, floor(((365-JCap)+JHarv)/7), Weeks2Harv)) %>%
  dplyr::select(BirdID, Weeks2Harv)
Weeks2Harv.ordered <- merge(correctorder, capdate.df, by = "BirdID", all.x = T)
weeks2harv <- Weeks2Harv.ordered$Weeks2Harv


### Spatial Predictive Process ###
require(raster)
#Load WMD Boundaried and Capture locations, convert to sf objects
wmd_boundaries <- readOGR(".", "Maine_Wildlife_Management_Districts")
cap_locations <- readOGR(".", "cap_locations")
cap_locations <- spTransform(cap_locations, CRS(projection(wmd_boundaries)))
#vector of capture site ID numbers for the predictive process to reference in the regression
trap.cov <- trap.raw %>% dplyr::select(BirdID = AlumBand, Sex, Location, Recapture) %>%
  filter(Recapture == "N") %>% dplyr::select(-Recapture) %>%
  filter(Sex == "M")
capids <- cap_locations@data %>%
  filter(Location %in% unique(trap.cov$Location)) %>%
  mutate(CapID = 1:length(unique(trap.cov$Location)))
capnums <- merge(trap.cov, capids, by = "Location", all.x = T)
cap_sf <- st_as_sf(cap_locations) %>%
  filter(Location %in% capids$Location)
wmd_sf <- st_as_sf(wmd_boundaries, fill = T)
wmd_centroid <- st_centroid(st_geometry(wmd_sf))


#Make initial grid and then subset to only those within WMDs
spatialknots1 <- st_make_grid(wmd_sf, square = T, what = "centers", cellsize = c( 24000,24000)) #create a grid
spatialknots2 <- c(spatialknots1, wmd_centroid)
spatialknots3 <- st_join(st_sf(spatialknots2), st_sf(wmd_sf), join = st_within)
knotscoords.df <- st_coordinates(spatialknots3) #extract coordinates
spatialknots <- cbind(knotscoords.df, spatialknots3) %>% #bind with coordinates
  filter(!is.na(IDENTIFIER)) %>% #remove knots not in a wmd
  filter(IDENTIFIER != 29) %>%
  filter(IDENTIFIER != 0) %>%
  filter(Y < 5170000) # remove knots below this latitude
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
                                y=knots.xy, method = "euclidean")
# knot to knot distances (in meters)
KnotDis.mat <- proxy::dist(x=knots.xy, y=knots.xy, method = "euclidean")


#Need to make sure these are in right order.
correctorder <- data.frame(BirdID = as.numeric(rownames(EH_raw)))
capsite.df <- merge(correctorder, capnums, by = "BirdID", all.x = T)
ind.cap.site <- capsite.df$CapID

#Need a vector saying which WMD a knot is in
WhichWMD.df <- as.data.frame(spatialknots) %>%
  dplyr::select(Knot_ID, WMD_IN = IDENTIFIER) %>%
  group_by(WMD_IN) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(values_from = Knot_ID, names_from = row, names_prefix = 'Knot') %>%
  filter(WMD_IN != 4)
WMD.matrix <- as.matrix(WhichWMD.df[2:ncol(WhichWMD.df)])

CountWMD.df <- as.data.frame(spatialknots) %>%
  dplyr::select(Knot_ID, WMD_IN = IDENTIFIER) %>%
  group_by(WMD_IN) %>%
  summarize(Total = n()) %>%
  filter(WMD_IN != 4) %>%
  arrange(match(WMD_IN, WhichWMD.df$WMD_IN))
WMD.vec <- CountWMD.df$Total
WMD.id <- WhichWMD.df$WMD_IN

