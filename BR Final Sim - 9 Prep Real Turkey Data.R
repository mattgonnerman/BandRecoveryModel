require(magrittr)
require(coda)
require(R2jags)
require(dplyr)
require(reshape2)
require(lubridate)
require(sf)
require(rgdal)
require(tidyr)

### Load Relevant Databases
trap.raw <- read.csv("Trapping - Data.csv")
harvest.raw <- read.csv("Harvests - Harvested Birds.csv")
telem.raw <- read.csv("Telemetry_Data - Telemetry.csv")
ifw.harvest.2018.raw <- read.csv("IFW_Turkey_Harvest_2018.csv")
ifw.harvest.2019.raw <- read.csv("IFW_Turkey_Harvest_2019.csv")
ifw.harvest.2021.raw <- read.csv("IFW_Turkey_Harvest_2021.csv")

simrun = "N"

######################################################
### Create Adult Weekly Survival Encounter History ###
######################################################
#List of male VHF birds
males.df <- trap.raw %>% filter(Sex == "M") %>% filter(!is.na(TransFreq))
males.vec <- males.df$AlumBand

#Simplify databases for construction of EH
#Simplify telemetry database to just BirdID, date of "visit" and Fate at "visit"
telemetry_eh <- telem.raw %>% dplyr::select(BirdID = AlumBand, Date, Fate) %>% 
  mutate(BirdID = as.character(BirdID)) %>% filter(!is.na(Fate)) %>%
  filter(BirdID %in% males.vec)


#Simplify trap database to just BirdID, date of capture, add Fate(L) for that date
#filter to remove birds without transmitters
trap_eh <- trap.raw %>% dplyr::select(BirdID = AlumBand, Date) %>% mutate(BirdID = as.character(BirdID)) %>%
  mutate(Fate = "L") %>% filter(BirdID %in% unique(telemetry_eh$BirdID)) %>%
  filter(BirdID %in% males.vec)

#Combine two databases into one so all visits are together
visits_eh <- rbind(telemetry_eh, trap_eh) %>%
  mutate(Date = as.POSIXlt(Date, format="%m/%d/%Y")) %>%
  arrange(BirdID, Date) %>%
  #Convert date to ordinal day 
  mutate(OrdDate = yday(Date)) %>%
  mutate(Year = year(Date)) %>%
  #Standardize ordinal date to be referenced against first year
  #JulDate will represent the total number of days since Jan1 2018
  mutate(JulDate = OrdDate + 365*(Year - 2018)) %>%
  #Create a Week column that is based on the first week of the first trapping event and populate it
  mutate(WeekRef = floor(JulDate/7)) %>%
  #Create a Week column that is standardized to first week for each bird
  group_by(BirdID) %>%
  mutate(Week = (WeekRef + 1)-min(WeekRef)) %>%
  #Check for non standard entries for Fate and fix
  mutate(Fate = ifelse(Fate %in% c("l", "L"), "L",
                       ifelse(Fate %in% c("d", "D"), "D",
                              ifelse(Fate %in% c("", " ", "MIA", "?"), NA, Fate)))) %>%
  ungroup() %>%
  arrange(BirdID, Date)

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


visits_eh_filter <- visits_eh %>% arrange(BirdID, Week, Date) %>% 
  #Remove NA values
  filter(!is.na(Fate)) %>% 
  #Retain last visit for each week
  group_by(BirdID, Week) %>%
  slice_tail() %>%
  #Change To 0,1 format
  ungroup() %>%
  mutate(Fate = ifelse(Fate == "L", 1, 0)) %>%
  #Remove birds that died in first two weeks
  group_by(BirdID) %>%
  mutate(Cens = ifelse(max(Week)-min(Week) > 2, 0, 1)) %>%
  ungroup() %>%
  filter(Cens == 0)

#Transform into EH format
weeklysurvival_eh <- visits_eh_filter %>% dcast(BirdID ~ Week, value.var = "Fate")

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
#WMD
trap.cov <- trap.raw %>% dplyr::select(BirdID = AlumBand, WMD, Recapture) %>%
  filter(Recapture == "N") %>% dplyr::select(-Recapture)
wmd.cov <- merge(weeklysurvival_eh_harvest, trap.cov, by = "BirdID", all.x = T)
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

#Age
#Need to have a column for each week and have the Age change depending on when they were caught.
trapage <- trap.raw %>%
  dplyr::select(BirdID = AlumBand, TrapDate = Date, AgeatCap = Age) %>%
  mutate(TrapDate = as.POSIXct(TrapDate, format = "%m/%d/%Y"))

trapage.df <- merge(visits_eh_filter, trapage, by = "BirdID", all.x = T) %>%
  mutate(AgeTransDate = as.Date(paste("06/1/", year(TrapDate), sep = ""), format = "%m/%d/%Y")) %>%
  mutate(Age = ifelse(Date > AgeTransDate, "A", AgeatCap)) %>%
  mutate(Age = ifelse(Age == "A", 1, ifelse(Age =="J", 0, NA)))

#Transform into EH format
Agecast <- trapage.df %>% dcast(BirdID ~ Week, value.var = "Age", fun.aggregate = max, na.rm = T) %>%
  dplyr::select(-BirdID, -'1')
wsr_age.df <- do.call(data.frame,lapply(Agecast, function(x) replace(x, is.infinite(x),NA)))
wsr_age.df[is.na(eh_matrix_edit)] <- NA
wsr_age1 <- as.vector(t(wsr_age.df))
wsr_adult <- wsr_age1[!is.na(wsr_age1)]

#S2W vs W2S
data_c2h <- visits_eh_filter %>%
  dplyr::select(BirdID, Date, Week) %>%
  pivot_wider(names_from = Week, values_from = Date) %>%
  arrange(match(BirdID, weeklysurvival_eh_harvest$BirdID)) %>%
  dplyr::select(-BirdID) %>%
  dplyr::select(order(as.numeric(colnames(.)))) %>%
  mutate_all(as.Date) %>%
  mutate_all(function(x) ifelse(x %in% c(seq(as.Date("2018/02/10"), by = "day", length.out = 79),
                      seq(as.Date("2019/02/10"), by = "day", length.out = 78),
                      seq(as.Date("2020/02/10"), by = "day", length.out = 84),
                      seq(as.Date("2021/02/10"), by = "day", length.out = 82)),
                 1,ifelse(is.na(x), NA, 0))) %>%
  dplyr::select(-'1')

data_c2h[is.na(eh_matrix_edit)] <- NA
wsr_c2h1 <- as.vector(t(data_c2h))
wsr_W2S <- wsr_c2h1[!is.na(wsr_c2h1)]

data_h2c <- visits_eh_filter %>%
  dplyr::select(BirdID, Date, Week) %>%
  pivot_wider(names_from = Week, values_from = Date) %>%
  arrange(match(BirdID, weeklysurvival_eh_harvest$BirdID)) %>%
  dplyr::select(-BirdID) %>%
  dplyr::select(order(as.numeric(colnames(.)))) %>%
  mutate_all(as.Date) %>%
  mutate_all(function(x) ifelse(x %in% c(seq(as.Date("2017/06/10"), by = "day", length.out = 245),
                                         seq(as.Date("2018/06/02"), by = "day", length.out = 253),
                                         seq(as.Date("2019/06/01"), by = "day", length.out = 254),
                                         seq(as.Date("2020/06/06"), by = "day", length.out = 249),
                                         seq(as.Date("2021/06/05"), by = "day", length.out = 250)),
                                1,ifelse(is.na(x), NA, 0))) %>%
  dplyr::select(-'1')

data_h2c[is.na(eh_matrix_edit)] <- NA
wsr_h2c1 <- as.vector(t(data_h2c))
wsr_S2W <- wsr_h2c1[!is.na(wsr_h2c1)]

### Band Recovery ###
#Number of Banding Years
n.band.years <- 4

#Age
br.age.raw <- first.encounter.eh1 %>%
  mutate(BirdID = as.character(BirdID)) %>%
  mutate(Age = ifelse(Age == "J", 0, 1)) %>%
  filter(BirdID %in% rownames(EH_raw)) %>%
  arrange(match(BirdID, rownames(EH_raw))) %>%
  mutate(CapYear = as.numeric(substr(CapSeason, 1,4)))
  
br_adult_hr <- matrix(1, nrow = nrow(EH_raw), ncol = ncol(EH_raw))
for(i in 1:nrow(br.age.raw)){
  if(br.age.raw$Age[i] == 0){
    br_adult_hr[i,1:(2*(br.age.raw$CapYear[i] - 2017))] <- 0
  }
}

br_adult_s <- matrix(1, nrow = nrow(EH_raw), ncol = ncol(EH_raw))
for(i in 1:nrow(br.age.raw)){
  if(br.age.raw$Age[i] == 0){
    # br_adult_s[i,1:((2*hr.ind.cap$CapYear[i])-1)] <- 0
    br_adult_s[i,1:(2*(br.age.raw$CapYear[i] - 2017))] <- 0
  }
}

#Year
#Matrix for Year of Harvest for JAGS
br_year <- rep(1:n.band.years,each = 2)

#vector to designate capture
odd <- seq(1, 2*n.band.years, 2)
br_season <- c()
for(t in 1:(2*n.band.years)){
  if(t %in% odd){br_season[t] <- 1}else{br_season[t] <- 0}
}

#Season
br_s2w <- matrix(ncol = ncol(br_adult_hr), nrow = nrow(br_adult_hr))
br_w2s <- matrix(ncol = ncol(br_adult_hr), nrow = nrow(br_adult_hr))
odd <- seq(1, 2*n.band.years, 2)
even <- seq(2, 2*n.band.years, 2)
br_s2w[,odd] <- 0
br_s2w[,even] <- 1
br_w2s[,odd] <- 1
br_w2s[,even] <- 0

#WMD
br.wmd <- trap.raw %>% dplyr::select(BirdID = AlumBand, WMD) %>%
  filter(BirdID %in% rownames(EH_raw)) %>%
  arrange(match(BirdID, rownames(EH_raw))) %>%
  distinct()
br_wmd <- br.wmd$WMD

# Need number of weeks from capture to first hunting season for each banded bird
correctorder <- data.frame(BirdID = as.numeric(rownames(EH_raw)))

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



##################################################
### Prepare WMD Specific Total Harvest numbers ###
##################################################
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
ifw.harvest.2021.J <- ifw.harvest.2021.raw %>%
  mutate(KillDate = as.Date(KillDate, format = "%m/%d/%Y")) %>% #To check that all spring harvests
  mutate(WMD = KillWMD) %>%
  filter(SexAge == "Juvenile Male (Jake)")%>%
  group_by(WMD) %>%
  summarize(J_2021 = n())
ifw.harvest.2021.A <- ifw.harvest.2021.raw %>%
  mutate(KillDate = as.Date(KillDate, format = "%m/%d/%Y")) %>% #To check that all spring harvests
  mutate(WMD = KillWMD) %>%
  filter(SexAge == "Adult Male (Tom)")%>%
  group_by(WMD) %>%
  summarize(A_2021 = n())
#No Age specific data for 2020
ifw.harvest.2020.A <- ifw.harvest.2021.A %>%
  rename(A_2020 = A_2021) %>%
  mutate(A_2020 = NA)
ifw.harvest.2020.J <- ifw.harvest.2021.J %>%
  rename(J_2020 = J_2021) %>%
  mutate(J_2020 = NA)


totalharvest.df <- data.frame(WMD = sort(c(7:26,28))) # Select WMDs 
totalharvest.df <- merge(totalharvest.df, ifw.harvest.2018.J, by = "WMD", all.x = T)
totalharvest.df <- merge(totalharvest.df, ifw.harvest.2018.A, by = "WMD", all.x = T)
totalharvest.df <- merge(totalharvest.df, ifw.harvest.2019.J, by = "WMD", all.x = T)
totalharvest.df <- merge(totalharvest.df, ifw.harvest.2019.A, by = "WMD", all.x = T)
totalharvest.df <- merge(totalharvest.df, ifw.harvest.2020.J, by = "WMD", all.x = T)
totalharvest.df <- merge(totalharvest.df, ifw.harvest.2020.A, by = "WMD", all.x = T)
totalharvest.df <- merge(totalharvest.df, ifw.harvest.2021.J, by = "WMD", all.x = T)
totalharvest.df <- merge(totalharvest.df, ifw.harvest.2021.A, by = "WMD", all.x = T)
totalharvest.df[is.na(totalharvest.df)] <- 0

TH_J2018 <- totalharvest.df$J_2018
TH_A2018 <- totalharvest.df$A_2018
TH_J2019 <- totalharvest.df$J_2019
TH_A2019 <- totalharvest.df$A_2019
TH_J2021 <- totalharvest.df$J_2021
TH_A2021 <- totalharvest.df$A_2021




sampledwmd <- unique(c(wsr_wmd, br_wmd))

#################################################
### Prepare Pre2018 Total Harvest Information ###
#################################################
#Load Pre2018 harvest data
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
allyearsharvest$J_2020 <- NA
allyearsharvest$A_2020 <- NA

allyearharv.A <- as.matrix(allyearsharvest[,1:((ncol(allyearsharvest)-1)/2)])
allyearharv.J <- as.matrix(allyearsharvest[,((ncol(allyearsharvest)+1)/2):(ncol(allyearsharvest)-1)])

#Change years for SSPop here
#-11 for 2010 to present as of 2021 data
#-7 for 2014 to present as of 2021 data 
totharv.A <- allyearharv.A[,(ncol(allyearharv.A)-7):ncol(allyearharv.A)] 
totharv.J <- allyearharv.J[,(ncol(allyearharv.J)-7):ncol(allyearharv.J)]





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
wmd_sf <- st_as_sf(wmd_boundaries, fill = T) %>%
  filter(IDENTIFIER %in% totalharvest.df$WMD)
wmd_centroid <- st_centroid(st_geometry(wmd_sf))


#Make initial grid and then subset to only those within WMDs
spatialknots1 <- st_make_grid(wmd_sf, square = T, what = "centers", cellsize = c( 24000,24000)) #create a grid
spatialknots2 <- c(spatialknots1, wmd_centroid)
spatialknots3 <- st_join(st_sf(spatialknots2), st_sf(wmd_sf), join = st_within)
knotscoords.df <- st_coordinates(spatialknots3) #extract coordinates
spatialknots <- cbind(knotscoords.df, spatialknots3) %>% #bind with coordinates
  filter(!is.na(IDENTIFIER)) %>% #remove knots not in a wmd
  filter(IDENTIFIER %in% totalharvest.df$WMD) %>%
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
WMD.id <- sapply(CountWMD.df$WMD_IN, function(x) which(x == totalharvest.df$WMD))

#Which years have missing data? (Provide COlumn index, not year itself)
missing.2020 <- 7