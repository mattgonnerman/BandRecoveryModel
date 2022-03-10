##########################
### SIMULATED DATA SET ###
##########################
require(parallel)
require(dplyr)
require(stringr)
require(tidyr)
require(miscTools)
require(ggplot2)

### MCMC settings
ni <- 10000 #number of iterations
nt <- 8 #thinning
nb <- 5000 #burn in period
nc <- detectCores()/2 #number of cores

### Set Variable Parameters 
## HR Gaussian Process
# Magnitude of variation, value that variogram levels out at 
psill.hr <- 0.01 #c(0.001, 0.01, 0.1)
# Maximal distance of autocorrelation, where variogram levels out
hr.sc <- 7 #c(2, 7, 15)
# Small-scale variations
nugget.hr <- 0.01 #c(0.001, 0.005, 0.01)

#Which Trial area you running (e.g. "LowNugget", "MedPSill", "HighRange")
trialname <- "HighNugget"

for(looprun in 1:100){
  print(paste("Run", looprun, "Start Time:", Sys.time(), sep = " "))
  
  #Generate Simulated Dataset
  source(file = "BR Final Sim - 1 Simulated Data.R")
  
  #Save Simulated Parameter Values Individually
  source(file = "BR Final Sim - 2 Save Real Values.R")
  
  #Run Model - SS with no year difference in HR - No R Pooling
  source(file = "BR Final Sim - 3 Execute JAGS.R")
  
  #Save raw model outputs individually
  write.csv(BR_w_SPP_output$BUGSoutput$summary,
            file = paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/",
                         trialname, " - Sim",
                         looprun, " - RawModel Estimates.csv", sep = ""))
  
  #Summarize Model Results
  source(file = "BR Final Sim - 5 Summarize Model Results.R")
  
  #Save Simulated Data Information
  source(file = "BR Final Sim - 6 Dataset Info.R")
  
  #Calculate Model Bias for Simulation
  source(file = "BR Final Sim - 7 Calculate Bias.R")
  
  print(paste("Run", looprun, "End Time:", Sys.time(), sep = " "))
}

#If failure/computer freeze/etc, load the most recent info using code below and start loop at appropriate number
master.bias.hr <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/", trialname, " - Master HR Bias.csv", sep = ""))
master.bias.wsr <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/", trialname, " - Master WSR Bias.csv", sep = ""))
master.bias.N <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/", trialname, " - Master N Bias.csv", sep = ""), check.names=FALSE)
master.bias.R <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/", trialname, " - Master R Bias.csv", sep = ""), check.names=FALSE)
mastersiteinfo <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/", trialname, " - MasterSimInfo.csv", sep = ""))
max(mastersiteinfo$Trial)



#Create Graphs to Visualize Results
source(file = "BR Final Sim - 8 Preliminary Graphs.R")


########################
### REAL TURKEY DATA ###
########################
require(parallel)
require(dplyr)
require(stringr)
require(tidyr)
require(miscTools)
require(ggplot2)

### MCMC settings
ni <- 50000 #number of iterations
nt <- 8 #thinning
nb <- 20000 #burn in period
nc <- detectCores()/2 #number of cores

#Prep Turkey Data
source(file = "BR Final Sim - 9 Prep Real Turkey Data.R")

source(file = "BR Final Sim - 3a Execute JAGS - Real Data.R")

#Save model
save(BR_w_SPP_output, file ="RealDataModelOutput.RData")
# load( file ="RealDataModelOutput.RData") #BR_w_SPP_output

#Summarize Model Results
source(file = "BR Final Sim - 5a Summarize Model Results - Real Data.R")

### Run a model without the SPP, assume constant harvest rate across the state
### MCMC settings
ni <- 50000 #number of iterations
nt <- 8 #thinning
nb <- 20000 #burn in period
nc <- detectCores()/2 #number of cores

#Prep Turkey Data
source(file = "BR Final Sim - 9 Prep Real Turkey Data.R")

#Run Model
source(file = "BR Final Sim - 3b Execute JAGS - Real Data NO SPP.R")

write.csv(BR_w_SPP_output$BUGSoutput$summary,
          file = "realdataNOSPPOutput2021.csv")
source(file = "BR Final Sim - 5b Summarize Model Results - Real Data NO SPP.R")
