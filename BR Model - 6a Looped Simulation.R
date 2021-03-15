require(dplyr)
require(stringr)
require(tidyr)
require(miscTools)
require(ggplot2)




#Simplified loop, broken into multiple scripts
#Saves individual loop outputs to their own files
for(looprun in 10:15){
  
  print(paste("Run", looprun, "Start Time:", Sys.time(), sep = " "))
  
  source(file = "BR Model - 5f Simulated Data - JtoA at Cap2.R")
  
  # #MCMC settings
  ni <- 10000 #number of iterations
  nt <- 8 #thinning
  nb <- 50000 #burn in period
  nc <- 5 #number of chains

  #SS with no year difference - No Pooling
  source(file = "BR Model - 3m Execute JAGS.R")
  source(file = "BR Model - 7m Individual Simulation Results.R")
  
  print(paste("Run", looprun, "End Time:", Sys.time(), sep = " "))
}

