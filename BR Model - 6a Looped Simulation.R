require(dplyr)
require(stringr)
require(tidyr)
require(miscTools)
require(ggplot2)




#Simplified loop, broken into multiple scripts
#Saves individual loop outputs to their own files
for(looprun in 1:15){
  
  print(paste("Run", looprun, "Start Time:", Sys.time(), sep = " "))
  
  source(file = "BR Model - 5f Simulated Data - JtoA at Cap2.R")
  
  # #MCMC settings
  ni <- 15000 #number of iterations
  nt <- 8 #thinning
  nb <- 5000 #burn in period
  nc <- 8 #number of cores

  #SS with no year difference - No Pooling
  source(file = "BR Model - 3m Execute JAGS.R")
  # source(file = "BR Model - 7m Individual Simulation Results.R") # For J to A after 1st harvest season
  source(file = "BR Model - 7m1 Individual Simulation Results.R") # For J to A before 2nd capture
  
  print(paste("Run", looprun, "End Time:", Sys.time(), sep = " "))
}

