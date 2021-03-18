require(dplyr)
require(stringr)
require(tidyr)
require(miscTools)
require(ggplot2)

# #MCMC settings
ni <- 100 #number of iterations
nt <- 8 #thinning
nb <- 5 #burn in period
nc <- 5 #number of cores

#How Many Simulations would you like to run?
looprun1 <- 10

#Simplified loop, broken into multiple scripts
#Saves individual loop outputs to their own files
for(looprun in 1:looprun1){
  
  print(paste("Run", looprun, "Start Time:", Sys.time(), sep = " "))
  
  #Create Simulated Data Set
  source(file = "BR Model - 5f Simulated Data - JtoA at Cap2.R")
  
  #Save Real Parameter Values Individually
  source(file = "BR Model - 8b Looped Simulation - Save Real Values.R")

  #Run Model - SS with no year difference in HR - No R Pooling
  source(file = "BR Model - 3m Execute JAGS.R")
  
  #Save raw model outputs individually
  write.csv(BR_w_SPP_output$BUGSoutput$summary,
            file = paste("Model Bias Comparison/SampleSize/Trial ",looprun," - 3M Raw Output.csv", sep = ""))
  
  #Summarize Model Results
  source(file = "BR Model - 8c Looped Simulation - Summarize Model Results.R")
  
  #Save Simulated Data Information
  source(file = "BR Model - 8d Looped Simulation - Dataset Info.R")
  
  #Save Bias Information
  source(file = "BR Model - 8e Looped Simulation - Model Bias.R")
  
  print(paste("Run", looprun, "End Time:", Sys.time(), sep = " "))
}

write.csv(master.bias.hr, "Model Bias Comparison/SampleSize/Master HR Bias.csv")
write.csv(master.bias.wsr, "Model Bias Comparison/SampleSize/Master WSR Bias.csv")
write.csv(master.bias.N, "Model Bias Comparison/SampleSize/Master N Bias.csv")
write.csv(master.bias.R, "Model Bias Comparison/SampleSize/Master R Bias.csv")

#Analyze Results
source(file = "BR Model - 8f Looped Simulation - Analyze Results.R")