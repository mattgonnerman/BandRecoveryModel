setwd("E:/GitHub/BandRecoveryModel")

#Run Data Managment and prepare inputs for JAGS
# source(file = "BR Model - 1a Data Management.R") #Actual Turkey Data
# source(file = "BR Model - 5 Simulated Data.R") # Simulated Data - Spatially Correlated using Gaussian Random Field
# source(file = "BR Model - 5c Simulated Data.R") # Frozen, do not change
source(file = "BR Model - 5d Simulated Data.R") #

# #MCMC settings
ni <- 30000 #number of iterations
nt <- 8 #thinning
nb <- 15000 #burn in period
nc <- 5 #number of chains

print(Sys.time())
#Run Current Model in JAGS and save output to CSV
# source(file = "BR Model - 3 Execute JAGS.R") # Original, Do NOT Change
# source(file = "BR Model - 3b Execute JAGS.R") # No Temporal Variation in HR
### This is the current model I am favoring. Still have the initial drop in HR from year 1 to 2 no matter the input data
### also still underestimating harvest rates compared to the nonState Space model.
# source(file = "BR Model - 3a Execute JAGS.R") # Band Recovery-WSR SubModel Only, HR by year
# source(file = "BR Model - 3c Execute JAGS.R") # Temporal Variation in HR within SS but not within BR (no covariates for year when estimating HR)
# source(file = "BR Model - 3d Execute JAGS.R") # Simplified State Space approach
# source(file = "BR Model - 3z Execute JAGS.R") # Master Script for testing various options for dealing with underestimation of HR
# source(file = "BR Model - 3f Execute JAGS.R") #Kery and Schaub version of the SS
# source(file = "BR Model - 3g Execute JAGS.R") #cloglog link
# source(file = "BR Model - 3i Execute JAGS.R") #SS with year difference in HR (built from 3g)
source(file = "BR Model - 3j Execute JAGS.R") #SS with no year difference (built from 3g)

#Examine Output of Real Dataset
# source(file = "BR Model - 4 Examine Model Output.R")

#Examine Output of Single Simulated Dataset
source(file = "BR Model - 7 Individual Simulation Results.R")

#Run multiple simulations and compare to realized harvest
source(file = "BR Model - 6a Looped Simulation.R")



