#Run Data Managment and prepare inputs for JAGS
source(file = "BR Model - 1 Data Management.R")

#Run Current Model in JAGS and save output to CSV
# source(file = "BR Model - 3 Execute JAGS.R")
source(file = "BR Model - 3c Execute JAGS.R") #Testing Options for dealing with underestimation of HR by 

#Return Estimated values
source(file = "BR Model - 4 Examine Model Output.R")


#Try no temporal variation in WMD specific HR

#Try including 2020 data?

#May need to reconsider the assumption of no non-harvest mortality during the hunting season.