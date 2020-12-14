#Run Data Managment and prepare inputs for JAGS
source(file = "BR Model - 1 Data Management.R")

#Run Current Model in JAGS and save output to CSV
source(file = "BR Model - 3 Execute JAGS.R")

#Return Estimated values
source(file = "ExamineModelOutput.R")
