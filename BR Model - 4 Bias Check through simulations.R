require(dplyr)
require(stringr)
require(tidyr)
require(miscTools)
require(ggplot2)

source(file = "BR Model - 5 Simulated Data.R")

###Checking for bias in the BR/WSR only model
#How many simulations do you want to do?
numsims <- 1
#Loop to test code
realized.hr <- matrix(NA, nrow = numsims, ncol = n.band.years*2)
mean.est.values <- matrix(NA, nrow = numsims, ncol = n.band.years*2)
estvalues <- list()
runlengths <- c()
nchains <- c()
time1 <- Sys.time()
for(looprun in 1:numsims){
  #Simulate Data
  source(file = "BR Model - 5 Simulated Data.R")

  # Calculate Realized Harvest Rates
  EH.check <- EH_list[[1]][,seq(2,(n.band.years*2),2)]
  true.S.check <- EH_list[[2]][,seq(2,(n.band.years*2),2)]
  Age.check <- br_adult_hr[,seq(2,(n.band.years*2),2)]
  EH.check.adult <- matrix(NA, nrow = nbandind, ncol = n.band.years)
  EH.check.juv <- matrix(NA, nrow = nbandind, ncol = n.band.years)
  S.check.adult <- matrix(NA, nrow = nbandind, ncol = n.band.years)
  S.check.juv <- matrix(NA, nrow = nbandind, ncol = n.band.years)
  for(checki in 1:nbandind){
    for(checkj in 1:n.band.years){
      if(Age.check[checki, checkj] == 1){EH.check.adult[checki, checkj] <- EH.check[checki, checkj]}
      if(Age.check[checki, checkj] == 0){EH.check.juv[checki, checkj] <- EH.check[checki, checkj]}
      if(Age.check[checki, checkj] == 1){S.check.adult[checki, checkj] <- true.S.check[checki, checkj]}
      if(Age.check[checki, checkj] == 0){S.check.juv[checki, checkj] <- true.S.check[checki, checkj]}
    }
  }
  totalavail.Adult <- colSums(S.check.adult, na.rm = T) + colSums(EH.check.adult, na.rm = T)
  totalavail.Juv <- colSums(S.check.juv, na.rm = T) + colSums(EH.check.juv, na.rm = T)
  true.hr.adult <- colSums(EH.check.adult, na.rm = T)/totalavail.Adult
  true.hr.juv <- colSums(EH.check.juv, na.rm = T)/totalavail.Juv
  realized.hr[looprun,] <- c(true.hr.adult,true.hr.juv)
  
  # #MCMC settings
  ni <- 10000 #number of iterations
  nt <- 8 #thinning
  nb <- 5000 #burn in period
  nc <- 5 #number of chains
  
  #Run JAGS model
  # source(file = "BR Model - 3a Execute JAGS.R")
  source(file = "BR Model - 3g Execute JAGS.R")
  
  #Save HR estimates
  estvalues[[looprun]] <- as.data.frame(BR_w_SPP_output$BUGSoutput$summary) %>%
    mutate(ID = rownames(BR_w_SPP_output$BUGSoutput$summary)) %>%
    filter(grepl("WMD.HR.", ID)) %>%
    mutate(Year = str_extract(ID, "[:digit:]+")) %>%
    mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\])")) %>%
    mutate(Age = substr(ID,8,8)) %>%
    dplyr::select(mean, Year, WMD, Age) %>%
    pivot_wider(names_from = c(Age, Year), values_from = c(mean), names_sort = T) %>%
    mutate(RunID = looprun)
  mean.est.values[looprun,] <- colMeans(estvalues[[looprun]][,2:((n.band.years*2)+1)])

  
  
  print(looprun)
  print(Sys.time())
}
meanbias.hr <- colMeans(realized.hr-mean.est.values, na.rm = T)
bias.hr <- realized.hr[1:(looprun),]-mean.est.values[1:(looprun),]
# 
# View(realized.hr[1:(looprun-1),])
# View(mean.est.values[1:(looprun-1),])



















# ##################################################################
# ###Checking for bias in the SS model
# #How many simulations do you want to do?
# numsims <- 50
# #Loop to test code
# realized.hr.SS <- matrix(NA, nrow = numsims, ncol = 2)
# Nbias.byregion <- matrix(NA, nrow =  C*D, ncol = n.years.totharv*2)
# mean.est.values.SS <- matrix(NA, nrow = numsims, ncol = 2)
# estvalues.SS <- list()
# estN.SS <- list()
# pop.real.SS <- list()
# NA.MES <- c()
# NJ.MES <- c()
# for(looprun in 1:numsims){
#   #Simulate Data
#   source(file = "BR Model - 5 Simulated Data.R")
#   
#   # Calculate Realized Harvest Rates
#   EH.check <- EH_list[[1]][,seq(2,(n.band.years*2),2)]
#   true.S.check <- EH_list[[2]][,seq(2,(n.band.years*2),2)]
#   Age.check <- br_adult_hr[,seq(2,(n.band.years*2),2)]
#   EH.check.adult <- matrix(NA, nrow = nbandind, ncol = n.band.years)
#   EH.check.juv <- matrix(NA, nrow = nbandind, ncol = n.band.years)
#   S.check.adult <- matrix(NA, nrow = nbandind, ncol = n.band.years)
#   S.check.juv <- matrix(NA, nrow = nbandind, ncol = n.band.years)
#   for(checki in 1:nbandind){
#     for(checkj in 1:n.band.years){
#       if(Age.check[checki, checkj] == 1){EH.check.adult[checki, checkj] <- EH.check[checki, checkj]}
#       if(Age.check[checki, checkj] == 0){EH.check.juv[checki, checkj] <- EH.check[checki, checkj]}
#       if(Age.check[checki, checkj] == 1){S.check.adult[checki, checkj] <- true.S.check[checki, checkj]}
#       if(Age.check[checki, checkj] == 0){S.check.juv[checki, checkj] <- true.S.check[checki, checkj]}
#     }
#   }
#   totalavail.Adult <- colSums(S.check.adult, na.rm = T) + colSums(EH.check.adult, na.rm = T)
#   totalavail.Juv <- colSums(S.check.juv, na.rm = T) + colSums(EH.check.juv, na.rm = T)
#   true.hr.adult <- mean(colSums(EH.check.adult, na.rm = T)/totalavail.Adult)
#   true.hr.juv <- mean(colSums(EH.check.juv, na.rm = T)/totalavail.Juv)
#   realized.hr.SS[looprun,] <- c(true.hr.adult,true.hr.juv)
#   pop.real.SS[[looprun]] <- cbind(N.A, N.J)
#   
#   
#   # #MCMC settings
#   ni <- 30000 #number of iterations
#   nt <- 8 #thinning
#   nb <- 20000 #burn in period
#   nc <- 5 #number of chains
#   
#   #Run Current Model in JAGS and save output to CSV
#   # source(file = "BR Model - 3 Execute JAGS.R") # Original, Do NOT Change
#   # source(file = "BR Model - 3b Execute JAGS.R") # No Temporal Variation in HR
#   ### This is the current model I am favoring. Still have the initial drop in HR from year 1 to 2 no matter the input data
#   ### also still underestimating harvest rates compared to the nonState Space model.
#   source(file = "BR Model - 3b Execute JAGS.R")
#   
#   estvalues.SS[[looprun]] <- as.data.frame(BR_w_SPP_output$BUGSoutput$summary) %>%
#     mutate(ID = rownames(BR_w_SPP_output$BUGSoutput$summary)) %>%
#     filter(grepl("mean.WMD.HR.", ID)) %>%
#     mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\])")) %>%
#     mutate(Age = substr(ID,13,13)) %>%
#     dplyr::select(mean, WMD, Age) %>%
#     pivot_wider(names_from = c(Age), values_from = c(mean), names_sort = T)
#   mean.est.values.SS[looprun,] <- colMeans(estvalues.SS[[looprun]][,2:3])
#   
#   estN.SS[[looprun]] <- as.data.frame(BR_w_SPP_output$BUGSoutput$summary) %>%
#     mutate(ID = rownames(BR_w_SPP_output$BUGSoutput$summary)) %>%
#     filter(grepl("N.", ID)) %>%
#     mutate(Year = as.numeric(substr(str_extract(ID, "[,^][:digit:]+"), 2,4))) %>%
#     mutate(WMD = str_extract(ID, "[:digit:]+")) %>%
#     mutate(Age = substr(ID,3,3)) %>%
#     dplyr::select(mean, Year, WMD, Age) %>%
#     pivot_wider(names_from = c(Age, Year), values_from = c(mean), names_sort = T)
#   
#   a.A <- pop.real.SS[[looprun]][,1:(n.years.totharv)]
#   a.J <- pop.real.SS[[looprun]][,(n.years.totharv+1):(n.years.totharv*2)]
#   b.A <- estN.SS[[looprun]][,2:(n.years.totharv+1)]
#   b.J <- estN.SS[[looprun]][,(n.years.totharv+2):(ncol(estN.SS[[looprun]]))]
#   NA.MES[looprun] <- sum((a.A-b.A)^2)/(nrow(a.A)*ncol(a.A))
#   NJ.MES[looprun] <- sum((a.J-b.J)^2)/(nrow(a.J)*ncol(a.J))  
#   
#   print(looprun)
#   }
# 
# bias.hr.SS <- colMeans(realized.hr.SS-mean.est.values.SS)