
{
  numsims <- 7
  #Loop to test code
  realized.hr <- matrix(NA, nrow = numsims, ncol = n.band.years*2)
  mean.est.values <- matrix(NA, nrow = numsims, ncol = n.band.years*2)
  mean.est.WSR <- matrix(NA, nrow = numsims, ncol = 2)
  mean.real.WSR.wsr <- matrix(NA, nrow = numsims, ncol = 2)
  mean.real.WSR.br <- matrix(NA, nrow = numsims, ncol = 2)
  mean.real.WSR <- matrix(NA, nrow = numsims, ncol = 2)
  estvalues <- list()
  runlengths <- c()
  nchains <- c()
  rm(WSR.results.sim)
  rm(HR.results.sim)
  for(looprun in 1:numsims){
    print(paste("Run", looprun, "Start Time:", Sys.time(), sep = " "))
    #Simulate Data
    # source(file = "BR Model - 5 Simulated Data.R")
    # source(file = "BR Model - 5c Simulated Data.R") #Frozen at 2g to preserve code
    # source(file = "BR Model - 5d Simulated Data.R")
    # source(file = "BR Model - 5e Simulated Data.R")
    source(file = "BR Model - 5f Simulated Data - JtoA at Cap2.R")
    
    # Calculate Realized Harvest Rates
    EH.check <- EH_list_br[[1]][,seq(2,(n.band.years*2),2)]
    true.S.check <- EH_list_br[[2]][,seq(2,(n.band.years*2),2)]
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
    source(file = "BR Model - 3m Execute JAGS.R")
    source(file = "BR Model - 7m1 Individual Simulation Results.R")
    
    #Save HR estimates
    estvalues[[looprun]] <- as.data.frame(BR_w_SPP_output$BUGSoutput$summary) %>%
      mutate(ID = rownames(BR_w_SPP_output$BUGSoutput$summary)) %>%
      filter(grepl("WMD.HR.", ID)) %>%
      mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\,)")) %>%
      mutate(Year = as.numeric(str_extract(ID, "(?<=\\,).*?(?=\\])"))) %>%
      mutate(Age = substr(ID,8,8)) %>%
      dplyr::select(mean, Year, WMD, Age) %>%
      pivot_wider(names_from = c(Age, Year), values_from = c(mean), names_sort = T) %>%
      mutate(RunID = looprun)
    mean.est.values[looprun,] <- colMeans(estvalues[[looprun]][,2:((n.band.years*2)+1)])
    
    #Save WSR estimates
    est.WSR <- as.data.frame(BR_w_SPP_output$BUGSoutput$summary) %>%
      mutate(ID = rownames(BR_w_SPP_output$BUGSoutput$summary)) %>%
      filter(grepl("WSR_M_", ID)) %>%
      mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\])")) %>%
      mutate(Age = substr(ID,7,7)) %>%
      dplyr::select(mean, WMD, Age) %>%
      group_by(Age) %>%
      summarize(Mean.WSR = mean(mean)) %>%
      mutate(SimID = looprun)
    mean.est.WSR[looprun,] <- est.WSR$Mean.WSR
    
    print(paste("Run", looprun, "End Time:", Sys.time(), sep = " "))
  }
  
  #Estimate Bias between realized and estimated HR
  bias.hr <- realized.hr[1:(looprun),]-mean.est.values[1:(looprun),]
  meanbias.hr <- data.frame(Age = rep(c("A", "J"), each = n.years.br),
                            Year = rep(1:n.years.br,2),
                            HRMeanBias = colMeans(realized.hr-mean.est.values, na.rm = T),
                            HRSDBias = apply(realized.hr-mean.est.values, 2, sd, na.rm = T))
  
  #Estimate Bias between realized and estimated WSR
  bias.wsr <- mean.real.WSR - mean.est.WSR
  meanbias.wsr <- data.frame(Age = rep(c("A", "J")),
                             WSRMeanBias = colMeans(bias.wsr, na.rm = T),
                             WSRSDBias = apply(mean.real.WSR - mean.est.WSR, 2, sd, na.rm = T))
  #For WSR only
  # bias.wsr <- mean.real.WSR.wsr - mean.est.WSR
  # meanbias.wsr <- data.frame(Age = rep(c("A", "J")),
  #                            WSRMeanBias = colMeans(bias.wsr, na.rm = T),
  #                            WSRSDBias = apply(mean.real.WSR.wsr - mean.est.WSR, 2, sd, na.rm = T))
  # mean.real.WSR.wsr
  
  sink("BiasResults.csv")
  cat("Average Estimated HR")
  cat('\n')
  write.csv(mean.est.values)
  cat('\n')
  cat('\n')
  cat("Average HR Bias (Realized - Estimated)")
  cat('\n')
  write.csv(meanbias.hr)
  cat('\n')
  cat('\n')
  cat("Individual Simulation Bias - HR")
  cat('\n')
  write.csv(bias.hr)
  cat('\n')
  cat('\n')
  cat("Average Estimated WSR")
  cat('\n')
  write.csv(mean.est.WSR)
  cat('\n')
  cat('\n')
  cat("Average WSR Bias (Realized - Estimated)")
  cat('\n')
  write.csv(meanbias.wsr)
  cat('\n')
  cat('\n')
  cat("Individual Simulation Bias - WSR")
  cat('\n')
  write.csv(bias.wsr)
  cat('\n')
  cat('\n')
  sink()
}


{### Non State Space Loop
  
  # source(file = "BR Model - 5 Simulated Data.R")
  # source(file = "BR Model - 5c Simulated Data.R") #Frozen at 2g to preserve code
  source(file = "BR Model - 5d Simulated Data.R")
  # source(file = "BR Model - 5e Simulated Data.R")
  # source(file = "BR Model - 5f Simulated Data - JtoA at Cap2.R")
  
  ###Checking for bias in the BR/WSR only model
  #How many simulations do you want to do?
  numsims <- 5
  #Loop to test code
  realized.hr <- matrix(NA, nrow = numsims, ncol = n.band.years*2)
  mean.est.values <- matrix(NA, nrow = numsims, ncol = n.band.years*2)
  mean.est.WSR <- matrix(NA, nrow = numsims, ncol = 2)
  mean.real.WSR.wsr <- matrix(NA, nrow = numsims, ncol = 2)
  mean.real.WSR.br <- matrix(NA, nrow = numsims, ncol = 2)
  mean.real.WSR <- matrix(NA, nrow = numsims, ncol = 2)
  estvalues <- list()
  runlengths <- c()
  nchains <- c()
  rm(WSR.results.sim)
  rm(HR.results.sim)
  for(looprun in 1:numsims){
    print(paste("Run", looprun, "Start Time:", Sys.time(), sep = " "))
    #Simulate Data
    # source(file = "BR Model - 5 Simulated Data.R")
    # source(file = "BR Model - 5c Simulated Data.R") #Frozen at 2g to preserve code
    # source(file = "BR Model - 5d Simulated Data.R")
    # source(file = "BR Model - 5e Simulated Data.R")
    source(file = "BR Model - 5f Simulated Data - JtoA at Cap2.R")
    
    # Calculate Realized Harvest Rates
    EH.check <- EH_list_br[[1]][,seq(2,(n.band.years*2),2)]
    true.S.check <- EH_list_br[[2]][,seq(2,(n.band.years*2),2)]
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
    
    
    #Realized WSR from BR Data
    true.S.br.check <- EH_list_br[[2]]
    EH.br.check <- EH_list_br[[1]]
    EH.br.check.adult <- matrix(NA, nrow = nbandind, ncol = n.band.years*2)
    EH.br.check.juv <- matrix(NA, nrow = nbandind, ncol = n.band.years*2)
    S.br.check.adult <- matrix(NA, nrow = nbandind, ncol = n.band.years*2)
    S.br.check.juv <- matrix(NA, nrow = nbandind, ncol = n.band.years*2)
    for(checki in 1:nbandind){
      for(checkj in 1:(n.band.years*2)){
        if(br_adult_hr[checki, checkj] == 1){EH.br.check.adult[checki, checkj] <- EH.br.check[checki, checkj]}
        if(br_adult_hr[checki, checkj] == 0){EH.br.check.juv[checki, checkj] <- EH.br.check[checki, checkj]}
        if(br_adult_hr[checki, checkj] == 1){S.br.check.adult[checki, checkj] <- true.S.br.check[checki, checkj]}
        if(br_adult_hr[checki, checkj] == 0){S.br.check.juv[checki, checkj] <- true.S.br.check[checki, checkj]}
      }
    }
    EH.br.W2S.check.adult <- EH.br.check.adult
    EH.br.W2S.check.adult[,seq(1,(n.band.years*2),2)] <- 0
    W2S.br.A.totals <- colSums(S.br.check.adult + EH.br.W2S.check.adult, na.rm = T)
    realized.WSR.br.A.W2S <- (W2S.br.A.totals[seq(2,(n.band.years*2),2)]/W2S.br.A.totals[seq(1,(n.band.years*2),2)])^(1/11)
    
    EH.br.W2S.check.juv <- EH.br.check.juv
    EH.br.W2S.check.juv[,seq(1,(n.band.years*2),2)] <- 0
    W2S.br.J.totals <- colSums(S.br.check.juv + EH.br.W2S.check.juv, na.rm = T)
    realized.WSR.br.J.W2S <- (W2S.br.J.totals[seq(2,(n.band.years*2),2)]/W2S.br.J.totals[seq(1,(n.band.years*2),2)])^(1/11)
    mean.real.WSR.br[looprun,2] <- mean(realized.WSR.br.J.W2S)
    
    EH.br.S2W.check.adult <- EH.br.check.adult
    EH.br.S2W.check.adult[,seq(4,(n.band.years*2),2)] <- 0
    W2S.br.Aalive <- colSums(S.br.check.adult, na.rm = T)[seq(2,(n.band.years*2),2)]
    W2S.br.Jalive <- colSums(S.br.check.juv, na.rm = T)[seq(2,(n.band.years*2),2)]
    W2S.br.AendH <- W2S.br.Aalive + W2S.br.Aalive
    W2S.br.AaliveC <- colSums(S.br.check.adult, na.rm = T)[seq(1,(n.band.years*2),2)]
    W2S.br.Acap <- colSums(EH.br.check.adult, na.rm = T)[seq(1,(n.band.years*2),2)]
    W2S.br.AstartC <- W2S.br.AaliveC - W2S.br.Acap
    realized.WSR.br.A.S2W <- (W2S.br.AstartC[2:n.band.years]/W2S.br.AaliveC[1:(n.band.years-1)])^(1/36)
    mean.real.WSR.br[looprun,1] <- mean(c(realized.WSR.br.A.S2W,realized.WSR.br.A.W2S))
    
    
    #Realized WSR from WSR Data
    true.WSR.check <- EH.wsr.list[[2]]
    true.WSR.A.check <- matrix(NA, nrow = nbandind, ncol = n.occasions.wsr)
    true.WSR.J.check <- matrix(NA, nrow = nbandind, ncol = n.occasions.wsr)
    for(checki in 1:nrow(WSR.adult)){
      for(checkj in 1:ncol(WSR.adult)){
        if(WSR.adult1[checki, checkj] == 1){true.WSR.A.check[checki, checkj] <- true.WSR.check[checki, checkj]}
        if(WSR.adult1[checki, checkj] == 0){true.WSR.J.check[checki, checkj] <- true.WSR.check[checki, checkj]}
        if(WSR.sex1[checki, checkj] == 1){true.WSR.A.check[checki, checkj] <- NA}
        if(WSR.sex1[checki, checkj] == 1){true.WSR.J.check[checki, checkj] <- NA}
      }
    }
    
    true.wsr.A.1 <- colSums(true.WSR.A.check[,1:(ncol(true.WSR.A.check)-1)], na.rm = T)
    true.wsr.A.2 <- colSums(true.WSR.A.check[,2:(ncol(true.WSR.A.check))], na.rm = T)
    mean.real.WSR.wsr[looprun,1] <- sum((true.wsr.A.2/true.wsr.A.1)*(true.wsr.A.1/sum(true.wsr.A.1)), na.rm = T)
    true.wsr.J.1 <- colSums(true.WSR.J.check[,1:(ncol(true.WSR.J.check)-1)], na.rm = T)[1:11]
    true.wsr.J.2 <- colSums(true.WSR.J.check[,2:(ncol(true.WSR.J.check))], na.rm = T)[1:11]
    mean.real.WSR.wsr[looprun,2] <- sum((true.wsr.J.2/true.wsr.J.1)*(true.wsr.J.1/sum(true.wsr.J.1)), na.rm = T)
    mean.real.WSR[looprun,1] <- mean(c(mean.real.WSR.wsr[looprun,1], mean.real.WSR.br[looprun,1]))
    mean.real.WSR[looprun,2] <- mean(c(mean.real.WSR.wsr[looprun,2], mean.real.WSR.br[looprun,2]))
    
    ###Run JAGS Code
    #MCMC settings
    ni <- 10000 #number of iterations
    nt <- 8 #thinning
    nb <- 5000 #burn in period
    nc <- 5 #number of chains
    
    #Run JAGS model
    # source(file = "BR Model - 3a Execute JAGS.R")
    source(file = "BR Model - 3g Execute JAGS.R") #cloglog for WSR and HR
    # source(file = "BR Model - 3h Execute JAGS.R") #clog for WSR, logit for HR
    
    #Save HR estimates
    estvalues[[looprun]] <- as.data.frame(BR_w_SPP_output$BUGSoutput$summary) %>%
      mutate(ID = rownames(BR_w_SPP_output$BUGSoutput$summary)) %>%
      filter(grepl("WMD.HR.", ID)) %>%
      mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\,)")) %>%
      mutate(Year = as.numeric(str_extract(ID, "(?<=\\,).*?(?=\\])"))) %>%
      mutate(Age = substr(ID,8,8)) %>%
      dplyr::select(mean, Year, WMD, Age) %>%
      pivot_wider(names_from = c(Age, Year), values_from = c(mean), names_sort = T) %>%
      mutate(RunID = looprun)
    mean.est.values[looprun,] <- colMeans(estvalues[[looprun]][,2:((n.band.years*2)+1)])
    
    #Save WSR estimates
    est.WSR <- as.data.frame(BR_w_SPP_output$BUGSoutput$summary) %>%
      mutate(ID = rownames(BR_w_SPP_output$BUGSoutput$summary)) %>%
      filter(grepl("WSR_M_", ID)) %>%
      mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\])")) %>%
      mutate(Age = substr(ID,7,7)) %>%
      dplyr::select(mean, WMD, Age) %>%
      group_by(Age) %>%
      summarize(Mean.WSR = mean(mean)) %>%
      mutate(SimID = looprun)
    mean.est.WSR[looprun,] <- est.WSR$Mean.WSR
    
    print(paste("Run", looprun, "End Time:", Sys.time(), sep = " "))
  }
  
  #Estimate Bias between realized and estimated HR
  bias.hr <- realized.hr[1:(looprun),]-mean.est.values[1:(looprun),]
  meanbias.hr <- data.frame(Age = rep(c("A", "J"), each = n.years.br),
                            Year = rep(1:n.years.br,2),
                            HRMeanBias = colMeans(realized.hr-mean.est.values, na.rm = T),
                            HRSDBias = apply(realized.hr-mean.est.values, 2, sd, na.rm = T))
  
  #Estimate Bias between realized and estimated WSR
  bias.wsr <- mean.real.WSR - mean.est.WSR
  meanbias.wsr <- data.frame(Age = rep(c("A", "J")),
                             WSRMeanBias = colMeans(bias.wsr, na.rm = T),
                             WSRSDBias = apply(mean.real.WSR - mean.est.WSR, 2, sd, na.rm = T))
  #For WSR only
  # bias.wsr <- mean.real.WSR.wsr - mean.est.WSR
  # meanbias.wsr <- data.frame(Age = rep(c("A", "J")),
  #                            WSRMeanBias = colMeans(bias.wsr, na.rm = T),
  #                            WSRSDBias = apply(mean.real.WSR.wsr - mean.est.WSR, 2, sd, na.rm = T))
  # mean.real.WSR.wsr
  
  sink("BiasResults.csv")
  cat("Average Estimated HR")
  cat('\n')
  write.csv(mean.est.values)
  cat('\n')
  cat('\n')
  cat("Average HR Bias (Realized - Estimated)")
  cat('\n')
  write.csv(meanbias.hr)
  cat('\n')
  cat('\n')
  cat("Individual Simulation Bias - HR")
  cat('\n')
  write.csv(bias.hr)
  cat('\n')
  cat('\n')
  cat("Average Estimated WSR")
  cat('\n')
  write.csv(mean.est.WSR)
  cat('\n')
  cat('\n')
  cat("Average WSR Bias (Realized - Estimated)")
  cat('\n')
  write.csv(meanbias.wsr)
  cat('\n')
  cat('\n')
  cat("Individual Simulation Bias - WSR")
  cat('\n')
  write.csv(bias.wsr)
  cat('\n')
  cat('\n')
  sink()
  
  
  
  
  
  
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
}