#Examine Model Outputs more easily
outputs <- as.data.frame(BR_w_SPP_output$BUGSoutput$summary)
outputs$X <- rownames(outputs)
# outputs <- read.csv("3G_output.csv")
require(dplyr)
require(stringr)
require(tidyr)
require(miscTools)
require(ggplot2)

########################
### Estimated Values ###
########################
### WMD and Time Specific Abundance
# Adult
N.A.long <- outputs %>% 
  filter(substr(X,1,3) == "N.A") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
N.A.est <- N.A.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)
N.A.total <- data.frame(mean = colSums(N.A.est[2:ncol(N.A.est)]),
                        Year = min(N.A.long$Year):max(N.A.long$Year),
                        WMD = "Total")
# write.csv(N.A.est, "N_A_WMDestimates.csv", row.names = F)


#Juvenile
N.J.long <- outputs %>% 
  filter(substr(X,1,3) == "N.J") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
N.J.est <- N.J.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)
N.J.total <- data.frame(mean = colSums(N.J.est[2:ncol(N.J.est)]),
                        Year = min(N.A.long$Year):max(N.J.long$Year),
                        WMD = "Total")
# write.csv(N.J.est, "N_J_WMDestimates.csv", row.names = F)


### Recruitment
#Individual R for WMD and Year
R.long <- outputs %>%
  filter(substr(X,1,2) == "R[") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
R.est <- R.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)

# #Individual R for Year Only
# R.long <- outputs %>% 
#   filter(substr(X,1,2) == "R[") %>%
#   mutate(Year = str_extract(X, "[:digit:]+")) %>%
#   dplyr::select(Year, mean)


### WMD and Time Specific Harvest Rate Estimates
#Save HR estimates
estvalues.HR.br <- outputs %>%
  rename(ID = X) %>%
  filter(grepl("WMD.HR.", ID)) %>%
  mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\,)")) %>%
  mutate(Year = as.numeric(str_extract(ID, "(?<=\\,).*?(?=\\])"))) %>%
  filter(!is.na(Year)) %>%
  mutate(Age = substr(ID,8,8)) %>%
  filter(!grepl("SS", ID)) %>%
  dplyr::select(mean, Year, WMD, Age) %>%
  pivot_wider(names_from = c(Age, Year), values_from = c(mean), names_sort = T)
mean.est.values.HR <- colMeans(estvalues.HR.br[,2:((n.band.years*2)+1)])

estvalues.HR.ss <- outputs %>%
  rename(ID = X) %>%
  filter(grepl("WMD.HR.", ID)) %>%
  mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\,)")) %>%
  mutate(Year = as.numeric(str_extract(ID, "(?<=\\,).*?(?=\\])"))) %>%
  filter(!is.na(Year)) %>%
  mutate(Age = substr(ID,8,8)) %>%
  filter(grepl("SS", ID)) %>%
  dplyr::select(mean, Year, WMD, Age) %>%
  pivot_wider(names_from = c(Age, Year), values_from = c(mean), names_sort = T)

#Adult
HR.A.long <- outputs %>% 
  filter(substr(X,1,9) == "WMD.HR.A[") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
HR.A.est <- HR.A.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)
# write.csv(HR.A.est, "HR_A_WMDestimates.csv", row.names = F)


#Juvenile
HR.J.long <- outputs %>% 
  filter(substr(X,1,9) == "WMD.HR.J[") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
HR.J.est <- HR.J.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)
# write.csv(HR.J.est, "HR_J_WMDestimates.csv", row.names = F)


### WMD and Time Specific Non Harvest Survival Estimates
#Save WSR estimates
est.WSR <- outputs %>%
  rename(ID = X) %>%
  filter(grepl("WSR_M_", ID)) %>%
  filter(!grepl("WSR_M_J_S2W", ID)) %>% #No Juvenile in S2W so ignore estimate
  mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\])")) %>%
  mutate(Age = substr(ID,7,7)) %>%
  dplyr::select(mean, WMD, Age) %>%
  group_by(Age) %>%
  summarize(Mean.WSR = mean(mean))
mean.est.WSR <- est.WSR$Mean.WSR


# Adult
S.A.long <- outputs %>%
  rename(ID = X) %>%
  filter(grepl("WSR_M_", ID)) %>%
  mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\])")) %>%
  mutate(Age = substr(ID,7,7)) %>%
  filter(Age == "A") %>%
  mutate(Season = substr(ID,9,11)) %>%
  dplyr::select(mean, WMD, Season)
S.A.est <- S.A.long %>%
  pivot_wider(names_from = c(Season), values_from = mean)
# write.csv(S.A.est, "S_A_WMDestimates.csv", row.names = F)


#Juvenile
S.J.long <- outputs %>%
  rename(ID = X) %>%
  filter(grepl("WSR_M_", ID)) %>%
  mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\])")) %>%
  mutate(Age = substr(ID,7,7)) %>%
  filter(Age == "J") %>%
  mutate(Season = substr(ID,9,11)) %>%
  dplyr::select(mean, WMD, Season)
S.J.est <- S.J.long %>%
  pivot_wider(names_from = c(Season), values_from = mean)
# write.csv(S.J.est, "S_J_WMDestimates.csv", row.names = F)


###############################################################################
###################
### Real Values ###
###################
# Calculate Realized Harvest Rates from simulated data
EH.check <- EH_list_br[[1]][,seq(2,(n.band.years*2),2)] #EH for BR, only harvest occasions
true.S.check <- EH_list_br[[2]][,seq(2,(n.band.years*2),2)] #Actual survival of BR
Age.check <- br_adult_hr[,seq(2,(n.band.years*2),2)] #track age through time
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

# Available = Survived from capture to start of hunting season
totalavail.Adult <- colSums(S.check.adult, na.rm = T) + colSums(EH.check.adult, na.rm = T)
totalavail.Juv <- colSums(S.check.juv, na.rm = T) + colSums(EH.check.juv, na.rm = T)
true.hr.adult <- colSums(EH.check.adult, na.rm = T)/totalavail.Adult
true.hr.juv <- colSums(EH.check.juv, na.rm = T)/totalavail.Juv
true.hr <- c(true.hr.adult,true.hr.juv)

#Realized WSR from BR Data
true.S.br.check <- EH_list_br[[2]] #Actual survival from BR
EH.br.check <- EH_list_br[[1]] #EH from BR
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
realized.WSR.br.J <- mean(realized.WSR.br.J.W2S)

EH.br.S2W.check.adult <- EH.br.check.adult
EH.br.S2W.check.adult[,seq(4,(n.band.years*2),2)] <- 0
W2S.br.Aalive <- colSums(S.br.check.adult, na.rm = T)[seq(2,(n.band.years*2),2)]
W2S.br.Jalive <- colSums(S.br.check.juv, na.rm = T)[seq(2,(n.band.years*2),2)]
W2S.br.AendH <- W2S.br.Aalive + W2S.br.Aalive
W2S.br.AaliveC <- colSums(S.br.check.adult, na.rm = T)[seq(1,(n.band.years*2),2)]
W2S.br.Acap <- colSums(EH.br.check.adult, na.rm = T)[seq(1,(n.band.years*2),2)]
W2S.br.AstartC <- W2S.br.AaliveC - W2S.br.Acap
realized.WSR.br.A.S2W <- (W2S.br.AstartC[2:n.band.years]/W2S.br.AaliveC[1:(n.band.years-1)])^(1/36)
realized.WSR.br.A <- mean(c(realized.WSR.br.A.S2W,realized.WSR.br.A.W2S))


#Realized WSR from WSR Data
true.WSR.check <- EH.wsr.list[[2]]
true.WSR.A.check <- matrix(NA, nrow = nbandind, ncol = n.occasions.wsr)
true.WSR.J.check <- matrix(NA, nrow = nbandind, ncol = n.occasions.wsr)
for(checki in 1:nrow(WSR.adult)){
  for(checkj in 1:ncol(WSR.adult)){
    if(WSR.adult1[checki, checkj] == 1){true.WSR.A.check[checki, checkj] <- true.WSR.check[checki, checkj]}
    if(WSR.adult1[checki, checkj] == 0){true.WSR.J.check[checki, checkj] <- true.WSR.check[checki, checkj]}
  }
}

true.wsr.A.1 <- colSums(true.WSR.A.check[,1:(ncol(true.WSR.A.check)-1)], na.rm = T)
true.wsr.A.2 <- colSums(true.WSR.A.check[,2:(ncol(true.WSR.A.check))], na.rm = T)
mean.real.WSR.wsr.A <- sum((true.wsr.A.2/true.wsr.A.1)*(true.wsr.A.1/sum(true.wsr.A.1)), na.rm = T)
true.wsr.J.1 <- colSums(true.WSR.J.check[,1:(ncol(true.WSR.J.check)-1)], na.rm = T)[1:11]
true.wsr.J.2 <- colSums(true.WSR.J.check[,2:(ncol(true.WSR.J.check))], na.rm = T)[1:11]
mean.real.WSR.wsr.J <- sum((true.wsr.J.2/true.wsr.J.1)*(true.wsr.J.1/sum(true.wsr.J.1)), na.rm = T)

mean.real.WSR.A <- mean(c(realized.WSR.br.A, mean.real.WSR.wsr.A))
mean.real.WSR.J <- mean(c(realized.WSR.br.J, mean.real.WSR.wsr.J))
mean.real.WSR <- c(mean.real.WSR.A, mean.real.WSR.J)

#Realized N from Total Harvest Data
real.N.A <- N.A
real.N.J <- N.J


###############################################################################
#########################################
### Compare Real and Estimated Values ###
#########################################
#Estimate Bias between realized and estimated HR
bias.hr <- true.hr - mean.est.values.HR
meanbias.hr.A <- mean(true.hr[1:(length(true.hr)/2)] - mean.est.values.HR[1:(length(true.hr)/2)])
meanbias.hr.J <- mean(true.hr[(1+length(true.hr)/2):length(true.hr)] - mean.est.values.HR[(1+length(true.hr)/2):length(true.hr)])


#Estimate Bias between realized and estimated WSR
bias.wsr <- mean.real.WSR - mean.est.WSR
bias.wsr.A <- bias.wsr[1]
bias.wsr.J <- bias.wsr[2]

#Estimate Bias between realized and estimated N
N.bias.A.diff <- (as.data.frame(real.N.A) - as.data.frame(N.A.est)[,-1])
N.bias.J.diff <- (as.data.frame(real.N.J) - as.data.frame(N.J.est)[,-1])

#Estimate Bias between realized and estimated R
# R.bias <- colMeans(r.vector - R.est[,-1])
R.bias <- matrix(r.vector, ncol = length(r.vector), nrow = C*D, byrow = T) - as.matrix(R.est[,-1])

sink(paste("BiasResults - SingleRun - 3I Trial ",looprun,".csv", sep = ""))
cat(paste("Years BR Data:", n.band.years, sep = " "))
cat('\n')
cat(paste("Number of Individuals in BR Data:", nbandind, sep = " "))
cat('\n')
cat(paste("Additional Years of Total Harvest Data in SS:", n.years.totharv))
cat('\n')
cat(paste("Years WSR Data:", n.years.telem, sep = " "))
cat('\n')
cat(paste("Number of Individuals in WSR Data:", ntelemind, sep = " "))
cat('\n')
cat('\n')
cat('\n')
cat("Average HR Bias (Realized - Estimated)")
cat('\n')
write.csv(bias.hr)
cat('\n')
cat('\n')
cat("Average Estimated HR")
cat('\n')
write.csv(mean.est.values.HR)
cat('\n')
cat('\n')
cat("Average Real HR")
cat('\n')
write.csv(true.hr)
cat('\n')
cat('\n')
cat("Average WSR Bias (Realized - Estimated)")
cat('\n')
write.csv(bias.wsr)
cat('\n')
cat('\n')
cat("Average Estimated WSR")
cat('\n')
write.csv(mean.est.WSR)
cat('\n')
cat('\n')
cat("Average Real WSR")
cat('\n')
write.csv(mean.real.WSR)
cat('\n')
cat('\n')
cat("Average Bias by Year (absolute value) - N.A")
cat('\n')
write.csv(colMeans(abs(N.bias.A.diff)))
cat('\n')
cat('\n')
cat("Bias by Year - N.A")
cat('\n')
write.csv(N.bias.A.diff)
cat('\n')
cat('\n')
cat("Average Bias by Year (absolute value) - N.J")
cat('\n')
write.csv(colMeans(abs(N.bias.J.diff)))
cat('\n')
cat('\n')
cat("Real N.A")
cat('\n')
write.csv(as.data.frame(real.N.A))
cat('\n')
cat('\n')
cat("Estimated N.A")
cat('\n')
write.csv(as.data.frame(N.A.est)[,-1])
cat('\n')
cat('\n')
cat("Real N.J")
cat('\n')
write.csv(as.data.frame(real.N.J))
cat('\n')
cat('\n')
cat("Estimated N.J")
cat('\n')
write.csv(as.data.frame(N.J.est)[,-1])
cat('\n')
cat('\n')
cat("Mean R Bias by year")
cat('\n')
write.csv(R.bias)
cat('\n')
cat('\n')
sink()