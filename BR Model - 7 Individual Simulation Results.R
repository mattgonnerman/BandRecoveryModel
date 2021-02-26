#Examine Model Outputs more easily
# outputs <- as.data.frame(BR_w_SPP_output$BUGSoutput$summary)
# outputs$X <- rownames(outputs)
outputs <- read.csv("3J_output.csv")
require(dplyr)
require(stringr)
require(tidyr)
require(miscTools)
require(ggplot2)

#######################
### Estimate Values ###
#######################
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

ggplot(data = N.A.long, aes(y = mean, x = Year, group = WMD)) +
  geom_line(aes(color = WMD), alpha = .8, size = 1.2)
ggsave("N_A_WMDestimates.jpeg", width = 12, height = 8, units = "in")

ggplot(data = N.A.long, aes(y = mean, x = as.factor(Year), group = WMD)) +
  stat_smooth(method = lm, aes(color = WMD, fill = WMD), alpha = .1)+
  geom_point(aes(color = WMD), alpha = .8, size = 1.2)
ggsave("N_A_WMDtrends.jpeg", width = 12, height = 8, units = "in")

write.csv(N.A.est, "N_A_WMDestimates.csv", row.names = F)


#Juvenile
N.J.long <- outputs %>% 
  filter(substr(X,1,3) == "N.J") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
N.J.est <- N.J.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)
N.J.total <- data.frame(mean = colSums(N.J.est[2:ncol(N.J.est)]),
                        Year = min(N.A.long$Year):max(N.J.long$Year),
                        WMD = "Total")

ggplot(data = N.J.long, aes(y = mean, x = as.factor(Year), group = WMD)) +
  geom_line(aes(color = WMD), alpha = .8, size = 1.2)
ggsave("N_J_WMDestimates.jpeg", width = 12, height = 8, units = "in")

ggplot(data = N.J.long, aes(y = mean, x = as.factor(Year), group = WMD)) +
  stat_smooth(method = lm, aes(color = WMD, fill = WMD), alpha = .1)+
  geom_point(aes(color = WMD), alpha = .8, size = 1.2)
ggsave("N_J_WMDtrends.jpeg", width = 12, height = 8, units = "in")

write.csv(N.J.est, "N_J_WMDestimates.csv", row.names = F)


### Recruitment
R.long <- outputs %>% 
  filter(substr(X,1,2) == "R[") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
R.est <- R.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)
ggplot(data = R.long, aes(y = mean, x = as.factor(Year), group = WMD)) +
  geom_line(aes(color = WMD), alpha = .8, size = 1.2)
ggsave("R_WMDestimates.jpeg", width = 12, height = 8, units = "in")
write.csv(R.est, "R_WMDestimates.csv", row.names = F)


### WMD and Time Specific Harvest Rate Estimates
#Adult
HR.A.long <- outputs %>% 
  filter(substr(X,1,8) == "WMD.HR.A") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
HR.A.est <- HR.A.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)
ggplot(data = HR.A.long, aes(y = mean, x = as.factor(Year), group = WMD)) +
  geom_line(aes(color = WMD), alpha = .8, size = 1.2)
ggsave("HR_A_WMDestimates.jpeg", width = 12, height = 8, units = "in")
write.csv(HR.A.est, "HR_A_WMDestimates.csv", row.names = F)


#Juvenile
HR.J.long <- outputs %>% 
  filter(substr(X,1,8) == "WMD.HR.J") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
HR.J.est <- HR.J.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)
ggplot(data = HR.J.long, aes(y = mean, x = as.factor(Year), group = WMD)) +
  geom_line(aes(color = WMD), alpha = .8, size = 1.2)
ggsave("HR_J_WMDestimates.jpeg", width = 12, height = 8, units = "in")
write.csv(HR.J.est, "HR_J_WMDestimates.csv", row.names = F)


### WMD and Time Specific Non Harvest Survival Estimates
# Adult
S.A.long <- outputs %>% 
  filter(substr(X,1,8) == "totalS.A") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
S.A.est <- S.A.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)
ggplot(data = S.A.long, aes(y = mean, x = as.factor(Year), group = WMD)) +
  geom_line(aes(color = WMD), alpha = .8, size = 1.2)
ggsave("S_A_WMDestimates.jpeg", width = 12, height = 8, units = "in")
write.csv(S.A.est, "S_A_WMDestimates.csv", row.names = F)


#Juvenile
S.J.long <- outputs %>% 
  filter(substr(X,1,8) == "totalS.J") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
S.J.est <- S.J.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)
ggplot(data = S.J.long, aes(y = mean, x = as.factor(Year), group = WMD)) +
  geom_line(aes(color = WMD), alpha = .8, size = 1.2)
ggsave("S_J_WMDestimates.jpeg", width = 12, height = 8, units = "in")
write.csv(S.J.est, "S_J_WMDestimates.csv", row.names = F)




###############################################################################
###############################################################################
#######################
### RHat Values ###
#######################
### Total Model Rhat
ggplot(data = outputs, aes(x = Rhat)) +
  geom_histogram() +
  xlim(min(outputs$Rhat) - .1, ifelse(max(outputs$Rhat) > 2, max(outputs$Rhat) + .3, 2)) +
  labs(title = "Total Tracked Parameters Convergence")
ggsave("FullTracked_RHat.jpeg", width = 8, height = 8, units = "in")


### WMD and Time Specific Abundance Rhat
# Adult
N.A.rhat <- outputs %>% 
  filter(substr(X,1,3) == "N.A") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, Rhat, WMD)
ggplot(data = N.A.rhat, aes(x = Rhat)) +
  geom_histogram() +
  xlim(min(outputs$Rhat) - .1, ifelse(max(outputs$Rhat) > 2, max(outputs$Rhat) + .3, 2)) +
  labs(title = "WMD and Time Specific N.A Convergence")
ggsave("N_A_RHat.jpeg", width = 8, height = 8, units = "in")


#Juvenile
N.J.rhat <- outputs %>% 
  filter(substr(X,1,3) == "N.J") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, Rhat, WMD)
ggplot(data = N.J.rhat, aes(x = Rhat)) +
  geom_histogram() +
  xlim(min(outputs$Rhat) - .1, ifelse(max(outputs$Rhat) > 2, max(outputs$Rhat) + .3, 2)) +
  labs(title = "WMD and Time Specific N.J Convergence")
ggsave("N_J_RHat.jpeg", width = 8, height = 8, units = "in")


### Recruitment
R.rhat <- outputs %>% 
  filter(substr(X,1,2) == "R[") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, Rhat, WMD)
ggplot(data = R.rhat, aes(x = Rhat)) +
  geom_histogram() +
  xlim(min(outputs$Rhat) - .1, ifelse(max(outputs$Rhat) > 2, max(outputs$Rhat) + .3, 2)) +
  labs(title = "WMD and Time Specific R Convergence")
ggsave("R_RHat.jpeg", width = 8, height = 8, units = "in")


### WMD and Time Specific Harvest Rate Estimates
#Adult
HR.A.rhat <- outputs %>% 
  filter(substr(X,1,8) == "WMD.HR.A") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, Rhat, WMD)
ggplot(data = HR.A.rhat, aes(x = Rhat)) +
  geom_histogram() +
  xlim(min(outputs$Rhat) - .1, ifelse(max(outputs$Rhat) > 2, max(outputs$Rhat) + .3, 2)) +
  labs(title = "WMD and Time Specific HR.A Convergence")
ggsave("HR_A_RHat.jpeg", width = 8, height = 8, units = "in")


#Juvenile
HR.J.rhat <- outputs %>% 
  filter(substr(X,1,8) == "WMD.HR.J") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, Rhat, WMD)
ggplot(data = HR.J.rhat, aes(x = Rhat)) +
  geom_histogram() +
  xlim(min(outputs$Rhat) - .1, ifelse(max(outputs$Rhat) > 2, max(outputs$Rhat) + .3, 2)) +
  labs(title = "WMD and Time Specific HR.J Convergence")
ggsave("HR_J_RHat.jpeg", width = 8, height = 8, units = "in")


### WMD and Time Specific Non Harvest Survival Estimates
# Adult
S.A.rhat <- outputs %>% 
  filter(substr(X,1,8) == "totalS.A") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, Rhat, WMD)
ggplot(data = S.A.rhat, aes(x = Rhat)) +
  geom_histogram() +
  xlim(min(outputs$Rhat) - .1, ifelse(max(outputs$Rhat) > 2, max(outputs$Rhat) + .3, 2)) +
  labs(title = "WMD and Time Specific S.A Convergence")
ggsave("S_A_RHat.jpeg", width = 8, height = 8, units = "in")


#Juvenile
S.J.rhat <- outputs %>% 
  filter(substr(X,1,8) == "totalS.J") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, Rhat, WMD)
ggplot(data = S.J.rhat, aes(x = Rhat)) +
  geom_histogram() +
  xlim(min(outputs$Rhat) - .1, ifelse(max(outputs$Rhat) > 2, max(outputs$Rhat) + .3, 2)) +
  labs(title = "WMD and Time Specific S.J Convergence")
ggsave("S_J_RHat.jpeg", width = 8, height = 8, units = "in")


###############################################################################
###############################################################################
#########################################
### Compare Estimated and Real Values ###
#########################################
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
    if(WSR.sex1[checki, checkj] == 1){true.WSR.A.check[checki, checkj] <- NA}
    if(WSR.sex1[checki, checkj] == 1){true.WSR.J.check[checki, checkj] <- NA}
  }
}

true.wsr.A.1 <- colSums(true.WSR.A.check[,1:(ncol(true.WSR.A.check)-1)], na.rm = T)
true.wsr.A.2 <- colSums(true.WSR.A.check[,2:(ncol(true.WSR.A.check))], na.rm = T)
mean.real.WSR.wsr.A <- sum((true.wsr.A.2/true.wsr.A.1)*(true.wsr.A.1/sum(true.wsr.A.1)), na.rm = T)
true.wsr.J.1 <- colSums(true.WSR.J.check[,1:(ncol(true.WSR.J.check)-1)], na.rm = T)[1:11]
true.wsr.J.2 <- colSums(true.WSR.J.check[,2:(ncol(true.WSR.J.check))], na.rm = T)[1:11]
mean.real.WSR.wsr.J <- sum((true.wsr.J.2/true.wsr.J.1)*(true.wsr.J.1/sum(true.wsr.J.1)), na.rm = T)
mean.real.WSR <- mean(c(mean.real.WSR.wsr[looprun,1], mean.real.WSR.br[looprun,1]))
mean.real.WSR <- mean(c(mean.real.WSR.wsr[looprun,2], mean.real.WSR.br[looprun,2]))

#Save HR estimates
estvalues.HR <- as.data.frame(BR_w_SPP_output$BUGSoutput$summary) %>%
  mutate(ID = rownames(BR_w_SPP_output$BUGSoutput$summary)) %>%
  filter(grepl("WMD.HR.", ID)) %>%
  mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\,)")) %>%
  mutate(Year = as.numeric(str_extract(ID, "(?<=\\,).*?(?=\\])"))) %>%
  mutate(Age = substr(ID,8,8)) %>%
  dplyr::select(mean, Year, WMD, Age) %>%
  pivot_wider(names_from = c(Age, Year), values_from = c(mean), names_sort = T)
mean.est.values.HR <- colMeans(estvalues.HR[,2:((n.band.years*2)+1)])

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


#Realized N from Total Harvest Data
real.N.A <- N.A
real.N.J <- N.J


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

#Estimate Bias between realized and estimated N
(as.data.frame(real.N.A) - as.data.frame(N.A.est)[,-1])/as.data.frame(real.N.A)

