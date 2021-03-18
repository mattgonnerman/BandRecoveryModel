### Real Parameter Values ###

require(dplyr)
require(stringr)
require(tidyr)
require(miscTools)

###################
### Real Values ###
###################
### Harvest Rates
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
true.hr <- data.frame(Age = c(rep("A", length(true.hr.adult)),rep("J", length(true.hr.adult))),
                      Year = rep(1:length(true.hr.adult), 2),
                      Mean = c(true.hr.adult,true.hr.juv))


### WSR from BR Data
true.S.br.check <- EH_list_br[[2]] #Actual survival from BR
EH.br.check <- EH_list_br[[1]] #EH from BR
EH.br.check.adult <- matrix(NA, nrow = nbandind, ncol = n.band.years*2)
EH.br.check.juv <- matrix(NA, nrow = nbandind, ncol = n.band.years*2)
S.br.check.adult <- matrix(NA, nrow = nbandind, ncol = n.band.years*2)
S.br.check.juv <- matrix(NA, nrow = nbandind, ncol = n.band.years*2)
S.br.check.juv.preC1 <- matrix(NA, nrow = nbandind, ncol = n.band.years*2) #To account for the later transition of J

for(checki in 1:nbandind){
  for(checkj in 1:(n.band.years*2)){
    if(br_adult_hr[checki, checkj] == 1){EH.br.check.adult[checki, checkj] <- EH.br.check[checki, checkj]}
    if(br_adult_hr[checki, checkj] == 0){EH.br.check.juv[checki, checkj] <- EH.br.check[checki, checkj]}
    if(br_adult_hr[checki, checkj] == 1){S.br.check.adult[checki, checkj] <- true.S.br.check[checki, checkj]}
    if(br_adult_hr[checki, checkj] == 0){S.br.check.juv[checki, checkj] <- true.S.br.check[checki, checkj]}
  }
  for(checkj in 2:(n.band.years*2)){ #Addition for when J transition at 2nd capture season
    if(br_adult_hr[checki, checkj-1] == 0 & br_adult_hr[checki, checkj] == 1){
      S.br.check.juv.preC1[checki, checkj] <- true.S.br.check[checki, checkj]
    }
  }
}
S.br.check.juv.preC1[,seq(2,(n.band.years*2),2)] <- NA

##Juvenile W2S Survival
EH.br.W2S.check.juv <- EH.br.check.juv
EH.br.W2S.check.juv[,seq(1,(n.band.years*2),2)] <- 0
W2S.br.J.totals <- colSums(S.br.check.juv + EH.br.W2S.check.juv, na.rm = T) #Need to add individuals who were shot in H
realized.WSR.br.J.W2S <- (W2S.br.J.totals[seq(2,(n.band.years*2),2)]/W2S.br.J.totals[seq(1,(n.band.years*2),2)])^(1/11)
##Juvenile S2W Survival
S.br.check.juv.postH <- colSums(S.br.check.juv, na.rm = T)[seq(2,(n.band.years*2-1),2)]
S.br.check.juv.preC <- colSums(S.br.check.juv.preC1, na.rm = T)[seq(3,(n.band.years*2),2)]
realized.WSR.br.J.S2W <- (S.br.check.juv.preC/S.br.check.juv.postH)^(1/36)
#Mean Juvenile Survival
realized.WSR.br.J <- mean(c(realized.WSR.br.J.W2S,realized.WSR.br.J.S2W))

#Adult W2S Survival
EH.br.W2S.check.adult <- EH.br.check.adult
EH.br.W2S.check.adult[,seq(1,(n.band.years*2),2)] <- 0
W2S.br.A.totals <- colSums(S.br.check.adult + EH.br.W2S.check.adult, na.rm = T) #Need to add individuals who were shot in H
realized.WSR.br.A.W2S <- (W2S.br.A.totals[seq(2,(n.band.years*2),2)]/W2S.br.A.totals[seq(1,(n.band.years*2),2)])^(1/11)
#Adult S2W Survival 
W2S.br.Aalive <- colSums(S.br.check.adult, na.rm = T)[seq(3,(n.band.years*2),2)]
W2S.br.Jtrans <- colSums(S.br.check.juv.preC1, na.rm = T)[seq(3,(n.band.years*2),2)]
W2S.br.ACap <- hr.ind.cap %>% filter(Adult==1) %>% group_by(CapYear) %>% summarize(Total = n())
W2S.br.ASurv <- W2S.br.Aalive - W2S.br.ACap$Total[2:n.band.years] - W2S.br.Jtrans
W2S.br.Aavail <- colSums(S.br.check.adult, na.rm = T)[seq(2,(n.band.years*2)-1,2)]
realized.WSR.br.A.S2W <- (W2S.br.ASurv/W2S.br.Aavail)^(1/36)
#Mean Adult Survival
realized.WSR.br.A <- mean(c(realized.WSR.br.A.S2W,realized.WSR.br.A.W2S))


### WSR from WSR Data
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


### Combined WSR Average
mean.real.WSR.A <- mean(c(realized.WSR.br.A, mean.real.WSR.wsr.A))
mean.real.WSR.J <- mean(c(realized.WSR.br.J, mean.real.WSR.wsr.J))
mean.real.WSR <- data.frame(Age = c("A", "J"), Mean = c(mean.real.WSR.A, mean.real.WSR.J))


### N Values
real.N.A <- N.A[,6:ncol(N.A)]
real.N.J <- N.J[,6:ncol(N.A)]


### R Values
real.R <- data.frame(Year = 1:(ncol(N.A)-6),
                     Mean = r.vector[6:(ncol(N.A)-1)])


### Save Real Values
sink(paste("Model Bias Comparison/SampleSize/Trial ",looprun," - RealParameterValues.csv", sep = ""))
cat("Average Real HR")
cat('\n')
write.csv(true.hr, row.names = F)
cat('\n')
cat('\n')
cat("Average Real WSR")
cat('\n')
write.csv(mean.real.WSR, row.names = F)
cat('\n')
cat('\n')
cat("Real N.A")
cat('\n')
write.csv(real.N.A, row.names = F)
cat('\n')
cat('\n')
cat("Real N.J")
cat('\n')
write.csv(real.N.J, row.names = F)
cat('\n')
cat('\n')
cat("Real R")
cat('\n')
write.csv(real.R, row.names = F)
cat('\n')
cat('\n')
sink()