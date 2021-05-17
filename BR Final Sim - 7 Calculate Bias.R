### Calculate Estimated Parameter Bias Compared to Real Values ###

#Estimate Bias between realized and estimated HR
bias.hr <- data.frame(Parameter = trialname,
                      Trial = looprun,
                      Age = true.hr$Age,
                      Year = true.hr$Year,
                      Real = true.hr$Mean,
                      Est = mean.est.HR$Mean,
                      Bias = true.hr$Mean - mean.est.HR$Mean)


#Estimate Bias between realized and estimated WSR
bias.wsr <- data.frame(Parameter = trialname,
                       Trial = looprun,
                       Age = c("A", "J"),
                       Real = mean.real.WSR$Mean,
                       Est = mean.est.WSR$Mean,
                       Bias = mean.real.WSR$Mean - mean.est.WSR$Mean)


#Estimate Bias between realized and estimated N
#Adult
N.abs.bias.A.diff <- as.data.frame(N.A.est[,-1] - real.N.A) %>%
  mutate(WMD = 1:(C*D),
         Parameter = trialname,
         Trial = looprun,
         BiasType = "Abs.Bias",
         Age = "A") %>%
  dplyr::select(Parameter, Trial, BiasType, WMD, Age, 1:ncol(real.N.A))
N.rel.bias.A.diff <- as.data.frame( (N.A.est[,-1] - real.N.A)/real.N.A) %>%
  mutate(WMD = 1:(C*D),
         Parameter = trialname,
         Trial = looprun,
         BiasType = "Rel.Bias",
         Age = "A") %>%
  dplyr::select(Parameter, Trial, BiasType, WMD, Age, 1:ncol(real.N.A))

N.abs.bias.A.diff.Total <- as.data.frame(t(colSums(N.A.est[,-1]) - colSums(real.N.A))) %>%
  mutate(WMD = -999,
         Parameter = trialname,
         Trial = looprun,
         BiasType = "Abs.Bias",
         Age = "A") %>%
  dplyr::select(Parameter, Trial, BiasType, WMD, Age, 1:ncol(real.N.A))

N.rel.bias.A.diff.Total <- as.data.frame(t((colSums(N.A.est[,-1]) - colSums(real.N.A))/colSums(real.N.A))) %>%
  mutate(WMD = -999,
         Parameter = trialname,
         Trial = looprun,
         BiasType = "Rel.Bias",
         Age = "A") %>%
  dplyr::select(Parameter, Trial, BiasType, WMD, Age, 1:ncol(real.N.A))

#Juvenile
N.abs.bias.J.diff <- as.data.frame(N.J.est[,-1] - real.N.J) %>%
  mutate(WMD = 1:(C*D),
         Parameter = trialname,
         Trial = looprun,
         BiasType = "Abs.Bias",
         Age = "J") %>%
  dplyr::select(Parameter, Trial, BiasType, WMD, Age, 1:ncol(real.N.J))
N.rel.bias.J.diff <- as.data.frame( (N.J.est[,-1] - real.N.J)/real.N.J) %>%
  mutate(WMD = 1:(C*D),
         Parameter = trialname,
         Trial = looprun,
         BiasType = "Rel.Bias",
         Age = "J") %>%
  dplyr::select(Parameter, Trial, BiasType, WMD, Age, 1:ncol(real.N.J))

N.abs.bias.J.diff.Total <- as.data.frame(t(colSums(N.J.est[,-1]) - colSums(real.N.J))) %>%
  mutate(WMD = -999,
         Parameter = trialname,
         Trial = looprun,
         BiasType = "Abs.Bias",
         Age = "J") %>%
  dplyr::select(Parameter, Trial, BiasType, WMD, Age, 1:ncol(real.N.J))

N.rel.bias.J.diff.Total <- as.data.frame(t((colSums(N.J.est[,-1]) - colSums(real.N.J))/colSums(real.N.J))) %>%
  mutate(WMD = -999,
         Parameter = trialname,
         Trial = looprun,
         BiasType = "Rel.Bias",
         Age = "J") %>%
  dplyr::select(Parameter, Trial, BiasType, WMD, Age, 1:ncol(real.N.J))

N.bias <- rbind(N.abs.bias.A.diff, N.rel.bias.A.diff, N.abs.bias.A.diff.Total, N.rel.bias.A.diff.Total,
                N.abs.bias.J.diff, N.rel.bias.J.diff, N.abs.bias.J.diff.Total, N.rel.bias.J.diff.Total)


#Estimate Bias between realized and estimated R
# R.bias <- colMeans(r.vector - R.est[,-1])
R.bias <- as.matrix(real.R) - as.matrix(R.est[,-1])
R.bias.df <- data.frame(Parameter = trialname,
                        Trial = looprun,
                        WMD = 1:(C*D))
R.bias.df <- cbind(R.bias.df, R.bias)

#Master Bias File
if(looprun == 1){
  master.bias.hr <- bias.hr
  master.bias.wsr <- bias.wsr
  master.bias.N <- N.bias
  master.bias.R <- R.bias.df
}else{
  master.bias.hr <- rbind(master.bias.hr, bias.hr)
  master.bias.wsr <- rbind(master.bias.wsr, bias.wsr)
  master.bias.N <- plyr::rbind.fill(master.bias.N, N.bias)
  master.bias.R <- plyr::rbind.fill(master.bias.R, R.bias.df)
}

### Save Bias Values
sink(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/",
           trialname, " - Sim",
           looprun, " - ParameterBias.csv", sep = ""))
cat("Bias HR")
cat('\n')
write.csv(bias.hr, row.names = F)
cat('\n')
cat('\n')
cat("Real WSR")
cat('\n')
write.csv(bias.wsr, row.names = F)
cat('\n')
cat('\n')
cat("Bias N")
cat('\n')
write.csv(N.bias, row.names = F)
cat('\n')
cat('\n')
cat("Bias R")
cat('\n')
write.csv(R.bias.df, row.names = F)
cat('\n')
cat('\n')
sink()

write.csv(master.bias.hr, paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/", trialname, " - Master HR Bias.csv", sep = ""), row.names = F)
write.csv(master.bias.wsr, paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/", trialname, " - Master WSR Bias.csv", sep = ""), row.names = F)
write.csv(master.bias.N, paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/", trialname, " - Master N Bias.csv", sep = ""), row.names = F)
write.csv(master.bias.R, paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/", trialname, " - Master R Bias.csv", sep = ""), row.names = F)