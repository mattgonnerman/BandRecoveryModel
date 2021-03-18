### Calculate Estimated Parameter Bias Compared to Real Values ###

#Estimate Bias between realized and estimated HR
bias.hr <- data.frame(Trial = looprun,
                      Age = true.hr$Age,
                      Year = true.hr$Year,
                      Real = true.hr$Mean,
                      Est = mean.est.HR$Mean,
                      Bias = true.hr$Mean - mean.est.HR$Mean)


#Estimate Bias between realized and estimated WSR
bias.wsr <- data.frame(Trial = looprun,
                       Age = c("A", "J"),
                       Real = mean.real.WSR$Mean,
                       Est = mean.est.WSR$Mean,
                       Bias = mean.real.WSR$Mean - mean.est.WSR$Mean)


#Estimate Bias between realized and estimated N
N.bias.A.diff <- as.data.frame(real.N.A - N.A.est[,-1]) %>%
  mutate(WMD = 1:(C*D),
         Trial = looprun,
         Age = "A") %>%
  dplyr::select(Trial, WMD, Age, 1:ncol(real.N.A))
N.bias.J.diff <- as.data.frame(real.N.J - N.J.est[,-1]) %>%
  mutate(WMD = 1:(C*D),
         Trial = looprun, 
         Age = "J") %>%
  dplyr::select(Trial, WMD, Age, 1:ncol(real.N.A))

N.bias <- rbind(N.bias.A.diff, N.bias.J.diff)


#Estimate Bias between realized and estimated R
# R.bias <- colMeans(r.vector - R.est[,-1])
R.bias <- as.data.frame(matrix(r.vector[6:length(r.vector)], ncol = length(r.vector)-5, nrow = C*D, byrow = T) - as.matrix(R.est[,-1]))
R.bias.df <- data.frame(Trial = looprun,
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
sink(paste("Model Bias Comparison/SampleSize/Trial ",looprun," - ParameterBias.csv", sep = ""))
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