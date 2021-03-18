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