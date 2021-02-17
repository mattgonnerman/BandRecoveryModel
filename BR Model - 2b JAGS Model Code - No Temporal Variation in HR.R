#Only estimate 1 HR in the band recovery model, keep temporal variation.
function(){##############################################################################################
  ### Weekly Survival Rate ###
  alpha_s ~ dbeta(1,1)
  intercept_s <- logit(alpha_s)
  beta_F_s ~ dnorm(0,.01) #Effect of sex on WSR (Male reference)
  beta_A_s ~ dnorm(0,.01) #Effect of age on WSR (Juv reference)
  beta_A_F_s ~ dnorm(0,.01) #Interaction term for Age/Sex(Male Juv reference)
  beta_S2W_s ~ dnorm(0,.01) #Effect of S2W (W2S reference)
  for(i in sampledwmd){beta_wmd_s[i] ~ dnorm(0,.01)} #Effect of wmd (W2S reference)
  
  #WSR
  for(i in 1:nvisit){
    eta[i] <- intercept_s +
      beta_F_s*wsr_sex[i] + beta_A_s*wsr_age[i] + beta_A_F_s*wsr_age[i]*wsr_sex[i] +
      beta_S2W_s*wsr_time[i] + beta_wmd_s[wsr_wmd[i]]
    logit(phi[i])<-eta[i] # anti-logit to determine the daily survival rate
    mu[i]<-pow(phi[i],interval[i]) # period survival is DSR raised to the interval
    succ[i]~dbern(mu[i])  # the data is distributed as bernoulli with period survival as the mean
  }
  
  
  ##############################################################################################
  ### Band Recovery ###
  alpha_hr ~ dbeta(1,1)
  intercept_hr <- logit(alpha_hr)
  #beta_year[i] Could code this similarly to how you do wmd, just need a vector/matrix with year of capture
  beta_2019_hr ~ dnorm(0,.01) #Effect of Year on band recovery (2019 vs other)
  beta_2020_hr ~ dnorm(0,.01) #Effect of Year on band recovery (2020 vs other)
  beta_A_hr ~ dnorm(0,.01) #Effect of Age on band recovery (Juv reference)
  
  #Specify period specific survival/band recovery
  for(j in 1:nind){
    for(t in f[j]:n.occasions){
      #Survival Model
      logit(s.br[j,t]) <- intercept_s +
        beta_A_s*br_age_s[j,t] + beta_S2W_s*br_s2w[j,t] + beta_wmd_s[br_wmd[j]]
      #Band Recovery Model
      logit(hr.br[j,t]) <- intercept_hr + beta_A_hr*br_age_hr[j,t] + w.tilde[cap.site[j]] + e.cap[cap.site[j]]
      
      s[j,t] <- ifelse(t == f[j], pow(s.br[j,t], weeks2harv[j]),
                       ifelse(t == 1 || t == 3 || t == 5, pow(s.br[j,t], 11), pow(s.br[j,t], 36)))
      hr[j,t] <- ifelse(t == 1 || t == 3 || t == 5, 0, hr.br[j,t])
      
    } #t
  } #i
  
  # Residual error (w/ corrections)
  for(i in 1:N.cap){
    e.cap[i] ~ dnorm(0, prec[i])
    prec[i] <- 1/var.all[i]
    var.all[i] <- tausq + sigmasq[1] - correction[i]
  }
  
  ##Process##
  for(k in 1:nind){
    w[k,f[k]]<-1 #define true survival state at first capture
    z[k,f[k]]<-1 #define availability for natural risk at first capture
    
    for(t in (f[k]+1):n.occasions){ #Starts first occasion post capture
      z[k,t] ~ dbern(mu1[k,t]) #State process (Does bird survive t-1 to t, natural risk)
      mu1[k,t] <- s[k,t-1]*w[k,t-1] #Probability that a bird survives t-1 to t
      
      y[k,t] ~ dbern(mu2[k,t]) #Observation process (Is bird shot in t)
      mu2[k,t] <- hr[k,t]*z[k,t] #Probability of harvest * if bird was alive
      
      w[k,t] <- z[k,t]-y[k,t] #True latent survival state
    } #t
  } #k
  
  
  ##############################################################################################
  ### Spatial Predictive Process ###
  sigmasq <- 1/sigmasq.inv
  sigmasq.inv ~ dgamma(2,1)
  phi.spp ~ dgamma(1,0.1)
  tausq <- 1/tausq.inv
  tausq.inv ~ dgamma(0.1,0.1)
  
  w.tilde.star[1:N.knot] ~ dmnorm(mu.w.star[1:N.knot], C.star.inv[1:N.knot,1:N.knot])
  C.star.inv[1:N.knot,1:N.knot] = inverse(C.star[1:N.knot,1:N.knot])
  
  for (i in 1:N.knot) {
    mu.w.star[i] = 0
    C.star[i,i] = sigmasq
    for (j in 1:(i-1)) {
      C.star[i,j] = sigmasq*exp(-(d.s.star.star[i,j]/phi.spp))
      C.star[j,i] = C.star[i,j]
    } }
  # Interpolate spatial PP back on to original sites
  for(i in 1:N.cap) {
    for(j in 1:N.knot) {
      C.s.star[i,j] = sigmasq*exp(-(d.s.star[i,j]/phi.spp))
    } }
  w.tilde[1:N.cap] = C.s.star[1:N.cap,1:N.knot]%*%C.star.inv[1:N.knot,1:N.knot]%*%w.tilde.star[1:N.knot]
  #Variance Correction
  for(i in 1:N.cap){
    correction[i] = t(C.s.star[i,1:N.knot])%*%C.star.inv[1:N.knot,1:N.knot]%*%C.s.star[i,1:N.knot]
  }
  
  
  ##############################################################################################
  ### Derived Parameters - HR Combined Years ###
  #Knot specific harvest rates
  for(i in 1:N.knot){
    logit(HR.A.knot[i]) <- intercept_hr + beta_A_hr + w.tilde.star[i]
    logit(HR.J.knot[i]) <- intercept_hr + w.tilde.star[i]
  }
  
  ### WMD Specific Harvest Rates
  for(i in 1:N.wmd){
    mean.WMD.HR.A[WMD.id[i]] <- mean(HR.A.knot[WMD.matrix[i, 1:WMD.vec[i]]])
    mean.WMD.HR.J[WMD.id[i]] <- mean(HR.J.knot[WMD.matrix[i, 1:WMD.vec[i]]])
  }
  
  ### Period Specific Survival
  for(i in sampledwmd){
    logit(WSR_M_J_S2W[i]) <- intercept_s + beta_S2W_s + beta_wmd_s[i]
    logit(WSR_M_A_S2W[i]) <- intercept_s + beta_A_s + beta_S2W_s + beta_wmd_s[i]
    logit(WSR_M_J_W2S[i]) <- intercept_s + beta_wmd_s[i]
    logit(WSR_M_A_W2S[i]) <- intercept_s + beta_A_s + beta_wmd_s[i]
  }
  
  #Survival Estiamtes
  S_M_J_S2W <- pow(mean(WSR_M_J_S2W[sampledwmd]), 36)
  S_M_A_S2W <- pow(mean(WSR_M_A_S2W[sampledwmd]), 36)
  S_M_J_W2S <- pow(mean(WSR_M_J_W2S[sampledwmd]), 11)
  S_M_A_W2S <- pow(mean(WSR_M_A_W2S[sampledwmd]), 11)
  
  ##############################################################################################
  ### State-Space Abundance ###
  for(i in 1:N.wmd){
    for(t in 1:n.years){
      #Total harvest Observation #Temporal Variation in HR
      th.A[WMD.id[i],t] ~ dbin(mean.WMD.HR.A[WMD.id[i]], N.A[WMD.id[i],t]) #Temporal Variation in HR
      th.J[WMD.id[i],t] ~ dbin(mean.WMD.HR.J[WMD.id[i]], round(N.J[WMD.id[i],t])) #Temporal Variation in HR
    }
    
    # Total Annual Survival Probability #Temporal Variation in S and HR
    totalS.A[WMD.id[i]] <- S_M_A_W2S * S_M_A_S2W *(1-mean.WMD.HR.A[WMD.id[i]]) #
    totalS.J[WMD.id[i]] <- S_M_A_W2S * S_M_A_S2W *(1-mean.WMD.HR.J[WMD.id[i]]) # J transition to A after 1st hunting season, hence S.A used
    
    #Need to specify N[t=1], needs to be a whole number.
    #th.year1 are just the harvest totals from year 1
    #This assumes there is at least 10 turkeys in each WMD at the first timestep
    N.A[WMD.id[i],1] ~ dpois((10+th.year1.A[WMD.id[i]])/mean.WMD.HR.A[WMD.id[i]]) #Temporal Variation in HR
    N.J[WMD.id[i],1] ~ dpois((10+th.year1.J[WMD.id[i]])/mean.WMD.HR.J[WMD.id[i]]) #Temporal Variation in HR
    
    for(t in 1:(n.years-1)){
      # Number of birds to A to surive OR J that transition into A from t to t+1
      N.A[WMD.id[i],t+1] <- n.surv.A[WMD.id[i],t] + n.surv.J[WMD.id[i],t] #Requires Specific Starting Parameters
      n.surv.A[WMD.id[i],t] ~ dbin(totalS.A[WMD.id[i]], N.A[WMD.id[i],t]) #Requires Specific Starting Parameters
      n.surv.J[WMD.id[i],t] ~ dbin(totalS.J[WMD.id[i]], N.J[WMD.id[i],t]) #Requires Specific Starting Parameters
      
      #Number of Birds recruited to the Juvenile population in t
      N.J[WMD.id[i],t+1] ~ dpois(meanY1[WMD.id[i],t])
      meanY1[WMD.id[i],t] <- R[WMD.id[i],t] * N.A[WMD.id[i],t] * S_M_J_W2S
      log(R[WMD.id[i],t]) <- alpha.R[WMD.id[i],t]
      alpha.R[WMD.id[i],t] ~ dunif(-10,10)
    }
  }
}




















### Old version, saving just in case

# #Only estimate 1 HR in the band recovery model, keep temporal variation.
# function(){##############################################################################################
#   ### Weekly Survival Rate ###
#   alpha_s ~ dbeta(1,1)
#   intercept_s <- logit(alpha_s)
#   beta_F_s ~ dunif(0,1) #Effect of sex on WSR (Male reference)
#   beta_A_s ~ dunif(0,1) #Effect of age on WSR (Juv reference)
#   beta_A_F_s ~ dunif(0,1) #Interaction term for Age/Sex(Male Juv reference)
#   beta_S2W_s ~ dunif(0,1) #Effect of S2W (W2S reference)
#   for(i in sampledwmd){beta_wmd_s[i] ~ dunif(0,1)} #Effect of wmd (W2S reference)
#   
#   #WSR
#   for(i in 1:nvisit){
#     eta[i] <- intercept_s +
#       beta_F_s*wsr_sex[i] + beta_A_s*wsr_age[i] + beta_A_F_s*wsr_age[i]*wsr_sex[i] +
#       beta_S2W_s*wsr_time[i] + beta_wmd_s[wsr_wmd[i]]
#     logit(phi[i])<-eta[id[i]] # anti-logit to determine the daily survival rate
#     mu[i]<-pow(phi[i],interval[i]) # period survival is DSR raised to the interval
#     succ[i]~dbern(mu[i])  # the data is distributed as bernoulli with period survival as the mean
#   }
#   
#   ##############################################################################################
#   ### Band Recovery ###
#   alpha_hr ~ dbeta(1,1)
#   intercept_hr <- logit(alpha_hr)
#   beta_A_hr ~ dunif(0,1) #Effect of Age on band recovery (Juv reference)
#   beta_spp ~ dnorm(0, 1)
#   
#   #Specify period specific survival/band recovery
#   for(j in 1:nind){
#     for(t in f[j]:n.occasions){
#       #Survival Model
#       logit(s.br[j,t]) <- intercept_s +
#         beta_A_s*br_age[j,t] + beta_S2W_s*br_s2w[j,t] + beta_wmd_s[br_wmd[j]]
#       #Band Recovery Model
#       logit(hr.br[j,t]) <- intercept_hr +
#         beta_A_hr*br_age[j,t] + w.tilde[cap.site[j]] + e.cap[cap.site[j]]
#       
#       s[j,t] <- ifelse(t == f[j], pow(s.br[j,t], weeks2harv[j]),
#                        ifelse(t == 1 || t == 3 || t == 5, pow(s.br[j,t], 11), pow(s.br[j,t], 36)))
#       hr[j,t] <- ifelse(t == 1 || t == 3 || t == 5, 0, hr.br[j,t])
#       
#     } #t
#   } #i
#   
#   # Residual error (w/ corrections)
#   for(i in 1:N.cap){
#     e.cap[i] ~ dnorm(0, prec[i])
#     prec[i] <- 1/var.all[i]
#     var.all[i] <- tausq + sigmasq[1] - correction[i]
#   }
#   
#   ##Process##
#   for(k in 1:nind){
#     w[k,f[k]]<-1 #define true survival state at first capture
#     z[k,f[k]]<-1 #define availability for natural risk at first capture
#     
#     for(t in (f[k]+1):n.occasions){ #Starts first occasion post capture
#       z[k,t] ~ dbern(mu1[k,t]) #State process (Does bird survive t-1 to t, natural risk)
#       mu1[k,t] <- s[k,t-1]*w[k,t-1] #Probability that a bird survives t-1 to t
#       
#       y[k,t] ~ dbern(mu2[k,t]) #Observation process (Is bird shot in t)
#       mu2[k,t] <- hr[k,t]*z[k,t] #Probability of harvest * if bird was alive
#       
#       w[k,t] <- z[k,t]-y[k,t] #True latent survival state
#     } #t
#   } #k
#   
#   ##############################################################################################
#   ### Spatial Predictive Process ###
#   sigmasq <- 1/sigmasq.inv
#   sigmasq.inv ~ dgamma(2,1)
#   phi.spp ~ dgamma(1,0.1)
#   tausq <- 1/tausq.inv
#   tausq.inv ~ dgamma(0.1,0.1)
#   
#   w.tilde.star[1:N.knot] ~ dmnorm(mu.w.star[1:N.knot], C.star.inv[1:N.knot,1:N.knot])
#   C.star.inv[1:N.knot,1:N.knot] = inverse(C.star[1:N.knot,1:N.knot])
#   
#   for (i in 1:N.knot) {
#     mu.w.star[i] = 0
#     C.star[i,i] = sigmasq
#     for (j in 1:(i-1)) {
#       C.star[i,j] = sigmasq*exp(-(d.s.star.star[i,j]/phi.spp))
#       C.star[j,i] = C.star[i,j]
#     } }
#   # Interpolate spatial PP back on to original sites
#   for(i in 1:N.cap) {
#     for(j in 1:N.knot) {
#       C.s.star[i,j] = sigmasq*exp(-(d.s.star[i,j]/phi.spp))
#     } }
#   w.tilde[1:N.cap] = C.s.star[1:N.cap,1:N.knot]%*%C.star.inv[1:N.knot,1:N.knot]%*%w.tilde.star[1:N.knot]
#   #Variance Correction
#   for(i in 1:N.cap){
#     correction[i] = t(C.s.star[i,1:N.knot])%*%C.star.inv[1:N.knot,1:N.knot]%*%C.s.star[i,1:N.knot]
#   }
#   
#   ##############################################################################################
#   ### Derived Parameters ###
#   #Capture Site specific harvest rates
#   for(i in 1:N.cap){
#     logit(HR.A.cap[i]) <- intercept_hr + beta_A_hr + w.tilde[i] + e.cap[i]
#     logit(HR.J.cap[i]) <- intercept_hr + w.tilde[i] + e.cap[i]
#   }
#   
#   #Knot specific harvest rates
#   for(i in 1:N.knot){
#     logit(HR.A.knot[i]) <- intercept_hr + beta_A_hr + w.tilde.star[i]
#     logit(HR.J.knot[i]) <- intercept_hr + w.tilde.star[i]
#   }
#   
#   ### WMD Specific Harvest Rates
#   for(i in 1:N.wmd){
#     mean.WMD.HR.J[WMD.id[i]] <- mean(HR.J.knot[WMD.matrix[i, 1:WMD.vec[i]]])
#     mean.WMD.HR.A[WMD.id[i]] <- mean(HR.A.knot[WMD.matrix[i, 1:WMD.vec[i]]])
#   }
#   
#   ### Period Specific Survival
#   for(i in sampledwmd){
#     logit(WSR_M_J_S2W[i]) <- intercept_s + beta_S2W_s + beta_wmd_s[i]
#     logit(WSR_M_A_S2W[i]) <- intercept_s + beta_A_s + beta_S2W_s + beta_wmd_s[i]
#     logit(WSR_M_J_W2S[i]) <- intercept_s + beta_wmd_s[i]
#     logit(WSR_M_A_W2S[i]) <- intercept_s + beta_A_s + beta_wmd_s[i]
#   }
#   
#   #Averaged Period Specific Survival for all WMDs sampled
#   S_M_J_S2W <- pow(mean(WSR_M_J_S2W[sampledwmd]), 36)
#   S_M_A_S2W <- pow(mean(WSR_M_A_S2W[sampledwmd]), 36)
#   S_M_J_W2S <- pow(mean(WSR_M_J_W2S[sampledwmd]), 11)
#   S_M_A_W2S <- pow(mean(WSR_M_A_W2S[sampledwmd]), 11)
#   
#   #Average Non Harvest Survival by WMD
#   mean.AnnualS.A <- S_M_A_W2S * S_M_A_S2W
#   mean.AnnualS.J <- S_M_J_W2S * S_M_J_S2W
#   
#   ##############################################################################################
#   ### State-Space Abundance ###
#   #Total Harvest Observation Error
#   tau.obs.A <- pow(sigma.obs.A, -2)
#   sigma.obs.A ~ dunif(0,100)
#   tau.obs.J <- pow(sigma.obs.J, -2)
#   sigma.obs.J ~ dunif(0,100)
#   
#   #Survival Annual Process Error
#   tau.surv.A <- pow(sigma.surv.A, -2)
#   sigma.surv.A ~ dunif(0,1)
#   tau.surv.J <- pow(sigma.surv.J, -2)
#   sigma.surv.J ~ dunif(0,1)
#   
#   #Harvest Rate Annual Process Error
#   tau.harv.A <- pow(sigma.harv.A, -2)
#   sigma.harv.A ~ dunif(0,1)
#   tau.harv.J <- pow(sigma.harv.J, -2)
#   sigma.harv.J ~ dunif(0,1)
#   
#   for(i in 1:N.wmd){
#     for(t in 1:n.years){
#       # Total harvest Observation #No Temporal Variation in HR
#       th.A[WMD.id[i],t] ~ dbin(mean.WMD.HR.A[WMD.id[i]], N.A[WMD.id[i],t]) #No Temporal Variation in HR
#       th.J[WMD.id[i],t] ~ dbin(mean.WMD.HR.J[WMD.id[i]], N.J[WMD.id[i],t]) #No Temporal Variation in HR
#       
#       #Temporal Variation in probability of surviving non harvest risk in a year #Temporal Variation in S
#       logit(AnnualS.A[WMD.id[i],t]) <- l.AnnualS.A[WMD.id[i],t] #Temporal Variation in S
#       logit(AnnualS.J[WMD.id[i],t]) <- l.AnnualS.J[WMD.id[i],t] #Temporal Variation in S
#       
#       l.AnnualS.A[WMD.id[i],t] ~ dnorm(logit(mean.AnnualS.A), tau.surv.A) #Temporal Variation in S
#       l.AnnualS.J[WMD.id[i],t] ~ dnorm(logit(mean.AnnualS.J), tau.surv.J) #Temporal Variation in S
#       
#       # Total Annual Survival Probability #No Temporal Variation in HR
#       totalS.A[WMD.id[i],t] <- AnnualS.A[WMD.id[i],t]*(1-mean.WMD.HR.A[WMD.id[i]]) #No Temporal Variation in HR
#       totalS.J[WMD.id[i],t] <- AnnualS.J[WMD.id[i],t]*(1-mean.WMD.HR.J[WMD.id[i]]) #No Temporal Variation in HR
#     }
#     
#     #Need to specify N[t=1], needs to be a whole number.
#     #th.year1 are just the harvest totals from year 1
#     #This assumes there is at least 10 turkeys in each WMD at the first timestep
#     N.A[WMD.id[i],1] <- round(((10+th.year1.A[WMD.id[i]])/mean.WMD.HR.A[WMD.id[i]])) #No Temporal Variation in HR
#     N.J[WMD.id[i],1] <- round(((10+th.year1.J[WMD.id[i]])/mean.WMD.HR.J[WMD.id[i]])) #No Temporal Variation in HR
#     # #N.A[WMD.id[i],1] ~ dpois((round(((10+th.year1.A[WMD.id[i]])/mean.WMD.HR.A[WMD.id[i]])))) #No Temporal Variation in HR
#     # #N.J[WMD.id[i],1] ~ dpois((round(((10+th.year1.J[WMD.id[i]])/mean.WMD.HR.J[WMD.id[i]])))) #No Temporal Variation in HR
#     
#     for(t in 1:(n.years-1)){
#       # Number of birds to A to surive OR J that transition into A from t to t+1
#       N.A[WMD.id[i],t+1] <- n.surv.A[WMD.id[i],t] + n.surv.J[WMD.id[i],t] #Requires Specific Starting Parameters
#       n.surv.A[WMD.id[i],t] ~ dbin(totalS.A[WMD.id[i],t], N.A[WMD.id[i],t]) #Requires Specific Starting Parameters
#       n.surv.J[WMD.id[i],t] ~ dbin(totalS.J[WMD.id[i],t], N.J[WMD.id[i],t]) #Requires Specific Starting Parameters
#       # n.surv.A[WMD.id[i],t] <- totalS.A[WMD.id[i]] * N.A[WMD.id[i],t] #No Temporal Variation in HR or S
#       # n.surv.J[WMD.id[i],t] <- totalS.J[WMD.id[i]] * N.J[WMD.id[i],t] #No Temporal Variation in HR or S
#       # N.A[WMD.id[i],t+1] <- round(1+(totalS.A[WMD.id[i],t] * N.A[WMD.id[i],t]) + (totalS.J[WMD.id[i],t] * N.J[WMD.id[i],t])) #Issues initializing
#       
#       #Number of Birds recruited to the Juvenile population in t
#       N.J[WMD.id[i],t+1] ~ dpois(meanY1[WMD.id[i],t])
#       # meanY1[WMD.id[i],t] <- 10 + (R[WMD.id[i],t] * N.A[WMD.id[i],t]) #This was 11 when it ran well
#       meanY1[WMD.id[i],t] <- (R[WMD.id[i],t] * N.A[WMD.id[i],t]) #This was 11 when it ran well
#       
#       #Year Specific recruitment rate
#       R[WMD.id[i],t] ~ dlnorm(log(mean.R[WMD.id[i]]), tau.R)
#     }
#     #Average Recruitment Rate, WMD specific
#     mean.R[WMD.id[i]] ~ dunif(0.1,2)
#   }
#   #Recruitment Process Error
#   tau.R <- pow(sigma.R, -2)
#   sigma.R ~ dunif(0,10)
# }