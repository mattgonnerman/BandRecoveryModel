function(){##############################################################################################
  ### Weekly Survival Rate ###
  alpha_s ~ dbeta(1,1)
  intercept_s <- logit(alpha_s)
  beta_F_s ~ dunif(0,1) #Effect of sex on WSR (Male reference)
  beta_A_s ~ dunif(0,1) #Effect of age on WSR (Juv reference)
  beta_A_F_s ~ dunif(0,1) #Interaction term for Age/Sex(Male Juv reference)
  beta_S2W_s ~ dunif(0,1) #Effect of S2W (W2S reference)
  for(i in sampledwmd){beta_wmd_s[i] ~ dunif(0,1)} #Effect of wmd (W2S reference)
  
  #WSR
  for(i in 1:nvisit){
    eta[i] <- intercept_s + 
      beta_F_s*wsr_sex[i] + beta_A_s*wsr_age[i] + beta_A_F_s*wsr_age[i]*wsr_sex[i] + 
      beta_S2W_s*wsr_time[i] + beta_wmd_s[wsr_wmd[i]]
    logit(phi[i])<-eta[id[i]] # anti-logit to determine the daily survival rate
    mu[i]<-pow(phi[i],interval[i]) # period survival is DSR raised to the interval
    succ[i]~dbern(mu[i])  # the data is distributed as bernoulli with period survival as the mean
  }
  
  ##############################################################################################
  ### Band Recovery ###
  alpha_hr ~ dbeta(1,1)
  intercept_hr <- logit(alpha_hr)
  beta_A_hr ~ dunif(0,1) #Effect of Age on band recovery (Juv reference)
  beta_2019_hr ~ dunif(0,1) #Effect of Year on band recovery (2019 vs other) #This could probably be coded as a loop in the future to make it easier for IFW
  beta_2020_hr ~ dunif(0,1) #Effect of Year on band recovery (2020 vs other)
  beta_spp ~ dnorm(0, 1)
  
  #Specify period specific survival/band recovery
  for(j in 1:nind){
    for(t in f[j]:n.occasions){
      #Survival Model
      logit(s.br[j,t]) <- intercept_s + 
        beta_A_s*br_age[j,t] + beta_S2W_s*br_s2w[j,t] + beta_wmd_s[br_wmd[j]]
      #Band Recovery Model
      logit(hr.br[j,t]) <- intercept_hr + 
        beta_A_hr*br_age[j,t] + beta_2019_hr*br_2019[j,t] + beta_2020_hr*br_2020[j,t] + 
        w.tilde[cap.site[j]] + e.cap[cap.site[j]]
      
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
  ### Derived Parameters ###
  #Capture Site specific harvest rates
  for(i in 1:N.cap){
    logit(HR.A.2019.cap[i]) <- intercept_hr + beta_A_hr + beta_2019_hr + w.tilde[i] + e.cap[i]
    logit(HR.J.2019.cap[i]) <- intercept_hr + beta_2019_hr + w.tilde[i] + e.cap[i]
  }
  
  #Knot specific harvest rates
  for(i in 1:N.knot){
    logit(HR.A.2019.knot[i]) <- intercept_hr + beta_A_hr + beta_2019_hr + w.tilde.star[i]
    logit(HR.J.2019.knot[i]) <- intercept_hr + beta_2019_hr + w.tilde.star[i]
  }
  
  ### WMD Specific Harvest Rates
  for(i in 1:N.wmd){
    mean.WMD.HR.J[WMD.id[i]] <- mean(HR.J.2019.knot[WMD.matrix[i, 1:WMD.vec[i]]])
    mean.WMD.HR.A[WMD.id[i]] <- mean(HR.A.2019.knot[WMD.matrix[i, 1:WMD.vec[i]]])
  }
  
  ### Period Specific Survival
  for(i in sampledwmd){
    logit(WSR_M_J_S2W[i]) <- intercept_s + beta_S2W_s + beta_wmd_s[i]
    logit(WSR_M_A_S2W[i]) <- intercept_s + beta_A_s + beta_S2W_s + beta_wmd_s[i]
    logit(WSR_M_J_W2S[i]) <- intercept_s + beta_wmd_s[i]
    logit(WSR_M_A_W2S[i]) <- intercept_s + beta_A_s + beta_wmd_s[i]
  }
  
  #Averaged Period Specific Survival for all WMDs sampled
  # tau.S2W.A <- pow(sigma.S2W.A, -2)
  # sigma.S2W.A ~ dunif(0,100)
  # 
  # tau.W2S.A <- pow(sigma.W2S.A, -2)
  # sigma.W2S.A ~ dunif(0,100)
  # 
  # tau.S2W.J <- pow(sigma.S2W.J, -2)
  # sigma.S2W.J ~ dunif(0,100)  
  # 
  # tau.W2S.J <- pow(sigma.W2S.J, -2)
  # sigma.W2S.J ~ dunif(0,100)
  # 
  # l.S_M_J_S2W ~ dnorm(logit(pow(mean(WSR_M_J_S2W[sampledwmd]), 36)), tau.S2W.J)
  # l.S_M_A_S2W ~ dnorm(logit(pow(mean(WSR_M_A_S2W[sampledwmd]), 36)), tau.S2W.A)
  # l.S_M_J_W2S ~ dnorm(logit(pow(mean(WSR_M_J_W2S[sampledwmd]), 11)), tau.W2S.J)
  # l.S_M_A_W2S ~ dnorm(logit(pow(mean(WSR_M_A_W2S[sampledwmd]), 11)), tau.W2S.A)
  # 
  # logit(S_M_J_S2W) <- l.S_M_J_S2W
  # logit(S_M_A_S2W) <- l.S_M_A_S2W
  # logit(S_M_J_W2S) <- l.S_M_J_W2S
  # logit(S_M_A_W2S) <- l.S_M_A_W2S
  S_M_J_S2W <- pow(mean(WSR_M_J_S2W[sampledwmd]), 36)
  S_M_A_S2W <- pow(mean(WSR_M_A_S2W[sampledwmd]), 36)
  S_M_J_W2S <- pow(mean(WSR_M_J_W2S[sampledwmd]), 11)
  S_M_A_W2S <- pow(mean(WSR_M_A_W2S[sampledwmd]), 11)
  
  #Average Non Harvest Survival by WMD
  mean.AnnualS.A <- S_M_A_W2S * S_M_A_S2W
  mean.AnnualS.J <- S_M_J_W2S * S_M_J_S2W
  
  ##############################################################################################
  ### State-Space Abundance ###
  #Total Harvest Observation Error
  tau.obs.A <- pow(sigma.obs.A, -2)
  sigma.obs.A ~ dunif(0,100)
  tau.obs.J <- pow(sigma.obs.J, -2)
  sigma.obs.J ~ dunif(0,100)
  
  #Recruitment Process Error
  tau.R <- pow(sigma.R, -2)
  sigma.R ~ dunif(0,10)
  
  #Survival Annual Process Error
  tau.surv.A <- pow(sigma.surv.A, -2)
  sigma.surv.A ~ dunif(0,1)
  tau.surv.J <- pow(sigma.surv.J, -2)
  sigma.surv.J ~ dunif(0,1)
  
  #Harvest Rate Annual Process Error
  tau.harv.A <- pow(sigma.harv.A, -2)
  sigma.harv.A ~ dunif(0,1)
  tau.harv.J <- pow(sigma.harv.J, -2)
  sigma.harv.J ~ dunif(0,1)
  
  for(i in 1:N.wmd){
    for(t in 1:n.years){
      # #Total harvest Observation
      th.A[WMD.id[i],t] ~ dbin(WMD.HR.A[WMD.id[i],t], N.A[WMD.id[i],t])
      th.J[WMD.id[i],t] ~ dbin(WMD.HR.J[WMD.id[i],t], N.J[WMD.id[i],t])

      # th.A[WMD.id[i],t] ~ dnorm(totharv.A[WMD.id[i],t], tau.obs.A)
      # th.J[WMD.id[i],t] ~ dnorm(totharv.J[WMD.id[i],t], tau.obs.J)
      # #Total harvested = Harvest Rate * Total Abundance
      # totharv.A[WMD.id[i],t] <- N.A[WMD.id[i],t]*WMD.HR.A[WMD.id[i],t]
      # totharv.J[WMD.id[i],t] <- N.J[WMD.id[i],t]*WMD.HR.J[WMD.id[i],t]
      
      #Annual Variation in Harvest Rate
      l.WMD.HR.A[WMD.id[i],t] ~ dnorm(logit(mean.WMD.HR.A[WMD.id[i]]), tau.harv.A)
      l.WMD.HR.J[WMD.id[i],t] ~ dnorm(logit(mean.WMD.HR.J[WMD.id[i]]), tau.harv.J)

      logit(WMD.HR.A[WMD.id[i],t]) <- l.WMD.HR.A[WMD.id[i],t]
      logit(WMD.HR.J[WMD.id[i],t]) <- l.WMD.HR.J[WMD.id[i],t]
    }
    
    #Need to specify N[t=1], needs to be a whole number.
    #th.year1 are just the harvest totals from year 1 
    #This assumes there are some turkeys in each WMD at the first timestep
    N.A[WMD.id[i],1] <- round(((1+th.year1.A[WMD.id[i]])/mean.WMD.HR.A[WMD.id[i]]))
    N.J[WMD.id[i],1] <- round(((1+th.year1.J[WMD.id[i]])/mean.WMD.HR.J[WMD.id[i]]))

    #Average Recruitment Rate, WMD specific
    mean.R[WMD.id[i]] ~ dunif(0,2)
    
    for(t in 1:(n.years-1)){
      #Number of birds to A to surive OR J that transition into A from t to t+1
      N.A[WMD.id[i],t+1] <- n.surv.A[WMD.id[i],t] + n.surv.J[WMD.id[i],t]
      n.surv.A[WMD.id[i],t] ~ dbin(totalS.A[WMD.id[i],t], N.A[WMD.id[i],t])
      n.surv.J[WMD.id[i],t] ~ dbin(totalS.J[WMD.id[i],t], N.J[WMD.id[i],t])
      
      #Total Annual Survival Probability
      totalS.A[WMD.id[i],t] <- AnnualS.A[WMD.id[i],t]*(1-WMD.HR.A[WMD.id[i],t])
      totalS.J[WMD.id[i],t] <- AnnualS.J[WMD.id[i],t]*(1-WMD.HR.J[WMD.id[i],t])
      
      #Temporal Variation in probability of surviving non harvest risk in a year
      logit(AnnualS.A[WMD.id[i],t]) <- l.AnnualS.A[WMD.id[i],t]
      logit(AnnualS.J[WMD.id[i],t]) <- l.AnnualS.J[WMD.id[i],t]
      
      l.AnnualS.A[WMD.id[i],t] ~ dnorm(logit(mean.AnnualS.A), tau.surv.A)
      l.AnnualS.J[WMD.id[i],t] ~ dnorm(logit(mean.AnnualS.J), tau.surv.J)
      
      #Number of Birds recruited to the Juvenile population in t
      N.J[WMD.id[i],t+1] ~ dpois(meanY1[WMD.id[i],t])
      meanY1[WMD.id[i],t] <- R[WMD.id[i],t] * N.A[WMD.id[i],t] + N.J[WMD.id[i],t]
      # meanY1[WMD.id[i],t] <- R[WMD.id[i],t] * (N.A[WMD.id[i],t] )
      
      #Year Specific recruitment rate
      log.R[WMD.id[i],t] ~ dlnorm(log(mean.R[WMD.id[i]]), tau.R)
      log(R[WMD.id[i],t]) <- log.R[WMD.id[i],t]
    }
  }
}