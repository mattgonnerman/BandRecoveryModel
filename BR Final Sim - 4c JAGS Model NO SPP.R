function(){##############################################################################################
  # ### Weekly Survival Rate ###
  alpha_m ~ dbeta(1,1)
  intercept_m <- cloglog(alpha_m)
  beta_A_m ~ dnorm(0,.01) #Effect of age on WSR (Juv reference)
  beta_S2W_m ~ dnorm(0,.01) #Effect of S2W (W2S reference)
  beta_W2S_m ~ dnorm(0,.01) #Effect of S2W (W2S reference)
  for(i in sampledwmd){beta_wmd_m[i] ~ dnorm(0,.01)} #Effect of wmd (W2S reference)
  
  #WSR
  for(i in 1:nvisit){
    eta[i] <- intercept_m + beta_A_m*wsr_age[i] + beta_S2W_m*wsr_S2W[i] + beta_W2S_m*wsr_W2S[i] + beta_wmd_m[wsr_wmd[i]]
    log(phi[i])<-eta[i] # anti-logit to determine the daily survival rate
    mu[i]<- exp(-(interval[i]*phi[i])) # period survival is DSR raised to the interval
    succ[i]~dbern(mu[i])  # the data is distributed as bernoulli with period survival as the mean
  }
  
  
  ##############################################################################################
  ### Band Recovery ###
  alpha_hr ~ dbeta(1,1)
  intercept_hr <- cloglog(alpha_hr)
  for(i in 1:n.years.br){beta_year[i] ~ dnorm(0,.01)} #Effect of wmd (W2S reference)
  beta_A_hr ~ dnorm(0,.01) #Effect of Age on band recovery (Juv reference)
  
  #Specify period specific survival/band recovery
  for(j in 1:nind){
    for(t in f[j]:n.occasions){
      #Survival Model
      log(m.br[j,t]) <- intercept_m + beta_A_m*br_age_s[j,t] + beta_W2S_m*br_w2s[j,t] + beta_S2W_m*br_s2w[j,t] + beta_wmd_m[br_wmd[j]]
      
      #Band Recovery Model
      #Years Separate
      cloglog(hr.br[j,t]) <- intercept_hr + beta_A_hr*br_age_hr[j,t] + beta_year[br_year[t]]

      s[j,t] <- ifelse(t == f[j], exp(-(m.br[j,t]*weeks2harv[j])),
                       ifelse(br_season[t] == 1, exp(-(11*m.br[j,t])), exp(-(36*m.br[j,t]))
                       )
      )
      hr[j,t] <- ifelse(br_season[t] == 1, 0, hr.br[j,t])
      
    } #t
  } #i
  
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
  ### Derived Parameters - HR Combined Years ###
  #Year specific harvest rates
  for(j in 1:n.years.br){
    cloglog(HR.A.year[j]) <- intercept_hr + beta_year[j] + beta_A_hr
    cloglog(HR.J.year[j]) <- intercept_hr + beta_year[j]
  }

  mean.HR.A <- mean(HR.A.year[1:n.years.br])
  mean.HR.J <- mean(HR.J.year[1:n.years.br])

  
  ### Period Specific Survival
  for(i in sampledwmd){
    cloglog(m_M_J_S2W[i]) <- intercept_m + beta_S2W_m + beta_wmd_m[i]
    cloglog(m_M_A_S2W[i]) <- intercept_m + beta_S2W_m + beta_wmd_m[i] + beta_A_m 
    cloglog(m_M_J_W2S[i]) <- intercept_m + beta_W2S_m + beta_wmd_m[i]
    cloglog(m_M_A_W2S[i]) <- intercept_m + beta_W2S_m + beta_wmd_m[i] + beta_A_m 
    
    WSR_M_J_S2W[i] <- exp(-(m_M_J_S2W[i]))
    WSR_M_A_S2W[i] <- exp(-(m_M_A_S2W[i]))
    WSR_M_J_W2S[i] <- exp(-(m_M_J_W2S[i]))
    WSR_M_A_W2S[i] <- exp(-(m_M_A_W2S[i]))
  }
  
  #Survival Estiamtes
  S_M_J_S2W <- exp(-(36 * mean(m_M_J_S2W[sampledwmd])))
  S_M_A_S2W <- exp(-(36 * mean(m_M_A_S2W[sampledwmd])))
  S_M_J_W2S <- exp(-(11 * mean(m_M_J_W2S[sampledwmd])))
  S_M_A_W2S <- exp(-(11 * mean(m_M_A_W2S[sampledwmd])))
  
  
  ##############################################################################################
  ### State-Space Abundance ###
  for(i in 1:N.wmd){
    for(t in 1:n.years){
      #Total harvest Observation #Temporal Variation in HR
      th.A[WMD.id[i],t] ~ dbin(mean.HR.A, N.A[WMD.id[i],t]) #Temporal Variation in HR
      th.J[WMD.id[i],t] ~ dbin(mean.HR.J, N.J[WMD.id[i],t]) #Temporal Variation in HR
    }
    
    # Total Annual Survival Probability #Temporal Variation in S and HR
    totalS.A[WMD.id[i]] <- S_M_A_W2S * S_M_A_S2W *(1-mean.HR.A) #
    totalS.J[WMD.id[i]] <- S_M_A_W2S * S_M_J_S2W *(1-mean.HR.J) # J transition to A at 2nd capture season, hence S.A used

    #Need to specify N[t=1], needs to be a whole number.
    #th.year1 are just the harvest totals from year 1
    # #This assumes there is at least 1 turkeys in each WMD at the first timestep
    N.A[WMD.id[i],1] ~ dpois((1+th.year1.A[WMD.id[i]])/mean.HR.A) #Temporal Variation in HR
    N.J[WMD.id[i],1] ~ dpois((1+th.year1.J[WMD.id[i]])/mean.HR.J) #Temporal Variation in HR
    
    for(t in 1:(n.years-1)){
      # Number of birds to A to surive OR J that transition into A from t to t+1
      N.A[WMD.id[i],t+1] <- n.surv.A[WMD.id[i],t] + n.surv.J[WMD.id[i],t] #Requires Specific Starting Parameters
      n.surv.A[WMD.id[i],t] ~ dbin(totalS.A[WMD.id[i]], N.A[WMD.id[i],t]) #Requires Specific Starting Parameters
      n.surv.J[WMD.id[i],t] ~ dbin(totalS.J[WMD.id[i]], N.J[WMD.id[i],t]) #Requires Specific Starting Parameters
      
      #Number of Birds recruited to the Juvenile population in t
      N.J[WMD.id[i],t+1] ~ dpois(meanY1[WMD.id[i],t])
      meanY1[WMD.id[i],t] <- R[WMD.id[i],t] * N.A[WMD.id[i],t]
      alpha.R[WMD.id[i],t] ~ dunif(-10,10)
      log(R[WMD.id[i],t]) <- alpha.R[WMD.id[i],t]
    }
  }
}