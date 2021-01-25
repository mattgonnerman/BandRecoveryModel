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
        beta_A_s*br_age[j,t] + beta_S2W_s*br_s2w[j,t] + beta_wmd_s[br_wmd[j]]
      #Band Recovery Model
      #Years Separate
      logit(hr.br[j,t]) <- intercept_hr + 
        beta_A_hr*br_age[j,t] + beta_2019_hr*br_2019[j,t] + beta_2020_hr*br_2020[j,t] + 
        w.tilde[cap.site[j]] + e.cap[cap.site[j]]
      # #Combine years
      # logit(hr.br[j,t]) <- intercept_hr + beta_A_hr*br_age[j,t] + w.tilde[cap.site[j]] + e.cap[cap.site[j]]
      
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
  #Capture Site specific harvest rates
  for(i in 1:N.cap){
    logit(HR.A.2018.cap[i]) <- intercept_hr + beta_A_hr + w.tilde[i] + e.cap[i]
    logit(HR.J.2018.cap[i]) <- intercept_hr + w.tilde[i] + e.cap[i]
    logit(HR.A.2019.cap[i]) <- intercept_hr + beta_A_hr + beta_2019_hr + w.tilde[i] + e.cap[i]
    logit(HR.J.2019.cap[i]) <- intercept_hr + beta_2019_hr + w.tilde[i] + e.cap[i]
    logit(HR.A.2020.cap[i]) <- intercept_hr + beta_A_hr + beta_2020_hr + w.tilde[i] + e.cap[i]
    logit(HR.J.2020.cap[i]) <- intercept_hr + beta_2020_hr + w.tilde[i] + e.cap[i]
  }

  #Knot specific harvest rates
  for(i in 1:N.knot){
    logit(HR.A.2018.knot[i]) <- intercept_hr + beta_A_hr + w.tilde.star[i]
    logit(HR.J.2018.knot[i]) <- intercept_hr + w.tilde.star[i]
    logit(HR.A.2019.knot[i]) <- intercept_hr + beta_A_hr + beta_2019_hr + w.tilde.star[i]
    logit(HR.J.2019.knot[i]) <- intercept_hr + beta_2019_hr + w.tilde.star[i]
    logit(HR.A.2020.knot[i]) <- intercept_hr + beta_A_hr + beta_2020_hr + w.tilde.star[i]
    logit(HR.J.2020.knot[i]) <- intercept_hr + beta_2020_hr + w.tilde.star[i]
  }

  ### WMD Specific Harvest Rates
  for(i in 1:N.wmd){
    WMD.HR.A.2018[WMD.id[i]] <- mean(HR.A.2018.knot[WMD.matrix[i, 1:WMD.vec[i]]])
    WMD.HR.J.2018[WMD.id[i]] <- mean(HR.J.2018.knot[WMD.matrix[i, 1:WMD.vec[i]]])
    WMD.HR.A.2019[WMD.id[i]] <- mean(HR.A.2019.knot[WMD.matrix[i, 1:WMD.vec[i]]])
    WMD.HR.J.2019[WMD.id[i]] <- mean(HR.J.2019.knot[WMD.matrix[i, 1:WMD.vec[i]]])
    WMD.HR.A.2020[WMD.id[i]] <- mean(HR.A.2020.knot[WMD.matrix[i, 1:WMD.vec[i]]])
    WMD.HR.J.2020[WMD.id[i]] <- mean(HR.J.2020.knot[WMD.matrix[i, 1:WMD.vec[i]]])
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

  #Average Non Harvest Survival
  mean.AnnualS.A <- S_M_A_W2S * S_M_A_S2W
  mean.AnnualS.J <- S_M_J_W2S * S_M_J_S2W
}