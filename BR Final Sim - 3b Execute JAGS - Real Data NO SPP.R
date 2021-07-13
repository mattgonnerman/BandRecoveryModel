require(R2jags)

###################################################################################################################
###################################################################################################################
###################################################################################################################
#######################
### Models for JAGS ###
#######################
### Data needed for MCMC ###
dat <- list( succ = succ, #Adult Survival
             interval = interval, #Adult Survival
             nvisit = length(succ), #Adult Survival
             # wsr_sex = wsr_sex, #Adult Survival
             wsr_age = wsr_adult, #Adult Survival
             wsr_S2W = wsr_S2W, #Adult Survival
             wsr_W2S = wsr_W2S,
             wsr_wmd = wsr_wmd,
             
             y = EH_raw, #Band Recovery
             f = f, #Band Recovery
             weeks2harv = weeks2harv,#BandRecovery
             nind = dim(EH_raw)[1], #Band Recovery 
             n.occasions = dim(EH_raw)[2], #Band Recovery 
             z = known.state.mr(EH_raw), #Band Recovery
             br_age_hr = br_adult_hr, #Band Recovery
             br_age_s = br_adult_s, #Band Recovery
             br_year = br_year,
             br_s2w = br_s2w, #Band Recovery
             br_w2s = br_w2s, #Band Recovery
             br_wmd = br_wmd,
             n.years.br = n.band.years,
             br_season = br_season,
             
             # sampledwmd = sort(unique(wsr_wmd)), #For when looking at just WSR
             sampledwmd = sampledwmd, #list of wmd's where we sampled
             cap.site = ind.cap.site, #each individuals capture site as a numeric
             d.s.star=KnotLocalDis.mat/1000, #distance between spatial knots and cap sites
             d.s.star.star=KnotDis.mat/1000, #distance between spatial knots and other spatial knots
             N.cap = ifelse(simrun != "Y",length(unique(ind.cap.site)), max(ind.cap.site)), #number of capture sites for SPP
             N.knot = nrow(KnotDis.mat), #number of knots for SPP
             N.wmd = nrow(WMD.matrix), #number of WMDs we are using in SPP
             WMD.matrix = WMD.matrix, #matrix showing which wmd a spatial knot is in
             WMD.vec = WMD.vec, #vector for referencing WMD.matrix
             WMD.id = sort(WMD.id), #vector to ID WMDs in SPP
             
             th.A = totharv.A, #Total Adult Harvest by WMD '14-'19
             th.J = totharv.J, #Total Juvenile Harvest by WMD '14-'19 
             th.year1.A = as.integer(totharv.A[,1]), #Total Adult Harvest by WMD '14-'19
             th.year1.J = as.integer(totharv.J[,1]), #Total Juvenile Harvest by WMD '14-'19
             n.years = ncol(totharv.A) #Total Number of Years
)

#Parameters monitors
parameters.null <- c('intercept_m', #Non-Harvest Survival Intercept 
                     # 'beta_F_m', #Non-Harvest Survival Beta - Female
                     'beta_A_m', #Non-Harvest Survival Beta - Adult
                     # 'beta_A_F_m', #Non-Harvest Survival Beta - F*A Interaction
                     'beta_S2W_m', #Non-Harvest Survival Beta - Spring to Winter period
                     'beta_W2S_m', #Non-Harvest Survival Beta - Winter to Spring period
                     'beta_wmd_m', #Non-Harvest Survival Beta - WMD specific
                     
                     'intercept_hr', #Harvest Rate Intercept
                     'beta_A_hr', #Harvest Rate Betas - Adult
                     'beta_year',
                     
                     'HR.A.year',
                     'HR.J.year',
                     'mean.WMD.HR.A', #Mean WMD Harvest Rate
                     'mean.WMD.HR.J',
                     
                     'WSR_M_J_S2W',
                     'WSR_M_A_S2W',
                     'WSR_M_J_W2S',
                     'WSR_M_A_W2S',
                     'S_M_J_W2S', #Period Specific Survival 
                     'S_M_A_W2S', 
                     'S_M_J_S2W', 
                     'S_M_A_S2W', 
                     
                     'N.A', #SS Abundance
                     'N.J',
                     'n.surv.A',
                     'n.surv.J',
                     
                     'mean.R', #Mean WMD Specific Recruitment Rate
                     'R' #WMD and Time Specific Recruitment Rate
)


# Set Initial Values
#Initial Values to deal with missing years
est2020 <- read.csv("2020HarvEst.csv") %>% 
  filter(WMD %in% totalharvest.df$WMD)
totharv.A.init <- totharv.J.init <- matrix(NA, nrow = nrow(totharv.A), ncol = ncol(totharv.A))
totharv.A.init[,missing.2020] <- ceiling(est2020$TotalHarv*mean((1+totharv.A)/(1+totharv.J+totharv.A), na.rm = T))
totharv.J.init[,missing.2020] <- ceiling(est2020$TotalHarv*mean((1+totharv.J)/(1+totharv.J+totharv.A), na.rm = T))


tempharv.A <- totharv.A
tempharv.A[,missing.2020] <- totharv.A.init[,missing.2020]
tempharv.J <- totharv.J
tempharv.J[,missing.2020] <- totharv.J.init[,missing.2020]

N.J.init <- ceiling((tempharv.J+1)/.10)
N.A.init <- ceiling((tempharv.A+1)/.25)
N.A.init.c1 <- N.A.init
N.A.init.c1[,2:ncol(N.A.init)] <- NA

n.surv.A.init <- ceiling(N.A.init[,1:(ncol(N.A.init)-1)]*.4)
n.surv.J.init <- ceiling(N.J.init[,1:(ncol(N.J.init)-1)]*.4)



# For n.surv.A[WMD.id[i],t] ~ dbin(totalS.A[WMD.id[i],t], N.A[WMD.id[i],t])
# Need to follow the below rules
# n.surv.A.init[i,j] < (n.surv.A.init[i,j-1] + n.surv.J.init[i,j-1])
# totharv.A[i,j] < (n.surv.A.init[i,j-1] + n.surv.J.init[i,j-1])
for(i in 1:nrow(n.surv.A.init)){
  if(n.surv.A.init[i,1] > (5+as.integer(tempharv.A[i,1]))){
    n.surv.A.init[i,1] <- (5+as.integer(tempharv.A[i,1])) - 5
  }
  if(n.surv.J.init[i,1] > (5+as.integer(tempharv.J[i,1]))){
    n.surv.J.init[i,1] <- (5+as.integer(tempharv.J[i,1])) - 5
  }
  
  for(j in 2:ncol(n.surv.A.init)){
    if(n.surv.A.init[i,j] > (n.surv.A.init[i,j-1] + n.surv.J.init[i,j-1])){
      n.surv.A.init[i,j] <- ceiling((n.surv.A.init[i,j-1] + n.surv.J.init[i,j-1])*.5)
    }
    
    for(j in 2:ncol(tempharv.A)){
      if(tempharv.A[i,j] > n.surv.A.init[i,j-1] + n.surv.J.init[i,j-1]) {
        x <- tempharv.A[i,j] - n.surv.A.init[i,j-1] - n.surv.J.init[i,j-1]
        n.surv.A.init[i,j-1] <- n.surv.A.init[i,j-1] + ceiling(x/2)
        n.surv.J.init[i,j-1] <- n.surv.J.init[i,j-1] + ceiling(x/2)
      }
    }
  }
}

alpha.r.init <- matrix(2,
                       nrow = nrow(tempharv.A),
                       ncol = ncol(tempharv.A)-1,
                       byrow = T)


#Initial Values Packaged for Jags
inits.null <- function(){
  list(z = mr.init.z(EH_raw),
       n.surv.A = n.surv.A.init,
       n.surv.J = n.surv.J.init,
       N.J = N.J.init,
       N.A = N.A.init.c1,
       alpha.R = alpha.r.init,
       totharv.A = totharv.A.init,
       totharv.J = totharv.J.init
       )
}


names_for_parallel <- c("EH_raw", 
                        "alpha.r.init", 
                        "n.surv.A.init", 
                        "n.surv.J.init", 
                        "nb", 
                        "nt", 
                        "nc",
                        "ni",
                        "br_w_as_model",
                        "dat",
                        "parameters.null",
                        "inits.null", 
                        "mr.init.z",
                        "N.A.init.c1",
                        "N.J.init",
                        "totharv.A.init",
                        "totharv.J.init")

#Model for JAGS
br_w_as_model <- source(file = "BR Final Sim - 4b JAGS Model - Real Data NO SPP.R")$value


### Run Model ###
#Call JAGS
BR_w_SPP_output <- jags.parallel(data = dat,
                        parameters.to.save = parameters.null,
                        inits = inits.null,
                        model.file = br_w_as_model,
                        n.iter = ni,
                        n.burnin = nb,
                        n.thin = nt,
                        n.chains = nc,
                        export_obj_names = names_for_parallel) 
