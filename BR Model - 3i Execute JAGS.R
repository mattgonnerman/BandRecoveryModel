
require(R2jags)

###################################################################################################################
###################################################################################################################
###################################################################################################################
#######################
### Models for JAGS ###
#######################
### Data needed for MCMC ###
# Succ: vector of whether a nest survived an interval (1) or not (0)
# Interval: vector of interval lengths that corresponds with each value in succ
# X: matrix of covariates
# id: vector of nest IDs

dat <- list( succ = succ, #Adult Survival
             interval = interval, #Adult Survival
             nvisit = length(succ), #Adult Survival
             wsr_sex = wsr_sex, #Adult Survival
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
             # br_2019 = br_2019, #Band Recovery
             # br_2020 = br_2020, #Band Recovery
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
             n.years = ncol(totharv.A),
             yearsHRavail = yearsHRavail,
             HRnotavail = HRnotavail,
             max.notavail <- max(HRnotavail)
) #for Harvest rate estimates

#Parameters monitors
parameters.null <- c('intercept_m', #Non-Harvest Survival Intercept 
                     'beta_F_m', #Non-Harvest Survival Beta - Female
                     'beta_A_m', #Non-Harvest Survival Beta - Adult
                     'beta_A_F_m', #Non-Harvest Survival Beta - F*A Interaction
                     'beta_S2W_m', #Non-Harvest Survival Beta - Spring to Winter period
                     'beta_W2S_m', #Non-Harvest Survival Beta - Winter to Spring period
                     'beta_wmd_m', #Non-Harvest Survival Beta - WMD specific
                     'intercept_hr', #Harvest Rate Intercept
                     'beta_A_hr', #Harvest Rate Betas - Adult
                     'beta_2019_hr',
                     'beta_2020_hr',
                     # 'w.tilde',
                     # 'w.tilde.star',
                     # 'phi.spp', 
                     # 'HR.A.2019.knot',
                     # 'HR.J.2019.knot',
                     # 'HR.A.2019.cap',
                     # 'HR.J.2019.cap', 
                     # 'HR.A.2020.knot',
                     # 'HR.J.2020.knot',
                     # 'HR.A.2020.cap',
                     # 'HR.J.2020.cap',
                     # 'mean.WMD.HR.A', #Mean WMD Harvest Rate
                     # 'mean.WMD.HR.J',
                     'WMD.HR.A',
                     'WMD.HR.J',
                     'WSR_M_J_S2W',
                     'WSR_M_A_S2W',
                     'WSR_M_J_W2S',
                     'WSR_M_A_W2S',
                     'S_M_J_W2S', #Period Specific Survival 
                     'S_M_A_W2S', 
                     'S_M_J_S2W', 
                     'S_M_A_S2W'
)


#Initial values
inits.null <- function(){
  list(
       # n.surv.A = n.surv.A.init,
       # n.surv.J = n.surv.J.init,
       # N.J = N.J.init,
       # N.A = N.A.init
       # mean.R = mean.r.init,
       z = mr.init.z(EH_raw)
       )
}

# #MCMC settings
# ni <- 5000 #number of iterations
# nt <- 8 #thinning
# nb <- 3000 #burn in period
# nc <- 3 #number of chains

names_for_parallel <- c("EH_raw", 
                        "nb", 
                        "nt", 
                        "nc",
                        "ni",
                        "br_w_as_model",
                        "dat",
                        "parameters.null",
                        "inits.null", 
                        "mr.init.z")

#Model for JAGS
br_w_as_model <- source(file = "BR Model - 2i JAGS Model Code - 2g with SS.R")$value


### Run Model ###
#Call JAGS
# BR_w_SPP_output <- jags(data = dat,
#                         parameters.to.save = parameters.null,
#                         inits = inits.null,
#                         model.file = br_w_as_model,
#                         n.iter = ni,
#                         n.burnin = nb,
#                         n.thin = nt,
#                         n.chains = nc) 

BR_w_SPP_output <- jags.parallel(data = dat,
                                 parameters.to.save = parameters.null,
                                 inits = inits.null,
                                 model.file = br_w_as_model,
                                 n.iter = ni,
                                 n.burnin = nb,
                                 n.thin = nt,
                                 n.chains = nc,
                                 export_obj_names = names_for_parallel) 

BR_w_SPP_output

write.csv(BR_w_SPP_output$BUGSoutput$summary, file = "3G_output.csv")

# autocorr.plot(wmdspecific_wmdsurv_output,ask=F,auto.layout = T)
# 
# plot(as.mcmc(BR_w_SPP_output))
# 
# traceplot(BR_w_SPP_output)