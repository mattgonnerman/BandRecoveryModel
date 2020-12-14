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
             id = ID, #Adult Survival
             wsr_sex = wsr_sex, #Adult Survival
             wsr_age = wsr_age, #Adult Survival
             wsr_time = wsr_time, #Adult Survival
             wsr_wmd = wsr_wmd,
             y = EH_raw, #Band Recovery
             f = f, #Band Recovery
             weeks2harv = weeks2harv,#BandRecovery
             nind = dim(EH_raw)[1], #Band Recovery 
             n.occasions = dim(EH_raw)[2], #Band Recovery 
             z = known.state.mr(EH_raw), #Band Recovery
             br_age = br_age, #Band Recovery
             br_2019 = br_2019, #Band Recovery
             br_2020 = br_2020, #Band Recovery
             br_s2w = br_s2w, #Band Recovery
             br_wmd = br_wmd,
             th.A = totharv.A, #Total Adult Harvest by WMD '14-'19
             th.J = totharv.J, #Total Juvenile Harvest by WMD '14-'19 
             th.year1.A = as.integer(totharv.A[,1]), #Total Adult Harvest by WMD '14-'19
             th.year1.J = as.integer(totharv.J[,1]), #Total Juvenile Harvest by WMD '14-'19
             n.years = ncol(totharv.A),
             sampledwmd = sampledwmd, #list of wmd's where we sampled
             cap.site = ind.cap.site, #each individuals capture site as a numeric
             d.s.star=KnotLocalDis.mat/1000, #distance between spatial knots and cap sites
             d.s.star.star=KnotDis.mat/1000, #distance between spatial knots and other spatial knots
             N.cap = length(unique(ind.cap.site)), #number of capture sites for SPP
             N.knot = nrow(KnotDis.mat), #number of knots for SPP
             N.wmd = nrow(WMD.matrix), #number of WMDs we are using in SPP
             WMD.matrix = WMD.matrix, #matrix showing which wmd a knot is in
             WMD.vec = WMD.vec, #vector for referencing WMD.matrix
             WMD.id = sort(WMD.id) #vector to ID WMDs in SPP
) #for Harvest rate estimates

#Parameters monitors
parameters.null <- c('alpha_s', 
                     'alpha_hr', 
                     'intercept_s', #Non-Harvest Survival Intercept 
                     'beta_F_s', #Non-Harvest Survival Beta - Female
                     'beta_A_s', #Non-Harvest Survival Beta - Adult
                     'beta_A_F_s', #Non-Harvest Survival Beta - F*A Interaction
                     'beta_S2W_s', #Non-Harvest Survival Beta - Spring to Winter period
                     'beta_wmd_s', #Non-Harvest Survival Beta - WMD specific
                     'intercept_hr', #Harvest Rate Intercept
                     'beta_A_hr', #Harvest Rate Betas - Adult
                     'beta_2019_hr', #Harvest Rate Betas - 2019
                     'beta_2020_hr', #Harvest Rate Betas - 2020
                     # 'w.tilde', 
                     # 'w.tilde.star',
                     'phi.spp', 
                     'sigmasq',
                     'tausq',
                     'HR.A.2019.knot',
                     'HR.J.2019.knot',
                     'HR.A.2019.cap',
                     'HR.J.2019.cap',
                     'mean.WMD.HR.A',
                     'mean.WMD.HR.J',
                     'sigma.harv.A',
                     'sigma.harv.J',
                     'WSR_M_J_S2W', #Site Specific Survival, checking issue
                     'S_M_J_W2S', #Period Specific Survival 
                     # 'sigma.W2S.J',
                     'S_M_A_W2S', 
                     # 'sigma.W2S.A',
                     'S_M_J_S2W', 
                     # 'sigma.S2W.J',
                     'S_M_A_S2W', 
                     # 'sigma.S2W.A',
                     'N.A', #SS Abundance
                     'N.J',
                     'mean.R', #Recruitment Rate
                     'sigma.R',
                     'mean.AnnualS.J',#NonHarvest Survival in SS Abun
                     'mean.AnnualS.A',
                     'sigma.surv.J',
                     'sigma.surv.A',
                     'totalS.A'
)


#Initial values
inits.null <- function(){
  list(z = mr.init.z(EH_raw)
       )
}

#MCMC settings
ni <- 200 #number of iterations
nt <- 8 #thinning
nb <- 100 #burn in period
nc <- 3 #number of chains

#Model for JAGS

br_w_as_model <- source(file = "BR Model - 2 JAGS Model Code.R")$value


### Run Model ###
#Call JAGS
BR_w_SPP_output <- jags(data = dat,
                        parameters.to.save = parameters.null,
                        inits = inits.null,
                        model.file = br_w_as_model,
                        n.iter = ni,
                        n.burnin = nb,
                        n.thin = nt,
                        n.chains = nc) 

BR_w_SPP_output

write.csv(BR_w_SPP_output$BUGSoutput$summary, file = "BR_P_SPP_SSPop_output.csv")

# autocorr.plot(wmdspecific_wmdsurv_output,ask=F,auto.layout = T)
# 
# plot(as.mcmc(BR_w_SPP_output))
# 
# traceplot(BR_w_SPP_output)
