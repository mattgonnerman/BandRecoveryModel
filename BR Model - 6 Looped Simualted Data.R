require(tidyr)
parameter.df <- read.csv("SimParameters.csv")

for(lr in 1:nrow(parameter.df)){
  psill.hr <- parameter.df[lr,1] 
  hr.sc <- parameter.df[lr,2] 
  C <- D <- parameter.df[lr,3] 
  nbandsites <- parameter.df[lr,4]
    
  for(loop in 1:100){
    # source(file = "BR Model - 5a Simulated Data - looped version.R")
    # source(file = "BR Model - 3c Execute JAGS.R")
    
    #Adult Population Size  
    N.A.long <- outputs %>% 
      filter(substr(X,1,3) == "N.A") %>%
      mutate(WMD = str_extract(X, "[:digit:]+")) %>%
      mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
      dplyr::select(Year, mean, WMD)
    N.A.est <- N.A.long %>%
      pivot_wider(names_from = c(Year), values_from = mean) %>%
      dplyr::select(-WMD)
    
    est.N.A <- unlist(as.numeric(as.matrix(N.A.est)))
    real.N.A <- unlist(as.numeric(as.matrix(N.A)))
    
    
    #Juvenile Population Size
    N.J.long <- outputs %>% 
      filter(substr(X,1,3) == "N.J") %>%
      mutate(WMD = str_extract(X, "[:digit:]+")) %>%
      mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
      dplyr::select(Year, mean, WMD)
    N.J.est <- N.J.long %>%
      pivot_wider(names_from = c(Year), values_from = mean) %>%
      dplyr::select(-WMD)
    
    est.N.J <- unlist(as.numeric(as.matrix(N.J.est)))
    real.N.J <- unlist(as.numeric(as.matrix(N.J)))
    
    
    #Adult Harvest Rate
    HR.A.long <- outputs %>% 
      filter(substr(X,1,13) == "mean.WMD.HR.A") %>%
      mutate(WMD = str_extract(X, "[:digit:]+")) %>%
      dplyr::select(mean, WMD)
    
    est.HR.A <- HR.A.long$mean
    real.HR.A <- Region_Mean_HR$Mean.HR.A
    
    
    #Juvenile Harvest Rate
    HR.J.long <- outputs %>% 
      filter(substr(X,1,13) == "mean.WMD.HR.J") %>%
      mutate(WMD = str_extract(X, "[:digit:]+")) %>%
      dplyr::select(mean, WMD)
    
    est.HR.J <- HR.J.long$mean
    real.HR.J <- Region_Mean_HR$Mean.HR.J
    
    
    #Survival Rate?
    
    
    #All Values to single DF
    N.comparison.df <- data.frame(WMD = 1:25,
                                  Est.N.A = est.N.A,
                                  Real.N.A = real.N.A, 
                                  Est.N.J = est.N.J,
                                  Real.N.J = real.N.J,
                                  Psill.hr = parameter.df[lr,1], 
                                  Range = parameter.df[lr,2],
                                  GridCellSize = parameter.df[lr,3], 
                                  TotBandSites = parameter.df[lr,4],
                                  SitesinRegion = lengths(st_intersects(SA.grid, bandsiteselect)),
                                  SimRun = loop)
    
    HR.comparison.df <- data.frame(WMD = 1:25,
                                  Est.HR.A = est.HR.A,
                                  Real.HR.A = real.HR.A, 
                                  Est.HR.J = est.HR.J,
                                  Real.HR.J = real.HR.J,
                                  Psill.hr = parameter.df[lr,1], 
                                  Range = parameter.df[lr,2],
                                  GridCellSize = parameter.df[lr,3], 
                                  TotBandSites = parameter.df[lr,4],
                                  SitesinRegion = lengths(st_intersects(SA.grid, bandsiteselect)),
                                  SimRun = loop)
    #Save output for each loop in case of power loss
  }
}
    