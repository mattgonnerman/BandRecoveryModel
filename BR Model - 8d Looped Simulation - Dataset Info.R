### Summarize Simulated Data ###
SimInfo <- data.frame(Name = NA, WMD = NA, Value = NA)

#Number Years Band Recovery Data
SimInfo[1,] <- c("YearsBR", NA, n.band.years)

#Number Individuals BR Data
SimInfo[2,] <- c("TotNIndBR", NA, nbandind)

#Additional Years of Total Harvest Data in State Space
SimInfo[3,] <- c("YearsTotHarv", NA, n.years.totharv)

#Number of Years of WSR Data
SimInfo[4,] <- c("YearsWSR", NA, n.years.telem)

#Number Individuals WSR Data
SimInfo[5,] <- c("TotNIndWSR", NA, ntelemind)

#Number of BR Sites per Region
BR.sites.region <- data.frame(Name = "NSites BR",
                              WMD = 1:(C*D),
                              Value = lengths(st_intersects(SA.grid, bandsiteselect)))
SimInfo <- rbind(SimInfo, BR.sites.region)

#Number of WSR Sites per Region
BR.sites.region <- data.frame(Name = "NSites WSR",
                              WMD = 1:(C*D),
                              Value = lengths(st_intersects(SA.grid, telemsiteselect)))
SimInfo <- rbind(SimInfo, BR.sites.region)

#Number of BR Individuals per Region
br.sites.ind <- st_intersects(SA.grid, bandsiteselect)
reg.ind.count <- c()
for(i in 1:length(br.sites.ind)){
  reg.ind <- c()
  for(j in 1:length(br.sites.ind[[i]])){
    reg.ind[j] <- length(which(hr.ind.cap$CapSite == br.sites.ind[[i]][j]))
  }
  reg.ind.count[i] <- sum(reg.ind, na.rm = T)
}
BR.ind.region <- data.frame(Name = "NInd BR",
                              WMD = 1:(C*D),
                              Value = reg.ind.count)
SimInfo <- rbind(SimInfo, BR.ind.region)

#Number of WSR Individuals per Region
wsr.sites.ind <- st_intersects(SA.grid, telemsiteselect)
reg.ind.count <- c()
for(i in 1:length(wsr.sites.ind)){
  reg.ind <- c()
  for(j in 1:length(wsr.sites.ind[[i]])){
    reg.ind[j] <- length(which(hr.ind.cap$CapSite == wsr.sites.ind[[i]][j]))
  }
  reg.ind.count[i] <- sum(reg.ind, na.rm = T)
}
WSR.ind.region <- data.frame(Name = "NInd WSR",
                            WMD = 1:(C*D),
                            Value = reg.ind.count)
SimInfo <- rbind(SimInfo, WSR.ind.region) 


#Add Trial ID
SimInfo <- SimInfo %>%
  mutate(Trial = looprun)
#Save Information
write.csv(SimInfo, file = paste("Model Bias Comparison/SampleSize/Trial ",looprun," - Data Details.csv", sep = ""), row.names = F)


#Master SiteInfo File
if(looprun == 1){
  mastersiteinfo <- SimInfo
}else{
  mastersiteinfo <- rbind(mastersiteinfo, SimInfo)
}

write.csv(mastersiteinfo, "Model Bias Comparison/SampleSize/MasterSimInfo.csv", row.names = F)