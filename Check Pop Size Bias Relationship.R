require(dplyr)
require(tidyr)
require(ggplot2)

trialnames <- c("LowNugget", "HighNugget", "LowPSill", "HighPSill", "LowRange", "HighRange", "MedAll")

rm(Nreal)
for(j in 1:length(trialnames)){
  parameter <- trialnames[j]

  masterSimInfo <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/",parameter," - MasterSimInfo.csv", sep = "")) %>%
    filter(Name == "YearsBR")
  
  for(i in 1:100){
    nyBR <- masterSimInfo$Value[i]
    nskipA <- 2*nyBR + 11
    nskipJ <- nskipA + 29
    
    NrealA <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/",parameter," - Sim", i," - Real Parameter Values.csv", sep = ""),
                       skip = nskipA,
                       nrows = 25) %>%
      mutate(WMD = 1:25) %>%
      pivot_longer(cols = 1:(nyBR+10), names_to = "Year") %>%
      mutate(Year = substr(Year, 2, 3)) %>%
      mutate(Trial = i) %>%
      mutate(Age = "A")  %>%
      mutate(Parameter = parameter) %>%
      dplyr::rename(RealN = value)
    
    NrealJ <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/",parameter," - Sim", i," - Real Parameter Values.csv", sep = ""),
                       skip = nskipJ,
                       nrows = 25) %>%
      mutate(WMD = 1:25) %>%
      pivot_longer(cols = 1:(nyBR+10), names_to = "Year") %>%
      mutate(Year = substr(Year, 2, 3)) %>%
      mutate(Trial = i) %>%
      mutate(Age = "J") %>%
      mutate(Parameter = parameter) %>%
      dplyr::rename(RealN = value)
    Nreal.combo <- rbind(NrealA, NrealJ)
    if(exists("Nreal")){
      Nreal <- rbind(Nreal, Nreal.combo)
    }else{
      Nreal <- Nreal.combo
    }
  }
  
}


for(j in 1:length(trialnames)){
  parameter <- trialnames[j]
  N.masterbias.sing <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/",parameter," - Master N Bias.csv", sep = ""))
  if(j == 1){
    N.masterbias <- N.masterbias.sing
  } else {
    N.masterbias <- rbind(N.masterbias, N.masterbias.sing)
  }
}

Nreal.wide <- Nreal %>%
  pivot_wider(id_cols = c("WMD", "Trial", "Age", "Parameter"), 
              values_from = "RealN", names_from = "Year", names_prefix ="X")

N.masterbias <- N.masterbias %>%
  filter(WMD != -999) %>%
  filter(BiasType =="Rel.Bias") %>%
  pivot_longer(names_to = "Year",
               cols = 6:(6+17)) %>%
  dplyr::rename(Rel.Bias = value) %>%
  mutate(Year = substr(Year, 2, 3)) 

popsizebias <- merge(Nreal, N.masterbias, by = c("Parameter", "Trial", "WMD", "Year", "Age")) %>%
  group_by(Age) %>%
  mutate(RealNBin = cut(RealN, breaks = quantile(RealN, c(0,.2,.4,.6,.8,1)) - c(.01, 0,0,0,0, -.1), 
                        labels = c(1,2,3,4,5)))


age.labs <- c("Adult", "Juvenile")
names(age.labs) <- c("A", "J")

realn.plot <- ggplot(data = popsizebias, aes(x=as.factor(RealNBin), y = Rel.Bias, fill = as.factor(Parameter))) +
  geom_boxplot(outlier.shape = NA, lwd = .3) +
  theme_classic(base_size = 8)+
  labs(x = "Real Abundance (Quantile)", y = "Relative Bias (Abundance)") +
  theme(legend.position = "none")+
  facet_wrap(~Age, labeller = labeller(Age = age.labs))
realn.plot

jpeg("Graphs/Supplemental Real Population Bias.jpg", width = 800, height = 550, res = 300)
realn.plot
dev.off()



for(j in 1:length(trialnames)){
  parameter <- trialnames[j]
  
  masterSimInfo <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/",parameter," - MasterSimInfo.csv", sep = "")) %>%
    filter(Name == "NInd BR")
  
  if(j == 1){
    N.bandbyWMD <- masterSimInfo
  } else {
    N.bandbyWMD <- rbind(N.bandbyWMD, masterSimInfo)
  }
}

totalN.byyearwmd <-Nreal %>%
  group_by(Parameter, Trial, WMD, Year) %>%
  summarize(TotalN = sum(RealN)) %>%
  group_by(Parameter, Trial, WMD) %>%
  summarize(MeanN = mean(TotalN))


popbandratio <- merge(totalN.byyearwmd, N.bandbyWMD, by = c("Parameter", "Trial", "WMD")) %>%
  mutate(B2NRatio = Value/MeanN)

ratiobias <- merge(N.masterbias, popbandratio, by = c("Parameter", "Trial", "WMD"), all.x = T) %>%
  group_by(Age) %>%
  mutate(B2NBin = cut(B2NRatio, breaks = quantile(B2NRatio, c(0,.2,.4,.6,.8,1)) - c(.01, 0,0,0,0, -.1), 
                        labels = c(1,2,3,4,5)))

realn.plot <- ggplot(data = ratiobias, aes(x=as.factor(B2NBin), y = Rel.Bias, fill = as.factor(Parameter))) +
  geom_boxplot(outlier.shape = NA, lwd = .3) +
  # geom_hline(yintercept = 0, linetype = "longdash", lwd = 1.5, alpha = .6) +
  theme_classic(base_size = 8)+
  labs(x = "Proportion Banded (Quantile)", y = "Relative Bias (Abundance)") +
  theme(legend.position = "none")+
  facet_wrap(~Age, labeller = labeller(Age = age.labs))
realn.plot
jpeg("Graphs/Supplemental Real Population Bias2.jpg", width = 800, height = 550, res = 300)
realn.plot
dev.off()
