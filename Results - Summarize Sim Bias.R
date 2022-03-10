# Load Packages
require(dplyr)
require(tidyr)
require(stringr)

trialnames <- c("LowNugget", "HighNugget", "LowPSill", "HighPSill", "LowRange", "HighRange", "MedAll")

# Load Adbunance Files and Output Single DF
rm("N.bias.raw")
for(cov in 1:length(trialnames)){
  N.bias.sing <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/", trialnames[cov]
                                , " - Master N Bias.csv", sep = ""), check.names=FALSE)
  
  if(exists("N.bias.raw")){
    N.bias.raw <- rbind(N.bias.raw, N.bias.sing)
  }else{
    N.bias.raw <- N.bias.sing 
  }
}

###Summarize Abundance Bias
adultN.long <- N.bias.raw %>% filter(BiasType == "Rel.Bias", WMD != -999, Age == "A") %>%
  select(-Age, -BiasType) %>% 
  pivot_longer(cols = 4:21, names_to = "Year")
mean(adultN.long$value, na.rm = T)
sd(adultN.long$value, na.rm = T)

juvN.long <- N.bias.raw %>% filter(BiasType == "Rel.Bias", WMD != -999, Age == "J") %>%
  select(-Age, -BiasType) %>% 
  pivot_longer(cols = 4:21, names_to = "Year")
mean(juvN.long$value, na.rm = T)
sd(juvN.long$value, na.rm = T)


# Load Harvest Rate Files and Output Single DF
rm("HR.bias.raw")
for(cov in 1:length(trialnames)){
  HR.bias.sing <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/", trialnames[cov]
                                , " - Master HR Bias.csv", sep = ""), check.names=FALSE)
  
  if(exists("HR.bias.raw")){
    HR.bias.raw <- rbind(HR.bias.raw, HR.bias.sing)
  }else{
    HR.bias.raw <- HR.bias.sing 
  }
}

###Summarize Harvest Rate Bias
adultHR.long <- HR.bias.raw %>% filter(Age == "A") %>%
  mutate(Rel.Bias = (Real - Est)/Real)
mean(adultHR.long$Rel.Bias, na.rm = T)
sd(adultHR.long$Rel.Bias, na.rm = T)

juvHR.long <- HR.bias.raw %>% filter(Age == "J") %>%
  mutate(Rel.Bias = (Real - Est)/Real)
mean(juvHR.long$Rel.Bias, na.rm = T)
sd(juvHR.long$Rel.Bias, na.rm = T)


# Load Survival Files and Output Single DF
rm("S.bias.raw")
for(cov in 1:length(trialnames)){
  S.bias.sing <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/", trialnames[cov]
                                 , " - Master WSR Bias.csv", sep = ""), check.names=FALSE)
  
  if(exists("S.bias.raw")){
    S.bias.raw <- rbind(S.bias.raw, S.bias.sing)
  }else{
    S.bias.raw <- S.bias.sing 
  }
}

###Summarize Survival Rate Bias
adultS.long <- S.bias.raw %>% filter(Age == "A") %>%
  mutate(Rel.Bias = (Real - Est)/Real)
mean(adultS.long$Rel.Bias, na.rm = T)
sd(adultS.long$Rel.Bias, na.rm = T)

juvS.long <- S.bias.raw %>% filter(Age == "J") %>%
  mutate(Rel.Bias = (Real - Est)/Real)
mean(juvS.long$Rel.Bias, na.rm = T)
sd(juvS.long$Rel.Bias, na.rm = T)
