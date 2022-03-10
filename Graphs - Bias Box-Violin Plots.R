require(ggplot2)
require(dplyr)
require(tidyr)
require(wesanderson)
require(stringr)

### Load Files ###
# Create vector of names for use in looping through data
trialnames <- c("MedAll", "LowNugget", "HighNugget", "LowPSill", "HighPSill", "LowRange", "HighRange")

#Load Abundance Bias results
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


#Format abundance bias data
N.bias.long <- N.bias.raw %>%
  pivot_longer(cols = 6:ncol(N.bias.raw)) %>%
  rename(Year = name) %>%
  filter(!is.na(value))

abs.N.bias.A <- N.bias.long %>% filter(BiasType == "Abs.Bias", Age == "A") %>% 
  rename(Abs.Bias = value) %>% dplyr::select(-Parameter, -BiasType)
abs.N.bias.J <- N.bias.long %>% filter(BiasType == "Abs.Bias", Age == "J") %>% 
  rename(Abs.Bias = value) %>% dplyr::select(-Parameter, -BiasType)
rel.N.bias.A <- N.bias.long %>% filter(BiasType == "Rel.Bias", Age == "A") %>% 
  rename(Rel.Bias = value) %>% dplyr::select(-Parameter, -BiasType)
rel.N.bias.J <- N.bias.long %>% filter(BiasType == "Rel.Bias", Age == "J") %>% 
  rename(Rel.Bias = value) %>% dplyr::select(-Parameter, -BiasType)

abs.rel.N.bias.A <- merge(abs.N.bias.A, rel.N.bias.A, by = c("Trial", "WMD", "Year", "Age")) %>%
  filter(WMD != -999)
abs.rel.N.bias.J <- merge(abs.N.bias.J, rel.N.bias.J, by = c("Trial", "WMD", "Year", "Age")) %>%
  filter(WMD != -999)


#Load Simulation Information
rm("SimInfo.raw")
for(cov in 1:length(trialnames)){
  siminfo.sing <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/", trialnames[cov]
                                , " - MasterSimInfo.csv", sep = ""), check.names=FALSE)
  
  if(exists("SimInfo.raw")){
    SimInfo.raw <- rbind(SimInfo.raw, siminfo.sing)
  }else{
    SimInfo.raw <- siminfo.sing 
  }
}

#Individual WMD sample size
siminfo.nindBR <- SimInfo.raw %>% filter(Name == "NInd BR") %>%
  rename(NIndBR = Value) %>% dplyr::select(-Name) %>% filter(WMD != -999)
siminfo.nindWSR <- SimInfo.raw %>% filter(Name == "NInd WSR") %>%
  rename(NIndWSR = Value) %>% dplyr::select(-Name) %>% filter(WMD != -999)
siminfo.nsitesBR <- SimInfo.raw %>% filter(Name == "NSites BR") %>%
  rename(NSiteBR = Value) %>% dplyr::select(-Name) %>% filter(WMD != -999)
siminfo.nsitesWSR <- SimInfo.raw %>% filter(Name == "NSites WSR") %>%
  rename(NSiteWSR = Value) %>% dplyr::select(-Name) %>% filter(WMD != -999)
siminfo.YearsBR <- SimInfo.raw %>% filter(Name == "YearsBR") %>%
  rename(NYearBR = Value) %>% dplyr::select(-Name, -WMD)

siminfo.Total <- SimInfo.raw %>% filter(is.na(WMD)) %>% dplyr::select(-WMD) %>%
  pivot_wider(id_cols = c(Parameter, Trial), values_from = Value, names_from = Name)

#Merge SimInfo and Abundance Bias by Parameter, Trial, WMD

N.Bias.SimInfo1 <- merge(N.bias.long, siminfo.nindBR, by = c("Parameter", "WMD", "Trial"), all.x = T)
N.Bias.SimInfo2 <- merge(N.Bias.SimInfo1, siminfo.nindWSR, by = c("Parameter", "WMD", "Trial"), all.x = T)
N.Bias.SimInfo3 <- merge(N.Bias.SimInfo2, siminfo.nsitesBR, by = c("Parameter", "WMD", "Trial"), all.x = T)
N.Bias.SimInfo4 <- merge(N.Bias.SimInfo3, siminfo.nsitesWSR, by = c("Parameter", "WMD", "Trial"), all.x = T)
N.Bias.SimInfo5 <- merge(N.Bias.SimInfo4, siminfo.YearsBR, by = c("Parameter", "Trial"), all.x = T) %>%
  filter(WMD != -999) %>%
  filter(BiasType == "Rel.Bias") %>%
  mutate(Year = as.numeric(Year)) %>%
  rename(RelBias = value) %>%
  arrange(Parameter, Trial, WMD, Age, Year) %>%
  mutate(NIndBRbin = cut(NIndBR, breaks = quantile(NIndBR, c(0,.2,.4,.6,.8,1)) - c(.01, 0,0,0,0, -.1)))%>%
  mutate(NIndWSRbin = cut(NIndWSR, breaks = quantile(NIndWSR, c(0,.2,.4,.6,.8,1)) - c(.01, 0,0,0,0, -.1))) %>%
  mutate(NSiteBRbin = cut(NSiteBR, breaks = c(-0.1, 4.1, 5.1, 6.1, 8.1, 18)))%>%
  mutate(NSiteWSRbin = cut(NSiteWSR, breaks = c(-0.1,.1,1.1,2.1, 3.1, 10) )) %>%
  mutate(Parameter = factor(Parameter, levels = c("MedAll", "LowNugget", "HighNugget", "LowPSill", "HighPSill", "LowRange", "HighRange")))
N.Bias.SimInfo <- merge(N.Bias.SimInfo5, siminfo.Total, by = c("Parameter", "Trial"), all.x = T)

### BoxPlots ###
#Number of Banded Individuals
nindband.plot <- ggplot(data = N.Bias.SimInfo, aes(x=NIndBRbin, y = RelBias, fill = as.factor(Parameter))) +
  geom_boxplot(outlier.shape = NA, lwd = 1) +
  theme_classic(base_size = 20)+
  labs(x = "Number of Banded Individuals (Binned)", y = "Relative Bias (Abundance)") +
  scale_x_discrete(labels = c("0-60", "60-88", "88-119", "119-163", "163-456")) +
  theme(legend.position = "none")
# ggsave("E:/GitHub/BandRecoveryModel/Graphs/BoxPlot - NBandInd.jpg")

#Number of Transmittered Individuals
nindtrans.plot <- ggplot(data = N.Bias.SimInfo, aes(x=NIndWSRbin, y = RelBias, fill = as.factor(Parameter))) +
  geom_boxplot(outlier.shape = NA, lwd = 1) +
  theme_classic(base_size = 20) +
  labs(x = "Number of Transmitters Deployed (Binned)", y = "Relative Bias (Abundance)") +
  scale_x_discrete(labels = c("0-4", "4-10", "10-16", "16-25", "25-97")) +
  theme(legend.position = "none")
# ggsave("E:/GitHub/BandRecoveryModel/Graphs/BoxPlot - NTransmitterInd.jpg")

#Number of Banding Sites
nsitesband.plot <- ggplot(data = N.Bias.SimInfo, aes(x=as.factor(NSiteBRbin), y = RelBias, fill = as.factor(Parameter))) +
  geom_boxplot(outlier.shape = NA, lwd = 1) +
  theme_classic(base_size = 20)+
  labs(x = "Number of Banding Sites (Binned)", y = "Relative Bias (Abundance)") +
  scale_x_discrete(labels = c("0-4", "5", "6", "7-8", "9-18")) +
  theme(legend.position = "none")
# ggsave("E:/GitHub/BandRecoveryModel/Graphs/BoxPlot - NBandSite.jpg")

#Number of Transmitter Sites
nsitestrans.plot <- ggplot(data = N.Bias.SimInfo, aes(x=as.factor(NSiteWSRbin), y = RelBias, fill = as.factor(Parameter))) +
  geom_boxplot(outlier.shape = NA, lwd = 1) +
  theme_classic(base_size = 20)+
  labs(x = "Number of Transmitter Sites (Binned)", y = "Relative Bias (Abundance)") +
  scale_x_discrete(labels = c("0", "1", "2", "3", "4-10")) +
  theme(legend.position = "none")
# ggsave("E:/GitHub/BandRecoveryModel/Graphs/BoxPlot - NTransmitterSite.jpg")

#Number of Years Banding
nyearband.plot <- ggplot(data = N.Bias.SimInfo, aes(x=as.factor(NYearBR), y = RelBias, fill = as.factor(Parameter))) +
  geom_boxplot(outlier.shape = NA, lwd = 1) +
  theme_classic(base_size = 20)+
  labs(x = "Number of Years Banding", y = "Relative Bias (Abundance)") +
  theme(legend.position = "none")
# ggsave("E:/GitHub/BandRecoveryModel/Graphs/BoxPlot - NYearsBR.jpg")

#Total Number of Banded Individuals
ntotband.plot <- ggplot(data = N.Bias.SimInfo, aes(x=as.factor(TotNIndBR), y = RelBias, fill = as.factor(Parameter))) +
  geom_boxplot(outlier.shape = NA, lwd = 1) +
  theme_classic(base_size = 20)+
  labs(x = "Total Banded Individuals (Simulation)", y = "Relative Bias (Abundance)") +
  theme(legend.position = "none")
# ggsave("E:/GitHub/BandRecoveryModel/Graphs/BoxPlot - NYearsBR.jpg")

#Total Number of Transmittered Individuals
ntottrans.plot <- ggplot(data = N.Bias.SimInfo, aes(x=as.factor(TotNIndWSR), y = RelBias, fill = as.factor(Parameter))) +
  geom_boxplot(outlier.shape = NA, lwd = 1) +
  theme_classic(base_size = 20)+
  labs(x = "Total Transmitters Deployed (Simulation)", y = "Relative Bias (Abundance)") +
  theme(legend.position = "none")
# ggsave("E:/GitHub/BandRecoveryModel/Graphs/BoxPlot - NYearsBR.jpg")

samplesize.plot <- list(nindband.plot, nindtrans.plot,
                        nsitesband.plot, nsitestrans.plot,
                        ntotband.plot, ntottrans.plot,
                        nyearband.plot)

require(cowplot)
samplesize_grid <- plot_grid(plotlist = samplesize.plot,
                           nrow = 4,
                           labels = "AUTO",
                           label_size = 20,
                           align = "hv",
                           axis = "lb")

jpeg("Graphs/Supplemental Sample Size Bias Grid.jpg", width = 5000, height = 6500, res = 300)
samplesize_grid
dev.off()






# ### Violin Plots ###
# #Number of Banded Individuals
# ggplot(data = N.Bias.SimInfo, aes(x=NIndBRbin, y = RelBias, fill = as.factor(Parameter))) +
#   geom_violin(outlier.shape = NA) +
#   theme_classic()+
#   labs(x = "Number of Banded Individuals (Region)", y = "Relative Bias (Region)")
# ggsave("E:/GitHub/BandRecoveryModel/Graphs/ViolinPlot - NBandInd.jpg")
# 
# #Number of Transmittered Individuals
# ggplot(data = N.Bias.SimInfo, aes(x=NIndWSRbin, y = RelBias, fill = as.factor(Parameter))) +
#   geom_violin(outlier.shape = NA) +
#   theme_classic()+
#   labs(x = "Number of Transmitters Deployed (Region)", y = "Relative Bias (Region)")
# ggsave("E:/GitHub/BandRecoveryModel/Graphs/ViolinPlot - NTransmitterInd.jpg")
# 
# #Number of Banding Sites
# ggplot(data = N.Bias.SimInfo, aes(x=as.factor(NSiteBRbin), y = RelBias, fill = as.factor(Parameter))) +
#   geom_violin(outlier.shape = NA) +
#   theme_classic()+
#   labs(x = "Number of Banding Sites (Region)", y = "Relative Bias (Region)")
# ggsave("E:/GitHub/BandRecoveryModel/Graphs/ViolinPlot - NBandSite.jpg")
# 
# #Number of Transmitter Sites
# ggplot(data = N.Bias.SimInfo, aes(x=as.factor(NSiteWSRbin), y = RelBias, fill = as.factor(Parameter))) +
#   geom_violin(outlier.shape = NA) +
#   theme_classic()+
#   labs(x = "Number of Transmitter Sites (Region)", y = "Relative Bias (Region)")
# ggsave("E:/GitHub/BandRecoveryModel/Graphs/ViolinPlot - NTransmitterSite.jpg")
# 
# #Number of Years Banding
# ggplot(data = N.Bias.SimInfo, aes(x=as.factor(NYearBR), y = RelBias, fill = as.factor(Parameter))) +
#   geom_violin(outlier.shape = NA) +
#   theme_classic()+
#   labs(x = "Number of Years Banding", y = "Relative Bias (Region)")
# ggsave("E:/GitHub/BandRecoveryModel/Graphs/ViolinPlot - NYearsBR.jpg")