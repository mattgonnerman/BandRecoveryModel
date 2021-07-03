require(ggplot2)
require(dplyr)
require(tidyr)
require(wesanderson)
require(stringr)

trialname <- "LowRange"

# Load Files
N.bias.raw <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/", trialname, " - Master N Bias.csv", sep = ""), check.names=FALSE)
HR.bias.raw <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/", trialname, " - Master HR Bias.csv", sep = ""))
WSR.bias.raw <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/", trialname, " - Master WSR Bias.csv", sep = ""))
R.bias.raw <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/", trialname, " - Master R Bias.csv", sep = ""), check.names=FALSE)
SimInfo.raw <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/", trialname, " - MasterSimInfo.csv", sep = ""))

N.bias.long <- N.bias.raw %>%
  pivot_longer(cols = 6:ncol(N.bias.raw)) %>%
  rename(Year = name) %>%
  filter(!is.na(value))

SimInfo.NSitesBR <- SimInfo.raw %>%
  filter(Name == "NSites BR") %>%
  rename(NSites_BR = Value) %>% 
  dplyr::select(-Name)
SimInfo.NSitesWSR <- SimInfo.raw %>%
  filter(Name == "NSites WSR")%>%
  rename(NSites_WSR = Value) %>% 
  dplyr::select(-Name)
SimInfo.NIndBR <- SimInfo.raw %>%
  filter(Name == "NInd BR") %>%
  rename(NInd_BR = Value) %>% 
  dplyr::select(-Name)
SimInfo.NIndWSR <- SimInfo.raw %>%
  filter(Name == "NInd WSR")%>%
  rename(NInd_WSR = Value) %>% 
  dplyr::select(-Name)
SimInfo.totaldata <- SimInfo.raw %>%
  filter(Name %in% c("YearsBR", "TotNIndBR", "YearsTotHarv", "YearsWSR", "TotNIndBR")) %>%
  dplyr::select(-WMD) %>%
  pivot_wider(names_from = Name, values_from = Value)

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

#loop through individual model outputs and save as one big dataframe
rm(raw.model.output)
for(num in 1:100){
  ind.model.output <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/", trialname, " - Sim", num," - RawModel Estimates.csv", sep = ""), check.names=FALSE)
  colnames(ind.model.output)[1] <- "ID"
  ind.model.output$Trial <- num
  ind.model.output$Parameter <- trialname
  
  if(exists("raw.model.output")){
    raw.model.output <- rbind(raw.model.output, ind.model.output)
  }else{
    raw.model.output <- ind.model.output 
  }
  
}

Nest.A.rhat.values <-raw.model.output %>% filter(substr(ID, 1,3) == "N.A") %>%
  mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\,)")) %>%
  mutate(Year = as.numeric(str_extract(ID, "(?<=\\,).*?(?=\\])"))) %>%
  dplyr::select(Parameter, WMD, Year, Trial, Raw.Est = mean,  Rhat)

Nest.J.rhat.values <-raw.model.output %>% filter(substr(ID, 1,3) == "N.J") %>%
  mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\,)")) %>%
  mutate(Year = as.numeric(str_extract(ID, "(?<=\\,).*?(?=\\])"))) %>%
  dplyr::select(Parameter, WMD, Year, Trial, Raw.Est = mean,  Rhat)

N.A.est.bias.rhat <- merge(abs.rel.N.bias.A, Nest.A.rhat.values, by = c("Trial", "WMD", "Year"))
N.J.est.bias.rhat <- merge(abs.rel.N.bias.J, Nest.J.rhat.values, by = c("Trial", "WMD", "Year"))

### Relative Bias X Absolute Bias HexBin Plot
ggplot(N.A.est.bias.rhat, aes(x = log(Raw.Est), y = Rel.Bias)) +
  geom_hex(bins = 50) +
  scale_fill_gradientn(colours = wes_palette(n=5, name="Zissou1"),
                      values = c(0, .001, .05, .2, 1)) +
  labs(title = paste("Adult Males | ", trialname, sep = "")) +
  ylab("Relative Bias") + xlab("log(Real Value)") +
  theme_classic(base_size = 25)
ggsave(device = "jpeg", width = 14, height = 10,
       paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/Graphs/", trialname, " - RelBiasXReal_A_WMD.jpeg", sep = ""))

# ggplot(N.A.est.bias.rhat, aes(x = Raw.Est, y = Rel.Bias)) +
#   geom_hex(bins = 50) +
#   scale_fill_gradientn(colours = wes_palette(n=5, name="Zissou1"),
#                        values = c(0, .001, .05, .2, 1)) +
#   labs(title = "Real Estimates X Relative Bias (WMD Specific)", subtitle = "Adult Males") +
#   ylab("Relative Bias") + xlab("Real Value") +
#   theme_classic(base_size = 25) +
#   ylim(-1,1) +
#   xlim(0,10000)
# ggsave(device = "jpeg", width = 14, height = 10,
#        paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/Graphs/", trialname, " - RelBiasXReal_A_WMD_ZOOM.jpeg", sep = ""))

ggplot(N.J.est.bias.rhat, aes(x = log(Raw.Est), y = Rel.Bias)) +
  geom_hex(bins = 50) +
  scale_fill_gradientn(colours = wes_palette(n=5, name="Zissou1"),
                       values = c(0, .001, .05, .2, 1)) +
  labs(title = paste("Juvenile Males | ", trialname, sep = "")) +
  ylab("Relative Bias") + xlab("log(Real Value)") +
  theme_classic(base_size = 25)
ggsave(device = "jpeg", width = 14, height = 10,
       paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/Graphs/", trialname, " - RelBiasXReal_J_WMD.jpeg", sep = ""))

# ggplot(N.J.est.bias.rhat, aes(x = Raw.Est, y = Rel.Bias)) +
#   geom_hex(bins = 50) +
#   scale_fill_gradientn(colours = wes_palette(n=5, name="Zissou1"),
#                        values = c(0, .001, .05, .2, 1)) +
#   labs(title = "Real Estimates X Relative Bias (WMD Specific)", subtitle = "Juvenile Males | Medium Paramater Values") +
#   ylab("Relative Bias") + xlab("Real Value") +
#   theme_classic(base_size = 25) +
#   ylim(-1,1) +
#   xlim(0,10000)
# ggsave(device = "jpeg", width = 14, height = 10,
#        paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/Graphs/", trialname, " - RelBiasXReal_J_WMD_ZOOM.jpeg", sep = ""))

### Absolute Bias X Absolute Bias HexBin Plot
ggplot(N.A.est.bias.rhat, aes(x = Raw.Est, y = Abs.Bias)) +
  geom_hex(bins = 50) +
  scale_fill_gradientn(colours = wes_palette(n=5, name="Zissou1"),
                       values = c(0, .001, .05, .2, 1)) +
  labs(title = "Real Estimates X Absolute Bias (WMD Specific)", subtitle = "Adult Males | Medium Paramater Values") +
  ylab("Absolute Bias") + xlab("Real Value") +
  theme_classic(base_size = 25)
ggsave(device = "jpeg", width = 14, height = 10,
       paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/Graphs/", trialname, " - AbsbiasXReal_A_WMD.jpeg", sep = ""))

ggplot(N.A.est.bias.rhat, aes(x = Raw.Est, y = Abs.Bias)) +
  geom_hex(bins = 50) +
  scale_fill_gradientn(colours = wes_palette(n=5, name="Zissou1"),
                       values = c(0, .001, .05, .2, 1)) +
  labs(title = "Real Estimates X Absolute Bias (WMD Specific)", subtitle = "Adult Males | Medium Paramater Values") +
  ylab("Absolute Bias") + xlab("Real Value") +
  theme_classic(base_size = 25) +
  ylim(-1000,1000)
ggsave(device = "jpeg", width = 14, height = 10,
       paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/Graphs/", trialname, " - AbsbiasXReal_A_WMD_ZOOM.jpeg", sep = ""))

ggplot(N.J.est.bias.rhat, aes(x = Raw.Est, y = Abs.Bias)) +
  geom_hex(bins = 50) +
  scale_fill_gradientn(colours = wes_palette(n=5, name="Zissou1"),
                       values = c(0, .001, .05, .2, 1)) +
  labs(title = "Real Estimates X Absolute Bias (WMD Specific)", subtitle = "Juvenile Males | Medium Paramater Values") +
  ylab("Absolute Bias") + xlab("Real Value") +
  theme_classic(base_size = 25)
ggsave(device = "jpeg", width = 14, height = 10,
       paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/Graphs/", trialname, " - AbsbiasXReal_J_WMD.jpeg", sep = ""))

ggplot(N.J.est.bias.rhat, aes(x = Raw.Est, y = Abs.Bias)) +
  geom_hex(bins = 50) +
  scale_fill_gradientn(colours = wes_palette(n=5, name="Zissou1"),
                       values = c(0, .001, .05, .2, 1)) +
  labs(title = "Real Estimates X Absolute Bias (WMD Specific)", subtitle = "Juvenile Males | Medium Paramater Values") +
  ylab("Absolute Bias") + xlab("Real Value") +
  theme_classic(base_size = 25) +
  ylim(-1000,1000)
ggsave(device = "jpeg", width = 14, height = 10,
       paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/Graphs/", trialname, " - AbsbiasXReal_J_WMD_ZOOM.jpeg", sep = ""))


#Individual WMD Absolute Bias BoxPlots
N.bias <- merge(N.bias.long, SimInfo.NSitesBR, by = c("WMD","Trial", "Parameter"))
N.bias <- merge(N.bias, SimInfo.NSitesWSR, by = c("WMD","Trial", "Parameter"))
N.bias <- merge(N.bias, SimInfo.NIndBR, by = c("WMD","Trial", "Parameter"))
N.bias <- merge(N.bias, SimInfo.NIndWSR, by = c("WMD","Trial", "Parameter"))
N.bias <- merge(N.bias, SimInfo.totaldata, by = c("Trial", "Parameter")) %>%
  filter(WMD != -999) %>%
  filter(BiasType == "Abs.Bias") %>% 
  arrange(Trial, Age, WMD, Year) %>%
  group_by(Age) %>%
  mutate(NInd_BR_bin = as.numeric(cut(NInd_BR, breaks=(10*(1:ceiling(max(N.bias$NInd_BR)/10)-1)),
                                      labels=10*(10*(2:ceiling(max(N.bias$NInd_BR)/10)-1))))) %>%
  mutate(NInd_WSR_bin = as.numeric(cut(NInd_WSR, breaks=(10*(1:ceiling(max(N.bias$NInd_WSR)/10)-1)),
                                       labels=(10*(2:ceiling(max(N.bias$NInd_WSR)/10)-1))))) %>%
  mutate(NInd_BR_bin = ifelse(is.na(NInd_BR_bin), 0, 10*NInd_BR_bin)) %>%
  mutate(NInd_WSR_bin = ifelse(is.na(NInd_WSR_bin), 0, 10*NInd_WSR_bin))

hist(N.bias$value, breaks = 5000)

ggplot(N.bias, aes(x = Age, y = value, fill = factor(Age))) +
  geom_violin(outlier.shape = NA) + ylim(-500,500)

ggplot(N.bias, aes(x = NSites_BR, y = value, color = factor(NSites_BR))) +
  geom_violin(outlier.shape = NA) + ylim(-500,500)
ggplot(N.bias, aes(x = NSites_WSR, y = value, color = factor(NSites_WSR))) +
  geom_boxplot(outlier.shape = NA) + ylim(-500,500)
ggplot(N.bias, aes(x = NInd_BR_bin, y = value, color = factor(NInd_BR_bin))) +
  geom_boxplot(outlier.shape = NA) + ylim(-500,500)
ggplot(N.bias, aes(x = NInd_WSR_bin, y = value, color = factor(NInd_WSR_bin))) +
  geom_boxplot(outlier.shape = NA) + ylim(-500,500)


#Total Relative Bias BoxPlots
N.bias <- merge(N.bias.long, SimInfo.totaldata, by = c("Trial", "Parameter")) %>%
  filter(WMD == -999) %>%
  filter(BiasType == "Rel.Bias") %>% 
  arrange(Trial, Age, WMD, Year) %>%
  group_by(Age) 

hist(N.bias$value, xlim = c(-1,1))

ggplot(N.bias, aes(x = NSites_BR, y = value, color = factor(NSites_BR))) +
  geom_boxplot(outlier.shape = NA) + ylim(-1,1)
ggplot(N.bias, aes(x = NSites_WSR, y = value, color = factor(NSites_WSR))) +
  geom_boxplot(outlier.shape = NA) + ylim(-1,1)
ggplot(N.bias, aes(x = NInd_BR, y = value, color = factor(NInd_BR))) +
  geom_boxplot(outlier.shape = NA) + ylim(-1,1)
ggplot(N.bias, aes(x = NInd_WSR, y = value, color = factor(NInd_WSR))) +
  geom_boxplot(outlier.shape = NA) + ylim(-1,1)




#Density Plots
ggplot(N.bias, aes(x = value, group = Age)) +
  geom_density(aes(fill = Age), alpha = .7) +
  xlim(-500, 400) +
  labs(title = "Bias in WMD Specific Abundance",
       subtitle = "Juvenile vs Adult") +
  xlab("Difference From Real Value")
ggsave("Model Bias Comparison/SampleSize/Graphs/Nbias_Age.jpg")


require(RColorBrewer)
nb.cols <- length(unique(N.bias$NSites_BR))
mycolors <- colorRampPalette(brewer.pal(8, "Spectral"))(nb.cols)
ggplot(N.bias[N.bias$Age=="A",], aes(x = value, group = as.factor(NSites_BR))) +
  geom_density(aes(fill = as.factor(NSites_BR)), alpha = .7) +
  xlim(-1000, 300) +
  scale_fill_manual(values = mycolors) +
  labs(title = "Bias in WMD Specific Abundance",
       subtitle = "Number BR Sites in WMD | Adult") +
  xlab("Difference From Real Value") + 
  guides(fill=guide_legend(title="# BR Sites\nIn Region"))
ggsave("Model Bias Comparison/SampleSize/Graphs/Nbias_NSitesBR_A.jpg")

ggplot(N.bias[N.bias$Age=="J",], aes(x = value, group = as.factor(NSites_BR))) +
  geom_density(aes(fill = as.factor(NSites_BR)), alpha = .7) +
  xlim(-1000, 300)  +
  scale_fill_manual(values = mycolors) +
  labs(title = "Bias in WMD Specific Abundance",
       subtitle = "Number BR Sites in WMD | Juv") +
  xlab("Difference From Real Value") + 
  guides(fill=guide_legend(title="# BR Sites\nIn Region"))
ggsave("Model Bias Comparison/SampleSize/Graphs/Nbias_NSitesBR_J.jpg")

nb.cols <- length(unique(N.bias$NSites_WSR))
mycolors <- colorRampPalette(brewer.pal(8, "Spectral"))(nb.cols)
ggplot(N.bias[N.bias$Age=="A",], aes(x = value, group = as.factor(NSites_WSR))) +
  geom_density(aes(fill = as.factor(NSites_WSR)), alpha = .7) +
  xlim(-1000, 300) +
  scale_fill_manual(values = mycolors) +
  labs(title = "Bias in WMD Specific Abundance",
       subtitle = "Number WSR Sites in WMD | Adult") +
  xlab("Difference From Real Value") + 
  guides(fill=guide_legend(title="# WSR Sites\nIn Region"))
ggsave("Model Bias Comparison/SampleSize/Graphs/Nbias_NSitesWSR_A.jpg")

ggplot(N.bias[N.bias$Age=="J",], aes(x = value, group = as.factor(NSites_WSR))) +
  geom_density(aes(fill = as.factor(NSites_WSR)), alpha = .7) +
  xlim(-1000, 300)   +
  scale_fill_manual(values = mycolors) +
  labs(title = "Bias in WMD Specific Abundance",
       subtitle = "Number WSR Sites in WMD | Juv") +
  xlab("Difference From Real Value") + 
  guides(fill=guide_legend(title="# WSR Sites\nIn Region"))
ggsave("Model Bias Comparison/SampleSize/Graphs/Nbias_NSitesWSR_J.jpg")

nb.cols <- length(unique(N.bias$NInd_BR_bin))
mycolors <- colorRampPalette(brewer.pal(8, "Spectral"))(nb.cols)
ggplot(N.bias[N.bias$Age=="A",], aes(x = value, group = as.factor(NInd_BR_bin))) +
  geom_density(aes(fill = as.factor(NInd_BR_bin)), alpha = .7) +
  xlim(-1000, 300) +
  scale_fill_manual(values = mycolors) +
  labs(title = "Bias in WMD Specific Abundance",
       subtitle = "Number BR Individuals in WMD (Binned by 10s) | Adult") +
  xlab("Difference From Real Value") + 
  guides(fill=guide_legend(title="# BR Individuals\nIn Region"))
ggsave("Model Bias Comparison/SampleSize/Graphs/Nbias_NIndBR_A.jpg")

ggplot(N.bias[N.bias$Age=="J",], aes(x = value, group = as.factor(NInd_BR_bin))) +
  geom_density(aes(fill = as.factor(NInd_BR_bin)), alpha = .7) +
  xlim(-1000, 300) +
  scale_fill_manual(values = mycolors) +
  labs(title = "Bias in WMD Specific Abundance",
       subtitle = "Number BR Individuals in WMD (Binned by 10s) | Juv") +
  xlab("Difference From Real Value") + 
  guides(fill=guide_legend(title="# BR Indiviudals\nIn Region"))
ggsave("Model Bias Comparison/SampleSize/Graphs/Nbias_NIndBR_J.jpg")

nb.cols <- length(unique(N.bias$NInd_WSR_bin))
mycolors <- colorRampPalette(brewer.pal(8, "Spectral"))(nb.cols)
ggplot(N.bias[N.bias$Age=="A",], aes(x = value, group = as.factor(NInd_WSR_bin))) +
  geom_density(aes(fill = as.factor(NInd_WSR_bin)), alpha = .7) +
  xlim(-1000, 300) +
  scale_fill_manual(values = mycolors) +
  labs(title = "Bias in WMD Specific Abundance",
       subtitle = "Number WSR Individuals in WMD (Binned by 10s) | Adult") +
  xlab("Difference From Real Value") + 
  guides(fill=guide_legend(title="# WSR Individuals\nIn Region"))
ggsave("Model Bias Comparison/SampleSize/Graphs/Nbias_NIndWSR_A.jpg")

ggplot(N.bias[N.bias$Age=="J",], aes(x = value, group = as.factor(NInd_WSR_bin))) +
  geom_density(aes(fill = as.factor(NInd_WSR_bin)), alpha = .7) +
  xlim(-1000, 300)  +
  scale_fill_manual(values = mycolors) +
  labs(title = "Bias in WMD Specific Abundance",
       subtitle = "Number WSR Individuals in WMD (Binned by 10s) | Juv") +
  xlab("Difference From Real Value") + 
  guides(fill=guide_legend(title="# WSR Individuals\nIn Region"))
ggsave("Model Bias Comparison/SampleSize/Graphs/Nbias_NIndWSR_J.jpg")




#Point Graphs
ggplot(N.bias[N.bias$Age=="A",], aes(x = as.factor(NSites_BR), y = value)) +
  geom_point(aes(color = Age))
ggplot(N.bias[N.bias$Age=="J",], aes(x = as.factor(NSites_BR), y = value)) +
  geom_violin(aes(color = Age))
ggplot(N.bias[N.bias$Age=="A",], aes(x = NSites_WSR, y = value)) +
  geom_point(aes(color = Age))
ggplot(N.bias[N.bias$Age=="A",], aes(x = NInd_BR, y = value)) +
  geom_point(aes(color = Age))
ggplot(N.bias[N.bias$Age=="A",], aes(x = NInd_WSR, y = value)) +
  geom_point(aes(color = Age))
