require(ggplot2)
require(dplyr)
require(tidyr)

# Load Files
N.bias.raw <- read.csv("Model Bias Comparison/SampleSize/Master N Bias.csv", check.names=FALSE)
HR.bias.raw <- read.csv("Model Bias Comparison/SampleSize/Master HR Bias.csv")
WSR.bias.raw <- read.csv("Model Bias Comparison/SampleSize/Master WSR Bias.csv")
R.bias.raw <- read.csv("Model Bias Comparison/SampleSize/Master R Bias.csv", check.names=FALSE)
SimInfo.raw <- read.csv("Model Bias Comparison/SampleSize/MasterSimInfo.csv")

N.bias.long <- N.bias.raw %>%
  pivot_longer(cols = 4:ncol(N.bias.raw)) %>%
  rename(Year = name) %>%
  filter(!is.na(value))

SimInfo.NSitesBR <- SimInfo.raw %>%
  filter(Name == "NSites BR") %>%
  rename(NSites_BR = Value) %>% 
  select(-Name)
SimInfo.NSitesWSR <- SimInfo.raw %>%
  filter(Name == "NSites WSR")%>%
  rename(NSites_WSR = Value) %>% 
  select(-Name)
SimInfo.NIndBR <- SimInfo.raw %>%
  filter(Name == "NInd BR") %>%
  rename(NInd_BR = Value) %>% 
  select(-Name)
SimInfo.NIndWSR <- SimInfo.raw %>%
  filter(Name == "NInd WSR")%>%
  rename(NInd_WSR = Value) %>% 
  select(-Name)
SimInfo.totaldata <- SimInfo.raw %>%
  filter(Name %in% c("YearsBR", "TotNIndBR", "YearsTotHarv", "YearsWSR", "TotNIndBR")) %>%
  select(-WMD) %>%
  pivot_wider(names_from = Name, values_from = Value)

#Individual WMD Bias
N.bias <- merge(N.bias.long, SimInfo.NSitesBR, by = c("WMD","Trial"))
N.bias <- merge(N.bias, SimInfo.NSitesWSR, by = c("WMD","Trial"))
N.bias <- merge(N.bias, SimInfo.NIndBR, by = c("WMD","Trial"))
N.bias <- merge(N.bias, SimInfo.NIndWSR, by = c("WMD","Trial"))
N.bias <- merge(N.bias, SimInfo.totaldata, by = c("Trial")) %>%
  group_by(Age) %>%
  mutate(NInd_BR_bin = as.numeric(cut(NInd_BR, breaks=(10*(1:ceiling(max(N.bias$NInd_BR)/10)-1)),
                           labels=10*(10*(2:ceiling(max(N.bias$NInd_BR)/10)-1))))) %>%
  mutate(NInd_WSR_bin = as.numeric(cut(NInd_WSR, breaks=(10*(1:ceiling(max(N.bias$NInd_WSR)/10)-1)),
                           labels=(10*(2:ceiling(max(N.bias$NInd_WSR)/10)-1))))) %>%
  mutate(NInd_BR_bin = ifelse(is.na(NInd_BR_bin), 0, 10*NInd_BR_bin)) %>%
  mutate(NInd_WSR_bin = ifelse(is.na(NInd_WSR_bin), 0, 10*NInd_WSR_bin))

hist(N.bias$value, xlim = c(0,500), breaks = 5000)

ggplot(N.bias, aes(x = NSites_BR, y = value)) +
  geom_point(aes(color = Age))
ggplot(N.bias, aes(x = NSites_WSR, y = value)) +
  geom_point(aes(color = Age))
ggplot(N.bias, aes(x = NInd_BR, y = value)) +
  geom_point(aes(color = Age))
ggplot(N.bias, aes(x = NInd_WSR, y = value)) +
  geom_point(aes(color = Age))

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
