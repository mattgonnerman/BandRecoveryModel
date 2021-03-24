require(ggplot2)
require(dplyr)
require(tidyr)

# Load Files
N.bias.raw <- read.csv("Model Bias Comparison/SampleSize/Master N Bias.csv")
HR.bias.raw <- read.csv("Model Bias Comparison/SampleSize/Master HR Bias.csv")
WSR.bias.raw <- read.csv("Model Bias Comparison/SampleSize/Master WSR Bias.csv")
R.bias.raw <- read.csv("Model Bias Comparison/SampleSize/Master R Bias.csv")
SimInfo.raw <- read.csv("Model Bias Comparison/SampleSize/MasterSimInfo.csv")

N.bias.long <- N.bias.raw %>%
  pivot_longer(cols = 4:ncol(N.bias.raw)) %>%
  mutate(Year = as.integer(sub(".", "", name))) %>% select(-name)

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
N.bias <- merge(N.bias, SimInfo.totaldata, by = c("Trial"))

hist(N.bias$value, xlim = c(0,500), breaks = 5000)

ggplot(N.bias, aes(x = NSites_BR, y = value)) +
  geom_point(aes(color = Age))
ggplot(N.bias, aes(x = NSites_WSR, y = value)) +
  geom_point(aes(color = Age))
ggplot(N.bias, aes(x = NInd_BR, y = value)) +
  geom_point(aes(color = Age))
ggplot(N.bias, aes(x = NInd_WSR, y = value)) +
  geom_point(aes(color = Age))

NBias.fullmodel <- lm(value ~ NSites_BR + NSites_WSR + NInd_BR + NInd_WSR, N.bias)
summary(NBias.fullmodel)

#Summarize WMDs
N.bias.simple <- N.bias.long %>%
  group_by(Trial, WMD, Age) %>%
  summarize(Mean = mean(value, na.rm = T), StdDev= sd(value, na.rm = T))
N.bias.simple <- merge(N.bias.simple, SimInfo.NSitesBR, by = c("WMD","Trial"))
N.bias.simple <- merge(N.bias.simple, SimInfo.NSitesWSR, by = c("WMD","Trial"))
N.bias.simple <- merge(N.bias.simple, SimInfo.NIndBR, by = c("WMD","Trial"))
N.bias.simple <- merge(N.bias.simple, SimInfo.NIndWSR, by = c("WMD","Trial"))
N.bias.simple <- merge(N.bias.simple, SimInfo.totaldata, by = c("Trial"))

ggplot(N.bias.simple, aes(x = NSites_BR, y = Mean)) +
  geom_point(aes(color = Age))
ggplot(N.bias.simple, aes(x = NSites_WSR, y = Mean)) +
  geom_point(aes(color = Age))
ggplot(N.bias.simple, aes(x = NInd_BR, y = Mean)) +
  geom_point(aes(color = Age))
ggplot(N.bias.simple, aes(x = NInd_WSR, y = Mean)) +
  geom_point(aes(color = Age))

mean.NBias.fullmodel <- lm(Mean ~ NSites_BR + NSites_WSR + NInd_BR + NInd_WSR, N.bias.simple)
summary(mean.NBias.fullmodel)

ggplot(N.bias.simple, aes(x = NSites_BR, y = StdDev)) +
  geom_point(aes(color = Age))
ggplot(N.bias.simple, aes(x = NSites_WSR, y = StdDev)) +
  geom_point(aes(color = Age))
ggplot(N.bias.simple, aes(x = NInd_BR, y = StdDev)) +
  geom_point(aes(color = Age))
ggplot(N.bias.simple, aes(x = NInd_WSR, y = StdDev)) +
  geom_point(aes(color = Age))

sd.NBias.fullmodel <- lm(StdDev ~ NSites_BR + NSites_WSR + NInd_BR + NInd_WSR, N.bias.simple)
summary(sd.NBias.fullmodel)