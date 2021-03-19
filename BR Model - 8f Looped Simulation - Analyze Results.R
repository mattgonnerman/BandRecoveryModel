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
SimInfo.totaldata <- SimInfo.raw[1:5,] %>%
  select(-WMD) %>%
  pivot_wider(names_from = Name, values_from = Value)

N.bias <- merge(N.bias.long, SimInfo.NSitesBR, by = c("WMD","Trial"))
N.bias <- merge(N.bias, SimInfo.NSitesWSR, by = c("WMD","Trial"))
N.bias <- merge(N.bias, SimInfo.NIndBR, by = c("WMD","Trial"))
N.bias <- merge(N.bias, SimInfo.NIndWSR, by = c("WMD","Trial"))
N.bias <- merge(N.bias, SimInfo.totaldata, by = c("Trial"))

ggplot(N.bias, aes(x = NSites_BR, y = value)) +
  geom_point(aes(color = Age))
ggplot(N.bias, aes(x = NSites_WSR, y = value)) +
  geom_point(aes(color = Age))
ggplot(N.bias, aes(x = NInd_BR, y = value)) +
  geom_point(aes(color = Age))
ggplot(N.bias, aes(x = NInd_WSR, y = value)) +
  geom_point(aes(color = Age))
