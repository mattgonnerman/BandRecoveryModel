TrialNumber <- 1

# N.trial <- read.csv("\Model Bias Comparison\Trial Info - N.csv") %>%
#   filter(Trial == TrialNumber)
# WSR.trial <- read.csv("Model Bias Comparison/Trial Info - WSR.csv") %>%
#   filter(Trial == TrialNumber)
# HR.trial <- read.csv("Model Bias Comparison/Trial Info - HR.csv") %>%
#   filter(Trial == TrialNumber)
# N.WMDYear <- read.csv("Model Bias Comparison/WMDYearBias.csv") %>%
#   filter(Trial == TrialNumber)


require(dplyr)
require(ggplot2)

WSR.all <- read.csv("Model Bias Comparison/Trial Info - WSR.csv") %>%
  filter(Model %in% c("K")) %>%
  filter(Name == "Bias") %>%
  group_by(Age, Trial) %>%
  summarise(Mean = mean(Estimate))
HR.all <- read.csv("Model Bias Comparison/Trial Info - HR.csv") %>%
  filter(Model %in% c("K")) %>%
  filter(Name == "Bias") %>%
  group_by(Age, Trial) %>%
  summarise(Mean = mean(Estimate))
N.all <- read.csv("Model Bias Comparison/WMDYearBias.csv") %>%
  filter(Model %in% c("K")) %>%
  pivot_longer(cols = 5:ncol(N.WMDYear)) %>%
  filter(!is.na(value)) %>%
  group_by(Age, Trial) %>%
  summarise(Mean = mean(value), SD = sd(value))


# ### Abundance
# N.A.bias <- N.trial %>%
#   filter(Age == "A")
# 
# ggplot(data = N.A.bias, aes(x = Year, y = Estimate, group = Model)) +
#   geom_line(aes(color = Model), size = 1.4) +
#   labs(title = "Average Absolute Value of Difference from Real Estimate",
#        subtitle = paste("Adult | Trial ", TrialNumber, sep = ""))
# ggsave("N_A_bias.jpeg", width = 12, height = 8, units = "in")
# 
# N.J.bias <- N.trial %>%
#   filter(Age == "J")
# 
# ggplot(data = N.J.bias, aes(x = Year, y = Estimate, group = Model)) +
#   geom_line(aes(color = Model), size = 1.4) +
#   labs(title = "Average Absolute Value of Difference from Real Estimate",
#        subtitle = paste("Juvenile | Trial ", TrialNumber, sep = ""))
# ggsave("N_J_bias.jpeg", width = 12, height = 8, units = "in")


### Harvest Rates
HR.A.est <- HR.trial %>%
  filter(Age == "A") %>%
  filter(Name == "Est") %>%
  filter(Model != "Real")
HR.A.real <- HR.trial %>%
  filter(Age == "A") %>%
  filter(Name == "Real")
ggplot(data = HR.A.est, aes(x = Year, y = Estimate, group = Model)) +
  geom_line(aes(color = Model), size = 1.1) +
  geom_line(data = HR.A.real, color = "black", size = 1.4) +
  labs(title = "Estimates of Harvest Rate",
       subtitle = paste("Adult | Trial ", TrialNumber , sep = ""))
ggsave("HR_A_bias.jpeg", width = 12, height = 8, units = "in")

HR.J.est <- HR.trial %>%
  filter(Age == "J") %>%
  filter(Name == "Est") %>%
  filter(Model != "Real")
HR.J.real <- HR.trial %>%
  filter(Age == "J") %>%
  filter(Name == "Real")
ggplot(data = HR.J.est, aes(x = Year, y = Estimate, group = Model)) +
  geom_line(aes(color = Model), size = 1.1) +
  geom_line(data = HR.J.real, color = "black", size = 1.4) +
  labs(title = "Estimates of Harvest Rate",
       subtitle = paste("Juvenile | Trial ", TrialNumber, sep = ""))
ggsave("HR_J_bias.jpeg", width = 12, height = 8, units = "in")


### Weekly Survival Rate
WSR.est <- WSR.trial %>%
  filter(Name != "Bias")
ggplot(data = WSR.est, aes(x = Model, y = Estimate, group = Age)) +
  geom_point(aes(shape = Age, color = Model), size = 4) +
  labs(title = "Estimates of Weekly Survival Rate",
       subtitle = paste("Trial ", TrialNumber, sep = ""))
ggsave("WSR_bias.jpeg", width = 12, height = 8, units = "in")

### WMD X YEAR Abundance
##M
#A
N.WMDYear.M.A <- N.WMDYear %>%
  filter(Age == "A") %>%
  filter(Model == "M") %>%
  pivot_longer(cols = 5:ncol(N.WMDYear)) %>%
  mutate(Year = as.numeric(sub(".", "", name))) %>%
  mutate(WMD = as.factor(X))
  
ggplot(data = N.WMDYear.M.A, aes(x =Year, y = value, group = WMD)) +
  geom_line(aes(color = WMD), size = 1.3) +
  labs(title = "Difference from Real Value",
       subtitle = paste("Model M | Adult | Trial ", TrialNumber, sep = ""))
ggsave("WMDY_M_A.jpeg", width = 12, height = 8, units = "in")
#J
N.WMDYear.M.J <- N.WMDYear %>%
  filter(Age == "J") %>%
  filter(Model == "M") %>%
  pivot_longer(cols = 5:ncol(N.WMDYear)) %>%
  mutate(Year = as.numeric(sub(".", "", name))) %>%
  mutate(WMD = as.factor(X))

ggplot(data = N.WMDYear.M.J, aes(x =Year, y = value, group = WMD)) +
  geom_line(aes(color = WMD), size = 1.3) +
  labs(title = "Difference from Real Value",
       subtitle = paste("Model M | Juvenile | Trial ", TrialNumber, sep = ""))
ggsave("WMDY_M_J.jpeg", width = 12, height = 8, units = "in")
# ##K
# #A
# N.WMDYear.K.A <- N.WMDYear %>%
#   filter(Age == "A") %>%
#   filter(Model == "K") %>%
#   pivot_longer(cols = 5:ncol(N.WMDYear)) %>%
#   mutate(Year = as.numeric(sub(".", "", name))) %>%
#   mutate(WMD = as.factor(X))
# 
# ggplot(data = N.WMDYear.K.A, aes(x =Year, y = value, group = WMD)) +
#   geom_line(aes(color = WMD), size = 1.3) +
#   labs(title = "Difference from Real Value",
#        subtitle = paste("Model K | Adult | Trial ", TrialNumber, sep = ""))
# ggsave("WMDY_K_A.jpeg", width = 12, height = 8, units = "in")
# #J
# N.WMDYear.K.J <- N.WMDYear %>%
#   filter(Age == "J") %>%
#   filter(Model == "K") %>%
#   pivot_longer(cols = 5:ncol(N.WMDYear)) %>%
#   mutate(Year = as.numeric(sub(".", "", name))) %>%
#   mutate(WMD = as.factor(X))
# 
# ggplot(data = N.WMDYear.K.J, aes(x =Year, y = value, group = WMD)) +
#   geom_line(aes(color = WMD), size = 1.3) +
#   labs(title = "Difference from Real Value",
#        subtitle = paste("Model K | Juvenile | Trial ", TrialNumber, sep = ""))
# ggsave("WMDY_K_J.jpeg", width = 12, height = 8, units = "in")
# ##L
# #A
# N.WMDYear.L.A <- N.WMDYear %>%
#   filter(Age == "A") %>%
#   filter(Model == "L") %>%
#   pivot_longer(cols = 5:ncol(N.WMDYear)) %>%
#   mutate(Year = as.numeric(sub(".", "", name))) %>%
#   mutate(WMD = as.factor(X))
# 
# ggplot(data = N.WMDYear.L.A, aes(x =Year, y = value, group = WMD)) +
#   geom_line(aes(color = WMD), size = 1.3) +
#   labs(title = "Difference from Real Value",
#        subtitle = paste("Model L | Adult | Trial ", TrialNumber, sep = ""))
# ggsave("WMDY_L_A.jpeg", width = 12, height = 8, units = "in")
# #J
# N.WMDYear.L.J <- N.WMDYear %>%
#   filter(Age == "J") %>%
#   filter(Model == "L") %>%
#   pivot_longer(cols = 5:ncol(N.WMDYear)) %>%
#   mutate(Year = as.numeric(sub(".", "", name))) %>%
#   mutate(WMD = as.factor(X))
# 
# ggplot(data = N.WMDYear.L.J, aes(x =Year, y = value, group = WMD)) +
#   geom_line(aes(color = WMD), size = 1.3) +
#   labs(title = "Difference from Real Value",
#        subtitle = paste("Model L | Juvenile | Trial ", TrialNumber, sep = ""))
# ggsave("WMDY_L_J.jpeg", width = 12, height = 8, units = "in")
# ##I
# #A
# N.WMDYear.I.A <- N.WMDYear %>%
#   filter(Age == "A") %>%
#   filter(Model == "I") %>%
#   pivot_longer(cols = 5:ncol(N.WMDYear)) %>%
#   mutate(Year = as.numeric(sub(".", "", name))) %>%
#   mutate(WMD = as.factor(X))
# 
# ggplot(data = N.WMDYear.I.A, aes(x =Year, y = value, group = WMD)) +
#   geom_line(aes(color = WMD), size = 1.3) +
#   labs(title = "Difference from Real Value",
#        subtitle = paste("Model I | Adult | Trial ", TrialNumber, sep = ""))
# ggsave("WMDY_I_A.jpeg", width = 12, height = 8, units = "in")
# #J
# N.WMDYear.I.J <- N.WMDYear %>%
#   filter(Age == "J") %>%
#   filter(Model == "I") %>%
#   pivot_longer(cols = 5:ncol(N.WMDYear)) %>%
#   mutate(Year = as.numeric(sub(".", "", name))) %>%
#   mutate(WMD = as.factor(X))
# 
# ggplot(data = N.WMDYear.I.J, aes(x =Year, y = value, group = WMD)) +
#   geom_line(aes(color = WMD), size = 1.3) +
#   labs(title = "Difference from Real Value",
#        subtitle = paste("Model I | Juvenile | Trial ", TrialNumber, sep = ""))
# ggsave("WMDY_I_J.jpeg", width = 12, height = 8, units = "in")
# ##D
# #A
# N.WMDYear.D.A <- N.WMDYear %>%
#   filter(Age == "A") %>%
#   filter(Model == "D") %>%
#   pivot_longer(cols = 5:ncol(N.WMDYear)) %>%
#   mutate(Year = as.numeric(sub(".", "", name))) %>%
#   mutate(WMD = as.factor(X))
# 
# ggplot(data = N.WMDYear.D.A, aes(x =Year, y = value, group = WMD)) +
#   geom_line(aes(color = WMD), size = 1.3) +
#   labs(title = "Difference from Real Value",
#        subtitle = paste("Model D | Adult | Trial ", TrialNumber, sep = ""))
# ggsave("WMDY_D_A.jpeg", width = 12, height = 8, units = "in")
# #J
# N.WMDYear.D.J <- N.WMDYear %>%
#   filter(Age == "J") %>%
#   filter(Model == "D") %>%
#   pivot_longer(cols = 5:ncol(N.WMDYear)) %>%
#   mutate(Year = as.numeric(sub(".", "", name))) %>%
#   mutate(WMD = as.factor(X))
# 
# ggplot(data = N.WMDYear.D.J, aes(x =Year, y = value, group = WMD)) +
#   geom_line(aes(color = WMD), size = 1.3) +
#   labs(title = "Difference from Real Value",
#        subtitle = paste("Model D | Juvenile | Trial ", TrialNumber, sep = ""))
# ggsave("WMDY_D_J.jpeg", width = 12, height = 8, units = "in")



### For later Juvenile to Adult Transition 


N.WMDYear <- read.csv("Model Bias Comparison/M - JtoA after C2/Abundance by WMDYear.csv")
TrialNumber <- 5

N.WMDYear.M.A <- N.WMDYear %>%
  filter(Age == "A") %>%
  filter(Trial == TrialNumber) %>%
  pivot_longer(cols = 4:ncol(N.WMDYear)) %>%
  mutate(Year = as.numeric(sub(".", "", name))) %>%
  mutate(WMD = as.factor(WMD))

ggplot(data = N.WMDYear.M.A, aes(x =Year, y = value, group = WMD)) +
  geom_line(aes(color = WMD), size = 1.3) +
  labs(title = "Difference from Real Value",
       subtitle = paste("Model M | Adult | Trial ", TrialNumber, sep = ""))
ggsave(paste(TrialNumber, " - WMDY_M_A.jpeg", sep = ""), width = 12, height = 8, units = "in")
#J
N.WMDYear.M.J <- N.WMDYear %>%
  filter(Age == "J") %>%
  filter(Trial == TrialNumber) %>%
  pivot_longer(cols = 4:ncol(N.WMDYear)) %>%
  mutate(Year = as.numeric(sub(".", "", name))) %>%
  mutate(WMD = as.factor(WMD))

ggplot(data = N.WMDYear.M.J, aes(x =Year, y = value, group = WMD)) +
  geom_line(aes(color = WMD), size = 1.3) +
  labs(title = "Difference from Real Value",
       subtitle = paste("Model M | Juvenile | Trial ", TrialNumber, sep = ""))
ggsave(paste(TrialNumber, " - WMDY_M_J.jpeg", sep = ""), width = 12, height = 8, units = "in")