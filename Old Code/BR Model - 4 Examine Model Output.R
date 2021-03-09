#Examine Model Outputs more easily
# outputs <- as.data.frame(BR_w_SPP_output$BUGSoutput$summary)
# outputs$X <- rownames(outputs)
outputs <- read.csv("3J_output.csv")
require(dplyr)
require(stringr)
require(tidyr)
require(miscTools)
require(ggplot2)

#######################
### Estimate Values ###
#######################
### WMD and Time Specific Abundance
# Adult
N.A.long <- outputs %>% 
  filter(substr(X,1,3) == "N.A") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
N.A.est <- N.A.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)
N.A.total <- data.frame(mean = colSums(N.A.est[2:ncol(N.A.est)]),
                        Year = min(N.A.long$Year):max(N.A.long$Year),
                        WMD = "Total")

ggplot(data = N.A.long, aes(y = mean, x = Year, group = WMD)) +
  geom_line(aes(color = WMD), alpha = .8, size = 1.2)
ggsave("N_A_WMDestimates.jpeg", width = 12, height = 8, units = "in")

ggplot(data = N.A.long, aes(y = mean, x = as.factor(Year), group = WMD)) +
  stat_smooth(method = lm, aes(color = WMD, fill = WMD), alpha = .1)+
  geom_point(aes(color = WMD), alpha = .8, size = 1.2)
ggsave("N_A_WMDtrends.jpeg", width = 12, height = 8, units = "in")

write.csv(N.A.est, "N_A_WMDestimates.csv", row.names = F)


#Juvenile
N.J.long <- outputs %>% 
  filter(substr(X,1,3) == "N.J") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
N.J.est <- N.J.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)
N.J.total <- data.frame(mean = colSums(N.J.est[2:ncol(N.J.est)]),
                        Year = min(N.A.long$Year):max(N.J.long$Year),
                        WMD = "Total")

ggplot(data = N.J.long, aes(y = mean, x = as.factor(Year), group = WMD)) +
  geom_line(aes(color = WMD), alpha = .8, size = 1.2)
ggsave("N_J_WMDestimates.jpeg", width = 12, height = 8, units = "in")

ggplot(data = N.J.long, aes(y = mean, x = as.factor(Year), group = WMD)) +
  stat_smooth(method = lm, aes(color = WMD, fill = WMD), alpha = .1)+
  geom_point(aes(color = WMD), alpha = .8, size = 1.2)
ggsave("N_J_WMDtrends.jpeg", width = 12, height = 8, units = "in")

write.csv(N.J.est, "N_J_WMDestimates.csv", row.names = F)


### Recruitment
R.long <- outputs %>% 
  filter(substr(X,1,2) == "R[") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
R.est <- R.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)
ggplot(data = R.long, aes(y = mean, x = as.factor(Year), group = WMD)) +
  geom_line(aes(color = WMD), alpha = .8, size = 1.2)
ggsave("R_WMDestimates.jpeg", width = 12, height = 8, units = "in")
write.csv(R.est, "R_WMDestimates.csv", row.names = F)


### WMD and Time Specific Harvest Rate Estimates
#Adult
HR.A.long <- outputs %>% 
  filter(substr(X,1,8) == "WMD.HR.A") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
HR.A.est <- HR.A.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)
ggplot(data = HR.A.long, aes(y = mean, x = as.factor(Year), group = WMD)) +
  geom_line(aes(color = WMD), alpha = .8, size = 1.2)
ggsave("HR_A_WMDestimates.jpeg", width = 12, height = 8, units = "in")
write.csv(HR.A.est, "HR_A_WMDestimates.csv", row.names = F)


#Juvenile
HR.J.long <- outputs %>% 
  filter(substr(X,1,8) == "WMD.HR.J") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
HR.J.est <- HR.J.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)
ggplot(data = HR.J.long, aes(y = mean, x = as.factor(Year), group = WMD)) +
  geom_line(aes(color = WMD), alpha = .8, size = 1.2)
ggsave("HR_J_WMDestimates.jpeg", width = 12, height = 8, units = "in")
write.csv(HR.J.est, "HR_J_WMDestimates.csv", row.names = F)


### WMD and Time Specific Non Harvest Survival Estimates
# Adult
S.A.long <- outputs %>% 
  filter(substr(X,1,8) == "totalS.A") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
S.A.est <- S.A.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)
ggplot(data = S.A.long, aes(y = mean, x = as.factor(Year), group = WMD)) +
  geom_line(aes(color = WMD), alpha = .8, size = 1.2)
ggsave("S_A_WMDestimates.jpeg", width = 12, height = 8, units = "in")
write.csv(S.A.est, "S_A_WMDestimates.csv", row.names = F)


#Juvenile
S.J.long <- outputs %>% 
  filter(substr(X,1,8) == "totalS.J") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
S.J.est <- S.J.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)
ggplot(data = S.J.long, aes(y = mean, x = as.factor(Year), group = WMD)) +
  geom_line(aes(color = WMD), alpha = .8, size = 1.2)
ggsave("S_J_WMDestimates.jpeg", width = 12, height = 8, units = "in")
write.csv(S.J.est, "S_J_WMDestimates.csv", row.names = F)




###############################################################################
###############################################################################
#######################
### RHat Values ###
#######################
### Total Model Rhat
ggplot(data = outputs, aes(x = Rhat)) +
  geom_histogram() +
  xlim(min(outputs$Rhat) - .1, ifelse(max(outputs$Rhat) > 2, max(outputs$Rhat) + .3, 2)) +
  labs(title = "Total Tracked Parameters Convergence")
ggsave("FullTracked_RHat.jpeg", width = 8, height = 8, units = "in")


### WMD and Time Specific Abundance Rhat
# Adult
N.A.rhat <- outputs %>% 
  filter(substr(X,1,3) == "N.A") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, Rhat, WMD)
ggplot(data = N.A.rhat, aes(x = Rhat)) +
  geom_histogram() +
  xlim(min(outputs$Rhat) - .1, ifelse(max(outputs$Rhat) > 2, max(outputs$Rhat) + .3, 2)) +
  labs(title = "WMD and Time Specific N.A Convergence")
ggsave("N_A_RHat.jpeg", width = 8, height = 8, units = "in")


#Juvenile
N.J.rhat <- outputs %>% 
  filter(substr(X,1,3) == "N.J") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, Rhat, WMD)
ggplot(data = N.J.rhat, aes(x = Rhat)) +
  geom_histogram() +
  xlim(min(outputs$Rhat) - .1, ifelse(max(outputs$Rhat) > 2, max(outputs$Rhat) + .3, 2)) +
  labs(title = "WMD and Time Specific N.J Convergence")
ggsave("N_J_RHat.jpeg", width = 8, height = 8, units = "in")


### Recruitment
R.rhat <- outputs %>% 
  filter(substr(X,1,2) == "R[") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, Rhat, WMD)
ggplot(data = R.rhat, aes(x = Rhat)) +
  geom_histogram() +
  xlim(min(outputs$Rhat) - .1, ifelse(max(outputs$Rhat) > 2, max(outputs$Rhat) + .3, 2)) +
  labs(title = "WMD and Time Specific R Convergence")
ggsave("R_RHat.jpeg", width = 8, height = 8, units = "in")


### WMD and Time Specific Harvest Rate Estimates
#Adult
HR.A.rhat <- outputs %>% 
  filter(substr(X,1,8) == "WMD.HR.A") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, Rhat, WMD)
ggplot(data = HR.A.rhat, aes(x = Rhat)) +
  geom_histogram() +
  xlim(min(outputs$Rhat) - .1, ifelse(max(outputs$Rhat) > 2, max(outputs$Rhat) + .3, 2)) +
  labs(title = "WMD and Time Specific HR.A Convergence")
ggsave("HR_A_RHat.jpeg", width = 8, height = 8, units = "in")


#Juvenile
HR.J.rhat <- outputs %>% 
  filter(substr(X,1,8) == "WMD.HR.J") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, Rhat, WMD)
ggplot(data = HR.J.rhat, aes(x = Rhat)) +
  geom_histogram() +
  xlim(min(outputs$Rhat) - .1, ifelse(max(outputs$Rhat) > 2, max(outputs$Rhat) + .3, 2)) +
  labs(title = "WMD and Time Specific HR.J Convergence")
ggsave("HR_J_RHat.jpeg", width = 8, height = 8, units = "in")


### WMD and Time Specific Non Harvest Survival Estimates
# Adult
S.A.rhat <- outputs %>% 
  filter(substr(X,1,8) == "totalS.A") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, Rhat, WMD)
ggplot(data = S.A.rhat, aes(x = Rhat)) +
  geom_histogram() +
  xlim(min(outputs$Rhat) - .1, ifelse(max(outputs$Rhat) > 2, max(outputs$Rhat) + .3, 2)) +
  labs(title = "WMD and Time Specific S.A Convergence")
ggsave("S_A_RHat.jpeg", width = 8, height = 8, units = "in")


#Juvenile
S.J.rhat <- outputs %>% 
  filter(substr(X,1,8) == "totalS.J") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = 2013 + as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, Rhat, WMD)
ggplot(data = S.J.rhat, aes(x = Rhat)) +
  geom_histogram() +
  xlim(min(outputs$Rhat) - .1, ifelse(max(outputs$Rhat) > 2, max(outputs$Rhat) + .3, 2)) +
  labs(title = "WMD and Time Specific S.J Convergence")
ggsave("S_J_RHat.jpeg", width = 8, height = 8, units = "in")