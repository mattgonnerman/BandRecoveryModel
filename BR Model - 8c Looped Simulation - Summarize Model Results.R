#Examine Model Outputs more easily
outputs <- as.data.frame(BR_w_SPP_output$BUGSoutput$summary)
outputs$X <- rownames(outputs)
# outputs <- read.csv("3G_output.csv")
require(dplyr)
require(stringr)
require(tidyr)
require(miscTools)
require(ggplot2)

########################
### Estimated Values ###
########################
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
# write.csv(N.A.est, "N_A_WMDestimates.csv", row.names = F)


#Juvenile
N.J.long <- outputs %>% 
  filter(substr(X,1,3) == "N.J") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
N.J.est <- N.J.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)
N.J.total <- data.frame(mean = colSums(N.J.est[2:ncol(N.J.est)]),
                        Year = min(N.A.long$Year):max(N.J.long$Year),
                        WMD = "Total")
# write.csv(N.J.est, "N_J_WMDestimates.csv", row.names = F)


### Recruitment
#Individual R for WMD and Year
R.long <- outputs %>%
  filter(substr(X,1,2) == "R[") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
R.est <- R.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)

# #Individual R for Year Only
# R.long <- outputs %>% 
#   filter(substr(X,1,2) == "R[") %>%
#   mutate(Year = str_extract(X, "[:digit:]+")) %>%
#   dplyr::select(Year, mean)


### WMD and Time Specific Harvest Rate Estimates
#Save HR estimates
estvalues.HR.br <- outputs %>%
  rename(ID = X) %>%
  filter(grepl("WMD.HR.", ID)) %>%
  mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\,)")) %>%
  mutate(Year = as.numeric(str_extract(ID, "(?<=\\,).*?(?=\\])"))) %>%
  filter(!is.na(Year)) %>%
  mutate(Age = substr(ID,8,8)) %>%
  filter(!grepl("SS", ID)) %>%
  dplyr::select(mean, Year, WMD, Age) %>%
  pivot_wider(names_from = c(Age, Year), values_from = c(mean), names_sort = T)
mean.est.values.HR <- colMeans(estvalues.HR.br[,2:((n.band.years*2)+1)])

estvalues.HR.ss <- outputs %>%
  rename(ID = X) %>%
  filter(grepl("WMD.HR.", ID)) %>%
  mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\,)")) %>%
  mutate(Year = as.numeric(str_extract(ID, "(?<=\\,).*?(?=\\])"))) %>%
  filter(!is.na(Year)) %>%
  mutate(Age = substr(ID,8,8)) %>%
  filter(grepl("SS", ID)) %>%
  dplyr::select(mean, Year, WMD, Age) %>%
  pivot_wider(names_from = c(Age, Year), values_from = c(mean), names_sort = T)

#Adult
HR.A.long <- outputs %>% 
  filter(substr(X,1,9) == "WMD.HR.A[") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
HR.A.est <- HR.A.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)
# write.csv(HR.A.est, "HR_A_WMDestimates.csv", row.names = F)


#Juvenile
HR.J.long <- outputs %>% 
  filter(substr(X,1,9) == "WMD.HR.J[") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
HR.J.est <- HR.J.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)
# write.csv(HR.J.est, "HR_J_WMDestimates.csv", row.names = F)


### WMD and Time Specific Non Harvest Survival Estimates
#Save WSR estimates
est.WSR <- outputs %>%
  rename(ID = X) %>%
  filter(grepl("WSR_M_", ID)) %>%
  filter(!grepl("WSR_M_J_S2W", ID)) %>% #No Juvenile in S2W so ignore estimate
  mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\])")) %>%
  mutate(Age = substr(ID,7,7)) %>%
  dplyr::select(mean, WMD, Age) %>%
  group_by(Age) %>%
  summarize(Mean.WSR = mean(mean))
mean.est.WSR <- est.WSR$Mean.WSR


# Adult
S.A.long <- outputs %>%
  rename(ID = X) %>%
  filter(grepl("WSR_M_", ID)) %>%
  mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\])")) %>%
  mutate(Age = substr(ID,7,7)) %>%
  filter(Age == "A") %>%
  mutate(Season = substr(ID,9,11)) %>%
  dplyr::select(mean, WMD, Season)
S.A.est <- S.A.long %>%
  pivot_wider(names_from = c(Season), values_from = mean)
# write.csv(S.A.est, "S_A_WMDestimates.csv", row.names = F)


#Juvenile
S.J.long <- outputs %>%
  rename(ID = X) %>%
  filter(grepl("WSR_M_", ID)) %>%
  mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\])")) %>%
  mutate(Age = substr(ID,7,7)) %>%
  filter(Age == "J") %>%
  mutate(Season = substr(ID,9,11)) %>%
  dplyr::select(mean, WMD, Season)
S.J.est <- S.J.long %>%
  pivot_wider(names_from = c(Season), values_from = mean)
# write.csv(S.J.est, "S_J_WMDestimates.csv", row.names = F)