### Estimated Parameter Values ###

#Model Output
outputs <- as.data.frame(BR_w_SPP_output$BUGSoutput$summary)
outputs$X <- rownames(outputs)

require(dplyr)
require(stringr)
require(tidyr)
require(miscTools)
require(ggplot2)

########################
### Estimated Values ###
########################
### Harvest Rate Estimates
estvalues.HR.br <- outputs %>%
  rename(ID = X) %>%
  filter(grepl("mean.WMD.HR", ID)) %>%
  mutate(Age = substr(ID,13,13)) %>%
  dplyr::select(mean, Age) 

### Non Harvest Survival Estimates
est.WSR <- outputs %>%
  rename(ID = X) %>%
  filter(grepl("WSR_M_", ID)) %>%
  filter(!grepl("WSR_M_J_S2W", ID)) %>% #No Juvenile in S2W so ignore estimate
  mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\])")) %>%
  mutate(Age = substr(ID,7,7)) %>%
  dplyr::select(mean, WMD, Age) %>%
  group_by(Age) %>%
  summarize(Mean.WSR = mean(mean))
mean.est.WSR <- data.frame(Age = c("A", "J"),
                           Mean = est.WSR$Mean.WSR)


### WMD and Time Specific Abundance
# Adult
N.A.long <- outputs %>% 
  filter(substr(X,1,3) == "N.A") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
N.A.est <- N.A.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)

#Juvenile
N.J.long <- outputs %>% 
  filter(substr(X,1,3) == "N.J") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
N.J.est <- N.J.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)


### Recruitment
#Individual R for WMD and Year
R.long <- outputs %>%
  filter(substr(X,1,2) == "R[") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, WMD)
R.est <- R.long %>%
  pivot_wider(names_from = c(Year), values_from = mean)



###Save Model Estimates
sink("Real Data 2021 NO SPP - Summarize Model Estimates.csv")
cat("Average Estimated HR")
cat('\n')
write.csv(estvalues.HR.br, row.names = F)
cat('\n')
cat('\n')
cat("Average Estimated WSR")
cat('\n')
write.csv(mean.est.WSR, row.names = F)
cat('\n')
cat('\n')
cat("Estimated N.A")
cat('\n')
write.csv(N.A.est, row.names = F)
cat('\n')
cat('\n')
cat("Estimated N.J")
cat('\n')
write.csv(N.J.est, row.names = F)
cat('\n')
cat('\n')
cat("Estimated R")
cat('\n')
write.csv(R.est, row.names = F)
cat('\n')
cat('\n')
sink()
