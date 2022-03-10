# Load Packages
require(dplyr)
require(tidyr)
require(stringr)
require(R2jags)

#Load Results
load( file ="RealDataModelOutput.RData") #BR_w_SPP_output
realspp.raw <- as.data.frame(BR_w_SPP_output$BUGSoutput$summary)

### Abundance Data
N.A.long.spp <- realspp.raw %>% 
  mutate(X = rownames(.)) %>%
  filter(substr(X,1,3) == "N.A") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, sd, WMD) %>%
  mutate(Age = "A")
N.J.long.spp <- realspp.raw %>% 
  mutate(X = rownames(.)) %>%
  filter(substr(X,1,3) == "N.J") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, sd, WMD) %>%
  mutate(Age = "J")
N.spp <- rbind(N.A.long.spp, N.J.long.spp) %>% rename(Mean.SPP = mean, Sd.SPP = sd)
mean(N.spp$Mean.SPP[N.spp$Age=="A"])
range(N.spp$Mean.SPP[N.spp$Age=="A"])
mean(N.spp$Mean.SPP[N.spp$Age=="J"])
range(N.spp$Mean.SPP[N.spp$Age=="J"])

### Harvest Rate Data
HR.est.spp <- realspp.raw %>%
  mutate(X = rownames(.)) %>%
  rename(ID = X) %>%
  filter(grepl("WMD.HR.", ID)) %>%
  mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\,)")) %>%
  mutate(Year = as.numeric(str_extract(ID, "(?<=\\,).*?(?=\\])"))) %>%
  filter(!is.na(Year)) %>%
  mutate(Age = substr(ID,8,8)) %>%
  filter(!grepl("SS", ID)) %>%
  dplyr::select(mean, sd, Year, WMD, Age) %>%
  rename(HRSPP = mean, SDSPP = sd) %>%
  mutate(Upper = HRSPP + SDSPP) %>%
  mutate(Lower = HRSPP - SDSPP)
HR.A.est.spp <- HR.est.spp %>% filter(Age == "A")
mean(HR.A.est.spp$HRSPP)
range(HR.A.est.spp$HRSPP)
HR.J.est.spp <- HR.est.spp %>% filter(Age == "J")
mean(HR.J.est.spp$HRSPP)
range(HR.J.est.spp$HRSPP)

### Survival
adult_wsr <- c(BR_w_SPP_output$BUGSoutput$mean$WSR_M_A_S2W,BR_w_SPP_output$BUGSoutput$mean$WSR_M_A_W2S)
juv_wsr <- c(BR_w_SPP_output$BUGSoutput$mean$WSR_M_J_S2W,BR_w_SPP_output$BUGSoutput$mean$WSR_M_J_W2S)
mean(adult_wsr)
range(adult_wsr)
mean(juv_wsr)
range(juv_wsr)

### Productivity
mean(BR_w_SPP_output$BUGSoutput$mean$R)
range(BR_w_SPP_output$BUGSoutput$mean$R)
