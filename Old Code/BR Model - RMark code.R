###RMark code to confirm the simulation data is correct
#Load Packages
require(RMark)
require(dplyr)
require(tidyr)

head(EH_raw)

EH_rmark <- as.data.frame(EH_raw) %>%
  unite("ch", 1:6, sep = "")
  
head(EH_rmark)

EH_covs_rmark <- cbind(EH_rmark, hr.ind.coef) %>%
  rename(BirdOD = Ind.ID) %>%
  mutate(freq = 1) %>%
  mutate()
