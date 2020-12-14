#Examine Model Outputs more easily
outputs <- read.csv("BR_P_SPP_SSPop_output.csv")

require(dplyr)
require(stringr)
require(tidyr)

population.long <- outputs %>%
  filter(substr(X,1,1) == "N") %>%
  mutate(Age = substr(X,3,3)) %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = paste("Year",substr(str_extract(X, "[,^][:digit:]+"), 2,4), sep = "_")) %>%
  dplyr::select(Year,Age, mean, WMD)

population.est <- population.long %>%
  pivot_wider(names_from = c(Age, Year), values_from = mean)
View(population.est)
print(colSums(population.est[2:ncol(population.est)]))

