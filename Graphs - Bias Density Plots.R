require(ggplot2)
require(dplyr)
require(tidyr)
require(wesanderson)
require(stringr)

trialnames <- c("LowNugget", "HighNugget", "LowPSill", "HighPSill", "LowRange", "HighRange", "MedAll")

# Load Files
rm("N.bias.raw")
for(cov in 1:length(trialnames)){
  N.bias.sing <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/", trialnames[cov]
                               , " - Master N Bias.csv", sep = ""), check.names=FALSE)
  
  if(exists("N.bias.raw")){
    N.bias.raw <- rbind(N.bias.raw, N.bias.sing)
  }else{
    N.bias.raw <- N.bias.sing 
  }
}


N.bias.long <- N.bias.raw %>%
  pivot_longer(cols = 6:ncol(N.bias.raw)) %>%
  rename(Year = name) %>%
  filter(!is.na(value),
         BiasType == "Rel.Bias",
         WMD != -999)

#loop through individual model outputs and save as one big dataframe
rm(raw.model.output)
for(cov in 1:length(trialnames)){
  for(num in 1:100){
    ind.model.output <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/",
                                       trialnames[cov],
                                       " - Sim",
                                       num,
                                       " - RawModel Estimates.csv", sep = ""), check.names=FALSE)
    colnames(ind.model.output)[1] <- "ID"
    ind.model.output$Trial <- num
    ind.model.output$Parameter <- trialnames[cov]
    
    if(exists("raw.model.output")){
      raw.model.output <- rbind(raw.model.output, ind.model.output)
    }else{
      raw.model.output <- ind.model.output 
    }
  }
}


Nest.A.rhat.values <-raw.model.output %>% filter(substr(ID, 1,3) == "N.A") %>%
  mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\,)")) %>%
  mutate(Year = as.numeric(str_extract(ID, "(?<=\\,).*?(?=\\])"))) %>%
  dplyr::select(Parameter, WMD, Year, Trial, Raw.Est = mean,  Rhat) %>%
  mutate(Age = "A")

Nest.J.rhat.values <-raw.model.output %>% filter(substr(ID, 1,3) == "N.J") %>%
  mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\,)")) %>%
  mutate(Year = as.numeric(str_extract(ID, "(?<=\\,).*?(?=\\])"))) %>%
  dplyr::select(Parameter, WMD, Year, Trial, Raw.Est = mean,  Rhat) %>%
  mutate(Age = "J")

Nest.rhat.values <- rbind(Nest.A.rhat.values, Nest.J.rhat.values)

N.est.bias.rhat <- merge(N.bias.long, Nest.rhat.values, by = c("Parameter", "Trial", "WMD", "Year", "Age"))

N.est.bias.rhat.A <-  N.est.bias.rhat %>% filter(Age == "A")
N.est.bias.rhat.J <-  N.est.bias.rhat %>% filter(Age == "J")


#Age specific density plots for bias 
N.A.dens.plot <- ggplot(N.est.bias.rhat.A, aes(x = Raw.Est, y = value)) +
  geom_hex(bins = 50) +
  scale_fill_gradientn(name = "Count",
                       colours = wes_palette(n=5, name="Zissou1"),
                       values = c(0, .01, .05, .2, 1)) +
  labs(title = "Adult",
       y = "Relative Bias",
       x = element_blank()) +
  theme_classic(base_size = 80) +
  theme(legend.position = c(.85,.4),
        legend.key.width = unit(3, "cm"), 
        legend.key.height = unit(4, "cm"),
        legend.title = element_blank())
N.J.dens.plot <- ggplot(N.est.bias.rhat.J, aes(x = Raw.Est, y = value)) +
  geom_hex(bins = 50) +
  scale_fill_gradientn(name = "Count",
                       colours = wes_palette(n=5, name="Zissou1"),
                       values = c(0, .01, .05, .2, 1)) +
  labs(title = "Juvenile",
       y = "Relative Bias",
       x = "Real Abundance") +
  theme_classic(base_size = 80) +
  theme(legend.position = c(.85,.4),
        legend.key.width = unit(3, "cm"), #change legend key size
        legend.key.height = unit(4, "cm"),
        legend.title = element_blank()) #change legend key height

densplots <- list(N.A.dens.plot, N.J.dens.plot)
require(cowplot)
rel_bias_grid <- plot_grid(plotlist = densplots,
                           nrow = 2,
                           align = "hv",
                           axis = "lb")

jpeg("Graphs/Grouped Density Plots.jpg", width = 3000, height = 4000)
rel_bias_grid
dev.off()








# Nest.rhat.values <- rbind(Nest.A.rhat.values, Nest.J.rhat.values)
# 
# N.est.bias.rhat <- merge(N.bias.long, Nest.rhat.values, by = c("Parameter", "Trial", "WMD", "Year", "Age"))
# 
# abun.dens.plot.func <- function(x){
#   parametervalue <- trialnames[x]
#   PlotData <- N.est.bias.rhat %>% filter(Parameter == parametervalue)
#   ggplot(PlotData, aes(x = Raw.Est, y = value)) +
#     geom_hex(bins = 50) +
#     scale_fill_gradientn(name = "Count",
#                          colours = wes_palette(n=5, name="Zissou1"),
#                          values = c(0, .001, .05, .2, 1)) +
#     labs(subtitle = parametervalue) +
#     ylab("Relative Bias") + xlab("Real Abundance") +
#     theme_classic(base_size = 90) +
#     # ylim(-1,1) +
#     # xlim(0,10000) +
#     theme(legend.position = "none",
#           legend.key.width = unit(2, "cm"), #change legend key size
#           legend.key.height = unit(4, "cm")) #change legend key height
# }
# 
# abun.dens.plot <- lapply(c(1,7,2,3,7,4,5,7,6), abun.dens.plot.func)
# 
# 
