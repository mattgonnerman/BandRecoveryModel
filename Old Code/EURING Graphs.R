require(ggplot2)
require(dplyr)
require(tidyr)
require(wesanderson)
require(stringr)

trialname <- "MedAll"

# Load Files
N.bias.raw <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/Github Trials/Final Sim - Pre Carrying Capacity/", trialname, " - Master N Bias.csv", sep = ""), check.names=FALSE)
HR.bias.raw <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/Github Trials/Final Sim - Pre Carrying Capacity/", trialname, " - Master HR Bias.csv", sep = ""))
WSR.bias.raw <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/Github Trials/Final Sim - Pre Carrying Capacity/", trialname, " - Master WSR Bias.csv", sep = ""))
R.bias.raw <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/Github Trials/Final Sim - Pre Carrying Capacity/", trialname, " - Master R Bias.csv", sep = ""), check.names=FALSE)
SimInfo.raw <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/Github Trials/Final Sim - Pre Carrying Capacity/", trialname, " - MasterSimInfo.csv", sep = ""))

N.bias.long <- N.bias.raw %>%
  pivot_longer(cols = 6:ncol(N.bias.raw)) %>%
  rename(Year = name) %>%
  filter(!is.na(value))

SimInfo.NSitesBR <- SimInfo.raw %>%
  filter(Name == "NSites BR") %>%
  rename(NSites_BR = Value) %>% 
  dplyr::select(-Name)
SimInfo.NSitesWSR <- SimInfo.raw %>%
  filter(Name == "NSites WSR")%>%
  rename(NSites_WSR = Value) %>% 
  dplyr::select(-Name)
SimInfo.NIndBR <- SimInfo.raw %>%
  filter(Name == "NInd BR") %>%
  rename(NInd_BR = Value) %>% 
  dplyr::select(-Name)
SimInfo.NIndWSR <- SimInfo.raw %>%
  filter(Name == "NInd WSR")%>%
  rename(NInd_WSR = Value) %>% 
  dplyr::select(-Name)
SimInfo.totaldata <- SimInfo.raw %>%
  filter(Name %in% c("YearsBR", "TotNIndBR", "YearsTotHarv", "YearsWSR", "TotNIndBR")) %>%
  dplyr::select(-WMD) %>%
  pivot_wider(names_from = Name, values_from = Value)

abs.N.bias.A <- N.bias.long %>% filter(BiasType == "Abs.Bias", Age == "A") %>% 
  rename(Abs.Bias = value) %>% dplyr::select(-Parameter, -BiasType)
abs.N.bias.J <- N.bias.long %>% filter(BiasType == "Abs.Bias", Age == "J") %>% 
  rename(Abs.Bias = value) %>% dplyr::select(-Parameter, -BiasType)
rel.N.bias.A <- N.bias.long %>% filter(BiasType == "Rel.Bias", Age == "A") %>% 
  rename(Rel.Bias = value) %>% dplyr::select(-Parameter, -BiasType)
rel.N.bias.J <- N.bias.long %>% filter(BiasType == "Rel.Bias", Age == "J") %>% 
  rename(Rel.Bias = value) %>% dplyr::select(-Parameter, -BiasType)

abs.rel.N.bias.A <- merge(abs.N.bias.A, rel.N.bias.A, by = c("Trial", "WMD", "Year", "Age")) %>%
  filter(WMD != -999)
abs.rel.N.bias.J <- merge(abs.N.bias.J, rel.N.bias.J, by = c("Trial", "WMD", "Year", "Age")) %>%
  filter(WMD != -999)

#loop through individual model outputs and save as one big dataframe
rm(raw.model.output)
for(num in 1:100){
  ind.model.output <- read.csv(paste("E:/Maine Drive/Analysis/Band Recovery/Github Trials/Final Sim - Pre Carrying Capacity/", trialname, " - Sim", num," - RawModel Estimates.csv", sep = ""), check.names=FALSE)
  colnames(ind.model.output)[1] <- "ID"
  ind.model.output$Trial <- num
  ind.model.output$Parameter <- trialname
  
  if(exists("raw.model.output")){
    raw.model.output <- rbind(raw.model.output, ind.model.output)
  }else{
    raw.model.output <- ind.model.output 
  }
  
}

Nest.A.rhat.values <-raw.model.output %>% filter(substr(ID, 1,3) == "N.A") %>%
  mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\,)")) %>%
  mutate(Year = as.numeric(str_extract(ID, "(?<=\\,).*?(?=\\])"))) %>%
  dplyr::select(Parameter, WMD, Year, Trial, Raw.Est = mean,  Rhat)

Nest.J.rhat.values <-raw.model.output %>% filter(substr(ID, 1,3) == "N.J") %>%
  mutate(WMD = str_extract(ID, "(?<=\\[).*?(?=\\,)")) %>%
  mutate(Year = as.numeric(str_extract(ID, "(?<=\\,).*?(?=\\])"))) %>%
  dplyr::select(Parameter, WMD, Year, Trial, Raw.Est = mean,  Rhat)

N.A.est.bias.rhat <- merge(abs.rel.N.bias.A, Nest.A.rhat.values, by = c("Trial", "WMD", "Year"))
N.J.est.bias.rhat <- merge(abs.rel.N.bias.J, Nest.J.rhat.values, by = c("Trial", "WMD", "Year"))

### Relative Bias X Absolute Bias HexBin Plot
ad_rel <- ggplot(N.A.est.bias.rhat, aes(x = Raw.Est, y = Rel.Bias)) +
  geom_hex(bins = 50) +
  scale_fill_gradientn(name = "Count",
                       colours = wes_palette(n=5, name="Zissou1"),
                       values = c(0, .001, .05, .2, 1)) +
  labs(subtitle = "Adult Males") +
  ylab("Relative Bias") + xlab("Real Abundance") +
  theme_classic(base_size = 100) +
  ylim(-1,1) +
  xlim(0,10000) +
  theme(legend.key.width = unit(2, "cm"), #change legend key size
        legend.key.height = unit(4, "cm")) #change legend key height

juv_rel <- ggplot(N.J.est.bias.rhat, aes(x = Raw.Est, y = Rel.Bias)) +
  geom_hex(bins = 50) +
  scale_fill_gradientn(name = "Count",
                       colours = wes_palette(n=5, name="Zissou1"),
                       values = c(0, .001, .05, .2, 1)) +
  labs(subtitle = "Juvenile Males") +
  ylab("Relative Bias") + xlab("Real Abundance") +
  theme_classic(base_size = 100) +
  ylim(-1,1) +
  xlim(0,10000) +
  theme(legend.key.width = unit(2, "cm"), #change legend key size
        legend.key.height = unit(4, "cm")) #change legend key height

require(cowplot)
rel_bias_grid <- plot_grid(plotlist = list(ad_rel, juv_rel),
          nrow = 2,
          align = "hv",
          axis = "lb")

jpeg(paste("E:/Maine Drive/Analysis/Band Recovery/FinalSim/Graphs/EURING - ", trialname, " - RelBiasXReal_ZOOM.jpeg", sep = ""), width = 3200, height = 4000)
 rel_bias_grid
dev.off()


###################################################################################
###################################################################################
### SPP Map Example
# Run Simulated Data Code
require(parallel)
require(dplyr)
require(stringr)
require(tidyr)
require(miscTools)
require(ggplot2)


### Set Variable Parameters 
## HR Gaussian Process
# Magnitude of variation, value that variogram levels out at 
psill.hr <- 0.01 #c(0.001, 0.01, 0.1)
# Maximal distance of autocorrelation, where variogram levels out
hr.sc <- 7 #c(2, 7, 15)
# Small-scale variations
nugget.hr <- 0.005 #c(0.001, 0.005, 0.01)

trialname <- "MedAll"

source(file = "BR Final Sim - 1 Simulated Data.R")

spp_map <- ggplot()+  ## Initialize the ggplot layer
  geom_raster(data=HR_gaussian_field,aes(x=x,y=y,fill=HR.base))+ ## plot the observations as points with a colored yield gradient
  scale_fill_gradient(low="purple", high="yellow", guide = F) +   ## Set the colors of the gradient
  geom_sf(data = st_cast(SA.grid,"LINESTRING"), size = 2) +
  geom_sf(data = spatialknots, size = 16, aes(color = "Spatial Knot", shape = "Spatial Knot")) +
  geom_sf(data = cap_sf, size = 16, aes(color = "Capture Site", shape = "Capture Site")) +
  theme_void(base_size = 90) +
  labs(title = "Spatial Predictive Process") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.5,.025),
    legend.direction = "horizontal",
    plot.title = element_text(hjust = 0.5, vjust = -4.5)) +
  scale_colour_manual(name = "Point",
                      labels = c("Capture Site", "Spatial Knot"),
                      values = c("blue", "turquoise")) +   
  scale_shape_manual(name = "Point",
                     labels = c("Capture Site", "Spatial Knot"),
                     values = c(16, 17))

jpeg("SPP_Map.jpeg", width = 3000, height = 3000)
spp_map
dev.off()

###################################################################################
###################################################################################
### Simulated Data Examples
# Run Simulated Data Code
require(parallel)
require(dplyr)
require(stringr)
require(tidyr)
require(miscTools)
require(ggplot2)

A = 100
B = 100
Field = expand.grid(1:A, 1:B)
names(Field) = c('x','y')


### Set Variable Parameters 
## HR Gaussian Process
# Magnitude of variation, value that variogram levels out at 
psill.hr <- 0.01 #c(0.001, 0.01, 0.1)
# Maximal distance of autocorrelation, where variogram levels out
hr.sc <- 7 #c(2, 7, 15)
# Small-scale variations
nugget.hr <- 0.01 #c(0.001, 0.005, 0.01)

trialname <- "MedAll"

source(file = "BR Final Sim - 1 Simulated Data.R")

## PSILL
# Set the parameters of the semi-variogram
Psill = 0.001 ## Partial sill = Magnitude of variation, value that variogram levels out at 
Range= 7  ## Maximal distance of autocorrelation, where variogram levels out
Nugget= 0.005  ## Small-scale variations
# Set the semi-variogram model
Beta = 0   ## mean yield (tons/ha) in the field
HR_modelling=gstat(formula=z~1, ## We assume that there is a constant trend in the data
                   locations=~x+y,
                   dummy=T,    ## Logical value to set to True for unconditional simulation
                   beta=Beta,  ## Necessity to set the average value over the field
                   model=vgm(psill=Psill,
                             range=Range ,
                             nugget=Nugget,
                             model='Gau'), ## Spherical semi-variogram model
                   nmax=40) ## number of nearest observations used for each new prediction
# Simulate the spatial structure within the field
HR_gaussian_field <- predict(HR_modelling, newdata=Field, nsim=1) %>% ## nsim : Nombre de simulations
  rename(HR.SPP = sim1) %>%
  mutate(HR.base = exp(hr.b0+HR.SPP)/(1+exp(hr.b0+HR.SPP)))
HR_spvar.points <- st_as_sf(HR_gaussian_field, coords = c("x","y")) 
# #Visualize the spatial variation
require(ggplot2)
low_psill <- ggplot()+  ## Initialize the ggplot layer
  geom_raster(data=HR_gaussian_field,aes(x=x,y=y,fill=HR.base))+ ## plot the observations as points with a colored yield gradient
  scale_fill_gradient(low="purple", high="yellow") +   ## Set the colors of the gradient
  # geom_sf(data = bandsiteselect) +
  # geom_sf(data = st_cast(SA.grid,"LINESTRING"), size = 2) +
  # geom_sf(data = bandsiteselect, size = 3) +
  theme_void(base_size = 80) +
  labs(subtitle = "Low Magnitude of Variation") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.subtitle = element_text(hjust = 0.5))


## HARVEST RATE
# Set the parameters of the semi-variogram
Psill = 0.01 ## Partial sill = Magnitude of variation, value that variogram levels out at 
Range= 7  ## Maximal distance of autocorrelation, where variogram levels out
Nugget= 0.005  ## Small-scale variations
# Set the semi-variogram model
Beta = 0   ## mean yield (tons/ha) in the field
HR_modelling=gstat(formula=z~1, ## We assume that there is a constant trend in the data
                   locations=~x+y,
                   dummy=T,    ## Logical value to set to True for unconditional simulation
                   beta=Beta,  ## Necessity to set the average value over the field
                   model=vgm(psill=Psill,
                             range=Range ,
                             nugget=Nugget,
                             model='Gau'), ## Spherical semi-variogram model
                   nmax=40) ## number of nearest observations used for each new prediction
# Simulate the spatial structure within the field
HR_gaussian_field <- predict(HR_modelling, newdata=Field, nsim=1) %>% ## nsim : Nombre de simulations
  rename(HR.SPP = sim1) %>%
  mutate(HR.base = exp(hr.b0+HR.SPP)/(1+exp(hr.b0+HR.SPP)))
HR_spvar.points <- st_as_sf(HR_gaussian_field, coords = c("x","y")) 
# #Visualize the spatial variation
require(ggplot2)
med_psill <- ggplot()+  ## Initialize the ggplot layer
  geom_raster(data=HR_gaussian_field,aes(x=x,y=y,fill=HR.base))+ ## plot the observations as points with a colored yield gradient
  scale_fill_gradient(low="purple", high="yellow") +   ## Set the colors of the gradient
  # geom_sf(data = bandsiteselect) +
  # geom_sf(data = st_cast(SA.grid,"LINESTRING"), size = 2) +
  # geom_sf(data = bandsiteselect, size = 3) +
  theme_void(base_size = 80) +
  labs(subtitle = "Medium Magnitude of Variation") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.subtitle = element_text(hjust = 0.5))

## HARVEST RATE
# Set the parameters of the semi-variogram
Psill = 0.05 ## Partial sill = Magnitude of variation, value that variogram levels out at 
Range= 7  ## Maximal distance of autocorrelation, where variogram levels out
Nugget= 0.005  ## Small-scale variations
# Set the semi-variogram model
Beta = 0   ## mean yield (tons/ha) in the field
HR_modelling=gstat(formula=z~1, ## We assume that there is a constant trend in the data
                   locations=~x+y,
                   dummy=T,    ## Logical value to set to True for unconditional simulation
                   beta=Beta,  ## Necessity to set the average value over the field
                   model=vgm(psill=Psill,
                             range=Range ,
                             nugget=Nugget,
                             model='Gau'), ## Spherical semi-variogram model
                   nmax=40) ## number of nearest observations used for each new prediction
# Simulate the spatial structure within the field
HR_gaussian_field <- predict(HR_modelling, newdata=Field, nsim=1) %>% ## nsim : Nombre de simulations
  rename(HR.SPP = sim1) %>%
  mutate(HR.base = exp(hr.b0+HR.SPP)/(1+exp(hr.b0+HR.SPP)))
HR_spvar.points <- st_as_sf(HR_gaussian_field, coords = c("x","y")) 
# #Visualize the spatial variation
require(ggplot2)
high_psill <- ggplot()+  ## Initialize the ggplot layer
  geom_raster(data=HR_gaussian_field,aes(x=x,y=y,fill=HR.base))+ ## plot the observations as points with a colored yield gradient
  scale_fill_gradient(low="purple", high="yellow") +   ## Set the colors of the gradient
  # geom_sf(data = bandsiteselect) +
  # geom_sf(data = st_cast(SA.grid,"LINESTRING"), size = 2) +
  # geom_sf(data = bandsiteselect, size = 3) +
  theme_void(base_size = 80) +
  labs(subtitle = "High Magnitude of Variation") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.subtitle = element_text(hjust = 0.5))

require(patchwork)
psillgraphs <- low_psill + med_psill + high_psill






## NUGGEt
# Set the parameters of the semi-variogram
Psill = 0.01 ## Partial sill = Magnitude of variation, value that variogram levels out at 
Range= 7  ## Maximal distance of autocorrelation, where variogram levels out
Nugget= 0.001  ## Small-scale variations
# Set the semi-variogram model
Beta = 0   ## mean yield (tons/ha) in the field
HR_modelling=gstat(formula=z~1, ## We assume that there is a constant trend in the data
                   locations=~x+y,
                   dummy=T,    ## Logical value to set to True for unconditional simulation
                   beta=Beta,  ## Necessity to set the average value over the field
                   model=vgm(psill=Psill,
                             range=Range ,
                             nugget=Nugget,
                             model='Gau'), ## Spherical semi-variogram model
                   nmax=40) ## number of nearest observations used for each new prediction
# Simulate the spatial structure within the field
HR_gaussian_field <- predict(HR_modelling, newdata=Field, nsim=1) %>% ## nsim : Nombre de simulations
  rename(HR.SPP = sim1) %>%
  mutate(HR.base = exp(hr.b0+HR.SPP)/(1+exp(hr.b0+HR.SPP)))
HR_spvar.points <- st_as_sf(HR_gaussian_field, coords = c("x","y")) 
# #Visualize the spatial variation
require(ggplot2)
low_nugget <- ggplot()+  ## Initialize the ggplot layer
  geom_raster(data=HR_gaussian_field,aes(x=x,y=y,fill=HR.base))+ ## plot the observations as points with a colored yield gradient
  scale_fill_gradient(low="purple", high="yellow") +   ## Set the colors of the gradient
  # geom_sf(data = bandsiteselect) +
  # geom_sf(data = st_cast(SA.grid,"LINESTRING"), size = 2) +
  # geom_sf(data = bandsiteselect, size = 3) +
  theme_void(base_size = 80) +
  labs(subtitle = "Low Small Scale Variation") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.subtitle = element_text(hjust = 0.5))


## HARVEST RATE
# Set the parameters of the semi-variogram
Psill = 0.01 ## Partial sill = Magnitude of variation, value that variogram levels out at 
Range= 7  ## Maximal distance of autocorrelation, where variogram levels out
Nugget= 0.005  ## Small-scale variations
# Set the semi-variogram model
Beta = 0   ## mean yield (tons/ha) in the field
HR_modelling=gstat(formula=z~1, ## We assume that there is a constant trend in the data
                   locations=~x+y,
                   dummy=T,    ## Logical value to set to True for unconditional simulation
                   beta=Beta,  ## Necessity to set the average value over the field
                   model=vgm(psill=Psill,
                             range=Range ,
                             nugget=Nugget,
                             model='Gau'), ## Spherical semi-variogram model
                   nmax=40) ## number of nearest observations used for each new prediction
# Simulate the spatial structure within the field
HR_gaussian_field <- predict(HR_modelling, newdata=Field, nsim=1) %>% ## nsim : Nombre de simulations
  rename(HR.SPP = sim1) %>%
  mutate(HR.base = exp(hr.b0+HR.SPP)/(1+exp(hr.b0+HR.SPP)))
HR_spvar.points <- st_as_sf(HR_gaussian_field, coords = c("x","y")) 
# #Visualize the spatial variation
require(ggplot2)
med_nugget <- ggplot()+  ## Initialize the ggplot layer
  geom_raster(data=HR_gaussian_field,aes(x=x,y=y,fill=HR.base))+ ## plot the observations as points with a colored yield gradient
  scale_fill_gradient(low="purple", high="yellow") +   ## Set the colors of the gradient
  # geom_sf(data = bandsiteselect) +
  # geom_sf(data = st_cast(SA.grid,"LINESTRING"), size = 2) +
  # geom_sf(data = bandsiteselect, size = 3) +
  theme_void(base_size = 80) +
  labs(subtitle = "Medium Small Scale Variation") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.subtitle = element_text(hjust = 0.5))

## HARVEST RATE
# Set the parameters of the semi-variogram
Psill = 0.01 ## Partial sill = Magnitude of variation, value that variogram levels out at 
Range= 7  ## Maximal distance of autocorrelation, where variogram levels out
Nugget= 0.01  ## Small-scale variations
# Set the semi-variogram model
Beta = 0   ## mean yield (tons/ha) in the field
HR_modelling=gstat(formula=z~1, ## We assume that there is a constant trend in the data
                   locations=~x+y,
                   dummy=T,    ## Logical value to set to True for unconditional simulation
                   beta=Beta,  ## Necessity to set the average value over the field
                   model=vgm(psill=Psill,
                             range=Range ,
                             nugget=Nugget,
                             model='Gau'), ## Spherical semi-variogram model
                   nmax=40) ## number of nearest observations used for each new prediction
# Simulate the spatial structure within the field
HR_gaussian_field <- predict(HR_modelling, newdata=Field, nsim=1) %>% ## nsim : Nombre de simulations
  rename(HR.SPP = sim1) %>%
  mutate(HR.base = exp(hr.b0+HR.SPP)/(1+exp(hr.b0+HR.SPP)))
HR_spvar.points <- st_as_sf(HR_gaussian_field, coords = c("x","y")) 
# #Visualize the spatial variation
require(ggplot2)
high_nugget <- ggplot()+  ## Initialize the ggplot layer
  geom_raster(data=HR_gaussian_field,aes(x=x,y=y,fill=HR.base))+ ## plot the observations as points with a colored yield gradient
  scale_fill_gradient(low="purple", high="yellow") +   ## Set the colors of the gradient
  # geom_sf(data = bandsiteselect) +
  # geom_sf(data = st_cast(SA.grid,"LINESTRING"), size = 2) +
  # geom_sf(data = bandsiteselect, size = 3) +
  theme_void(base_size = 80) +
  labs(subtitle = "High Small Scale Variation") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.subtitle = element_text(hjust = 0.5))

require(patchwork)
nuggetgraphs <- low_nugget + med_nugget + high_nugget







## Range
# Set the parameters of the semi-variogram
Psill = 0.01 ## Partial sill = Magnitude of variation, value that variogram levels out at 
Range= 2  ## Maximal distance of autocorrelation, where variogram levels out
Nugget= 0.005  ## Small-scale variations
# Set the semi-variogram model
Beta = 0   ## mean yield (tons/ha) in the field
HR_modelling=gstat(formula=z~1, ## We assume that there is a constant trend in the data
                   locations=~x+y,
                   dummy=T,    ## Logical value to set to True for unconditional simulation
                   beta=Beta,  ## Necessity to set the average value over the field
                   model=vgm(psill=Psill,
                             range=Range ,
                             nugget=Nugget,
                             model='Gau'), ## Spherical semi-variogram model
                   nmax=40) ## number of nearest observations used for each new prediction
# Simulate the spatial structure within the field
HR_gaussian_field <- predict(HR_modelling, newdata=Field, nsim=1) %>% ## nsim : Nombre de simulations
  rename(HR.SPP = sim1) %>%
  mutate(HR.base = exp(hr.b0+HR.SPP)/(1+exp(hr.b0+HR.SPP)))
HR_spvar.points <- st_as_sf(HR_gaussian_field, coords = c("x","y")) 
# #Visualize the spatial variation
require(ggplot2)
low_range <- ggplot()+  ## Initialize the ggplot layer
  geom_raster(data=HR_gaussian_field,aes(x=x,y=y,fill=HR.base))+ ## plot the observations as points with a colored yield gradient
  scale_fill_gradient(low="purple", high="yellow") +   ## Set the colors of the gradient
  # # geom_sf(data = bandsiteselect) +
  # geom_sf(data = st_cast(SA.grid,"LINESTRING"), size = 2) +
  # geom_sf(data = bandsiteselect, size = 3) +
  theme_void(base_size = 80) +
  labs(subtitle = "Low Distance of Autocorrelation") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.subtitle = element_text(hjust = 0.5))


## HARVEST RATE
# Set the parameters of the semi-variogram
Psill = 0.01 ## Partial sill = Magnitude of variation, value that variogram levels out at 
Range= 7  ## Maximal distance of autocorrelation, where variogram levels out
Nugget= 0.005  ## Small-scale variations
# Set the semi-variogram model
Beta = 0   ## mean yield (tons/ha) in the field
HR_modelling=gstat(formula=z~1, ## We assume that there is a constant trend in the data
                   locations=~x+y,
                   dummy=T,    ## Logical value to set to True for unconditional simulation
                   beta=Beta,  ## Necessity to set the average value over the field
                   model=vgm(psill=Psill,
                             range=Range ,
                             nugget=Nugget,
                             model='Gau'), ## Spherical semi-variogram model
                   nmax=40) ## number of nearest observations used for each new prediction
# Simulate the spatial structure within the field
HR_gaussian_field <- predict(HR_modelling, newdata=Field, nsim=1) %>% ## nsim : Nombre de simulations
  rename(HR.SPP = sim1) %>%
  mutate(HR.base = exp(hr.b0+HR.SPP)/(1+exp(hr.b0+HR.SPP)))
HR_spvar.points <- st_as_sf(HR_gaussian_field, coords = c("x","y")) 
# #Visualize the spatial variation
require(ggplot2)
med_range <- ggplot()+  ## Initialize the ggplot layer
  geom_raster(data=HR_gaussian_field,aes(x=x,y=y,fill=HR.base))+ ## plot the observations as points with a colored yield gradient
  scale_fill_gradient(low="purple", high="yellow") +   ## Set the colors of the gradient
  # geom_sf(data = bandsiteselect) +
  # geom_sf(data = st_cast(SA.grid,"LINESTRING"), size = 2) +
  # geom_sf(data = bandsiteselect, size = 3) +
  theme_void(base_size = 80) +
  labs(subtitle = "Medium Distance of Autocorrelation") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.subtitle = element_text(hjust = 0.5))

## HARVEST RATE
# Set the parameters of the semi-variogram
Psill = 0.01 ## Partial sill = Magnitude of variation, value that variogram levels out at 
Range= 15  ## Maximal distance of autocorrelation, where variogram levels out
Nugget= 0.005  ## Small-scale variations
# Set the semi-variogram model
Beta = 0   ## mean yield (tons/ha) in the field
HR_modelling=gstat(formula=z~1, ## We assume that there is a constant trend in the data
                   locations=~x+y,
                   dummy=T,    ## Logical value to set to True for unconditional simulation
                   beta=Beta,  ## Necessity to set the average value over the field
                   model=vgm(psill=Psill,
                             range=Range ,
                             nugget=Nugget,
                             model='Gau'), ## Spherical semi-variogram model
                   nmax=40) ## number of nearest observations used for each new prediction
# Simulate the spatial structure within the field
HR_gaussian_field <- predict(HR_modelling, newdata=Field, nsim=1) %>% ## nsim : Nombre de simulations
  rename(HR.SPP = sim1) %>%
  mutate(HR.base = exp(hr.b0+HR.SPP)/(1+exp(hr.b0+HR.SPP)))
HR_spvar.points <- st_as_sf(HR_gaussian_field, coords = c("x","y")) 
# #Visualize the spatial variation
require(ggplot2)
high_range <- ggplot()+  ## Initialize the ggplot layer
  geom_raster(data=HR_gaussian_field,aes(x=x,y=y,fill=HR.base))+ ## plot the observations as points with a colored yield gradient
  scale_fill_gradient(low="purple", high="yellow") +   ## Set the colors of the gradient
  # geom_sf(data = bandsiteselect) +
  # geom_sf(data = st_cast(SA.grid,"LINESTRING"), size = 2) +
  # geom_sf(data = bandsiteselect, size = 3) +
  theme_void(base_size = 80) +
  labs(subtitle = "High Distance of Autocorrelation") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.subtitle = element_text(hjust = 0.5))

require(patchwork)
rangegraphs <- low_range + med_range + high_range

require(cowplot)
jpeg('Example Spatial Variation.png', width = 4000, height = 4000)
plot_grid(plotlist = list(psillgraphs,nuggetgraphs, rangegraphs),
          nrow = 3
)
dev.off()
