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
  geom_point(data=HR_gaussian_field,aes(x=x,y=y,col=HR.base), size=3)+ ## plot the observations as points with a colored yield gradient
  scale_colour_gradient(low="red",high="green") +   ## Set the colors of the gradient
  # geom_sf(data = bandsiteselect) +
  geom_sf(data = st_cast(SA.grid,"LINESTRING")) +
  theme_classic(base_size = 38) +
  labs(title = "Low Partial Sill (0.001)", subtitle = "Nugget = 0.005, Range = 7")


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
  geom_point(data=HR_gaussian_field,aes(x=x,y=y,col=HR.base), size=3)+ ## plot the observations as points with a colored yield gradient
  scale_colour_gradient(low="red",high="green") +   ## Set the colors of the gradient
  # geom_sf(data = bandsiteselect) +
  geom_sf(data = st_cast(SA.grid,"LINESTRING")) +
  theme_classic(base_size = 38) +
  labs(title = "Medium Partial Sill (0.01)", subtitle = "Nugget = 0.005, Range = 7")

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
  geom_point(data=HR_gaussian_field,aes(x=x,y=y,col=HR.base), size=3)+ ## plot the observations as points with a colored yield gradient
  scale_colour_gradient(low="red",high="green") +   ## Set the colors of the gradient
  # geom_sf(data = bandsiteselect) +
  geom_sf(data = st_cast(SA.grid,"LINESTRING")) +
  theme_classic(base_size = 38) +
  labs(title = "High Partial Sill (0.05)", subtitle = "Nugget = 0.005, Range = 7")

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
  geom_point(data=HR_gaussian_field,aes(x=x,y=y,col=HR.base), size=3)+ ## plot the observations as points with a colored yield gradient
  scale_colour_gradient(low="red",high="green") +   ## Set the colors of the gradient
  # geom_sf(data = bandsiteselect) +
  geom_sf(data = st_cast(SA.grid,"LINESTRING")) +
  theme_classic(base_size = 38) +
  labs(title = "Low Nugget (0.001)", subtitle = "Psill = 0.01, Range = 7")


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
  geom_point(data=HR_gaussian_field,aes(x=x,y=y,col=HR.base), size=3)+ ## plot the observations as points with a colored yield gradient
  scale_colour_gradient(low="red",high="green") +   ## Set the colors of the gradient
  # geom_sf(data = bandsiteselect) +
  geom_sf(data = st_cast(SA.grid,"LINESTRING")) +
  theme_classic(base_size = 38) +
  labs(title = "Medium Nugget (0.005)", subtitle = "Psill = 0.01, Range = 7")

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
  geom_point(data=HR_gaussian_field,aes(x=x,y=y,col=HR.base), size=3)+ ## plot the observations as points with a colored yield gradient
  scale_colour_gradient(low="red",high="green") +   ## Set the colors of the gradient
  # geom_sf(data = bandsiteselect) +
  geom_sf(data = st_cast(SA.grid,"LINESTRING")) +
  theme_classic(base_size = 38) +
  labs(title = "High Nugget (0.01)", subtitle = "Psill = 0.01, Range = 7")

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
  geom_point(data=HR_gaussian_field,aes(x=x,y=y,col=HR.base), size=3)+ ## plot the observations as points with a colored yield gradient
  scale_colour_gradient(low="red",high="green") +   ## Set the colors of the gradient
  # geom_sf(data = bandsiteselect) +
  geom_sf(data = st_cast(SA.grid,"LINESTRING")) +
  theme_classic(base_size = 38) +
  labs(title = "Low Range (2)", subtitle = "Psill = 0.01, Nugget = 0.005")


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
  geom_point(data=HR_gaussian_field,aes(x=x,y=y,col=HR.base), size=3)+ ## plot the observations as points with a colored yield gradient
  scale_colour_gradient(low="red",high="green") +   ## Set the colors of the gradient
  # geom_sf(data = bandsiteselect) +
  geom_sf(data = st_cast(SA.grid,"LINESTRING")) +
  theme_classic(base_size = 38) +
  labs(title = "Medium Range (7)", subtitle = "Psill = 0.01, Nugget = 0.005")

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
  geom_point(data=HR_gaussian_field,aes(x=x,y=y,col=HR.base), size=3)+ ## plot the observations as points with a colored yield gradient
  scale_colour_gradient(low="red",high="green") +   ## Set the colors of the gradient
  geom_sf(data = st_cast(SA.grid,"LINESTRING")) +
  theme_classic(base_size = 38) +
  labs(title = "High Range (15)", subtitle = "Psill = 0.01, Nugget = 0.005")

require(patchwork)
rangegraphs <- low_range + med_range + high_range

require(cowplot)
jpeg('Example Spatial Variation.png', width = 3000, height = 3000)
plot_grid(plotlist = list(psillgraphs,nuggetgraphs, rangegraphs),
          nrow = 3
)
dev.off()