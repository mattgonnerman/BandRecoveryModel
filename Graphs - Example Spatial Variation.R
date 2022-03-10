require(parallel)
require(dplyr)
require(stringr)
require(tidyr)
require(miscTools)
require(ggplot2)
require(gstat)
require(sf)

#Values for graphs
A <- B <- 100
C <- D <- 5
Field = expand.grid(1:A, 1:B)
names(Field) = c('x','y')
hr <- sample(seq(.10,.30, .01), 1)  #set weekly survival rate for simulation
hr.b0 <- log(hr/(1-hr))

# Create SF grid for study area regions
SA.points.df <- data.frame(x = rep(1:A, B),
                           y = rep(1:B, each = A))
SA.points.sf <- st_as_sf(SA.points.df, coords = c("x","y"))
SA.grid <- st_as_sf(st_make_grid(SA.points.sf, n = c(C,D))) %>%
  mutate(RegionID = 1:(C*D))

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
HR_spvar.grid <- st_as_sf(st_make_grid(HR_spvar.points, cellsize = 1, what = "polygons"))
HR_spvar.grid <- st_join(HR_spvar.grid, HR_spvar.points)

# #Visualize the spatial variation
require(ggplot2)
low_psill <- ggplot()+  ## Initialize the ggplot layer
  geom_sf(data=HR_spvar.grid, aes(col=HR.base, fill = HR.base))+ ## plot the observations as points with a colored yield gradient
  scale_colour_gradient(low="red",high="green") +   ## Set the colors of the gradient
  scale_fill_gradient(low="red",high="green") +
  # geom_sf(data = bandsiteselect) +
  geom_sf(data = st_cast(SA.grid,"LINESTRING")) +
  theme_void(base_size = 14) +
  labs(title = "Low Partial Sill") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
low_psill

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
HR_spvar.grid <- st_as_sf(st_make_grid(HR_spvar.points, cellsize = 1, what = "polygons"))
HR_spvar.grid <- st_join(HR_spvar.grid, HR_spvar.points)
# #Visualize the spatial variation
require(ggplot2)
med_psill <- ggplot()+  ## Initialize the ggplot layer
  geom_sf(data=HR_spvar.grid, aes(col=HR.base, fill = HR.base))+ ## plot the observations as points with a colored yield gradient
  scale_colour_gradient(low="red",high="green") +   ## Set the colors of the gradient
  scale_fill_gradient(low="red",high="green") +
  # geom_sf(data = bandsiteselect) +
  geom_sf(data = st_cast(SA.grid,"LINESTRING")) +
  theme_void(base_size = 14) +
  labs(title = "Medium Partial Sill")+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

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
HR_spvar.grid <- st_as_sf(st_make_grid(HR_spvar.points, cellsize = 1, what = "polygons"))
HR_spvar.grid <- st_join(HR_spvar.grid, HR_spvar.points)
# #Visualize the spatial variation
require(ggplot2)
high_psill <- ggplot()+  ## Initialize the ggplot layer
  geom_sf(data=HR_spvar.grid, aes(col=HR.base, fill = HR.base))+ ## plot the observations as points with a colored yield gradient
  scale_colour_gradient(low="red",high="green") +   ## Set the colors of the gradient
  scale_fill_gradient(low="red",high="green") +
  # geom_sf(data = bandsiteselect) +
  geom_sf(data = st_cast(SA.grid,"LINESTRING")) +
  theme_void(base_size = 14) +
  labs(title = "High Partial Sill")+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

require(patchwork)
psillgraphs <- low_psill + med_psill + high_psill + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 14))






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
HR_spvar.grid <- st_as_sf(st_make_grid(HR_spvar.points, cellsize = 1, what = "polygons"))
HR_spvar.grid <- st_join(HR_spvar.grid, HR_spvar.points) 
# #Visualize the spatial variation
require(ggplot2)
low_nugget <- ggplot()+  ## Initialize the ggplot layer
  geom_sf(data=HR_spvar.grid, aes(col=HR.base, fill = HR.base))+ ## plot the observations as points with a colored yield gradient
  scale_colour_gradient(low="red",high="green") +   ## Set the colors of the gradient
  scale_fill_gradient(low="red",high="green") +
  # geom_sf(data = bandsiteselect) +
  geom_sf(data = st_cast(SA.grid,"LINESTRING")) +
  theme_void(base_size = 14) +
  labs(title = "Low Nugget")+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))


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
HR_spvar.grid <- st_as_sf(st_make_grid(HR_spvar.points, cellsize = 1, what = "polygons"))
HR_spvar.grid <- st_join(HR_spvar.grid, HR_spvar.points)
# #Visualize the spatial variation
require(ggplot2)
med_nugget <- ggplot()+  ## Initialize the ggplot layer
  geom_sf(data=HR_spvar.grid, aes(col=HR.base, fill = HR.base))+ ## plot the observations as points with a colored yield gradient
  scale_colour_gradient(low="red",high="green") +   ## Set the colors of the gradient
  scale_fill_gradient(low="red",high="green") +
  # geom_sf(data = bandsiteselect) +
  geom_sf(data = st_cast(SA.grid,"LINESTRING")) +
  theme_void(base_size = 14) +
  labs(title = "Medium Nugget")+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

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
HR_spvar.grid <- st_as_sf(st_make_grid(HR_spvar.points, cellsize = 1, what = "polygons"))
HR_spvar.grid <- st_join(HR_spvar.grid, HR_spvar.points)
# #Visualize the spatial variation
require(ggplot2)
high_nugget <- ggplot()+  ## Initialize the ggplot layer
  geom_sf(data=HR_spvar.grid, aes(col=HR.base, fill = HR.base))+ ## plot the observations as points with a colored yield gradient
  scale_colour_gradient(low="red",high="green") +   ## Set the colors of the gradient
  scale_fill_gradient(low="red",high="green") +
  # geom_sf(data = bandsiteselect) +
  geom_sf(data = st_cast(SA.grid,"LINESTRING")) +
  theme_void(base_size = 14) +
  labs(title = "High Nugget")+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

require(patchwork)
nuggetgraphs <- low_nugget + med_nugget + high_nugget + 
  plot_annotation(tag_levels =  list(c("D", "E", "F"))) & 
  theme(plot.tag = element_text(size = 14))







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
HR_spvar.grid <- st_as_sf(st_make_grid(HR_spvar.points, cellsize = 1, what = "polygons"))
HR_spvar.grid <- st_join(HR_spvar.grid, HR_spvar.points)
# #Visualize the spatial variation
require(ggplot2)
low_range <- ggplot()+  ## Initialize the ggplot layer
  geom_sf(data=HR_spvar.grid, aes(col=HR.base, fill = HR.base))+ ## plot the observations as points with a colored yield gradient
  scale_colour_gradient(low="red",high="green") +   ## Set the colors of the gradient
  scale_fill_gradient(low="red",high="green") +
  # geom_sf(data = bandsiteselect) +
  geom_sf(data = st_cast(SA.grid,"LINESTRING")) +
  theme_void(base_size = 14) +
  labs(title = "Low Range")+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))


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
HR_spvar.grid <- st_as_sf(st_make_grid(HR_spvar.points, cellsize = 1, what = "polygons"))
HR_spvar.grid <- st_join(HR_spvar.grid, HR_spvar.points)

# #Visualize the spatial variation
require(ggplot2)
med_range <- ggplot()+  ## Initialize the ggplot layer
  geom_sf(data=HR_spvar.grid, aes(col=HR.base, fill = HR.base))+ ## plot the observations as points with a colored yield gradient
  scale_colour_gradient(low="red",high="green") +   ## Set the colors of the gradient
  scale_fill_gradient(low="red",high="green") +
  # geom_sf(data = bandsiteselect) +
  geom_sf(data = st_cast(SA.grid,"LINESTRING")) +
  theme_void(base_size = 14) +
  labs(title = "Medium Range")+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

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
HR_spvar.grid <- st_as_sf(st_make_grid(HR_spvar.points, cellsize = 1, what = "polygons"))
HR_spvar.grid <- st_join(HR_spvar.grid, HR_spvar.points)
# #Visualize the spatial variation
require(ggplot2)
high_range <- ggplot()+  ## Initialize the ggplot layer
  geom_sf(data=HR_spvar.grid, aes(col=HR.base, fill = HR.base))+ ## plot the observations as points with a colored yield gradient
  scale_colour_gradient(low="red",high="green") +   ## Set the colors of the gradient
  scale_fill_gradient(low="red",high="green") +
  geom_sf(data = st_cast(SA.grid,"LINESTRING")) +
  theme_void(base_size = 14) +
  labs(title = "High Range")+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

require(patchwork)
rangegraphs <- low_range + med_range + high_range + 
  plot_annotation(tag_levels = list(c("G", "H", "I"))) & 
  theme(plot.tag = element_text(size = 14))

require(cowplot)
jpeg('./Graphs/Example Spatial Variation.jpeg', res = 300, width = 3000, height = 3000)
plot_grid(plotlist = list(psillgraphs,nuggetgraphs, rangegraphs),
          nrow = 3
)
dev.off()
