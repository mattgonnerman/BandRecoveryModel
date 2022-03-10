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
  geom_sf(data = spatialknots, size = 3, aes(color = "Spatial Knot", shape = "Spatial Knot")) +
  geom_sf(data = cap_sf, size = 3, aes(color = "Capture Site", shape = "Capture Site")) +
  theme_void(base_size = 24) +
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

jpeg("./Graphs/SPP_Map.jpeg", width = 3000, height = 3000, res = 300)
spp_map
dev.off()
