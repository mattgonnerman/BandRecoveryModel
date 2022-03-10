require(dplyr)
require(sf)
require(ggplot2)

#Run real data prep so you have access to that info when needed
source(file = "BR Final Sim - 9 Prep Real Turkey Data.R")
wmdids <- sort(CountWMD.df$WMD_IN)

###Load Model outputs
load( file ="RealDataModelOutput.RData") #BR_w_SPP_output

#Load WMD Boundaried and Capture locations as sf objects
wmd_boundaries <- st_read(".", "Maine_Wildlife_Management_Districts")

cap_locations <- st_read(".", "cap_locations")

#Number of individuals per capture location
numperloc <- read.csv("Trapping - Data.csv") %>%
  filter(Sex == "M") %>%
  group_by(Location) %>%
  summarize(NumInd = n())
cap_numind <- merge(cap_locations, numperloc, by = "Location")

#Where there Transmitters deployed
transdepl <- read.csv("Trapping - Data.csv") %>%
  filter(Sex == "M") %>%
  group_by(Location) %>%
  summarize(TransDepl = as.factor(ifelse(any(!is.na(TransFreq)),"Y","N")))
cap_numind <- merge(cap_numind, transdepl, by = "Location")

#Create Spatial Knots
wmd_sf <- st_as_sf(wmd_boundaries, fill = T) %>%
  filter(IDENTIFIER %in% c(7:26,28))
wmd_centroid <- st_centroid(st_geometry(wmd_sf))

#Make initial grid and then subset to only those within WMDs
spatialknots1 <- st_make_grid(wmd_sf, square = T, what = "centers", cellsize = c( 24000,24000)) #create a grid
spatialknots2 <- c(spatialknots1, wmd_centroid)
spatialknots3 <- st_join(st_sf(spatialknots2), st_sf(wmd_sf), join = st_within)
knotscoords.df <- st_coordinates(spatialknots3) #extract coordinates
spatialknots <- cbind(knotscoords.df, spatialknots3) %>% #bind with coordinates
  filter(!is.na(IDENTIFIER)) %>% #remove knots not in a wmd
  filter(IDENTIFIER %in% c(7:26,28)) %>%
  filter(Y < 5170000) # remove knots below this latitude
spatialknots$Knot_ID <- 1:length(spatialknots$X) #Add a reference ID

#Create Map
#Capture sites as dots with size = number of individuals there, color = transmitter deployed
#Spatial knots as triangles

samplemap <- ggplot(wmd_boundaries) +
  geom_sf(aes(), fill = NA, size = 1, color = "black") +
  geom_sf(data = spatialknots, shape = 17, size = 2, color = 'red') +
  geom_sf(data = cap_numind, aes(size = NumInd, color = TransDepl), alpha = .7) +
  scale_fill_gradientn(colors = c("#ffeda0", "#feb24c", "#f03b20"),
                       na.value = "grey",
                       breaks = seq(3, 11, 2),
                       labels= seq(3, 11, 2)) +
  scale_size(breaks = c(floor(min(cap_numind$NumInd)),floor(mean(cap_numind$NumInd)), floor(max(cap_numind$NumInd))),
             range = c(1.5,7), guide = F) +
  scale_color_manual(values = c("blue4", 'turquoise3'), guide = F) +
  theme_void(base_size = 18) +
  theme(legend.key.height = unit(.3, "inches"), 
        legend.key.width = unit(.2, "inches"),
        legend.title = element_blank(),
        legend.position = "none")

#Save plot
ggsave(plot = samplemap, "Graphs/Real Data Distribution Map.jpeg", width = 6.5, height = 8, units = "in", dpi = 300)
