require(dplyr)
require(sf)
require(ggplot2)
require(stringr)

#Run real data prep so you have access to that info when needed
source(file = "BR Final Sim - 9 Prep Real Turkey Data.R")
wmdids <- sort(CountWMD.df$WMD_IN)

#Load WMD Boundaried and Capture locations as sf objects
wmd_boundaries <- st_read(".", "Maine_Wildlife_Management_Districts") %>%
  rename(WMD = IDENTIFIER)
cap_locations <- st_read(".", "cap_locations")

#Load Results
modeloutput <- read.csv("2011start - realdataOutput2021.csv")

#2021 Abundance
N.A.2021 <- modeloutput %>% 
  filter(substr(X,1,3) == "N.A") %>%
  mutate(WMDIndex = as.numeric(str_extract(X, "[:digit:]+"))) %>%
  mutate(Year = as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  filter(Year == max(Year)) %>%
  mutate(WMD = wmdids[WMDIndex]) %>%
  dplyr::select(N.A = mean, WMD)

N.J.2021 <- modeloutput %>% 
  filter(substr(X,1,3) == "N.J") %>%
  mutate(WMDIndex = as.numeric(str_extract(X, "[:digit:]+"))) %>%
  mutate(Year = as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4)))%>%
  filter(Year == max(Year)) %>%
  mutate(WMD = wmdids[WMDIndex]) %>%
  dplyr::select(N.J = mean, WMD) 

#Merge estimates with WMD sf object
wmd_boundaries1 <- merge(wmd_boundaries, N.A.2021, by = "WMD", all.x = T)
WMD_Estimates.sf <- merge(wmd_boundaries1, N.J.2021, by = "WMD", all.x = T)

#Adult Abundance Map
PopA_Graph_heat <- ggplot() +
  geom_sf(data = WMD_Estimates.sf, aes(fill = N.A), color = "black", size = 1) +
  scale_fill_gradientn(colors = c("#c33764", "white", "#1d2671"),
                       na.value = "grey",
                       breaks = c(100, 1000, 2000, 3000),
                       labels=c("<100", 1000, 2000, 3000)) +
  theme_void(base_size = 16) + 
  guides(fill=guide_colorbar(title="Adult\nAbundance\n(2021)")) +
  theme(legend.key.height = unit(.3, "inches"),
        legend.key.width = unit(.25, "inches"),
        legend.position = c(.12, .8)) 

#Juvenile Abundance Map
PopJ_Graph_heat <- ggplot() +
  geom_sf(data = WMD_Estimates.sf, aes(fill = N.J), color = "black", size = 1) +
  scale_fill_gradientn(colors = c("#c33764", "white", "#1d2671"),
                       na.value = "grey",
                       breaks = c(100, 2000, 4000, 5500),
                       labels=c("<100", 2000, 4000, 5500)) +
  theme_void(base_size = 16) +
  guides(fill=guide_colorbar(title="Juvenile\nAbundance\n(2021)")) +
  theme(legend.key.height = unit(.3, "inches"),
        legend.key.width = unit(.25, "inches"),
        legend.position = c(.12, .8))

### Total Abundance Over Time
years <- 2011:2021
N.A <- modeloutput %>% 
  filter(substr(X,1,3) == "N.A") %>%
  mutate(WMDIndex = as.numeric(str_extract(X, "[:digit:]+"))) %>%
  mutate(Year = as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  mutate(WMD = wmdids[WMDIndex]) %>%
  dplyr::select(N.A = mean, WMD, Year) %>%
  mutate(Year = years[Year])
N.A.total <- N.A %>%
  pivot_wider(names_from = c(Year), values_from = N.A) %>%
  dplyr::select(-WMD) %>%
  colSums(.)

N.J <- modeloutput %>% 
  filter(substr(X,1,3) == "N.J") %>%
  mutate(WMDIndex = as.numeric(str_extract(X, "[:digit:]+"))) %>%
  mutate(Year = as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  mutate(WMD = wmdids[WMDIndex]) %>%
  dplyr::select(N.J = mean, WMD, Year) %>%
  mutate(Year = years[Year])
N.J.total <- N.J %>%
  pivot_wider(names_from = c(Year), values_from = N.J) %>%
  dplyr::select(-WMD) %>%
  colSums(.)

mean(N.A.total + N.J.total)
min(N.A.total + N.J.total)
max(N.A.total + N.J.total)
N.A.total + N.J.total

N.total <- data.frame(Abundance = c(N.J.total,N.A.total),
                      Age = c(rep("Juvenile", length(N.J.total)),rep("Adult", length(N.J.total))),
                      Year = rep(names(N.J.total),2))

N.time <- ggplot(data = N.total, aes(x = as.factor(Year), y = Abundance, group = as.factor(Age))) +
  geom_line(aes(linetype = as.factor(Age)), size = 2) +
  theme_classic(base_size = 20) +
  theme(legend.position = "none", 
        axis.title.x=element_blank()) +
  labs(x = "Year", y = "Abundance") +
  ylim(0,35000)

### Final Figure
require(patchwork)
maps <- (PopA_Graph_heat + PopJ_Graph_heat)/N.time
maps <- maps + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(heights = c(3, 1))

jpeg('Graphs/RealDataResults - Abundance Maps and Line Plots.png', width = 4000, height = 4000, res = 300)
maps
dev.off()





### Adult Harvest Rate Map
#Load Model outputs
load( file ="RealDataModelOutput.RData") #BR_w_SPP_output
HR.data <- data.frame(HR.A = BR_w_SPP_output$BUGSoutput$mean$WMD.HR.A[,4],
                      WMD = wmdids)
WMD.HR <- merge(wmd_boundaries, HR.data, by = "WMD", all.x = T)

#Adult Harvest Rate Map
HR_A_Graph_heat <- ggplot() +
  geom_sf(data = WMD.HR, aes(fill = HR.A), color = "black") +
  scale_fill_gradientn(colors = c("#c33764", "white", "#1d2671"),
                       na.value = "grey",
                       breaks = c(.2, .3, .4),
                       labels=c(.2, .3, .4)) +
  theme_void(base_size = 18) +
  # scalebar(wmd_sf, dist = 50, dist_unit = "km", transform = FALSE, model = "GRS80",
  #          st.size = 3) +
  labs(fill = "Harvest\nRate") +
  theme(legend.key.height = unit(.3, "inches"),
        legend.key.width = unit(.2, "inches"),
        legend.position = c(.12, .8))
ggsave(plot = HR_A_Graph_heat, "Graphs/Adult Harvest Rate Stand Alone Map.jpeg", width = 6.5, height = 8, units = "in", dpi = 300)

