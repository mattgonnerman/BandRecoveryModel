require(ggplot2)
require(dplyr)
require(stringr)

load( file ="RealDataModelOutput.RData") #BR_w_SPP_output
realspp.raw <- as.data.frame(BR_w_SPP_output$BUGSoutput$summary)
realnospp.raw <- read.csv("realdataNOSPPOutput2021.csv")

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

N.A.long.nospp <- realnospp.raw %>% 
  filter(substr(X,1,3) == "N.A") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, sd, WMD) %>%
  mutate(Age = "A")
N.J.long.nospp <- realnospp.raw %>% 
  filter(substr(X,1,3) == "N.J") %>%
  mutate(WMD = str_extract(X, "[:digit:]+")) %>%
  mutate(Year = as.numeric(substr(str_extract(X, "[,^][:digit:]+"), 2,4))) %>%
  dplyr::select(Year, mean, sd, WMD) %>%
  mutate(Age = "J")
N.nospp <- rbind(N.A.long.nospp, N.J.long.nospp) %>% rename(Mean.NoSPP = mean, Sd.NoSPP = sd)

N.merged <- merge(N.spp, N.nospp, by = c("WMD", "Year", "Age"))
N.merged.A <- N.merged %>% filter(Age == "A")
N.merged.J <- N.merged %>% filter(Age == "J")

N.merged.summary <- N.merged %>%
  mutate(Diff = Mean.NoSPP - Mean.SPP) %>%
  group_by(Age) %>%
  summarize(MeanDiff = mean(Diff),
            SD = sd(Diff))

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
HR.J.est.spp <- HR.est.spp %>% filter(Age == "J")

HR.est.nospp <- realnospp.raw %>%
  rename(ID = X) %>%
  filter(grepl("HR.*.year", ID)) %>%
  mutate(Age = substr(ID,4,4)) %>%
  mutate(Year = substr(ID,11,11)) %>%
  dplyr::select(mean, sd, Age, Year) %>%
  rename(HRNOSPP = mean, SDNoSPP = sd)%>%
  mutate(Upper = HRNOSPP + SDNoSPP) %>%
  mutate(Lower = HRNOSPP - SDNoSPP)
HR.A.est.nospp <- HR.est.nospp %>% filter(Age == "A")
HR.J.est.nospp <- HR.est.nospp %>% filter(Age == "J")


HR.est.merge <- merge(HR.est.spp, HR.est.nospp, by = c("Age", "Year"))
HR.est.merge.A <- HR.est.merge %>% filter(Age == "A")
HR.est.merge.J <- HR.est.merge %>% filter(Age == "J")


### Abundance Plots
N.A.plot <- ggplot(data = N.merged.A, aes(x = Mean.NoSPP, y = Mean.SPP, 10)) +
  geom_point(size = .8) +
  geom_errorbar(aes(xmin = ifelse(Mean.NoSPP - Sd.NoSPP <= 0, 1, Mean.NoSPP - Sd.NoSPP), 
                xmax = Mean.NoSPP + Sd.NoSPP), lwd = .3) +
  geom_errorbar(aes(ymin = ifelse(Mean.SPP - Sd.SPP <= 0, 1, Mean.SPP - Sd.SPP), 
                    ymax = Mean.SPP + Sd.SPP), lwd = .3) +
  geom_abline(intercept = 0) +
  theme_classic(base_size = 10) + 
  scale_y_continuous(limits = c(1, 7100), breaks=c(0, 10, 100, 1000, 4000)) +
  scale_x_continuous(limits = c(1, 7100), breaks=c(0, 10, 100, 1000, 4000)) +
  coord_trans(x = "log10", y = "log10") +
  labs(title = "Adult Abundance", x = "No SPP", y = "SPP")

N.J.plot <- ggplot(data = N.merged.J, aes(x = Mean.NoSPP, y = Mean.SPP, 10)) +
  geom_point(size = .8) +
  geom_errorbar(aes(xmin = ifelse(Mean.NoSPP - Sd.NoSPP <= 0, 1, Mean.NoSPP - Sd.NoSPP), 
                    xmax = Mean.NoSPP + Sd.NoSPP), lwd = .3) +
  geom_errorbar(aes(ymin = ifelse(Mean.SPP - Sd.SPP <= 0, 1, Mean.SPP - Sd.SPP), 
                    ymax = Mean.SPP + Sd.SPP), lwd = .3) +
  geom_abline(intercept = 0) +
  theme_classic(base_size = 10) + 
  scale_y_continuous(limits = c(1, 7100), breaks=c(0, 10, 100, 1000, 4000)) +
  scale_x_continuous(limits = c(1, 7100), breaks=c(0, 10, 100, 1000, 4000)) +
  coord_trans(x = "log10", y = "log10") +
  labs(title = "Juvenile Abundance", x = "No SPP", y = "SPP")


### Harvest Rate Plots
HR.A.plot <- ggplot(data = HR.A.est.spp, aes(x = as.factor(Year), y = HRSPP)) +
  # geom_boxplot(aes(fill = as.factor(Year)), outlier.shape = NA) +
  geom_pointrange(aes(ymin = Lower, ymax = Upper), 
                  position=position_jitter(width=0.3), 
                  linetype='dashed',
                  shape = 20,
                  size = .65,
                  fatten = 2, 
                  alpha = .6) +
  geom_errorbar(data = HR.A.est.nospp,
                aes(x = as.factor(Year), 
                    y = HRNOSPP, 
                    ymin = Lower, ymax = Upper),
                width = NA,
                position = position_nudge(x = .0), 
                size = 1, color = "red") +
  geom_point(data = HR.A.est.nospp,
             aes(y = HRNOSPP, 
                 x = as.factor(Year)),
             fill = "red",
             size = 4, shape = 24, color = "black", stroke = .25,
             position = position_nudge(x = .0)) +
  theme_classic(base_size = 12) +
  labs(title = "Adult Harvest Rate", x = "Year", y = "Harvest Rate") +
  theme(legend.position = "none") +
  scale_x_discrete(labels = 2018:2021)

HR.J.plot <- ggplot(data = HR.J.est.spp, aes(x = as.factor(Year), y = HRSPP)) +
  # geom_boxplot(aes(fill = as.factor(Year)), outlier.shape = NA) +
  geom_pointrange(aes(ymin = Lower, ymax = Upper), 
                  position=position_jitter(width=0.3), 
                  linetype='dashed',
                  shape = 20,
                  size = .65,
                  fatten = 2, 
                  alpha = .6) +
  geom_errorbar(data = HR.J.est.nospp,
                aes(x = as.factor(Year), 
                    y = HRNOSPP, 
                    ymin = Lower, ymax = Upper),
                width = NA,
                position = position_nudge(x = .0), 
                size = 1, color = "red") +
  geom_point(data = HR.J.est.nospp,
             aes(y = HRNOSPP, 
                 x = as.factor(Year)),
                 fill = "red",
             size = 4, shape = 24, color = "black", stroke = .25,
             position = position_nudge(x = .0)) +
  theme_classic(base_size = 12) +
  labs(title = "Juvenile Harvest Rate", x = "Year", y = "Harvest Rate") +
  theme(legend.position = "none") +
  scale_x_discrete(labels = 2018:2021)


### Package Plots as Grid
compareSPPplots <- list(N.A.plot, N.J.plot, HR.A.plot, HR.J.plot)
require(cowplot)
compareSPP_grid <- plot_grid(plotlist = compareSPPplots,
                           nrow = 2,
                           labels = "AUTO",
                           label_size = 14,
                           align = "hv",
                           axis = "lb")

jpeg("Graphs/Compare With and Without SPP.jpg", width = 5000, height = 5000, res = 600)
compareSPP_grid
dev.off()
