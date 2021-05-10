library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(lubridate)    # To handle dates and times


# Task 1

caro <- read_delim("caro60.csv",",")

caro <- caro %>%
  mutate(
    nMinus3 = sqrt((lag(E,3)-E)^2+(lag(N,3)-N)^2),   # distance to pos -3 minutes
    nMinus2 = sqrt((lag(E,2)-E)^2+(lag(N,2)-N)^2),   # distance to pos -2 minutes
    nMinus1 = sqrt((lag(E,1)-E)^2+(lag(N,1)-N)^2),   # distance to pos -1 minutes
    nPlus1  = sqrt((E-lead(E,1))^2+(N-lead(N,1))^2), # distance to pos +1 mintues
    nPlus2  = sqrt((E-lead(E,2))^2+(N-lead(N,2))^2), # distance to pos +2 mintues
    nPlus3  = sqrt((E-lead(E,3))^2+(N-lead(N,3))^2)) # distance to pos +3 minute

# calculating mean
caro <- caro %>%
  rowwise() %>%
  mutate(
    stepMean = mean(c(nMinus3, nMinus2, nMinus1,nPlus1,nPlus2, nPlus3))
  ) %>%
  ungroup()


# Task 2

summary(caro)

ggplot(caro, aes(x= stepMean)) +
  geom_histogram()

ggplot(caro, aes(y=stepMean)) +
  geom_boxplot()

# summary data: min. 1.431, median 3.962, mean 5.951, max 62.368

# Median as Threshold
caromedian <- caro %>% 
  ungroup() %>%
  mutate(static = stepMean < median(stepMean, na.rm = TRUE))

caromedian <- caromedian %>%
  filter(!static)

caromedian %>%
  ggplot(aes(E, N))  +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")

# mean as threshold
caromean <- caro %>% 
  ungroup() %>%
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))

caromean <- caromean %>%
  filter(!static)

caromean %>%
  ggplot(aes(E, N))  +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")

# Treshold is 8
caro8 <- caro %>% 
  ungroup() %>%
  mutate(static = stepMean < 8)

caro8 <- caro8 %>%
  filter(!static)

caro8 %>%
  ggplot(aes(E, N))  +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")

# Treshold is 9
caro9 <- caro %>% 
  ungroup() %>%
  mutate(static = stepMean < 9)

caro9_filt <- caro9 %>%
  filter(!static)

caro9_filt %>%
  ggplot(aes(E, N))  +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")

# Task 3

Plot_segm <- ggplot(data = caro9, aes(x= E, y= N)) +
  geom_path() +
  geom_point(aes(colour=static)) + 
  theme_light() +
  labs(color = "Segments")+
  theme(panel.border = element_blank(),
        title = element_text(vjust=2, size=15),
        axis.title = element_text(vjust = 2, size = 15))

# Task 4

rle_id <- function(vec){
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times=x))
}

caro9 <- caro9 %>%
  mutate(segment_id = rle_id(static))

caro9 <- caro9 %>%
  filter(!static)

Plot_segm_col_unclean <- ggplot(data = caro9, aes(x= E, y= N)) +
  geom_path() +
  geom_point(aes(colour=segment_id)) +
  theme_light() +
  labs(color = "Segments", titel = "All segments (uncleaned)") +
  theme(panel.border = element_blank(),
        title = element_text(vjust = 2, size = 15),
        axis.title = element_text(vjust = 2, size = 15))

caro9 <- caro9 %>%
  group_by(segment_id)  %>%
  mutate(n = length(TierID)) %>%
  filter(n >= 5)

Plot_segm_col_clean <- ggplot(data = caro9, aes(x= E, y= N)) +
  geom_path() +
  geom_point(aes(colour=segment_id)) +
  theme_light() +
  labs(color = "Segments", title = "Long segments (removed segments <5)") +
  theme(panel.border = element_blank(),
        title = element_text(vjust = 2, size = 15),
        axis.title = element_text(vjust = 2, size = 15))

# Task 5 - similarity measures

pedestrian <- read_delim("pedestrian.csv",",")

pedestrian$TrajID <- as.factor(pedestrian$TrajID)
str(pedestrian)

Plot_pede_facet <- ggplot(data = pedestrian, aes(x= E, y= N, colour= TrajID)) +
  geom_path() +
  geom_point() + 
  facet_wrap(~TrajID, nrow = 2) +
  theme_light() +
  theme(panel.border = element_blank(),
        title = element_text(vjust = 2, size = 15),
        axis.title = element_text(vjust = 2, size = 15),
        legend.position = "none")

# Task 6 - Calculate similarity 

library(SimilarityMeasures)

pedestrian <- select(pedestrian, TrajID, N, E)

Traj1 <- pedestrian  [1:47,]
Traj2 <- pedestrian  [48:95,]
Traj3 <- pedestrian  [96:141,]
Traj4 <- pedestrian  [142:190,]
Traj5 <- pedestrian  [191:242,]
Traj6 <- pedestrian  [143:289,]

Traj1 <- select(Traj1, E,N)
Traj2 <- select(Traj2, E,N)
Traj3 <- select(Traj3, E,N)
Traj4 <- select(Traj4, E,N)
Traj5 <- select(Traj5, E,N)
Traj6 <- select(Traj6, E,N)

Traj1 <- as.matrix(Traj1)
Traj2 <- as.matrix(Traj2)
Traj3 <- as.matrix(Traj3)
Traj4 <- as.matrix(Traj4)
Traj5 <- as.matrix(Traj5)
Traj6 <- as.matrix(Traj6)

# DTW
dtw2 <- DTW(Traj1, Traj2, -1)
dtw3 <- DTW(Traj1, Traj3, -1)
dtw4 <- DTW(Traj1, Traj4, -1)
dtw5 <- DTW(Traj1, Traj5, -1)
dtw6 <- DTW(Traj1, Traj6, -1)


# Edit dist
?EditDist

edt2 <- EditDist(Traj1, Traj2, 10)
edt3 <- EditDist(Traj1, Traj3, 10)
edt4 <- EditDist(Traj1, Traj4, 10)
edt5 <- EditDist(Traj1, Traj5, 10)
edt6 <- EditDist(Traj1, Traj6, 10)

# Frechet 
?Frechet
fre2 <- Frechet(Traj1, Traj2)
fre3 <- Frechet(Traj1, Traj3)
fre4 <- Frechet(Traj1, Traj4)
fre5 <- Frechet(Traj1, Traj5)
fre6 <- Frechet(Traj1, Traj6)

# LCSS
?LCSS
lcss2 <- LCSS(Traj1, Traj2, 10,5,0.5)
lcss3 <- LCSS(Traj1, Traj3, 10,5,0.5)
lcss4 <- LCSS(Traj1, Traj4, 10,5,0.5)
lcss5 <- LCSS(Traj1, Traj5, 10,5,0.5)
lcss6 <- LCSS(Traj1, Traj6, 10,5,0.5)

dtw <- c(NA, dtw2, dtw3, dtw4, dtw5, dtw6)
edt <- c(NA, edt2, edt3, edt4, edt5, edt6)
fre <- c(NA, fre2, fre3, fre4, fre5, fre6)
lcss <- c(NA, lcss2, lcss3, lcss4, lcss5, lcss6)

res.sim.mea <- data.frame(dtw, edt, fre, lcss)
View(res.sim.mea)

res.sim.mea$traj <- c(1,2,3,4,5,6)

# Plots

plot_dtw <- ggplot(res.sim.mea, aes(x=traj, y=dtw, fill = traj)) +
  geom_bar(stat = "identity") +
  theme_light()+
  theme(legend.position = "none") +
  labs(title = "Distance Time Warping")

plot_edt <- ggplot(res.sim.mea, aes(x=traj, y=edt, fill = traj)) +
  geom_bar(stat = "identity") +
  theme_light()+
  theme(legend.position = "none") +
  labs(title = "Edit Distance")

plot_fre <- ggplot(res.sim.mea, aes(x=traj, y=fre, fill = traj)) +
  geom_bar(stat = "identity") +
  theme_light()+
  theme(legend.position = "none") +
  labs(title = "Frechet Calculation")

plot_lcss <- ggplot(res.sim.mea, aes(x=traj, y=lcss, fill = traj)) +
  geom_bar(stat = "identity") +
  theme_light()+
  theme(legend.position = "none") +
  labs(title = "Longest Common subsequence")

library(cowplot)
grid <- plot_grid(plot_dtw, plot_edt, plot_fre, plot_lcss)

grid + plot_annotation(title = "Computed similarities using differnet measures /n between trajectory 1 to all other trajectories")
