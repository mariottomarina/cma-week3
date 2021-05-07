library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
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

caro9 <- caro9 %>%
  filter(!static)

caro9 %>%
  ggplot(aes(E, N))  +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")

# Task 3

Plot_segm <- ggplot(data = caro9, aes(x= E, y= N)) +
  geom_path() +
  geom_point(aes(colour=static)) + 
  coord_sf(datum = st_crs(2056)) +
  theme_light() +
  labs(color = "Segments")+
  theme(panel.border = element_blank(),
        title = element_text(vjust=2, size=15),
        axis.title = element_text(vjust = 2, size = 15))


