library(ggplot2)
library(lubridate)
library(dplyr)

bike <- read.csv("london_merged.csv")

bike$season  <- factor(bike$season, labels = c("Spring", "Summer", "Fall", "Winter"))
bike$hour <- factor(hour(ymd_hms(bike$timestamp)))

p1 <-
  bike %>%
  group_by(hour) %>%
  summarise(mcount = mean(cnt)) %>%
  ggplot(aes(x = hour, y = mcount, fill = hour)) +
  geom_bar(stat = 'identity') +
  guides(fill = 'none') +
  theme_minimal()
p1

p2 <-
  bike %>%
  group_by(season, hour) %>%
  summarise(mcount = mean(cnt)) %>%
  ggplot(aes(x = hour, y = mcount, colour = season)) +
  geom_line(aes(group = season)) +
  theme_bw() +
  geom_point()
p2

p3 <-
  bike %>%
  group_by(season) %>%
  summarise(mcount = mean(cnt)) %>%
  ggplot(aes(x = reorder(season, mcount), y = mcount, fill = season)) +
  geom_bar(stat = 'identity') +
  labs(x = 'senson', y = 'mcount') +
  guides(fill = 'none') +
  theme_minimal()
p3


