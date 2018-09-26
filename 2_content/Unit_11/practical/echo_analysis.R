rm(list=ls())

library(tidyverse)
library(ggfortify)

dd <- read_csv("~/Desktop/echocardiogram.data.csv", col_names=T, na="?")

## web pages says don't use WMS, use WMI
dd <- select(dd, c(3:7, 9, 13))



ggplot(dd, aes(x=Lvdd, y=Alive)) +
  geom_point(position=position_jitter(height=0.1, width=0),
             alpha=0.5) +
  stat_smooth(method="glm", method.args = list(family = "binomial"))

ggplot(dd, aes(x=Fract_short, y=Alive)) +
  geom_point(position=position_jitter(height=0.1, width=0),
             alpha=0.5) +
  stat_smooth(method="glm", method.args = list(family = "binomial"))


