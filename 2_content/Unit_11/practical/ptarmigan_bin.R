rm(list=ls())

library(tidyverse)
library(ggfortify)

dd <- read_csv("~/Desktop/presabs_both_IndYears_allvars_final.csv")

ggplot(dd, aes(x=Elevation, y=pres)) +
  geom_point(position=position_jitter(height=0.1, width=0),
             alpha=0.5) +
  stat_smooth(method="glm", method.args = list(family = "binomial"))



m1 <- glm(pres ~ Elevation, data=dd, family=binomial)
autoplot(m1)
anova(m1)
