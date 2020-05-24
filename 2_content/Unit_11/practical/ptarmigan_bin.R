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



ggplot(dd, aes(x=Elevation, y=pres)) +
  geom_point (alpha=0.5) +
  geom_smooth(method="glm", method.args = list(family = "binomial"))
And when must I do this?
  
  new <- expand.grid (Elevation = seq (min(dd$Elevation), max (dd$Elevation), length = 500))

new2 <- predict (m1, newdata = new, se.fit = TRUE, type = 'response')
together <- cbind (new, new2)

ggplot (dd, aes (x = Elevation, y = pres)) +
  geom_point (position=position_jitter(height=0.05, width=0), alpha = 0.5) +
  geom_smooth (data = together, mapping = aes (x = Elevation, y = fit), stat = "identity")
