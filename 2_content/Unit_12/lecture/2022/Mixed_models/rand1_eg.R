rm(list=ls())

library(lme4)
library(tidyverse)


dd <- data.frame(individual=c(rep(letters[1:10], c(5,6,7,5,4,3,7,6,6,4)),
                              rep(letters[11:20], c(5,6,7,5,4,3,7,6,6,4))),
                 y=c(rnorm(53, 1)*1:53, rnorm(53, 2)),
                 sex=rep(c("male", "female"), each=53))
dd

fixed_model <- glm(y ~ sex + individual, data=dd)
summary(fixed_model)

random_model <- lmer(y ~ sex + (1|individual), data=dd)
summary(random_model)

summarise(dd, grand_mean=mean(y))
