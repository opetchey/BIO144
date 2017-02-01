##


## Clear R
rm(list=ls())

## load some libraries
library(rethinking)
library(tidyverse)
library(ggfortify)
library(cowplot)

## load the dataset
milk <- read_csv("https://github.com/opetchey/BIO144/raw/master/datasets/milk_rethinking.csv")

## look at the data and we see there are NAs in the neocortex.perc variable.
## Lets remove the rows with these NAs
milk <- na.omit(milk)

## Have a look at the distribution of the variables
ggplot(gather(milk, key=variable, value=value, 3:8), aes(value)) +
  geom_histogram(bins=10) + facet_wrap(~variable, scales = "free")

## the response variable, kcal.per.g, and the mass variable seem a bit right skewed
## so we can try log transformation
milk <- mutate(milk,
               log10_kcal.per.g = log10(kcal.per.g),
               log10_mass=log10(mass))

## not that much better
ggplot(gather(milk, key=variable, value=value, 3:10), aes(value)) +
  geom_histogram(bins=10) + facet_wrap(~variable, scales = "free")

## look at the bivariate scatter plots
ggplot(milk, aes(neocortex.perc, kcal.per.g)) + geom_point()
ggplot(milk, aes(log10_mass, kcal.per.g)) + geom_point()
## Not much going on here

ggplot(milk, aes(neocortex.perc, kcal.per.g)) + geom_point() + facet_wrap(~cut(log10_mass,2))
with(milk, pairs(cbind(kcal.per.g,neocortex.perc, log10_mass)))

## and are the explanatory variables correlated?
ggplot(milk, aes(neocortex.perc, log10_mass)) + geom_point()
## Yes, they are positively correlated.

## degrees of freedom for regression with one explanatory variable should be 15
## (one intercept and one slope estimated)
## degrees of freedom for regression with two explanatory variable should be 14
## (one intercept and two slope estimated)

## one of the regressions
m1 <- lm(kcal.per.g ~ neocortex.perc, data=milk)
autoplot(m1) ## pretty bad qqplot!
summary(m1)
## nothing significant, supporting our eyeball
## r2 0.024

## the other of the regressions
m2 <- lm(kcal.per.g ~ log10_mass, data=milk)
autoplot(m2) ## better qqplot
summary(m2)
## nothing significant, supporting our eyeball
## r2 0.12

## both explanatory variables...
m3 <- lm(kcal.per.g ~ log10_mass + neocortex.perc, data=milk)
autoplot(m3) ## pretty bad qqplot, though few data points
summary(m3)
anova(m3)
## both variables significant
## r2 0.53
## WOW!

library(car)
vif(m3)
## this happens due to correlation in the explantory variables.

## Here is the observed versus predicted plot
ggplot(milk, aes(kcal.per.g, predict(m3))) + geom_point() + geom_abline(intercept=0, slope=1)

## Here is plot of kcal.per.g corrected for variation in neocortex.perc, plotted against log10_mass
new_data <- expand.grid(log10_mass=mean(milk$log10_mass),
                        neocortex.perc=milk$neocortex.perc)
new_data <- mutate(new_data, predicted=predict(m3, newdata = new_data))
p1 <- ggplot(new_data, aes(milk$log10_mass, predicted)) + geom_point()

## Here is plot of kcal.per.g corrected for variation in log10_mass, plotted against neocortex.perc
new_data <- expand.grid(log10_mass=milk$log10_mass,
                        neocortex.perc=mean(milk$neocortex.perc))
new_data <- mutate(new_data, predicted=predict(m3, newdata = new_data))
p2 <- ggplot(new_data, aes(milk$neocortex.perc, predicted)) + geom_point()

plot_grid(p1, p2, labels=c("A", "B"), ncol = 2, nrow = 1)


## scaled explanatory variables
milk <- mutate(milk,
               s_log10_mass=scale(log10_mass),
               s_neocortex.perc=scale(neocortex.perc))
m3 <- lm(kcal.per.g ~ s_log10_mass + s_neocortex.perc, data=milk)
summary(m3)

