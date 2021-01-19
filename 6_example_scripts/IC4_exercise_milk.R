## Clear R
rm(list=ls())

## load some libraries
library(rethinking)
library(tidyverse)
library(ggfortify)
library(cowplot)

## load the dataset
## Here we get it from online, so I don't have to fiddle with the path/working directory
milk <- read_csv("https://github.com/opetchey/BIO144/raw/master/3_datasets/milk_rethinking.csv")

## How many observations:
nrow(milk)

## Check for NAs, if there are none, then this should not affect number of observations
milk <- na.omit(milk)
nrow(milk)
## still 17, so no NAs in any variables in any rows.

## Look at the distribution of the three variables of interest
milk %>%
  ggplot() +
  geom_histogram(aes(x = kcal.per.g), bins=6)
milk %>%
  ggplot() +
  geom_histogram(aes(x = mass), bins=6)
milk %>%
  ggplot() +
  geom_histogram(aes(x = neocortex.perc), bins=6)

## or the quick way
milk %>%
  gather(key=variable, value=value, kcal.per.g, mass, neocortex.perc) %>%
  ggplot() +
  geom_histogram(aes(x = value), bins=6) +
  facet_wrap(~variable, scales = "free", nrow = 3)

## the response variable, kcal.per.g, and the mass variable seem a bit right skewed
## so we can try log transformation
milk <- mutate(milk,
               log10_kcal.per.g = log10(kcal.per.g),
               log10_mass=log10(mass))

## not that much better...
milk %>%
  gather(key=variable, value=value, log10_kcal.per.g, log10_mass, neocortex.perc) %>%
  ggplot() +
  geom_histogram(aes(x = value), bins=6) +
  facet_wrap(~variable, scales = "free", nrow = 3)

## let us continue with the non-transformed variables

## look at the bivariate scatter plots
ggplot(milk, aes(neocortex.perc, kcal.per.g)) + geom_point()
ggplot(milk, aes(mass, kcal.per.g)) + geom_point()
## Not much going on here

## and are the explanatory variables correlated?
ggplot(milk, aes(neocortex.perc, log10_mass)) + geom_point()
## Yes, they are positively correlated.

## degrees of freedom for regression with one explanatory variable should be 15
## (one intercept and one slope estimated)
## degrees of freedom for regression with two explanatory variable should be 14
## (one intercept and two slopes estimated)

## one of the regressions
m1 <- lm(kcal.per.g ~ neocortex.perc, data=milk)
autoplot(m1, smooth.colour = NA) ## pretty bad qqplot!
summary(m1)
## nothing significant, supporting our eyeball
## r2 0.024

## the other of the regressions
m2 <- lm(kcal.per.g ~ mass, data=milk)
autoplot(m2, smooth.colour = NA) ## better qqplot
summary(m2)
## nothing significant, supporting our eyeball
## r2 0.13

## multiple regression
m3 <- lm(kcal.per.g ~ mass + neocortex.perc, data=milk)
autoplot(m3) ## pretty bad qqplot, though few data points
summary(m3)
#anova(m3)
## both variables significant (just)
## r2 0.35
## WOW!

## Make a graph of the modelled/predicted relationship of
## kcal.per.g against neocortex percent for mean mass
## I.e. this is the predicted relationships when mass is kept constant
## first make the new data to predict over
new_data <- expand.grid(neocortex.perc = seq(min(milk$neocortex.perc),
                                             max(milk$neocortex.perc),
                                             length=100),
                        mass=mean(milk$mass))
## Now predict using the model and that new data
p1 <- predict(m3, newdata=new_data, interval="confidence")
## some house keeping:
pred1 <- cbind(new_data, p1)
## and plot the relationship
pred1 %>%
  ggplot() +
  geom_ribbon(aes(x = neocortex.perc, ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line(aes(x = neocortex.perc, y = fit)) +
  ylab("Predicted kcal.per.g")


## And now for the other relationship:
## Make a graph of the modelled/predicted relationship of
## kcal.per.g against mass for mean neocortex percent
## first make the new data to predict over
new_data <- expand.grid(neocortex.perc = mean(milk$mass),
                        mass = seq(min(milk$mass),
                                   max(milk$mass),
                                   length=100))
## Now predict using the model and that new data
p1 <- predict(m3, newdata=new_data, interval="confidence")
## some house keeping:
pred1 <- cbind(new_data, p1)
## and plot the relationship
pred1 %>%
  ggplot() +
  geom_ribbon(aes(x = mass, ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line(aes(x = mass, y = fit)) +
  ylab("Predicted kcal.per.g")



