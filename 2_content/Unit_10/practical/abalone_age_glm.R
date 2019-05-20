## clear R
rm(list=ls())

## Load required libraries
library(tidyverse)
library(AICcmodavg)
library(MASS)
library(glmulti)
library(ggfortify)

## see file abalone.names.txt for description of dataset
## found at https://archive.ics.uci.edu/ml/datasets/Abalone

dd <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/abalone_age.csv")



## The response variable is Rings, which is age -1.5 years apparently
ggplot(dd, aes(x=Rings)) + geom_histogram(bins=15)
## not too bad, lets leave it like this

## the full model with no interactions
full_mod <- glm(Rings ~ Sex + Length_mm + Diameter_mm + Height_mm + Whole_weight_g +
                 Shuck_weight_g + Viscera_weight_g + Shell_weight_g, data=dd,
                family=poisson)

full_mod2 <- glm(Rings ~ Sex +  Diameter_mm + Height_mm + Whole_weight_g +
                  Shuck_weight_g + Viscera_weight_g + Shell_weight_g +Length_mm, data=dd,
                family=poisson)

summary(full_mod)
anova(full_mod)
anova(full_mod2)

## remove length_mm
m1 <- glm(Rings ~ Sex + Diameter_mm + Height_mm + Whole_weight_g +
                 Shuck_weight_g + Viscera_weight_g + Shell_weight_g, data=dd,
          family=poisson)
summary(m1)
## remove shell weight
m2 <- glm(Rings ~ Sex + Diameter_mm + Height_mm + Whole_weight_g +
           Shuck_weight_g + Viscera_weight_g, data=dd,
          family=poisson)
summary(m2)
## remove height
m3 <- glm(Rings ~ Sex + Diameter_mm + Whole_weight_g +
           Shuck_weight_g + Viscera_weight_g, data=dd,
          family=poisson)
summary(m3)
## at this point in the lm we stopped. All were significant at 0.05
## Viscera_weight_g -14.5064     4.1044  -3.534 0.000457 ***

m4 <- glm(Rings ~ Sex + Diameter_mm + Whole_weight_g +
            Shuck_weight_g, data=dd,
          family=poisson)
summary(m4)

m5 <- glm(Rings ~ Diameter_mm + Whole_weight_g +
            Shuck_weight_g, data=dd,
          family=poisson)
summary(m5)
autoplot(m5)

## for next line to work, need to do linear m3
lm3 <- lm(Rings ~ Sex + Diameter_mm + Whole_weight_g +
Shuck_weight_g + Viscera_weight_g, dd)
ggplot(mapping=aes(x=fitted(m5), y=fitted(lm3))) + geom_point() +
  xlab("Predictions of glm") +
  ylab("Predictions of lm") +
  geom_abline(intercept=0, slope=1)
cor(cbind(x=fitted(m5), y=fitted(m3)))


dropterm(full_mod, sorted=TRUE)
m1 <- update(full_mod, . ~ . - Length_mm)
AICc(m1)

dropterm(m1, sorted=TRUE)
m2 <- update(m1, . ~ . - Shell_weight_g)
AICc(m2)

dropterm(m2, sorted=TRUE)
m3 <- update(m2, . ~ . - Height_mm)
AICc(m3)

dropterm(m3, sorted=TRUE)
m4 <- update(m3, . ~ . - Sex)
AIC(m4)

dropterm(m4, sorted=TRUE)
m5 <- update(m4, . ~ . - Sex)
AIC(m5)

dropterm(m5, sorted=TRUE)
m6 <- update(m5, . ~ . - Viscera_weight_g)
AIC(m6)

dropterm(m6, sorted=TRUE)


summary(m6)
autoplot(m6)

## for next line to work, need to do linear m3
lm3 <- lm(Rings ~ Sex + Diameter_mm + Whole_weight_g +
Shuck_weight_g + Viscera_weight_g, dd)
ggplot(mapping=aes(x=fitted(m5), y=fitted(lm3))) + geom_point() +
  xlab("Predictions of glm") +
  ylab("Predictions of lm") +
  geom_abline(intercept=0, slope=1)
cor(cbind(x=fitted(m5), y=fitted(m3)))





