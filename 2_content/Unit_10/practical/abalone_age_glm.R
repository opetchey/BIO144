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
full_glm <- glm(Rings ~ Sex + Length_mm + Diameter_mm + Height_mm + Whole_weight_g +
                 Shuck_weight_g + Viscera_weight_g + Shell_weight_g, data=dd,
                family=poisson)


summary(full_glm)
anova(full_glm)

dropterm(full_glm, sorted = T)
## remove length_mm
glm1 <- update(full_glm, . ~ . - Length_mm)

dropterm(glm1, sorted = T)
## remove length_mm
glm2 <- update(glm1, . ~ . - Shell_weight_g)

dropterm(glm2, sorted = T)
## remove length_mm
glm3 <- update(glm2, . ~ . - Height_mm)

dropterm(glm3, sorted = T)
## remove length_mm
glm4 <- update(glm3, . ~ . - Sex)

dropterm(glm4, sorted = T)
## remove length_mm
glm5 <- update(glm4, . ~ . - Sex)

dropterm(glm5, sorted = T)
## remove length_mm
glm6 <- update(glm5, . ~ . - Viscera_weight_g)

dropterm(glm6, sorted = T)



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





