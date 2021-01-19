## clear R
rm(list=ls())

## Load required libraries
library(tidyverse)
library(MASS)
library(ggfortify)
library(MuMIn)
library(patchwork) ## for the final plot

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

summary(glm6)

## lets see how different are the predictions of the glm
## with those of the lm we made in IC8

## make the linear (gaussian) model here
lm3 <- lm(Rings ~ Sex + Diameter_mm + Whole_weight_g +
            Shuck_weight_g + Viscera_weight_g, dd)
ggplot(mapping=aes(x=fitted(glm6), y=fitted(lm3))) + geom_point() +
  xlab("Predictions of glm") +
  ylab("Predictions of lm") +
  geom_abline(intercept=0, slope=1)
cor(cbind(x=fitted(glm6), y=fitted(lm3)))
## very strong correlation

