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

Predicting the age of abalone from physical measurements.  The age of
abalone is determined by cutting the shell through the cone, staining it,
and counting the number of rings through a microscope -- a boring and
time-consuming task.  Other measurements, which are easier to obtain, are
used to predict the age.  Further information, such as weather patterns
and location (hence food availability) may be required to solve the problem.

From the original data examples with missing values were removed (the
                                                                  majority having the predicted value missing), and the ranges of the
continuous values have been scaled for use with an ANN (by dividing by 200).

Data comes from an original (non-machine-learning) study:
  
  Warwick J Nash, Tracy L Sellers, Simon R Talbot, Andrew J Cawthorn and
Wes B Ford (1994) "The Population Biology of Abalone (_Haliotis_
species) in Tasmania. I. Blacklip Abalone (_H. rubra_) from the North
Coast and Islands of Bass Strait", Sea Fisheries Division, Technical
Report No. 48 (ISSN 1034-3288)



## load the data
#dd_all <- read_csv("~/Desktop/abalone.data_fulldata_dont_use.csv")
#View(dd)
## remove some of the data to make it more interesting!
#set.seed(1)
#dd <- slice(dd_all, sample(1:4177, 400))
#write.csv(dd, "~/Desktop/abalone_age.csv", row.names = F)

dd <- read_csv("~/Desktop/abalone_age.csv")



## The response variable is Rings, which is age -1.5 years apparently
ggplot(dd, aes(x=Rings)) + geom_histogram(bins=15)
## not too bad, lets leave it like this

## the full model with no interactions
full_mod <- lm(Rings ~ Sex + Length_mm + Diameter_mm + Height_mm + Whole_weight_g +
                 Shuck_weight_g + Viscera_weight_g + Shell_weight_g, dd)
summary(full_mod)
## remove length_mm
m1 <- lm(Rings ~ Sex + Diameter_mm + Height_mm + Whole_weight_g +
                 Shuck_weight_g + Viscera_weight_g + Shell_weight_g, dd)
summary(m1)
## remove shell weight
m2 <- lm(Rings ~ Sex + Diameter_mm + Height_mm + Whole_weight_g +
           Shuck_weight_g + Viscera_weight_g, dd)
summary(m2)
## remove height
m3 <- lm(Rings ~ Sex + Diameter_mm + Whole_weight_g +
           Shuck_weight_g + Viscera_weight_g, dd)
summary(m3)
autoplot(m3)

ggplot(mapping=aes(x=fitted(m3), y=dd$Rings)) + geom_point()
cor(cbind(x=fitted(m3), y=dd$Rings))

## qqplot not great, and some increase in residuals with fitted...
## actually, we could have anticipated this, for a particular reason that
## we'll discuss later in the course.
## Then we'll attempt to correct the situation.


mods <- list(full_mod=full_mod, m1=m1, m2=m2, m3=m3)
aictab(mods)



## Should do this at the beginning really!
pairs(dplyr::select(dd, -Sex))


## Use step AIC
m0 <- lm(Rings ~ 1, dd)
s1 <- stepAIC(full_mod, direction = "backward", AICc=TRUE)

s2 <- stepAIC(m0, direction = "forward", AICc=TRUE,
              scope=list(lower=m0, upper=full_mod))

s3 <- stepAIC(m0, direction = "both", AICc=TRUE,
              scope=list(lower=m0, upper=m1))

aictab(list(m3=m3, s1=s1, s2=s2, s3=s3))
s1

## package glmulti model selection:: the death star!

multi1 <- glmulti(Rings ~ Sex + Length_mm + Diameter_mm + Height_mm + Whole_weight_g +
                    Shuck_weight_g + Viscera_weight_g + Shell_weight_g, data=dd,
                  level = 1,
                  method = "h",
                  crit = "aicc",
                  confsetsize = 10,
                  plotty=F, report=F,
                  fitfunction = "lm")

print(multi1)
plot(multi1, type="s")
plot(multi1, type="p")
plot(multi1, type="w")

coef(multi1)

predict(multi1)



