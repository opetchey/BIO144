## clear R
rm(list=ls())

## Load required libraries
library(tidyverse)
#library(AICcmodavg)
#library(MASS)
#library(glmulti)
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
dropterm(full_mod, sorted = T)
dt1 <- update(full_mod, . ~ . - Length_mm)

summary(full_mod)
summary(dt1)

dropterm(dt1, sorted = T)
dt2 <- update(dt1, . ~ . - Shell_weight_g)

dropterm(dt2, sorted = T)
dt3 <- update(dt2, . ~ . - Height_mm)

dropterm(dt3, sorted = T)

autoplot(dt3)

ggplot(mapping=aes(x=fitted(dt2), y=dd$Rings)) + geom_point()
cor(cbind(x=fitted(dt3), y=dd$Rings))

## qqplot not great, and some increase in residuals with fitted...
## actually, we could have anticipated this, for a particular reason that
## we'll discuss later in the course.
## Then we'll attempt to correct the situation.


mods <- list(full_mod=full_mod, dt1=dt1, dt2=dt2, dt3=dt3)
model.sel(mods)


## Should do this at the beginning really!
pairs(dplyr::select(dd, -Sex))


## Use step AIC
m0 <- lm(Rings ~ 1, dd)
s1 <- stepAIC(full_mod, direction = "backward", AICc=TRUE)

s2 <- stepAIC(m0, direction = "forward", AICc=TRUE,
              scope=list(lower=m0, upper=full_mod))

s3 <- stepAIC(m0, direction = "both", AICc=TRUE,
              scope=list(lower=m0, upper=full_mod))

model.sel(list(dt3=dt3, s1=s1, s2=s2, s3=s3))
s1

## dredge

dredge_out <- dredge(full_mod)
best_dredge <- get.models(dredge_out, subset = delta==0)[[1]]
anova(best_dredge)


s1_subset <- get.models(dredge_out, subset = delta < 5)
mod_ave <- model.avg(s1_subset)
summary(mod_ave)


