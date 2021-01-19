## clear R
rm(list=ls())

## Load required libraries
library(tidyverse)
library(MASS)
library(ggfortify)
library(MuMIn)
library(patchwork) ## for the final plot


## load the data
dd <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/abalone_age.csv")
## 400 observations

## we will use all the variables in the dataset.
## let us therefore create a version without NAs in any variables
dd <- na.omit(dd)
## still 400 observations, so we know there were no NAs

## The response variable is Rings, which is age -1.5 years apparently
ggplot(dd, aes(x=Rings)) + geom_histogram(bins=15)
## not too bad, lets leave it like this

## the full model with no interactions
full_mod <- lm(Rings ~ Sex + Length_mm + Diameter_mm + Height_mm + Whole_weight_g +
                 Shuck_weight_g + Viscera_weight_g + Shell_weight_g, dd)

## get the AIC of the full model
extractAIC(full_mod)

## get the AIC of the full model and of models without each of the individual variables:
dropterm(full_mod, sorted = T)

## for interest, we can look at some of the features of the full model
summary(full_mod)
## e.g explanatory pwoer is about 50%
anova(full_mod)
## and f-tests suggest strong relationships for many of the included variables

## remove the Length_mm variable from the full model
dt1 <- update(full_mod, . ~ . - Length_mm)

## get the the r-squared of this new model
summary(dt1)
## no difference from when Length_mm was included

## look at which variable we could next drop
dropterm(dt1, sorted = T)
## removing shell weight will decrease the AIC..
dt2 <- update(dt1, . ~ . - Shell_weight_g)

## look at which variable we could next drop
dropterm(dt2, sorted = T)
dt3 <- update(dt2, . ~ . - Height_mm)

## look at which variable we could next drop
dropterm(dt3, sorted = T)
## now, if we drop anything, there would be an increase in AIC of over 4 units 

## we can review the models we created by putting them in a list
mods <- list(full_mod=full_mod, dt1=dt1, dt2=dt2, dt3=dt3)
## and giving that to the model.sel function
model.sel(mods)
## we see that dt2 has the lowes AIC, and that dt3 has 0.12 units
## AIC greater


## model diagnostics
autoplot(dt3)
## qqplot not great, and some increase in residuals with fitted...
## actually, we could have anticipated this, for a particular reason that
## we'll discuss later in the course.
## Then we'll attempt to correct the situation.






## Use step AIC
m0 <- lm(Rings ~ 1, dd)
## backward stepwise selection
s1 <- stepAIC(full_mod, direction = "backward", AICc=TRUE)
## forward stepwise selection
s2 <- stepAIC(m0, direction = "forward", AICc=TRUE,
              scope=list(lower=m0, upper=full_mod))
## backwards and forwards stepwise selection
s3 <- stepAIC(m0, direction = "both", AICc=TRUE,
              scope=list(lower=m0, upper=full_mod))

model.sel(list(dt3=dt3, s1=s1, s2=s2, s3=s3))
## all quite close. only s2 (forward) is far off, but then only about 2 units

## and now we look at all possible models with the dredge function
## to use this we have to set the na.action argument we we make the full model
full_mod <- lm(Rings ~ Sex + Length_mm + Diameter_mm + Height_mm + Whole_weight_g +
               Shuck_weight_g + Viscera_weight_g + Shell_weight_g,
               data = dd,
               na.action = "na.fail")
## there we have said that if there are any NAs, please create and error and stop
## we already checked there are no NAs, but this double safety check is still very wise

## do the dredge
dredge_out <- dredge(full_mod)

## get and look at the best model
best_dredge <- get.models(dredge_out, subset = delta==0)[[1]]
anova(best_dredge)

## look at the models that are with five AIC units of the best
s1_subset <- get.models(dredge_out, subset = delta < 5)
model.sel(s1_subset)
## diameter is included in 9 of these models.
## I think dt3 is the second best! and it has one less parameter

## we could create a model average of this subset of models:
mod_ave <- model.avg(s1_subset)
summary(mod_ave)


## How to report the findings, focusing on practical importance?
## and to validate how well the model works
## this is an interesting and important graph --
## it shows the predicted versus the observed number of rings
p1 <- ggplot(mapping=aes(x=dd$Rings, y = fitted(dt3))) +
  geom_point() +
  geom_abline(intercept = 0, slope=1) +
  coord_fixed(ratio = 1) +
  xlab("Observed age\n(number of growth rings)") +
  ylab("Predicted age\n(number of growth rings)")
p2 <- ggplot(mapping = aes(x = dd$Rings - fitted(dt3))) +
  geom_histogram() +
  xlab("Error in predicted age")
p1 + p2 + plot_annotation(tag_levels = 'A')



