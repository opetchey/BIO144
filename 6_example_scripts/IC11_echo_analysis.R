## clear R
rm(list=ls())

## load the required libraries
library(tidyverse)
library(ggfortify)
library(janitor) ## you may not have this, so get it if you need to
library(MuMIn)

## read in the data
dd <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/echocardiogram.data.csv",
               col_names=T, na="?")


## check one of the relationships
ggplot(dd, aes(x=lvdd, y=alive_at_1)) +
  geom_point(position=position_jitter(height=0.1, width=0),
             alpha=0.5) +
  stat_smooth(method="glm", method.args = list(family = "binomial"))

## and another
ggplot(dd, aes(x=fractional_shortening, y=alive_at_1)) +
  geom_point(position=position_jitter(height=0.1, width=0),
             alpha=0.5) +
  stat_smooth(method="glm", method.args = list(family = "binomial"))

## and lets use dredge to get the models that best predict alive_at_1
## first make data without nas
dd_no_nas <- na.omit(dd)
## Then specify the full model
full_mod <- glm(alive_at_1 ~ pericardial_effusion +
                 fractional_shortening +
                 epss +
                 lvdd +
                 wall_motion_index,
               data = dd_no_nas,
               family = "binomial",
               na.action = "na.fail")
## there we have said that if there are any NAs, please create and error and stop
## we already checked there are no NAs, but this double safety check is still very wise

## do the dredge
dredge_out <- dredge(full_mod)

## get and look at the best model
best_dredge <- get.models(dredge_out, subset = delta==0)[[1]]
anova(best_dredge, test = "LRT")
## the model with lowest AIC has only wall motion score as a variable

## look at the models that are with five AIC units of the best
s1_subset <- get.models(dredge_out, subset = delta < 5)
model.sel(s1_subset)
## diameter is included in 9 of these models.
## I think dt3 is the second best! and it has one less parameter

## there are no other variables, so we don't need to hold them
## constant when making a graph
ggplot(dd, aes(x=wall_motion_index, y=alive_at_1)) +
  geom_point(position=position_jitter(height=0.1, width=0),
             alpha=0.5) +
  stat_smooth(method="glm", method.args = list(family = "binomial"))
## not really so impressive!

## and I leave the results sentence up to you... post your ideas in the
## Discussion Forum
