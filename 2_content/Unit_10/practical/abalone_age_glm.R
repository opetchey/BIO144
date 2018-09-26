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
summary(full_mod)
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


## qqplot not great, and some increase in residuals with fitted...
## actually, we could have anticipated this, for a particular reason that
## we'll discuss later in the course.
## Then we'll attempt to correct the situation.


mods <- list(full_mod=full_mod, m1=m1, m2=m2, m3=m3, m4=m4, m5=m5)
aictab(mods)



## Use step AIC
m0 <- glm(Rings ~ 1, data=dd, family=poisson)
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
                  fitfunction = "glm", family=poisson)

print(multi1)
plot(multi1, type="s")
plot(multi1, type="p")
plot(multi1, type="w")

coef(multi1)

predict(multi1)



