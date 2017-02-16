## clear R
rm(list=ls())

## Load required libraries
library(tidyverse)
library(AICcmodavg)
library(MASS)
library(glmulti)

## Load the bodyfat data
dd <- read_delim("~/Desktop/bodyfat.txt", 
                 "\t", escape_double = FALSE, trim_ws = TRUE)

## Manual model selection
m1 <- lm(bodyfat ~ age + weight + height + neck + chest + abdomen + hip + thigh+
           knee + ankle + biceps + forearm + wrist, data=dd)
summary(m1)
m2 <- lm(bodyfat ~ age + weight + height + neck + chest + abdomen + hip + thigh+
           ankle + biceps + forearm + wrist, data=dd)
summary(m2)
m3 <- lm(bodyfat ~ age + weight + height + neck + abdomen + hip + thigh+
           ankle + biceps + forearm + wrist, data=dd)
summary(m3)
m4 <- lm(bodyfat ~ age + weight + neck + abdomen + hip + thigh+
           ankle + biceps + forearm + wrist, data=dd)
summary(m4)
m5 <- lm(bodyfat ~ age + weight + neck + abdomen + hip + thigh+
            biceps + forearm + wrist, data=dd)
summary(m5)
m6 <- lm(bodyfat ~ age + weight + neck + abdomen + hip + thigh+
           forearm + wrist, data=dd)
summary(m6)
m7 <- lm(bodyfat ~ age + weight + neck + abdomen + thigh+
           forearm + wrist, data=dd)
summary(m7)

## and without abdomen
m8 <- lm(bodyfat ~ age + weight + neck + thigh+
           forearm + wrist, data=dd)
summary(m8)

## and intercept only
## and without abdomen
m0 <- lm(bodyfat ~ 1, data=dd)
summary(m0)

## report the models
mods <- list(m1=m1, m2=m2, m3=m3, m4=m4, m5=m5, m6=m6, m7=m7, m8=m8, m0=m0)
aictab(mods)

## automated model selection 1
s1 <- stepAIC(m1, direction="backward", AICc=TRUE)
s2 <- stepAIC(m0, direction="forward", AICc=TRUE,
              scope=list(lower=m0, upper=m1))
s3 <- stepAIC(m0, direction="both", AICc=TRUE,
              scope=list(lower=m0, upper=m1))

mods_s <- list(s1=s1, s2=s2, s3=s3)
aictab(mods_s)


## package glmulti model selection:: the death star!

multi1 <- glmulti(bodyfat ~ age + weight + height + neck + chest + abdomen + hip + thigh+
                    knee + ankle + biceps + forearm + wrist, data=dd,
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



