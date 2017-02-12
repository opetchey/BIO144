## clear R
rm(list=ls())

## laod the required libraries
library(tidyverse)
library(GGally)
library(ggfortify)


## read in the data
bodyfat <- read_delim("https://raw.githubusercontent.com/opetchey/BIO144/master/datasets/bodyfat.txt", 
                      delim="\t", escape_double = FALSE, trim_ws = TRUE)

## histogram of the bodyfat measurements
ggplot(data=bodyfat, aes(x=bodyfat)) + geom_histogram(bins=20)

## plot distributions of each of the variables
## this uses the gather function to wrangle the data into tidy format, for easy plotting by qplot
ggplot(data=gather(bodyfat, key=variable, value=value),
       aes(x=value)) +
  geom_histogram(bins=20) +
  facet_wrap(~variable, scales="free")

## look at the graphs of variables plotted against each other,
## to get an idea of which variables might best predict body fat.
ggpairs(bodyfat)


bodyfat <- filter(bodyfat, weight<300)

ggplot(bodyfat, aes(weight, bodyfat)) + geom_point()
m0 <- lm(bodyfat ~ 1, bodyfat)
m1 <- lm(bodyfat ~ weight, data=bodyfat)
autoplot(m1)
summary(m1)
## 38%

ggplot(bodyfat, aes(weight, abdomen)) + geom_point()
m2 <- lm(bodyfat ~ abdomen, data=bodyfat)
autoplot(m2)
summary(m2)
## 68%

m3 <- lm(bodyfat ~ abdomen + weight, data=bodyfat)
autoplot(m3)
summary(m3)
## 72%

## So the whole is 72%.
## common is 34%
## abd is 34
## weight is 4

summary(m0)
anova(m0)
anova(m1, m3) ## +- abdomen; strong effect 
anova(m2, m3) ## +- weight, very significant, though little explanatory power



