rm(list=ls())

library(tidyverse)
library(ggfortify)

dd <- read_csv("~/Desktop/MoleRatLayabouts.csv")

# The dataset you'll look at is about naked mole rats, and what influences how much energy the expend
# Naked mole rats are particularly interesting animals, because they are the only known mammals
# with distinct social castes (well, apart from humans perhaps). Within the worker caste there
# seem to be two castes: hard workers, and lazy individuals. Researchers were interested
# in physiological difference between the hard workers and the lazy individuals, so 
# measured their energy expenditure, and their body mass. Mass was measured because
# it was already known that the lazy individuals are bigger than the hard workers.
# So, the question was "after taking into account differences in mass, was there a difference
# in energy expenditure?".
# 
# By the way, this example comes from the book "The analysis of biological data", by Whitlock and Schluter.
# 
# Get the dataset and do the usual preliminaries in RStudio.

## What is the question?
## Does the relationship differ between castes (intercept and slope)?


## Look at the data
## how many and what variables
## how many observations
## how many of each caste

table(dd$caste)

## distributions
ggplot(dd, aes(x=Mass)) + geom_histogram(bins=10)
ggplot(dd, aes(x=Energy)) + geom_histogram(bins=10)

## log transform
dd <- mutate(dd, log10_Mass=log10(Mass),
             log10_Energy=log10(Energy))

## Check distributions
ggplot(dd, aes(x=log10_Mass)) + geom_histogram(bins=10)
ggplot(dd, aes(x=log10_Energy)) + geom_histogram(bins=10)

## bivariate plot
ggplot(dd, aes(x=log10_Mass, y=log10_Energy)) + geom_point()
## coloured
ggplot(dd, aes(x=log10_Mass, y=log10_Energy, colour=caste)) + geom_point()

## degrees of freedom for error for four different models?
## meaning of the four models, intercepts slopes

## do the full model:
m1 <- lm(log10_Energy ~ caste * log10_Mass, dd)
autoplot(m1)
summary(m1)
anova(m1)

## without the interaction
m2 <- lm(log10_Energy ~ caste + log10_Mass, dd)
autoplot(m2)
summary(m2)
anova(m2)

## what difference in R2?

## in the full model, what is the estimate intercept for the lazy caste
## and the worker caste

## in the full model, what is the estimate slope for the lazy caste
## and the worker caste




## What would have happened if we didn't take into account Mass?
ggplot(dd, aes(x=caste, y=log10_Energy)) + geom_point()
m3 <- lm(log10_Energy ~ caste, dd)
autoplot(m3)
summary(m3)

0.4278-0.409


ggplot(dd, aes(x=log10_Mass, y=log10_Energy, colour=caste)) +
  geom_point() +
  geom_smooth(method="lm", formula=y ~ x)

new_data <- expand.grid(log10_Mass=seq(min(dd$log10_Mass), max(dd$log10_Mass), length=100),
                        caste=unique(dd$caste))
p1 <- predict(m2, newdata = new_data, interval = "confidence")
n1 <- cbind(p1, new_data)

ggplot(dd, aes(x=log10_Mass, y=log10_Energy, colour=caste)) +
  geom_point() +
  geom_smooth(data=n1, mapping=aes(y=fit, ymin=lwr, ymax=upr), stat="identity")
