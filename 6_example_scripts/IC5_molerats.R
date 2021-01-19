rm(list=ls())

library(tidyverse)
library(ggfortify)


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


## read in the data
dd <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/MoleRatLayabouts.csv")


## Look at the data
## how many and what variables
## how many observations
## how many of each caste

table(dd$caste)

## check distributions
ggplot(dd, aes(x=Mass)) + geom_histogram(bins=10)
ggplot(dd, aes(x=Energy)) + geom_histogram(bins=10)

## log transform the variables
dd <- mutate(dd, log10_Mass=log10(Mass),
             log10_Energy=log10(Energy))

## Check distributions of log transformed variables
ggplot(dd, aes(x=log10_Mass)) + geom_histogram(bins=10)
ggplot(dd, aes(x=log10_Energy)) + geom_histogram(bins=10)

## bivariate plot
ggplot(dd, aes(x=log10_Mass, y=log10_Energy)) + geom_point()

## coloured by caste
ggplot(dd, aes(x=log10_Mass, y=log10_Energy, colour=caste)) + geom_point()

## figure out the degrees of freedom for error for four different models?
## be sure of the meaning of the four models, intercepts slopes

## do the full model:
m1 <- lm(log10_Energy ~ caste * log10_Mass, dd)
## check model assumptions are reasonably well me
autoplot(m1)
## get the summary table
summary(m1)
## and the anova table
anova(m1)

## without the interaction
m2 <- lm(log10_Energy ~ caste + log10_Mass, dd)
## check model assumptions are reasonably well me
autoplot(m2)
## get the summary table
summary(m2)
## and the anova table
anova(m2)

## what difference in R2?

## in the full model, what is the estimate intercept for the lazy caste
## and the worker caste

## in the full model, what is the estimate slope for the lazy caste
## and the worker caste


## What would have happened if we didn't take into account Mass?
ggplot(dd, aes(x=caste, y=log10_Energy)) + geom_point()
## make a model without mass
m3 <- lm(log10_Energy ~ caste, dd)
## check model assumptions
autoplot(m3)
## get the summary table
summary(m3)

0.4278-0.409

## Here is what we get if we use geom_smooth
## ... the fitted lines have different slopes
## though this is not supported by the statistics
ggplot(dd, aes(x=log10_Mass, y=log10_Energy, colour=caste)) +
  geom_point() +
  geom_smooth(method="lm", formula = y ~ x)

## Here we can make a graph (using predict with the model without the interaction)
## where the lines have the same slope
## make new data to plot over.
new_data <- expand.grid(log10_Mass=seq(min(dd$log10_Mass), max(dd$log10_Mass), length=100),
                        caste=unique(dd$caste))
## make predictions with that data and model m2 (no interaction)
p1 <- predict(m2, newdata = new_data, interval = "confidence")
## combine the predicted and new data to make plotting easier
n1 <- cbind(p1, new_data)
## And make the graph
ggplot(dd, aes(x=log10_Mass, y=log10_Energy, colour=caste)) +
  geom_point() +
  geom_smooth(data=n1, mapping=aes(y=fit, ymin=lwr, ymax=upr), stat="identity")
