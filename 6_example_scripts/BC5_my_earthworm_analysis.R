## Clear R
rm(list=ls())

## load libraries
library(tidyverse)
library(ggfortify)

## read the data (from where Owen put it online)
dd <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/earthworm.csv")

## Get the number of each type of earthworm
table(dd$Gattung)

## plot the distribution of weights
ggplot(dd, aes(x=Gewicht)) +
  geom_histogram()

## log transform the weight variable
dd <- mutate(dd, log10_Gewicht=log10(Gewicht))

## look at the distribution of the log transformed weight variables
ggplot(dd, aes(x=log10_Gewicht)) +
  geom_histogram(bins=20)

## check distribution of gut measurement
ggplot(dd, aes(x=Magenumf)) +
  geom_histogram(bins=20)

## look at the relationship between the two
ggplot(dd, aes(x=Magenumf, y=log10_Gewicht)) +
  geom_point()

## slope guess of 0.31

## make the linear model
m1 <- lm(log10_Gewicht ~ Magenumf, dd)

## check the model diagnostics
autoplot(m1)

## get the summary information table
summary(m1)

## make a lovely graph!
ggplot(dd, aes(x=Magenumf, y=log10_Gewicht)) +
  geom_point() +
  xlab("Gut circumference (mm)") +
  ylab("Worm weight (g)") +
  geom_smooth(method="lm")
  

## Below is some additional script, for looking at the different Gattung
## It is used once we get a bit further in the course, so I suggest
## you don't pay too much attention to it during Unit 5

## View the weights for each gattung
ggplot(dd, aes(x=Gattung, y=log10_Gewicht)) +
  geom_boxplot() +
  geom_point()

## guess the means
# L = 0.4
# N = -0.2
# Oc = -0.2
 
## and calculate the difference between the guesses
# N - L = -0.6
# Oc - L = -0.6

## make a model of weight with gattung as the explanatory variable
m2 <- lm(log10_Gewicht ~ Gattung, dd)

## model diagnostics
autoplot(m2)

## get the summary table, check the differences are close to what we guessed
summary(m2)

## look at the f statistic in the anova table
anova(m2)

## now we look at the relationship between weight
## and gut size, for each gattung
ggplot(dd, aes(x=Magenumf, y=log10_Gewicht, colour=Gattung)) +
  geom_point()

## and make a model with both gattung and gut size
## but no interaction between them (parallel lines)
m3 <- lm(log10_Gewicht ~ Magenumf + Gattung, dd)

## model diagnostics
autoplot(m3)

## summary table
summary(m3)

## and make a model with both gattung and gut size
## and interaction between them (parallel lines)
m4 <- lm(log10_Gewicht ~ Magenumf * Gattung, dd)
autoplot(m4)
summary(m4)

## And make a nice graph
ggplot(dd, aes(x=Magenumf, y=log10_Gewicht, colour=Gattung)) +
  geom_point() +
  xlab("Gut circumference (mm)") +
  ylab("Worm weight (g)") +
  geom_smooth(method="lm")
## Note that we did not see strong evidence of an interaction,
## so it may be best to plot the graph with lines with the
## same slope, so the graph shows the best supported model.

