## clear R's brain
rm(list=ls())

## load some useful packages
library(readr)
library(tidyverse)
library(ggfortify)

## load the data
## Here I'm loading it direct from online where the dataset are stored
fhc <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144/master/datasets/financing_healthcare.csv")

## tell me which countries are in the dataset
unique(fhc$country)

## tell me which years are in the dataset
unique(fhc$year)

## give me the rows in which both child mortality and health care expenditure are not NA
filter(fhc, !is.na(child_mort) & !is.na(health_exp_total))

## Wrange the data
fhc1 <- fhc %>%
  filter(year==2013) %>% ## only data fro 2013 please
  select(year:continent, health_exp_total, child_mort, life_expectancy) %>% ## only these columns please
  drop_na() ## drop rows with any NAs
 
## plot the relationship between health care expenditure and child mortality
qplot(x=health_exp_total, y=child_mort, data=fhc1) 

## plot the relationship between health care expenditure and child mortality
ggplot(data=fhc1, aes(x=health_exp_total, y=child_mort)) + geom_point()

## plot the distribution of child mortality
ggplot(fhc1, aes(x=child_mort)) + geom_histogram()
## plot the distribution of health care expenditure
ggplot(fhc1, aes(x=health_exp_total)) + geom_histogram()

## Look to see if log transformation help the situation
ggplot(data=fhc1, aes(x=log10(health_exp_total), y=log10(child_mort))) + geom_point()
ggplot(fhc1, aes(x=log10(child_mort))) + geom_histogram(bins=20)
ggplot(fhc1, aes(x=log10(health_exp_total))) + geom_histogram(bins=20)


## create new log transformed variables
fhc1 <- mutate(fhc1,
               log10_health_exp_total=log10(health_exp_total),
               log10_child_mort=log10(child_mort))

## fit the linear model of the log transformed data and assign it to object named m1
m1 <- lm(log10_child_mort ~ log10_health_exp_total, data=fhc1)

## First the residuals versus the predicted values
qplot(fitted(m1), residuals(m1))

## And not the distribution of the residuals
qplot(residuals(m1))


## Do this with the raw, non log transformed data, to see just how bad it is.
## fit the linear model and assign it to object named m1
m2 <- lm(child_mort ~ health_exp_total, data=fhc1)
qplot(fitted(m2), residuals(m2))
qplot(residuals(m2))


## look that the model terms
summary(m1)


## make a nice graph with regression line
qplot(x=log10_health_exp_total, y=log10_child_mort, data=fhc1) +
  geom_smooth(method="lm") +
  theme_bw() +
  xlab("Log10 Per capita total health care spend (dollar)") +
  ylab("Log10 Child mortality\n(number of children per 1000 dying before age 5)")


## Or by making the axes have log scales...
qplot(x=health_exp_total, y=child_mort, data=fhc1, log="xy") +
  geom_smooth(method="lm") +
  theme_bw() +
  xlab("Per capita total health care spend (dollar)") +
  ylab("Child mortality\n(number of children per 1000 dying before age 5)")


## make a graph with regression line on the raw axes
qplot(x=health_exp_total, y=child_mort, data=fhc1) +
  geom_line(aes(y=10^predict(m1)), col="red")


## Make the graph beautiful
qplot(x=health_exp_total, y=child_mort, data=fhc1) +
  geom_line(aes(y=10^predict(m1)), col="red") +
  theme_bw() +
  xlab("Per capita total health care spend (dollar)") +
  ylab("Child mortality\n(number of children per 1000 dying before age 5)")


