## clear R's brain
rm(list=ls())
 
## The part relevant to IC4 starts at line 74 (but you must still run the code between here and there)

## load some useful packages
library(readr)
library(tidyverse)
library(ggfortify)

## load the data
## Here I'm loading it direct from online where the dataset are stored
fhc <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/financing_healthcare.csv")
## this produces some warnings. They concern a column we are not interested in (health_exp_public)
## and also are about the first column, which has no header.


## tell me which countries are in the dataset
unique(fhc$country)

## tell me which years are in the dataset
unique(fhc$year)

## give me the rows in which both child mortality and health care expenditure are not NA
filter(fhc, !is.na(child_mort) & !is.na(health_exp_total))

## Wrangle the data
fhc1 <- fhc %>%
  filter(year==2013) %>% ## only data fro 2013 please
  select(year:continent, health_exp_total, child_mort) %>% ## only these columns please
  drop_na() ## drop rows with any NAs

## here is the answer to the first question... the number of rows in this dataset.

## plot the relationship between health care expenditure and child mortality
ggplot(data=fhc1, aes(x=health_exp_total, y=child_mort)) + geom_point()

## plot the distribution of child mortality
ggplot(fhc1, aes(x=child_mort)) + geom_histogram()
## plot the distribution of health care expenditure
ggplot(fhc1, aes(x=health_exp_total)) + geom_histogram()

## Look to see if log transformation helps the situation
fhc1 <- fhc1 %>%
  mutate(log10_health_exp_total = log10(health_exp_total),
         log10_child_mort = log10(child_mort))

ggplot(data=fhc1, aes(x=log10_health_exp_total, y=log10_child_mort)) +
  geom_point() +
  ylim(0,5) +
  xlim(0,5)
ggplot(fhc1, aes(x=log10_child_mort)) + geom_histogram(bins=20)
ggplot(fhc1, aes(x=log10_health_exp_total)) + geom_histogram(bins=20)


## fit the linear model of the log transformed data and assign it to object named m1
m1 <- lm(log10_child_mort ~ log10_health_exp_total, data=fhc1)

## First the residuals versus the predicted values
ggplot(mapping=aes(x=fitted(m1), y=residuals(m1))) + geom_point()

## And now the distribution of the residuals
ggplot(mapping=aes(residuals(m1))) + geom_histogram()
## That was the answer to question 14



## Do this with the raw, non log transformed data, to see just how bad it is.
## fit the linear model and assign it to object named m1
m2 <- lm(child_mort ~ health_exp_total, data=fhc1)
ggplot(mapping=aes(x=fitted(m2), y=residuals(m2))) + geom_point()
ggplot(mapping=aes(residuals(m2))) + geom_histogram()


## Here is where we get the estimated intercept and slope:
summary(m1)


## make a nice graph with regression line
ggplot(data=fhc1, aes(x=log10_health_exp_total, y=log10_child_mort)) + geom_point() +
  geom_smooth(method="lm") +
  theme_bw() +
  xlab("Log10 Per capita total health care spend (dollar)") +
  ylab("Log10 Child mortality\n(number of children per 1000 dying before age 5)")



## make a graph with regression line on the raw axes
ggplot(data=fhc1, aes(x=health_exp_total, y=child_mort)) +
  geom_point() +
  geom_line(aes(y=10^predict(m1)), col="red") +
  theme_bw() +
  xlab("Log10 Per capita total health care spend (dollar)") +
  ylab("Log10 Child mortality\n(number of children per 1000 dying before age 5)")


