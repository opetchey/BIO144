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
 
## From previous work we know we need to log transform the data
fhc1 <- mutate(fhc1,
               log10_health_exp_total=log10(health_exp_total),
               log10_child_mort=log10(child_mort))

## plot the relationship between health care expenditure and child mortality
qplot(x=log10_health_exp_total, y=log10_child_mort, data=fhc1)

## fit the linear model of the log transformed data and assign it to object named m1
m1 <- lm(log10_child_mort ~ log10_health_exp_total, data=fhc1)

## The diagnostic plots we've so far looked at
autoplot(m1, smooth.colour = NA, which=c(1,2))


## Lets have a closer look at the predict function
## 1. We can ask it to also give us confidence or prediction intervals
p1 <- predict(m1, interval = "confidence")
p1 <- predict(m1, interval = "prediction")
## Which both of these we get back a data frame with three columns.
## The predicted value, the lower bound of the interval (lwr) and the upper bound (upr)

## 2. The second thing to look at is how predict can predict for new values of the explanatory variable
## First we make the new values, they must be in a data frame with correctly named variable:
new_data <- data.frame(log10_health_exp_total=seq(1,4,0.01))
p_conf <- predict(m1, newdata = new_data,
              interval="confidence")
p_pred <- predict(m1, newdata = new_data,
              interval="prediction")
## it is convenient to put these together:
p_conf <- cbind(new_data, p_conf)
p_pred <- cbind(new_data, p_pred)
## also, we get some future value from renaming the fit variable
p_conf <- rename(p_conf, log10_child_mort=fit)
p_pred <- rename(p_pred, log10_child_mort=fit)

## Super, this is really important to be able to do: get predictions of a model using predict
qplot(x=log10_health_exp_total, y=log10_child_mort, data=fhc1) +
  geom_smooth(data = p_conf, aes(ymin = lwr, ymax = upr), stat = 'identity') +
  geom_smooth(data = p_pred, aes(ymin = lwr, ymax = upr), stat = 'identity') +
  theme_bw() +
  xlab("Log10 Per capita total health care spend (dollar)") +
  ylab("Log10 Child mortality\n(number of children per 1000 dying before age 5)")


## make a graph with regression line on the raw axes
qplot(x=health_exp_total, y=child_mort, data=fhc1) +
  geom_smooth(data = p_conf,
              aes(x=10^log10_health_exp_total, y=10^log10_child_mort,
                  ymin = 10^lwr, ymax = 10^upr), stat = 'identity') +
  geom_smooth(data = p_pred,
              aes(x=10^log10_health_exp_total, y=10^log10_child_mort,
                  ymin = 10^lwr, ymax = 10^upr), stat = 'identity') +
  theme_bw() +
  xlab("Per capita total health care spend (dollar)") +
  ylab("Child mortality\n(number of children per 1000 dying before age 5)") +
  coord_cartesian(xlim = c(0,7500), ylim = c(0,200))
  

