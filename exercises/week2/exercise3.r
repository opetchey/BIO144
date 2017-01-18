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

## get the average and standard deviation of child mortality for each continent
group_by(fhc1, continent) %>%
  summarise(mean=mean(child_mort),
            sd=sd(child_mort))

## make a box and whisker plot of child mortality by continent
qplot(x=continent, y=child_mort, data=fhc1, geom="boxplot")

## plot the relationship between health care expenditure and child mortality
qplot(x=health_exp_total, y=child_mort, data=fhc1) 

## plot the distribution of child mortality
qplot(child_mort, data=fhc1)
## plot the distribution of health care expenditure
qplot(health_exp_total, data=fhc1)

## Look to see if log transformation help the situation
qplot(x=log10(health_exp_total), y=log10(child_mort), data=fhc1)
qplot(log10(child_mort), data=fhc1, bins=30)
qplot(log10(health_exp_total), data=fhc1)




