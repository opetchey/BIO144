## clear R's brain
rm(list=ls())

## load some useful packages
library(readr)
library(tidyverse)
library(ggfortify)

## load the data
## Here I'm loading it direct from online where the dataset are stored
fhc <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/financing_healthcare.csv")

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
ggplot(fhc1, aes(x=continent, y=child_mort)) + geom_boxplot()


## Exercise 3, some visualisation practice


## plot the relationship between health care expenditure and child mortality
ggplot(data=fhc1, mapping=aes(x=health_exp_total, y=child_mort, col=continent)) +
  geom_point()
## is the same as
ggplot(data=fhc1) +
  geom_point(mapping=aes(x=health_exp_total, y=child_mort, col=continent))

## or with each continent in a separate graph
ggplot(data=fhc1) +
  geom_point(mapping=aes(x=health_exp_total, y=child_mort)) +
  facet_wrap(~continent)

## or with each continent in a separate graph with free axes scales
ggplot(data=fhc1) +
  geom_point(mapping=aes(x=health_exp_total, y=child_mort)) +
  facet_wrap(~continent, scales="free")





## plot the distribution of child mortality
ggplot(fhc1, aes(x=child_mort)) + geom_histogram()
## plot the distribution of health care expenditure
ggplot(fhc1, aes(x=health_exp_total)) + geom_histogram()

## Look to see if log transformation help the situation
ggplot(data=fhc1, aes(x=log10(health_exp_total), y=log10(child_mort))) + geom_point()
ggplot(fhc1, aes(x=log10(child_mort))) + geom_histogram(bins=20)
ggplot(fhc1, aes(x=log10(health_exp_total))) + geom_histogram(bins=20)




