rm(list=ls())

library(readr)
library(tidyverse)

fhc <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144/master/datasets/financing_healthcare.csv")

unique(fhc$country)
unique(fhc$year)

filter(fhc, !is.na(child_mort) & !is.na(health_exp_total))

fhc1 <- fhc %>%
  filter(year==2013) %>%
  select(year:continent, health_exp_total, child_mort, life_expectancy) %>% 
  drop_na() %>% 
  group_by(country)

qplot(x=health_exp_total, y=child_mort, data=fhc1) 

qplot(child_mort, data=fhc1)
qplot(health_exp_total, data=fhc1)

qplot(x=log10(health_exp_total), y=log10(child_mort), data=fhc1, xlim=c(0, 4), ylim=c(0, 4))


