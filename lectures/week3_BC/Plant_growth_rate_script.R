## Owen Petchey's analysis of the plant growth data
## 11.7.14

## Question:
## What is the effect of soil moisture on plant growth rate
## Make a hypothesis: positive

## Data organised in excel, and checked.

## Clear R's memory
rm(list=ls())

library(tidyverse)
library(readr)
library(ggfortify)


## import the data
plant_gr <- read_csv("https://raw.githubusercontent.com/opetchey/getting_started_with_R/master/datasets/plant.growth.rate.csv")

      
## check the data is imported correctly
glimpse(plant_gr)

## plot the data
ggplot(plant_gr, aes(x = soil.moisture.content, y = plant.growth.rate)) +
  geom_point() 

## now for the statistical model
m1 <- lm(plant.growth.rate ~ soil.moisture.content, data=dd)

## check the assumptions of the model
autoplot(m1, smooth.colour = NA)

## look at the summary table
summary(m1)


## Results sentence: Soil moisture had a positive effect on plant growth
## (linear regression, slope=12.7, t=12.5, p < 0.001)

## make a nice graph for communication
ggplot(plant_gr, aes(x = soil.moisture.content, y = plant.growth.rate)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ylab("Plant Growth Rate (mm/week)") +
  theme_bw()












