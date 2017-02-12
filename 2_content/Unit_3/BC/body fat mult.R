## clear R
rm(list=ls())

## laod the required libraries
library(tidyverse)
library(GGally)
library(ggfortify)


## read in the data
bodyfat <- read_delim("https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/bodyfat.txt", 
                      delim="\t", escape_double = FALSE, trim_ws = TRUE)

## histogram of the bodyfat measurements
ggplot(data=bodyfat, aes(x=bodyfat)) + geom_histogram(bins=20)

## plot distributions of each of the variables
## this uses the gather function to wrangle the data into tidy format, for easy plotting by qplot
ggplot(data=gather(bodyfat, key=variable, value=value),
       aes(x=value)) +
  geom_histogram(bins=20) +
  facet_wrap(~variable, scales="free")

## look at the graphs of variables plotted against each other,
## to get an idea of which variables might best predict body fat.
pairs(bodyfat)

#bodyfat <- filter(bodyfat, weight<300)

m1 <- lm(bodyfat ~ abdomen + weight, data=bodyfat)
autoplot(m1)
summary(m1)
## 72%



