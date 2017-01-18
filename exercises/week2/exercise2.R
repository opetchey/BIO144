## clear R
rm(list=ls())

## laod the required libraries
library(tidyverse)
library(GGally)


## read in the data
bodyfat <- read_delim("~/Desktop/git/BIO144/data_examples/bodyFat/bodyfat.txt", 
                      delim="\t", escape_double = FALSE, trim_ws = TRUE)

## histogram of the bodyfat measurements
qplot(x=bodyfat, data=bodyfat, bins=20)

## plot distributions of each of the variables
## this uses the gather function to wrangle the data into tidy format, for easy plotting by qplot
qplot(x=value, data=gather(bodyfat, key=variable, value=value), bins=20) +
  facet_wrap(~variable, scales="free")

## look at the graphs of variables plotted against each other,
## to get an idea of which variables might best predict body fat.
ggpairs(bodyfat)
