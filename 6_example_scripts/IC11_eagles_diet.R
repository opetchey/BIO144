## clear R
rm(list=ls())

## load required packages
library(tidyverse)
library(ggfortify)

## read in the data
dd <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/EAGLES.CSV")

## make a table that counts the number of prey of each type for each sex:
tt <- xtabs(Count ~ Prey + Sex, data = dd)

## and we can make a plot of the data
ggplot(dd, aes(x=Sex, y=Count, fill = Prey)) +
  geom_col(position="dodge")

## does it look like the males and females eat
## different proportions of different prey types?
## the proportion of voles and waterbirds are switched between the sexes.
## so, yes, it does look like the diets differ.

# check this with a chi-squared test on the contingency table
chisq.test(tt)
## yes, there is moderate evidence of a different in diet,
## as is indicated by the p-value less than 0.05, but not less than 0.01