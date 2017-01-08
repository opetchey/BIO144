## In the next line, write the line of code, starting with "rm" that clears R's memory.
rm(list=ls())

## Now load the required packages... its a good idea to do this
## at the start of your script, and load all packages here.
library(readr)
library(tidyverse) ## this line loads quite a few libraries, including dplyr and ggplot2
library(GGally)

## read in the dataset
Hg_urin <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144/master/data_examples/Hg/Hg_urin.csv")

## make a graph that allows you to see the frequency distribution of mercury concentrations
## in the invididual's urine in the study sample.
## Hint, use the qplot function.
qplot(x=Hg_urin_krea_new, data=Hg_urin, bins=20)

## Does the data look to be normally distributed, or skewed in some way.
## Are there any data points (i.e. individuals) that might be considered outliers?

## Use the filter function from the dplyr library to show the individuals with mercury concentrations above 2
## (Don't forget to load the dplyr library first: library(dplyr))
filter(Hg_urin, Hg_urin_krea_new>2)


## Make a new dataset that does not contain individuals with concentrations greater than 2.
Hg_urin_lt2<- filter(Hg_urin, Hg_urin_krea_new<=2)
qplot(x=Hg_urin_krea_new, data=Hg_urin_lt2, bins=20)


## Use the mutate function to create a new variable that is the log10 of the mercury concentration variable
Hg_urin_lt2 <- mutate(Hg_urin_lt2, log10_Hg_urin_krea_new=log10(Hg_urin_krea_new))


## make a similar graph but showing the frequency distribution of the log or the
## mercury concentrations.
qplot(x=log10_Hg_urin_krea_new, data=Hg_urin_lt2, bins=40)

pairs(Hg_urin_lt2)
ggpairs(Hg_urin_lt2)


