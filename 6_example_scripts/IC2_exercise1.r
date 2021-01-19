## clear R
rm(list=ls())

## laod the required libraries
library(tidyverse)
library(GGally)

## read in the data
bodyfat_dataset <- read_delim("https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/bodyfat.txt", 
                      delim="\t", escape_double = FALSE, trim_ws = TRUE)

## histogram of the bodyfat measurements
ggplot(data=bodyfat_dataset, aes(x=bodyfat)) + geom_histogram(bins=20)


## The number of individuals with body fat greater than 35
filter(bodyfat_dataset, bodyfat>35)
## four rows, so four individuals

## Owen found the bmi of the individual with the highest value of bodyfat
## by looking at the data with View(bodyfat), sorting by bodyfat, and looking at the bmi
## you could also do it like this:
filter(bodyfat_dataset, bodyfat == max(bodyfat)) %>%
  pull(bmi)


## plot distributions of each of the variables
## this uses the gather function to wrangle the data into tidy format, for easy plotting by qplot
ggplot(data=gather(bodyfat_dataset, key=variable, value=value),
       aes(x=value)) +
  geom_histogram(bins=20) +
  facet_wrap(~variable, scales="free")

## look at the graphs of variables plotted against each other,
## to get an idea of which variables might best predict body fat.
ggpairs(bodyfat_dataset)

## The formula is BMI = weight (kg) / height (m) ^2 
bodyfat_dataset <- bodyfat_dataset %>%
  mutate(bmi_calc = gewicht / (hoehe/100)^2 )

## Question 6 :: Across all variables, which individuals (identified by the number in the Nr column) seem to have rather extreme or unusual values in at least one variable?
## Owen did this by looking at the dataset with View(bodyfat_dataset) sorting, and reading!
## Let us know if you have a nice programmatic way of doing this.
## one attempt is here: https://raw.githubusercontent.com/opetchey/BIO144/master/2_content/Unit_2/practical/auto_outliers_Schano.R


