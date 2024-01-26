
#######################################################
## first line of code is to clear R's memory
rm(list=ls())
#######################################################


#######################################################
## First we load some required add-on package
## (you need to install these if you haven't already)
library(readr)
library(dplyr)
library(ggplot2)
library(skimr)
#######################################################


#######################################################
## Now read in the data, using the read_csv() function.
## First we should assign, using the assignment arrow,
## the URL of the published version of the google sheet data into an object.
the_URL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vT0S581py-IDp4bIQnqFFTttQ1JChFHMMjgkYEbSv88nD1yqV1ocNY1eqrLifEPOHxkCZ4q73XeJcTX/pub?gid=1441390006&single=true&output=csv"
## then use the read_csv function to read in the data from that URL
class_RTs <- read_csv(the_URL)
#######################################################

## DO NOT USE read.csv above!!!

## --->>> Once more, and even if a TA tells you to, do not use read.csv on line 27.
## --->>> You must keep the underscore (_) and not replace it with a dot (.)

#######################################################
## Have a look at the data in R, does it look OK?
class_RTs
#######################################################


#######################################################
## Now we need to do some data wrangling (cleaning and tidying)
## Clean up the column / variable names:
## Must be very careful to get the next line right!!! Really important!!!
## Otherwise columns will have the wrong names, which would be very confusing
names(class_RTs) <- c("Timestamp", "ID", "Gender", "Pref_Reaction_time_1",
                      "Verbal_memory_score", "Number_memory_score",
                      "Visual_memory_score",
                      "Weight_kgs", "Handed", "Nonpref_Reaction_time_ave",
                      "Pref_Reaction_time_2", "Pref_Reaction_time_3", 
                      "Pref_Reaction_time_4", "Pref_Reaction_time_5",
                      "Pref_Reaction_time", "Random_number")
## check the variable names are what we just tried to set them to be
class_RTs
#######################################################



#######################################################
## Check the variable types are correct
## (they should be in this case, but checking is a good habit.)
## Timestamp should be a character
## ID should be a character
## Gender should be a character
## Handed should be character
## The remaining variables should be numeric (<dbl>)
glimpse(class_RTs)
#######################################################


#######################################################
## Correct or exclude problematic data
## If we have problems here, with variables of the wrong type,
## it probably means some of the data entry is a bit messed up.
## the skim() function is a really nice one for looking at the data,
## including if any variables have missing values (NAs)
skim(class_RTs)


#######################################################
## and the number of observations of each gender
class_RTs %>%
  group_by(Gender) %>%
  summarise(number = n())
#######################################################


#######################################################
## Now make a figure containing the histogram of reaction times
ggplot(data=class_RTs, aes(x=Pref_Reaction_time)) +
  geom_histogram()

## Now make a figure containing two histograms histograms (i.e. two "facets"), one for each gender
ggplot(data=class_RTs, aes(x=Pref_Reaction_time)) +
  geom_histogram() +
  facet_grid(~ Gender)

## Make a graph to help see if reaction times depend on weight
## and if any effect differs between the sexes
class_RTs %>%
  ggplot(aes(x = Weight_kgs, y = Pref_Reaction_time, col = Gender)) +
  geom_point()

## looks unlikely, but first lets remove some outliers
filt_RTs <- class_RTs %>%
  filter(Weight_kgs < 120) %>%
  filter(Pref_Reaction_time > 200 & Pref_Reaction_time < 500)

## plot again
filt_RTs %>%
  ggplot(aes(x = Weight_kgs, y = Pref_Reaction_time, col = Gender)) +
  geom_point()
## looks unlikely there is any relationship

## check with a model
m1 <- lm(Pref_Reaction_time ~ Weight_kgs * Gender, data = filt_RTs)
## check model assumptions
autoplot(m1)
## not too bad!
## look at the summary table
summary(m1)
## in 2020 there was moderate evidence of a negative relationship
## between weight and reaction time (heavier is faster),
## and then no effect of sex or the interaction of weight and sex

## Check correlation of sex and weight
filt_RTs %>%
  ggplot(aes(y = Weight_kgs, x = Gender)) +
  geom_point()
## males are heavier...
## so it could be that either sex is the determining factor, or weight is.
## we can't tell, however, with this 
## Note that there is low explanatory power (r-squared of 0.08 = 8%)


