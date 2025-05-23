## This is the solution script for the first practical


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

## get the dataset from here:
## https://lms.uzh.ch/url/RepositoryEntry/17654940136/CourseFolder/0/path%3D~~human%5Fbenchmark%5Fdata%5FFS25%2Ecsv/0

#######################################################
## Now read in the data, using the read_csv() function.
## Use the read_csv function to read in the data
## Note that you will need to put the data in the same folder as this script, then
## open RStudio by double clicking on the script file.
class_RTs <- read_cvs("human_benchmark_data_FS25.csv")
#######################################################

## DO NOT USE read.csv above!!!

## --->>> Once more, and even if a TA tells you to, do not use read.csv on line 25.
## --->>> You must keep the underscore (_) and not replace it with a dot (.)

#######################################################
## Have a look at the data in R, does it look OK?
clas_RTs
#######################################################


#######################################################
## Now we need to do some data wrangling (cleaning and tidying)
## Clean up the column / variable names:
## Must be very careful to get the next line right!!! Really important!!!
## Otherwise columns will have the wrong names, which would be very confusing
names(class_RTs) <- c("Timestamp",
                      "Gender",
                      "Weight"
                      "Handedness",
                      "Pref_Reaction_time_1",
                      "Pref_Reaction_time_2",
                      "Pref_Reaction_time_3", 
                      "Pref_Reaction_time_4",
                      "Pref_Reaction_time_5",
                      "Pref_Reaction_time",
                      "Nonpref_Reaction_time_ave",
                      "Verbal_memory_score",
                      "Number_memory_score",
                      "Visual_memory_score",
                      "Random_number")
## check the variable names are what we just tried to set them to be
names(class_RT)
#######################################################

## View the data
View(class_RTs)

#######################################################
## Check the variable types are correct
## (they should be in this case, but checking is a good habit.)
## Timestamp should be a character
## ID should be a character
## Gender should be a character
## Handed should be character
## The remaining variables should be numeric (<dbl>)
str(Class_RTs)
#######################################################


#######################################################
## Correct or exclude problematic data
## If we have problems here, with variables of the wrong type,
## it probably means some of the data entry is a bit messed up.
## the skim() function is a one for looking at the data,
## including if any variables have missing values (NAs)
skim(class_RTs)
## Lots of information. Work through it, and ask for help if you need it.

#######################################################
## and the number of observations of each gender
class_RTs %>%
  group_by(Gender) %>%
  summarise(number = n())
#######################################################


#######################################################
## Now make a figure containing the histogram of reaction times
ggplot(data=class_RTs, aes(x=???)) +
  geom_histogram()

## Now make a figure containing two histograms histograms (i.e. two "facets"), one for each gender
ggplot(data=class_RTs, aes(x=???)) +
  geom_histogram() +
  facet_grid(~ Gender)

## And a box and whisker plot
ggplot(data=class_RTs, aes(x=???, y=???)) +
  geom_boxplot()

## Or just the data points (with some jitter, to separate overlapping points):
ggplot(data=class_RTs, aes(x=Gender, y=Pref_Reaction_time)) +
  geom_jitter(width=0.05)
#######################################################

#######################################################
## Perhaps filter out some extreme values
class_RTs_filtered <- class_RTs %>%
  filter(Pref_Reaction_time > 50,
         Pref_Reaction_time < 500)
class_RTs_filtered %>%
ggplot() +
  geom_jitter(mapping = aes(x=Gnder, y=Pref_Reaction_time),
              width=0.05)
#######################################################


#######################################################
## Do you think there is a difference in reaction times between females and males?
## What is the effect size (i.e. the magnitude of the difference?)
## Is this likely to be of practical significance?
## Look at your graphs and assess assumptions:
## - Do you think the residuals will be normally distributed?
## - Do the two groups have similar variance?
## - Do there seem to be any outliers?
## - Are data points independent? (You don't get this from the graph, but rather from knowing how the data were collected.)
#######################################################


#######################################################
## Do a t-test and assign the outcome to an object:
my_ttest <- t.test(??? ~ ???,
                   data=class_RTs,
                   var.equal=TRUE)
## look at the result of the t-test
t.test
#######################################################

###################
## And the same but with the filtered data:
my_ttest <- t.test(??? ~ ???,
                   data = ???,
                   var.equal=TRUE)
my_ttest



#######################################################
## Critical thinking
# How might the work be flawed?
# How might the analysis be flawed (assumptions violated)?
# Is the difference (i.e. effect size) small, medium, large, relative to differences caused by other factors?
# How general might be the finding?
# How do the qualitative and quantitative findings compare to those in previous studies?
# What could have been done better?
# What are the implications of the findings?
#######################################################


#######################################################
## Report and communicate the results
## Write a sentence that gives the direction and extent of difference,
## and a measure of certainty / uncertainty in that finding.
## Make a beautiful graph that very clearly communicates the findings!
ggplot(data=???, aes(x=???, y=???)) +
  geom_boxplot() +
  geom_jitter(width= 0.1) +
  ylab("Reaction time (milliseconds)")
