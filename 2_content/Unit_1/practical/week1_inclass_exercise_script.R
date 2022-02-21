## Warning: the code below contains deliberate errors.
## Also it sometimes contains "???" which you need to replace with appropriate text.


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
the_URL - "https://docs.google.com/spreadsheets/d/e/2PACX-1vQDI5oZ54MD4Qm_WDydUAWgRYX1PRWMoOWJCSFN5mZJ6Yf-irKq_n6-7CyPsJ13BoEksusqRUBdcgii/pub?gid=133427610&single=true&output=csv"
## then use the read_csv function to read in the data from that URL
class_RTs <- read_cvs(the_URL)
#######################################################

## DO NOT USE read.csv above!!!

## --->>> Once more, and even if a TA tells you to, do not use read.csv on line 27.
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
names(class_RTs) <- c("Timestamp", "ID", "Gender", "Weight"
                      "Handedness", "Pref_Reaction_time_1",
                      "Pref_Reaction_time_2", "Pref_Reaction_time_3", 
                      "Pref_Reaction_time_4", "Pref_Reaction_time_5",
                      "Pref_Reaction_time",
                      "Nonpref_Reaction_time_ave",
                      "Verbal_memory_score", "Number_memory_score",
                      "Visual_memory_score", "Random_number")
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
glimpse(class_RTs
#######################################################


#######################################################
## Correct or exclude problematic data
## If we have problems here, with variables of the wrong type,
## it probably means some of the data entry is a bit messed up.
## the skim() function is a really nice one for looking at the data,
## including if any variables have missing values (NAs)
skim(class_RT)


#######################################################
## and the number of observations of each gender
class_RTs %>%
  group_by(Gender) %%
  summarise(number = n())
#######################################################


#######################################################
## Now make a figure containing the histogram of reaction times
ggplot(data=class_RTs, aes(x=???)) +
  geom_histogram()

## Now make a figure containing two histograms histograms (i.e. two "facets"), one for each gender
ggplot(data=class_RTs, aes(x=???)) +
  geom_histogram() +
  facet_grid(~Gender)

## And a box and whisker plot
ggplot(data=class_RTs, aes(x=???, y=???)) +
  geom_boxplot()

## Or just the data points (with some jitter, to separate overlapping points):
ggplot(data=class_RTs, aes(x=???, y=???)) +
  geom_jitter(width=0.05)
#######################################################

#######################################################
## Perhaps filter out some extreme values
class_RTs_filtered <- class_RTs %>%
  filter(Pref_Reaction_time < 50,
         Pref_Reaction_time > 500)
class_RTs_filtred %>%
  ggplot2() +
  geom_jitter(mapping = aes(x=Gnder, y=Pref_reaction_time),
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
## Do the t test and assign the outcome to an object:
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
t.test



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
  ylab("Reaction time (seconds)")
