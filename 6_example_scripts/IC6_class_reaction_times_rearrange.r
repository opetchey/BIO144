

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

## Select only the variables we want to work with
class_RTs <- select(class_RTs, ID, Gender, Pref_Reaction_time, Nonpref_Reaction_time_ave)

## gather the two variables worth of reaction times into one variable
dd <- tidyr::gather(class_RTs, key=key, value=Reaction_time, 3:4)

## create variable that shows if the preferred or non-preferred hand was used
dd <- tidyr::separate(dd, col=key, sep="_", into=c("Hand", "Junk"))

## remove the junk variable
dd <- select(dd, -Junk)

## here we could remove extreme data points, if we wished to

## here is a graph that could help us answer the question...
ggplot(dd, aes(x=Gender, y=Reaction_time, col=Hand)) +
  geom_point(position = position_dodge(width=0.1))

## or this
ggplot(dd, aes(x=Reaction_time, col=Hand)) +
  geom_density() + facet_wrap(~Gender)

## And this is the model we could use to confirm what
## we conclud from the figure
m1 <- lm(Reaction_time ~ Gender * Hand, data=dd)
## check the model assumptions
autoplot(m1)
## and look at the statistical significance...
anova(m1)
## In 2020, only sex showed any evidence.
## there was no evidence of a handedness or interaction difference.

## I did not in the script make a beautiful graph...
## I'm sure you can by now
## do that
## yourself,
## and share on the Discussion Forum. Please :)