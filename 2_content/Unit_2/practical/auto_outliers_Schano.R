



##### U2 - How can I generalise finding outliers? - Christian Schano 2020-03-03 ##### And Owen 2020_03_06
## Our goal is (and the question was) to automatise the process of finding outliers within our (or basically any) dataset. 
## Therefore we need to find a way to define outliers in a generalisable way (i.e. applicable to all columns). 
## One way to do this is using z-scores. The z-score of a data point is a value defining its fractional distance from the mean measured in standard deviations, eneabling us to define the distance of each datapoint from the mean of the distribution it is part of.

## (the below "chapters" can be collapsed or decollapsed by clicking the arrow next to the script line number. 

##### Packages & Data #####
## read packages ##
library(readr) # reading in files
library(dplyr) # data wrangling
library(tidyr) # use to tidy up data (gather(), etc.)

## clear R`s brain
rm(list = ls())

## get data ##
## You will need to change the path...
RAW.fat <- read_delim("3_datasets/bodyfat.txt", delim = "\t") # read in raw (my personal preferance is never to alter the original object)

## First get the z-scores
dat.fat <- RAW.fat %>% # raw dataset that was read in
  gather(key="Variable", value="Value", 2:19) %>% # gather data in columns 2:19 into two new columns, while keeping Nr.
  group_by(Variable) %>% # group by Variable
  mutate(z_score = scale(Value)) ## standardise to z-scored by variable

## Set outlier criteria
def.zscoremin <- -4 # we define the lower threshold for the z-score not to be an outlier
def.zscoremax <- 4 # we define the upper threshold for the z-score not to be an outlier

## Find individuals with outlier in any Variable
ind_with_outlier <- dat.fat %>%
  group_by(Nr) %>%
  summarise(any_outlier = sum(z_score > def.zscoremax | z_score < def.zscoremin) > 0) %>%
  filter(any_outlier)
  
