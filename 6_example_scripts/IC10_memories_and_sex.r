
rm(list = ls())

library(tidyverse)
library(ggfortify)

## Now read in the data, using the read_csv() function. We give it the URL of the published version of the google sheet data.
the_URL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQFgYX1QhF9-UXep22XmPow1ZK5nbFHix9nkQIa0DzqUhPtZRxH1HtY-hsno32zDiuIHiLb2Hvphk1L/pub?gid=1188775314&single=true&output=csv"
class_RTs <- read_csv(the_URL)

## Must be very careful to get the next line right!!! Really important!!!
names(class_RTs) <- c("Timestamp", "ID", "Gender", "Pref_Reaction_time_1",
                      "Verbal_memory_score", "Number_memory_score",
                      "Visual_memory_score",
                      "Weight_kgs", "Handed", "Nonpref_Reactiontime",  
                      "Pref_Reaction_time_2", "Pref_Reaction_time_3",  
                      "Pref_Reaction_time_4", "Pref_Reaction_time_5",
                      "Pref_Reactiontime", "Random_number")

## we are interested in only the memory score variables, and the sex (gender) variable
## so lets keep only those

memories <- dplyr::select(class_RTs, Gender,
                   Verbal_memory_score, Number_memory_score, Visual_memory_score)
## I'm using dplyr::select so that the correct select function is used
## otherwise we might get an error if we only use select
## e.g...
memories <- select(class_RTs, Gender,
                          Verbal_memory_score, Number_memory_score, Visual_memory_score)
## maybe that gives you and error. If it doesn't don't worry!

## Graph the distribution of the three response variables:
## we'll use the pivot_long function, instead of gather (they do the same thing)
tidy_memories <- memories %>%
  pivot_longer(cols = 2:4, names_to = "variable", values_to = "value")
tidy_memories %>%
  ggplot() +
  geom_histogram(aes(x = value)) +
  facet_wrap( ~ variable, scales = "free")

## there are some rather extreme values (in 2020 class data)
## lets remove large values
filtered_memories <-  tidy_memories %>%
  mutate(value = case_when(variable == "Number_memory_score" & value > 20 ~ NA_real_,
         variable == "Verbal_memory_score" & value > 200 ~ NA_real_,
         variable == "Visual_memory_score" & value > 20 ~ NA_real_,
         TRUE ~ value))
  
## and check again the distributions
filtered_memories %>%
  ggplot() +
  geom_histogram(aes(x = value)) +
  facet_wrap( ~ variable, scales = "free")


## visualise the data to see if we see differences:
filtered_memories %>%
  ggplot(aes(x = Gender, y = value)) +
  geom_jitter(width = 0.1, height = 0) +
  facet_wrap( ~ variable, scale = "free")
## no clear differences (2020 data)



## now a glm for each , since they are count data, use poisson
varb_of_interest <- "Number_memory_score"
mod <- glm(value ~ Gender,
                    data = filter(filtered_memories,
                                  variable == varb_of_interest),
                    family = poisson)
autoplot(mod)
## qqplot on the edge of ok (2020 data)
summary(mod)
## model underdispersed... so change to quasipoisson
mod <- glm(value ~ Gender,
           data = filter(filtered_memories,
                         variable == varb_of_interest),
           family = quasipoisson)
autoplot(mod)
##summary(mod)
anova(mod, test = "Chisq")
## no difference

varb_of_interest <- "Verbal_memory_score"
mod <- glm(value ~ Gender,
           data = filter(filtered_memories,
                         variable == varb_of_interest),
           family = poisson)
autoplot(mod)
## qqplot fine (2020 data)
summary(mod)
## model very overdispersed... so change to quasipoisson
mod <- glm(value ~ Gender,
           data = filter(filtered_memories,
                         variable == varb_of_interest),
           family = quasipoisson)
autoplot(mod)
##summary(mod)
anova(mod, test = "Chisq")
## no difference
## here we would have concluded a difference for the poisson,
## so it was really important to switch to the quasipoisson

varb_of_interest <- "Visual_memory_score"
mod <- glm(value ~ Gender,
           data = filter(filtered_memories,
                         variable == varb_of_interest),
           family = poisson)
autoplot(mod)
summary(mod)
## model underdispersed... so change to quasipoisson
mod <- glm(value ~ Gender,
           data = filter(filtered_memories,
                         variable == varb_of_interest),
           family = quasipoisson)
autoplot(mod)
##summary(mod)
anova(mod, test = "Chisq")
## no difference
