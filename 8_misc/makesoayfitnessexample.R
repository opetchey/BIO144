library(tidyverse)

###############################
## making some data for the Soay sheep fitness - body size dataset
## which is used to learn about analysing count data

## Please note that the values produced below may not be the same as in 
## a soaysheepfitness dataset you may have

## set a seed, so we can get the same random numbers, if we want to
set.seed(1)

## let there be 50 mothers
num.mums <- 50

## create the dataset...
dd <- data.frame(body_size = rnorm(num.mums, mean=7, sd=1)) %>% ## assign body sizes
  mutate(expected_fitness = 0.5*exp(0.3*body_size), ## make the expected number of offspring depend on body size
         observed_fitness = rpois(num.mums, expected_fitness)) %>% ## observed number of individuals comes from a poisson distribution with
  select(-expected_fitness) ## remove expected fitness variable

## write the dataset to file
write.csv(dd, "Soay_fitness <- example.csv")
