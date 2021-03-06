## function adapted from
## http://www.nate-miller.org/1/post/2013/03/how-normal-is-normal-a-q-q-plot-approach.html
qqfunc <- function(model, num.reps) {
  
  N <- length(resid(model))
  sigma <- summary(model)$sigma
  
  x <- rnorm(N, 0, sigma)
  xx <- qqnorm(x, plot.it=F)
  xx$y <- xx$y[order(xx$x)]
  xx$x <- xx$x[order(xx$x)]
  plot(xx$x, scale(xx$y), pch=19, col="#00000011", type="l",
       xlab="Theoretical quantiles",
       ylab="Standardised residuals")
  ##qqline(x)
  
  for(i in 2:num.reps) {
    
    x <- rnorm(N, 0, sigma)
    xx <- qqnorm(x, plot.it=F)
    xx$y <- xx$y[order(xx$x)]
    xx$x <- xx$x[order(xx$x)]
    points(xx$x, scale(xx$y), pch=19, col="#00000011", type="l")
    
  }
  
  xx <- qqnorm(m1$residuals, plot.it=F)
  points(xx$x, scale(xx$y), col="red", pch=19)
  
}



## load some useful packages
library(readr)
library(tidyverse)
library(ggfortify)

## load the data
## Here I'm loading it direct from online where the dataset are stored
fhc <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144/master/datasets/financing_healthcare.csv")

## tell me which countries are in the dataset
unique(fhc$country)

## tell me which years are in the dataset
unique(fhc$year)

## give me the rows in which both child mortality and health care expenditure are not NA
filter(fhc, !is.na(child_mort) & !is.na(health_exp_total))

## Wrange the data
fhc1 <- fhc %>%
  filter(year==2013) %>% ## only data fro 2013 please
  select(year:continent, health_exp_total, child_mort, life_expectancy) %>% ## only these columns please
  drop_na() ## drop rows with any NAs

## From last weeks work we know we need to log transform the data
fhc1 <- mutate(fhc1,
               log10_health_exp_total=log10(health_exp_total),
               log10_child_mort=log10(child_mort))

## plot the relationship between health care expenditure and child mortality
ggplot(data=fhc1, aes(x=health_exp_total, y=child_mort)) + geom_point()

## fit the linear model of the log transformed data and assign it to object named m1
m1 <- lm(log10_child_mort ~ log10_health_exp_total, data=fhc1)

## Here we run the function that makes the qqplot with some random samples from a normal distribution
qqfunc(m1, 100)
abline(0,1)

