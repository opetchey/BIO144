## clear R
rm(list=ls())

## laod the required libraries
library(coefplot) ## a new library we will be using
library(tidyverse)
library(GGally)
library(ggfortify)


## read in the data
bodyfat <- readr::read_delim("https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/bodyfat.txt", 
                      delim="\t", escape_double = FALSE, trim_ws = TRUE)

## histogram of the bodyfat measurements
ggplot(data=bodyfat, aes(x=bodyfat)) + geom_histogram(bins=20)

## plot distributions of each of the variables
## this uses the gather function to wrangle the data into tidy format, for easy plotting by qplot
ggplot(data=gather(bodyfat, key=variable, value=value),
       aes(x=value)) +
  geom_histogram(bins=20) +
  facet_wrap(~variable, scales="free")

## look at the graphs of variables plotted against each other,
## to get an idea of which variables might best predict body fat.
pairs(bodyfat)

## OK, now we start with the IC9 exercise
## make the three models...
m_both <- lm(bodyfat ~ abdomen + weight, data=bodyfat)
m_weight <- lm(bodyfat ~ weight, data=bodyfat)
m_abdomen <- lm(bodyfat ~ abdomen, data=bodyfat)
## and get the r-squared of each
summary(m_both)$r.squared
summary(m_weight)$r.squared
summary(m_abdomen)$r.squared

## unique to abdomen
summary(m_both)$r.squared - summary(m_weight)$r.squared
## unique to weight
summary(m_both)$r.squared - summary(m_abdomen)$r.squared

## shared
summary(m_both)$r.squared - (summary(m_both)$r.squared - summary(m_weight)$r.squared)-
  (summary(m_both)$r.squared - summary(m_abdomen)$r.squared)

## if you want to make the venn diagram, this is one way...
# library(VennDiagram)
# draw.pairwise.venn(area1 = 66, area2 = 38, cross.area = 32, category = c("Abdomen", 
#                                                                         "Weight"))
# 
# draw.triple.venn(area1 = 22, area2 = 20, area3 = 13, n12 = 11, n23 = 4, n13 = 5, 
#                  n123 = 1)                         

## here's another package for venn diagrams
## I did not make it plot our current results
## I just played :)
# library(venn)
# venn(3)
# venn("1--")
# venn("1--", ilabels = TRUE)
# venn(4, lty = 5, col = "navyblue")
# venn(4, lty = 5, col = "navyblue", ellipse = TRUE)


### Rescaling
bodyfat <- mutate(bodyfat, weight_kg=weight*0.45)
m_pounds <- lm(bodyfat ~ abdomen + weight, data=bodyfat)
m_kg <- lm(bodyfat ~ abdomen + weight_kg, data=bodyfat)
summary(m_pounds)
summary(m_kg)
-0.14800/-0.32890

## out of interest, here is what happens if we put
## both weight variables in the model...
## for one we fail to get statistics... because it is
## perfectly correlated with the other weight variable
m_kg_pounds <- lm(bodyfat ~ abdomen + weight_kg + weight, data=bodyfat)
summary(m_kg_pounds)

# now scale the variables...
bodyfat <- dplyr::mutate(bodyfat, scaled_weight=scale(weight),
                  scaled_abdomen=scale(abdomen),
                  scaled_wrist=scale(wrist),
                  scaled_height=scale(height),
                  scaled_ankle=scale(ankle))
m_kg_pounds_scaled <- lm(bodyfat ~ scaled_abdomen + scaled_weight, data=bodyfat)
summary(m_kg_pounds_scaled)


## Visualising coefficients (effect sizes)
## Lets say we'd like to visualise the coefficients of a model
## with more of the explanatory variables... e.g. 
m_more <- lm(bodyfat ~ scaled_abdomen + scaled_weight +
                    scaled_wrist + scaled_height + scaled_ankle,
                  data=bodyfat)

## we can use either of two packages, or do it more manually
library(coefplot) 
coefplot(m_more) ## not working at the mo

library(arm)
coefplot(m_more, mar=c(1,6,6,1))
box()

## more manually
## use the tidy function from the broom packaage to put the coefficients and
## associated statistics into a tidy data frame...
library(broom)
tidy_mod <- tidy(m_more)
## and plot this, without the intercept, since its not so interesting
tidy_mod %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = estimate, y = term)) + 
  geom_vline(xintercept = 0, col = "grey") +
  geom_point() +
  geom_errorbarh(aes(xmin = estimate - 2 * std.error,
                     xmax = estimate + 2 * std.error),
                 height = 0.2)
