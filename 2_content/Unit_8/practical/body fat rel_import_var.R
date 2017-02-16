## clear R
rm(list=ls())

## laod the required libraries
library(coefplot)
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

#bodyfat <- filter(bodyfat, weight<300)

m_both <- lm(bodyfat ~ abdomen + weight, data=bodyfat)
m_weight <- lm(bodyfat ~ weight, data=bodyfat)
m_abdomen <- lm(bodyfat ~ abdomen, data=bodyfat)
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

# library(VennDiagram)
# draw.pairwise.venn(area1 = 66, area2 = 38, cross.area = 32, category = c("Abdomen", 
#                                                                          "Weight"))
# 
# draw.triple.venn(area1 = 22, area2 = 20, area3 = 13, n12 = 11, n23 = 4, n13 = 5, 
#                  n123 = 1)                         

# library(venn)
# venn(3)
# venn("1--")
# venn("1--", ilabels = TRUE)
# venn(4, lty = 5, col = "navyblue")
# venn(4, lty = 5, col = "navyblue", ellipse = TRUE)


### Rescaling

bodyfat <- mutate(bodyfat, weight_kg=weight*0.45)
bodyfat <- mutate(bodyfat, weight_tonne=weight_kg/1000)
m_pounds <- lm(bodyfat ~ abdomen + weight, data=bodyfat)
m_kg <- lm(bodyfat ~ abdomen + weight_kg, data=bodyfat)
m_tonne <- lm(bodyfat ~ abdomen + weight_tonne, data=bodyfat)
summary(m_pounds)
summary(m_kg)
-0.14800/-0.32890

m_kg_pounds <- lm(bodyfat ~ abdomen + weight_kg + weight, data=bodyfat)
summary(m_kg_pounds)

coef(m_pounds)
coef(m_kg)
coef(m_tonne)

bodyfat <- dplyr::mutate(bodyfat, scaled_weight=scale(weight),
                  scaled_abdomen=scale(abdomen),
                  scaled_wrist=scale(wrist),
                  scaled_height=scale(height),
                  scaled_ankle=scale(ankle))
m_kg_pounds <- lm(bodyfat ~ scaled_abdomen + scaled_weight, data=bodyfat)
summary(m_kg_pounds)


m_more <- lm(bodyfat ~ scaled_abdomen + scaled_weight +
                    scaled_wrist + scaled_height + scaled_ankle,
                  data=bodyfat)


library(coefplot) 
coefplot(m_kg_pounds) ## not working at the mo

library(arm)
coefplot(m_more, mar=c(1,6,6,1))
box()
