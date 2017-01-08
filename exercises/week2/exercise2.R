rm(list=ls())

library(tidyverse)
library(GGally)


bodyfat <- read_delim("~/Desktop/git/BIO144/data_examples/bodyFat/bodyfat.txt", 
                      delim="\t", escape_double = FALSE, trim_ws = TRUE)

qplot(x=bodyfat, data=bodyfat, bins=20)

qplot(x=value, data=gather(bodyfat, key=variable, value=value), bins=20) +
  facet_wrap(~variable, scales="free")



bodyfat_clean <- read_delim("~/Desktop/git/BIO144/data_examples/bodyFat/bodyfat.clean.txt", 
                            delim="\t", escape_double = FALSE, trim_ws = TRUE)

ggpairs(bodyfat)

bodyfat <- mutate(bodyfat, my_bmi=weight/height^2*703)
