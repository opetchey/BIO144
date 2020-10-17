setwd("/home/steffi/Teaching/Bio144/data_examples/anova/ProjektMaishybriden/")
d.mais <- read.table("MaishybridenDaten.txt",header=T)

str(d.mais)

r.aov <- aov(YIELD ~ HYBRID + LOCATION,data=d.mais)
summary(r.aov) # Resultate zeigen, ob Variabeln als Ganzes einen Einfluss haben

r.lm <- lm(YIELD ~ HYBRID + LOCATION,data=d.mais)
summary(r.lm) # Resultate zeigen Einflüsse der Faktorlevels im Vergleich zum Referenzlevel
anova(r.lm)


# compare also the coefficients:
r.aov$coefficients
summary(r.lm)$coef
