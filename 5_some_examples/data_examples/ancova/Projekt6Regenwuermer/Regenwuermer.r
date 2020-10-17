library(lattice)

setwd("/home/steffi/Teaching/Bio144/data_examples/ancova/Projekt6Regenwuermer/")

d.wurm <- read.table ("RegenwuermerDaten_Haupt",header=T)

str(d.wurm)


boxplot(GEWICHT ~ Gattung, d.wurm,xlab="Gattung",ylab="Gewicht (g)")
boxplot(MAGENUMF ~ Gattung, d.wurm,xlab="Gattung",ylab="Gewicht (g)")

boxplot(log(GEWICHT) ~ Gattung, d.wurm,xlab="Gattung",ylab="Gewicht (g)")
boxplot(log(MAGENUMF) ~ Gattung, d.wurm,xlab="Gattung",ylab="Gewicht (g)")

# 1. Frage: Unterscheiden sich die Wurm-Gattungen in Gewicht / Magenumfang?

r.aov <- aov(GEWICHT ~ Gattung,d.wurm)
summary(r.aov)
plot(r.aov)

r.aov2 <- aov(MAGENUMF ~ Gattung,d.wurm)
summary(r.aov2)

plot(r.aov2)

# Antwort: ja

# 2. Frage: Sind Magenumfang und Gewicht korreliert?

with(d.wurm,cor(GEWICHT,MAGENUMF)) # scheint so
plot(GEWICHT ~ MAGENUMF, d.wurm) # plot sieht etwas quadratisch aus. Dieses Gefuehl wird auch durch die einfache lin. Reg bestaetigt:

r.lm <- lm(GEWICHT ~ MAGENUMF, d.wurm)
summary(r.lm)
plot(r.lm$fitted,r.lm$resid)
abline(h=0,lty=2)

plot(r.lm) # Problem: Residuenplots sehen nicht sehr gut aus...

# Nehme also auch quadratischen Term mit rein:
r.lm <- lm(GEWICHT ~ MAGENUMF + I(MAGENUMF^2), d.wurm)
summary(r.lm)
plot(r.lm) # etwas besser, aber Normalitaet der Residuen scheint verletzt.



# 3. Frage: Veraendert sich die Abhaengigkeit von Gewicht und Magenumfang in den drei Gattungen? Falls ja, muessten die Forscher bei den Dachs-Exkrementen die Gattung kennen, um vom Magenumfang auf das Gewicht schliessen zu koennen.

xyplot(GEWICHT ~ MAGENUMF, d.wurm,
       grid = TRUE,
       scales = list(x = list(log = 10, equispaced.log = FALSE)),
       group = Gattung, auto.key = list(columns = nlevels(d.wurm$Gattung)),
       lwd = 2)

xyplot(GEWICHT ~ MAGENUMF, d.wurm,
       grid = TRUE,
       scales = list(x = list(log = 10, equispaced.log = FALSE)),
       group = Gattung, auto.key = list(columns = nlevels(d.wurm$Gattung)),
       type = c("p", "smooth"), lwd = 2)


# Modell mit Interaktion:

r.lm <- lm(GEWICHT ~ MAGENUMF  + I(MAGENUMF^2)*Gattung, d.wurm)
summary(r.lm)
anova(r.lm) # scheint, dass wirklich die Gattungen unterschiedlich reagieren
plot(r.lm) # Modellannahmen sind aber immer noch nicht richtig gut erfuellt.


# Teste weiter: log-Transformationen der Variablen
plot(log(GEWICHT) ~ log(MAGENUMF),d.wurm,col=d.wurm$Gattung) # sieht doch viel linearer aus!

r.lm.log <- lm(log(GEWICHT) ~ log(MAGENUMF) + I(log(MAGENUMF)^2),d.wurm)
summary(r.lm.log) # quadratischer Term nicht mehr noetig...

r.lm.log <- lm(log(GEWICHT) ~ log(MAGENUMF),d.wurm)
plot(r.lm.log$fitted,r.lm.log$resid)
abline(h=0,lty=2)

qqnorm(r.lm.log$resid)
qqline(r.lm.log$resid)

plot(r.lm.log)

# Ist Gattung nun immer noch relevant?
r.lm.log <- lm(log(GEWICHT) ~ log(MAGENUMF) + Gattung,d.wurm)
summary(r.lm.log)
anova(r.lm.log) # Nein!!!
