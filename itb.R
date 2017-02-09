# ----------------------------------
# baclophen trials: (placebo)effects
# ----------------------------------

library(readxl)
library(lattice)
library(ggplot2)
library(lme4)

setwd('D:/Other/ITB/')

itbdata.xlsx <- read_excel("ITB trials - data-analyse.xlsx", sheet = 1)


# dataset met effecten
itb.effect.2uur <- data.frame(itbdata.xlsx, TijdNaInj = 2, MAS.effect = itbdata.xlsx$MASna2uur - itbdata.xlsx$MASvoor)
itb.effect.4uur <- data.frame(itbdata.xlsx, TijdNaInj = 4, MAS.effect = itbdata.xlsx$MASna4uur - itbdata.xlsx$MASvoor)
itb.effect <- rbind(itb.effect.2uur, itb.effect.4uur)
# dosis van de vorige dag toevoegen
itb.effect <- data.frame(itb.effect, vorigeDosis = 0)
for (i in 1:NROW(itb.effect)) {
  if (itb.effect$Dag[i] > 1) {
    vorige.dag <- itb.effect$Dag[i] - 1
    selectie <- itb.effect$EAD.nummer == itb.effect$EAD.nummer[i] & itb.effect$Dag == vorige.dag
    # rij met effect na 2 en 4 uur worden geselecteerd maar de dosis vn de vorige dag is dezelfde, vandaar mean
    itb.effect$vorigeDosis[i] <- mean(itb.effect$Dosis[selectie])
  }
}
# dosis van 2 dage eerder toevoegen
itb.effect <- data.frame(itb.effect, DosisEergisteren = 0)
for (i in 1:NROW(itb.effect)) {
  if (itb.effect$Dag[i] > 2) {
    eergisteren <- itb.effect$Dag[i] - 1
    selectie <- itb.effect$EAD.nummer == itb.effect$EAD.nummer[i] & itb.effect$Dag == eergisteren
    # rij met effect na 2 en 4 uur worden geselecteerd maar de dosis vn de vorige dag is dezelfde, vandaar mean
    itb.effect$DosisEergisteren[i] <- mean(itb.effect$Dosis[selectie])
  }
}


jpeg("Overzicht_dosis_effect_per_patient.jpg", width = 4*480, height = 2*480)
p <- ggplot(data = itb.effect, aes(x = Dosis, y = MAS.effect, colour = factor(TijdNaInj))) + geom_point() + facet_wrap( ~ EAD.nummer)
p <- p  + geom_hline(aes(yintercept=0)) + stat_smooth(method = "lm") + theme_grey(base_size = 18)
p
dev.off()

lm.itb <- lm(MAS.effect ~ Dosis + TijdNaInj + vorigeDosis + DosisEergisteren, data = itb.effect, na.action = na.exclude)
summary(lm.itb)

lm.itb <- lm(MAS.effect ~ Dosis + vorigeDosis, data = itb.effect, na.action = na.exclude)
summary(lm.itb)


lm.itb <- lm(MAS.effect ~ Dosis + vorigeDosis, data = itb.effect, na.action = na.exclude)
summary(lm.itb)

plot(factor(itb.effect$EAD.nummer), residuals(lm.itb))


itb.lme4 = lmer(MAS.effect ~ Dosis + vorigeDosis + TijdNaInj + (1 | EAD.nummer), data = itb.effect)
summary(itb.lme4)

library(car)
Anova(itb.lme4)

















