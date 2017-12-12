# ----------------------------------
# baclophen trials: (placebo)effects
# ----------------------------------

# clean up
rm(list = ls())

library(readxl)
library(lattice)
library(ggplot2)
library(lme4)

setwd('D:/Other/ITB/')
# setwd('C:/Documenten/Statistiek/BaclophenTrials/')

# itbdata.xlsx <- read_excel("ITB trials - data-analyse.xlsx", sheet = 1)
# itbdata.xlsx <- read_excel("Kopie van ITB trials - data-analyse (002) 13-06-17.xlsx", sheet = 1)
itbdata.xlsx <- read_excel("ITB trials - data-analyse 20170831.xlsx", sheet = 1)
date.tag <- "20170831"

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

# make one plot with all trials
tiff(paste0("Overzicht_dosis_effect_per_patient_all_", date.tag, ".tiff"), width = 4*480, height = 6*480, 
     units = "px", pointsize = 12, res = 144, compression = "lzw")
p <- ggplot(data = itb.effect, aes(x = Dosis, y = MAS.effect, colour = factor(TijdNaInj))) + geom_point() + facet_wrap( ~ EAD.nummer)
p <- p + geom_hline(aes(yintercept=0)) + stat_smooth(method = "lm") + theme_grey(base_size = 18)
p <- p + theme(legend.position = "bottom")
p
dev.off()

# make 4 plots of 9
EAD.list <- unique(itb.effect$EAD.nummer)
np <- length(EAD.list)
graphs.per.plot <- 9
for (i in seq(1, 4)) {
  from.EAD <- 1 + (i - 1) * graphs.per.plot
  to.EAD <- (min(graphs.per.plot * i, np))
  selected.EADs <- EAD.list[from.EAD:to.EAD]
  tiff(paste0("Overzicht_dosis_effect_per_patient_", from.EAD, 'to', to.EAD, '_', date.tag, ".tiff"), 
       width = 6.5, height = 6.5, 
       units = "in", pointsize = 12, res = 144, compression = "lzw")
  p <- ggplot(data = itb.effect[itb.effect$EAD.nummer %in% selected.EADs,], 
              aes(x = Dosis, y = MAS.effect, colour = factor(TijdNaInj))) + geom_point() + facet_wrap( ~ EAD.nummer)
  p <- p  + geom_hline(aes(yintercept=0)) + stat_smooth(method = "lm") + theme_grey(base_size = 18)
  print(p)
  dev.off()
}

# linear regression for each individual patient
EAD.nummer <- 60764805
EAD.overview.df <- data.frame()
for (EAD.nummer in unique(itb.effect$EAD.nummer)) {
  lm.EAD <- lm(MAS.effect ~ Dosis, itb.effect[itb.effect$EAD.nummer == EAD.nummer,])
  EAD.df <- data.frame(EAD.nummer = EAD.nummer,
                       intercept = as.double(coef(lm.EAD)[1]),
                       intercept.low = as.double(confint(lm.EAD)[1,1]),
                       intercept.high = as.double(confint(lm.EAD)[1,2]),
                       slope = as.double(coef(lm.EAD)[2]),
                       slope.low = as.double(confint(lm.EAD)[2,1]),
                       slope.high = as.double(confint(lm.EAD)[2,2]))
  EAD.overview.df <- rbind(EAD.overview.df, EAD.df)
}
png('boxplot_slope_intercept.png')
par(mfrow = c(1,2))
for (parameter in c('intercept', 'slope')) {
  boxplot(EAD.overview.df[, parameter], main = parameter)
  for (i in 1:NROW(EAD.overview.df)) {
    if (EAD.overview.df[i, paste0(parameter, '.low')] * EAD.overview.df[i, paste0(parameter, '.high')] < 0) {
      kleur <- 'blue'
    } else {
      kleur <- 'red'
    }
    points(1, EAD.overview.df[i, parameter], col=kleur, pch = 16)
    abline(h = 0)
    legend('bottomright', pch = 16, legend = c('0 in CI', '0 outside CI'), col = c('blue', 'red'))
  }
}
dev.off()

# ----------------------------
# simple linear regression
# ----------------------------

lm.itb.1 <- lm(MAS.effect ~ Dosis + TijdNaInj + vorigeDosis + DosisEergisteren, 
               data = itb.effect, na.action = na.exclude)
summary(lm.itb.1)

# Call:
#   lm(formula = MAS.effect ~ Dosis + TijdNaInj + vorigeDosis + DosisEergisteren, 
#      data = itb.effect, na.action = na.exclude)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.08076 -0.48623  0.09928  0.41780  1.51989 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      -0.1132756  0.1334642  -0.849  0.39680    
# Dosis            -0.0060867  0.0009521  -6.393  7.3e-10 ***
#   TijdNaInj        -0.0622253  0.0373753  -1.665  0.09712 .  
# vorigeDosis       0.0066979  0.0025714   2.605  0.00971 ** 
#   DosisEergisteren -0.0077385  0.0023965  -3.229  0.00140 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.6114 on 265 degrees of freedom
# (54 observations deleted due to missingness)
# Multiple R-squared:  0.1644,	Adjusted R-squared:  0.1518 
# F-statistic: 13.04 on 4 and 265 DF,  p-value: 1.051e-09

tiff('residuals_linear_regression_full_model.tiff', width = 4*480, height = 2*480,
     units = "px", pointsize = 12, res = 144, compression = "lzw")
plot(factor(itb.effect$EAD.nummer), residuals(lm.itb.1),
     main = 'Residuals of linear regression',
     xlab = 'Patient number')
abline(h = 0, col = 'red')
dev.off()

lm.itb.2 <- lm(MAS.effect ~ 0 + Dosis + TijdNaInj + vorigeDosis + DosisEergisteren, data = itb.effect, na.action = na.exclude)
summary(lm.itb.2)

lm.itb.3 <- lm(MAS.effect ~ 0 + Dosis + TijdNaInj, data = itb.effect, na.action = na.exclude)
summary(lm.itb.3)



# residuals are not normaly distributed around zero => linear regression is not ok


# ----------------------------
# fitting a linear mixed model
# ----------------------------

# full model
itb.lme4.1 = lmer(MAS.effect ~ Dosis + vorigeDosis + DosisEergisteren + TijdNaInj + (Dosis | EAD.nummer), 
                  data = itb.effect)
summary(itb.lme4.1)
# => intercept is not significant

# full model without intercept
itb.lme4.2 = lmer(MAS.effect ~ 0 + Dosis + vorigeDosis + DosisEergisteren + TijdNaInj + (Dosis | EAD.nummer), 
                  data = itb.effect)
summary(itb.lme4.2)

# full model without intercept and without previous doses
itb.lme4.3 = lmer(MAS.effect ~ 0 + Dosis + TijdNaInj + (Dosis | EAD.nummer), 
                  data = itb.effect)
summary(itb.lme4.3)


# full model
itb.lme4.1 = lmer(MAS.effect ~ Dosis + vorigeDosis + TijdNaInj + (Dosis | EAD.nummer), data = itb.effect)
summary(itb.lme4.1)

itb.lme4.1 = lmer(MAS.effect ~ Dosis + (1 | EAD.nummer), data = itb.effect, REML = FALSE)
summary(itb.lme4.1)

itb.lme4.2 = lmer(MAS.effect ~ Dosis + (Dosis | EAD.nummer), data = itb.effect, REML = FALSE)
summary(itb.lme4.2)

AIC(itb.lme4.1, itb.lme4.2)

itb.lme4 = lmer(MAS.effect ~ 0 + Dosis + vorigeDosis + TijdNaInj + (1 | EAD.nummer), data = itb.effect)
summary(itb.lme4)


itb.lme4.4 = lmer(MAS.effect ~ 0 + Dosis + vorigeDosis + TijdNaInj + DosisEergisteren + (Dosis | EAD.nummer), 
                  data = itb.effect)
summary(itb.lme4.4)


anova(itb.lme4.3, itb.lme4.4)

itb.lme4.5 = lmer(MAS.effect ~ 0 + Dosis + vorigeDosis + (Dosis | EAD.nummer), data = itb.effect)
summary(itb.lme4.5)














