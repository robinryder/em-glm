# Données multinomiales : source d'énergie de chauffage

d = read.csv("RECS2009.csv")

library(nnet)
summary(multinom(FUELRR ~ MONEYPY + TOTSQFT, data = d))
# les coefficients s'interprètent tous par rapport à la valeur de référence, Electricity

m1 = multinom(FUELRR ~ MONEYPY + TOTSQFT, data = d)
m2 = multinom(FUELRR ~ TOTSQFT, data = d)
anova(m1, m2)
# on préfère m1



# Donnée multinomiales
library(nnet)
library(mlogit)
data(Mode)
summary(Mode)
?Mode


m = multinom(choice ~ ., data = Mode)
summary(m)
m3 = step(m)

m2 = multinom(choice ~ . - time.rail, data = Mode)
anova(m, m3)

m4 = multinom(choice ~  cost.bus + cost.rail, data = Mode)
m5 = multinom(choice ~  time.bus + time.rail, data = Mode)

# Données ordinales : temps en heures passé à regarder la télévision

library(ordinal)

d = read.csv("RECS2009.csv")

# vérifions la structure de la colonne TVRR
str(d$TVRR)
d$TVRR = ordered(d$TVRR)
str(d$TVRR)
# 1: <1; 2: 1-3; 3: 3-6; 4: 6-10; 5: >10

m1 = clm(TVRR ~ TVCOLOR + MONEYPY + NUMPC , data = d)
summary(m1)

# Régression de Poisson
d = read.csv("RECS2009.csv")
p1 = glm(NUMPC ~ TVCOLOR + MONEYPY, data = d,
         family = poisson)
summary(p1)

# Ajoutons un offset
p1c = glm(NUMPC ~ TVCOLOR + MONEYPY + I(log(NHSLDMEM)),
          data = d,
          family = poisson)
summary(p1c)
# ce modèle est moins bon que p1

# On peut aussi faire une méthode de quasi-vraisemblance, qui laisse de la flexibilité sur la variance
p2 = glm(NUMPC ~ MONEYPY + TVCOLOR, data = d,
         family = quasipoisson)
summary(p2)
