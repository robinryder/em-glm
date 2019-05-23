# Exercice 1
require(MASS)
data(survey)
table(survey$Exer, survey$Smoke)

chisq.test(table(survey$Exer, survey$Smoke))
# Sous l'hypothèse H0 d'indépendance, on a 5 degrés de liberté : 3 pour la cigarette, 2 pour l'exercice.
# Sous l'hypothèse alternative H1, on a 11 degrés de liberté.
# On fait donc un test du chi2 à 11-5=6 degrés de liberté


# Exercice 2
cred = read.csv("german_credit.csv")
View(cred)
# Commençons par un modèle simple
m1 = glm(
  Creditability ~ Account.Balance + Telephone + Guarantors,
  data = cred,
  family = binomial
)
summary(m1)

# Analysons la déviance. La sortie nous indique
# Null deviance: 1221.7  on 999  degrees of freedom
# Residual deviance: 1087.5  on 996  degrees of freedom
# On commence par comparer notre modèle au modèle sans covariable
pchisq(1221.7 - 1087.5, 999 - 996, lower = F)
# On obtient une p-valeur très faible : on rejette le modèle sans covariable. Notre modèle est donc utile.

# Comparons maintenant notre modèle au modèle saturé
pchisq(1087.5, 996, lower = F)
# Là aussi, la p-valeur est faible : on rejette donc notre modèle et on préfère le modèle saturé. Autrement dit, notre modèle n'est pas suffisant.

# Cherchons maintenant le meilleur modèle de manière automatique
m2 = step(glm(Creditability ~ . , data = cred, family = binomial))
summary(m2)
# Pour la déviance, la sortie indique
# Null deviance: 1221.73  on 999  degrees of freedom
# Residual deviance:  959.32  on 984  degrees of freedom
# Le test par rapport au modèle sans covariable donne
pchisq(1221.73 - 959.32, 999 - 984, lower = F)
# p-valeur très faible : on rejette le modèle sans covariable
# Le test par rapport au modèle saturé donne
pchisq(959.32, 984, lower = F)
# p-valeur élevée : on accepte notre modèle, qui suffit à expliquer les variations.

# ANOVA
anova(m1, m2, test = "LRT")
anova(m2, test = "LRT")

# régression probit
d = read.table("car_income.txt", header = T)
g = glm(formula = purchase ~ income + age,
        family = binomial,
        data = d)
summary(g)


g2 = glm(
  formula = purchase ~ income + age,
  family = binomial(link = "probit"),
  data = d
)
summary(g2)

# Validation croisée

d = read.csv("titanic.csv", na.strings = "")
summary(d)
d$child = d$Age < 18

# on crée un vecteurs de booléens, tirés aléatoirement
# les valeurs TRUE correspondent aux individus de la base d'entraînement
# les valeurs FALSE correspondent aux individus de la base de test
train = sample(c(T, F), nrow(d), replace = T, prob = c(.7, .3))

# on estime les paramètres beta, en utilisant uniquement la base d'entraînement
model3 = glm(
  Survived ~ factor(Pclass) + SibSp + child   + Sex + Sex:Age,
  family = binomial,
  data = d[train, ]
)
summary(model3)

# on effectue une prédiction, uniquement sur la base de test
pred3 = predict(model3, d[!train, ], type = "response")
# et on évalue l'erreur de prédiction
mean(abs(pred3 - d[!train, "Survived"]), na.rm = T)

# on peut répéter pour d'autres modèles
model2 = glm(Survived ~ factor(Pclass) + Sex + Sex:Age + SibSp,
             family = binomial,
             data = d[train,])
summary(model2)
pred2 = predict(model2, d[!train, ], type = "response")
mean(abs(pred2 - d[!train, "Survived"]), na.rm = T)

model1 = glm(
  Survived ~ factor(Pclass) + Sex + Age  + I(Parch + SibSp),
  family = binomial,
  data = d[train,]
)
summary(model1)
pred1 = predict(model1, d[!train, ], type = "response")
mean(abs(pred1 - d[!train, "Survived"]), na.rm = T)

# on choisit le modèle qui donne l'erreur la plus basse
