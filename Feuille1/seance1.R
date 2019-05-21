# Code R du cours du jeudi 9 mai 2019 (matin)

rm(list=ls())

# Question 1.1
p  = c(1e-6, .001, .01, .1, .25, .4, .8, .99, .999)

rbind(p, p / (1 - p))

curve(log(x / (1 - x)))
abline(h=0)
abline(v=1/2)

# Question 1.2.a
p = 18 / 152
q = 3 / 7
p / (1 - p) / (q / (1 - q))

# Question 1.2.b
p1 = 1.3 / 1000 # proba chez les fumeurs
p2 = .07 / 1000 # proba chez les non fumeurs
p3 = .67 / 1000 # proba chez les ex-fumeurs

log((p1 / (1 - p1)) / (p2 / (1 - p2))) # rapport des cotes associé au fait de se mettre à fumer (fumeur vs non-fumeur)
# le log-rapport est positif : le risque augmente

log((p3 / (1 - p3)) / (p1 / (1 - p1))) # rapport des cotes associé au fait d'arrêter de fumer (ex-fumeur vs fumeur)
# le log-rapport est négatif : le risque diminue

# Exemple sur l'achat de voitures
d = read.table("car_income.txt", header = T)

#modèle logistique de base
g = glm(purchase ~ income + age,
        family = binomial,
        data = d)
summary(g)

# on peut faire des formules plus compliquées
g2 = glm(purchase ~ income + I(age > 3 & age < 6),
         data = d,
         family = binomial)
summary(g2)


# Exercice 2 : données Titanic
d = read.csv("titanic.csv")
d = read.csv("titanic.csv", na.strings = "")
summary(d)
# Certaines colonnes ne semblent pas immédiatement pertinentes, notamment PassengerId, Name, Ticket, Cabin

cor.test(d$Survived, d$Parch)
boxplot(d$Age ~ d$Survived)
boxplot(d$Fare ~ d$Survived)
mosaicplot(d$Pclass ~ d$Survived, col=2:3)

model0 = glm(Survived ~ Age  + Pclass  + Sex,
             data=d, family=binomial)
summary(model0)


# modèle de base
model1 = glm(
  Survived ~ factor(Pclass) + Sex + Age  + I(Parch + SibSp),
  family = binomial,
  data = d
)
summary(model1)

# Pclass est codé comme un entier, mais on voudrait le coder comme variable catégorielle.
# On supprimer quelques autres covariables non pertinentes, et on peut essayer d'ajouter de l'interaction
model2 = glm(Survived ~ factor(Pclass) + Sex + Sex:Age + SibSp,
             family = binomial,
             data = d)
summary(model2)

# Créons une nouvelle covariable pour indiquer les enfants de moins de 18 ans
d$child = d$Age < 18

model3 = glm(
  Survived ~ factor(Pclass) + SibSp + child   + Sex + Sex:Age,
  family = binomial,
  data = d
)
summary(model3)
# dans ce modèle, on a un paramètre pour l'âge chez les enfants (childTRUE:Age) et
# un autre paramètre pour l'âge chez les adultes (childFALSE:Age), en plus du paramètre
# pour l'indicatrice que l'individu est un enfant (childTRUE). Tous ces paramètres sont
# significativement différents de 0.


# prédiction pour deux nouveaux individus
xnew = data.frame(
  Pclass = c(1, 3),
  Name = c("Rose", "Jack"),
  Sex = c("female", "male"),
  Age = c(17, 20),
  SibSp = c(0, 0),
  Parch = c(1, 0),
  child = c(F, F),
  Ticket = c(NA, NA),
  Fare = c(100, 10),
  Cabin = c(NA, NA),
  Embarked = c("S", "S")
)

predict(model3, xnew, type="response")
