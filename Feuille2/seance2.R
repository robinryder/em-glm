# Code R du cours du jeudi 9 mai 2019 (après-midi)

rm(list=ls())
# Données Challenger
shuttle = read.table("shuttle.txt", header = T)
summary(shuttle)
attach(shuttle)
# représentation graphique du lien entre température et incident
boxplot(tempF ~ damage)

# régression logistique
g = glm(damage ~ tempF, data = shuttle, family = binomial)
summary(g)

# prédiction - notez le paramètre type="response" pour que la prédiction 
# soit donnée comme probabilité et non sur l'échelle logistique
predict(g, new = data.frame(tempF = 31), 
        type = "response",
        se.fit = T)


# Données contraception
# les données sont binomiales : on observe un nombre de succès et un nombre d'échecs
cud = read.table("contraception_Fidji.txt", header = T)
# Dans la fonction glm, on met alors une matrice à 2 colonnes dans le membre gauche de la formule.
g.cud = glm(cbind(notUsing, using) ~ ., 
            data = cud, family = binomial)
summary(g.cud)

# Données Allocations au niveau de l'IRIS
d = read.csv("alloc_iris.csv")

# A nouveau, les données sont au format binomial et non Bernoulli : on met une matrice à deux colonnes
g.caf = glm(
  cbind(RSA, POP - RSA) ~ maternelle.public + maternelle.prive,
  data = d,
  family = binomial
)
summary(g.caf)

# on peut essayer d'autres modèles
g.caf = glm(
  cbind(RSA, POP - RSA) ~ . - LIBIRIS - LIBCOM - COM - X - POPH - POPF,
  data = d,
  family = binomial
)
summary(g.caf)

# comparons à une régression linéaire gaussienne : le résultat n'est pas probant
# car les hypothèses du modèle gaussien sont loin d'être vérifiées
r.caf = lm(RSA ~ . - LIBIRIS - LIBCOM - COM - X - POPH - POPF - POP, data =
             d)
summary(r.caf)
g.caf = glm(cbind(RSA, POP - RSA) ~ . ,
            data = d[,-(1:3)], family = binomial)


# Exercice 4

library(gclus)
data(bank)
# régression logistique classique
summary(glm(Status ~ ., data = bank, family = binomial))
# On obtient le message d'erreur
# Warning messages:
# 1: glm.fit: l'algorithme n'a pas convergé
# 2: glm.fit: des probabilités ont été ajustées numériquement à 0 ou 1
# On se rend compte que l'erreur apparaît dès lors qu'on inclut Bottom et Diagonal:
m = glm(Status ~ Bottom + Diagonal, data = bank, family = binomial)
summary(m)

# Commençons par visualiser la sortie avec d'autres covariables :
m2 = glm(Status ~ Bottom + Right, data = bank, family = binomial)
summary(m2)
plot(Bottom, Right, col = Status + 1)
beta = m2$coefficients
a = -beta[2] / beta[3]
b = -beta[1] / beta[3]
abline(b, a)


attach(bank)
plot(Bottom, Diagonal, col = Status + 1)
# On voit que la classification peut être parfaite. Cherchons la droite qui divise parfaitement les points rouges et les points noirs.
beta = m$coefficients
a = -beta[2] / beta[3]
b = -beta[1] / beta[3]
abline(b, a)
# Dans cette situation, la vraisemblance augmente quand on multiplie toutes les valeurs de beta par une même constante >1. Le max de vraisemblance est donc
# atteint à l'infini, d'où le message d'erreur.



# Données Pima
library(MASS)
data(Pima.tr)
data(Pima.te)
# premier modèle : on inclut toutes les covariables
g2 = glm(type ~ ., data = Pima.tr, family = binomial)
summary(g2)
# après tâtonnement, on aboutit au modèle suivant qui paraît raisonnable
g3 = glm(type ~ glu + ped, data = Pima.tr, family = binomial)
summary(g3)

# On fait une prédiction sur de nouvelles données
pred = predict(g3, newdata = Pima.te, type = "response")
pred2 = (pred >= 0.5)
table(pred2, Pima.te$type) #Table des prédictions comparées aux vraies valeurs :
# on a 50 faux négatifs et 18 faux positifs
mean(pred2 == (Pima.te == "Yes")) # 74% de bonnes prédictions

# cherchons le seuil optimal pour attribuer les individus aux 2 classes prédites
seuil = seq(0, 1, by=.1)
system.time({
res = rep(NA, length(seuil))
for(i in 1:length(seuil)){
  pred2 = (pred >= seuil[i])
  res[i] = 1 * sum(pred2 & Pima.te$type=="No") + 
    4 * sum(!pred2 & Pima.te$type=="Yes")
}
})
seuil[which.min(res)] # seuil optimal

# on peut mettre en place une autre fonction de coût
# qui pénalise différemment les faux positifs et les faux négatifs
cout = function(s){
  pred2 = (pred >=s)
  1 * sum(pred2 & Pima.te$type=="No") + 
    4 * sum(!pred2 & Pima.te$type=="Yes")
}

cout(0.2)

# cherchons le seuil optimal avec cette fonction de coût
# on peut implémenter en parallèle
library(parallel)

ncl = detectCores() - 1 # nombre de coeurs
cl = makeCluster(ncl, type="FORK")
clusterSetRNGStream(cl) # à inclure dès lors qu'il y a de la génération de nombres aléatoires en parallèle
system.time(sapply(seuil, cout))
system.time(parSapply(cl, seuil, cout))

# D'autres mesures de l'erreur sont possibles...
mean(abs(pred - (Pima.te$type == "Yes")))
mean(abs((pred > .5) - (Pima.te$type == "Yes")))

# Représentation graphique de notre classification
attach(Pima.tr)
plot(glu, ped, col = ifelse(type == "Yes", "red", "green"))
beta = g3$coefficients
a = -beta[1] / beta[3]
b = -beta[2] / beta[3]
abline(a, b)

