##################
##  title: "R_TP1"
##################


# Partie 1
## Dans le modèle linéaire gaussien
### Simulation
#1.(a)
n = 20
X = c(1:20) 
epsilon = rnorm(n , 1, sqrt(2))
Y = 2 + 4*X + epsilon
#2.(b)
plot(X,Y,col="darkgreen")
#3.(c)
model<-lm(Y~1+X)
res<-rstudent(model)
plot(res,col="red",ylim=c(-3,3))
abline(h=2,col="blue")
abline(h=-2,col="blue")
#s'il y a des points qui n'est pas contenu dans le -2 à 2, on peut le regarder comme un “aberrante”.
#mais ici, on trouve que c'est rare pour ce cas.
#2.(a)

X = c(1:20) 
epsilon = rnorm(n , 1, sqrt(2))
Y = 2 + 4*X + epsilon

X<-c(X[1:19],30)

#2.(b)
plot(X,Y)
model_X30<-lm(Y~1+X)
#2.(c)
res_X30<-rstudent(model_X30)
plot(res_X30,col="red",ylim=c(-5,5))
abline(h=2,col="blue")
abline(h=-2,col="blue")

#On trouve que il n'y a pas de chose bizzare pour les résidus studentisés.  

df<-dfbeta(model_X30)
dcook<-cooks.distance(model_X30)
cov_r<-covratio(model_X30)
abs(cov_r-1)


#on peut voir que le covratio de X20=0.9318  qui est plus grand (>3*(2+1)/20), alors X20 est aberrante




#3.
#3.(a)
X = c(1:20) 
epsilon = rnorm(n , 1, sqrt(2))
Y = 2 + 4*X + epsilon
Y<-c(Y[1:19],Y[20]-10)
#3.(b)
plot(X,Y)
#3.(c)
model_Y20<-lm(Y~1+X)
res_Y20<-rstudent(model_Y20)
plot(res_Y20,col="red",ylim=c(-10,10))
abline(h=2,col="blue")
abline(h=-2,col="blue")

#par le plot de résidus studentisé, on peut bien trouver le point aberrante.



#4.
#4.(a)
X = c(1:20) 
epsilon = rnorm(n , 1, sqrt(2))
Y = 2 + 4*X + epsilon

X<-c(X[1:19],30)
Y<-c(Y[1:19],Y[20]-10)

#4.(b)
plot(X,Y)
#4.(c)
model_XY<-lm(Y~1+X)

#res_studentisé
res_XY<-rstudent(model_XY)
plot(res_XY,col="red",ylim=c(-50,50))
abline(h=2,col="blue")
abline(h=-2,col="blue")







# Partie 2
## Faire pointer R vers votre répertoire
setwd(nomrepertoire)

## (Installer et) charger la librairie faraway
library(faraway)


# données "Ozone"
load("Ozone.Rdata")
attach(ozone)
summary(ozone)
names(ozone)
ozone1<-ozone[1:11]

pairs(maxO3~.,data=ozone1)
#2.1.1(a)
#les variables T9, T12, T15, Vx9, Vx12, Vx15, maxO3v sont linéairement liées à maxO3.
#2.1.1(b)
#Ne9, Ne12, Ne15.
#2.1.1(c)
#T9 : T12,  T15,  (maxO3v)
#T12 : T15, (Vx9),  (maxO3v)
#T15 : Vx9, Vx12, Vx15, (maxO3v)
#Vx9 : Vx12,  Vx15, maxO3v
#Vx12 : Vx15

#2.1.2
model<-lm(maxO3~.,data=ozone1)
summary(model)

# On peut voir que le p-values des variables sont "grandes", c-à-d, le model n'est pas bien.
pairs(maxO3~.,data=ozone1)



#2.1.3
#il faut changer les classes de Ne9 Ne12 et  numeric-->factor
#ozone1<-data.frame(ozone1[,1:4],Ne9=as.factor(ozone1[,5]),Ne12=as.factor(ozone1[,6]),Ne15=as.factor(ozone1[,7]),ozone1[,8:11])

pairs(maxO3~.,data=ozone1)

#par le scatterplot, on trouve que linéarités entre Vx9,Vx12,Vx15 et maxO3 ne sont pas très évidents. Alors, on les supprime.
ozone2<-ozone1[,c(1:7,11)]



#2.1.4




vif(model)
#par le vif, on peut voir que le vif de T12 et T15 sont plus grandes que 10, c-à-d, T12 et T15 sont fonctions linéaires des
#autres varaibles.
# on va supprimé T12 et T15.


#2.1.5
#on considère un modèle pour les variables T9, Ne9, Ne12, Ne15, maxO3v
ozone_selected<-ozone[,c(1,2,5,6,7,11,12,13)]#c'est pour le 2.2, je réserve les colonnes ventsud et pluiesec.



#distance-Cook
model<-lm(maxO3~.,data=ozone_selected)

dcook<-cooks.distance(model)
which(dcook>4/nrow(ozone_selected))

ozone_selected<- ozone_selected[-c(18,23,52,54,58,79,104),]


#covratio
model<-lm(maxO3~.,data=ozone_selected)
cov_r<-covratio(model)
which(abs(cov_r-1)>3*(5+1)/nrow(ozone4))

ozone_selected<- ozone_selected[c(1,2,3,12,15,21,23,25,27,28,29,32,34,35,37,38,41,44,47,48,
                                  52,53,54,59,60,62,63,64,65,67,68,71,74,77,86,87,88,91,92,98,99,
                                  101,102,104),]


#pour eviter de enlever trop d'individus, on ne considère que la critère de distance-Cook
#et covratio.
#On peut aussi utiliser le dfbeta.



#2.1.6
#résidus studentisés
model<-lm(maxO3~.,data=ozone_selected)
res_stu<-rstudent(model)
plot(res_stu,col="darkblue")
abline(h=2,col="red")
abline(h=-2,col="red")
which(abs(res_stu)>2)
#par le résidus studentisés, on va supprimer les lignes 34, 52, 58, 79
ozone4<-ozone3[-c(34,52,58,79),]




ozone_selected<-ozone[,c(1,2,5,6,7,11,12,13)]



#2.2
#2.2.1
model_complet<-aov(maxO3~.,data=ozone)
model_complet_lm<-lm(maxO3~.,data=ozone)
summary(model_complet)
summary(model_complet_lm)

#Ils sont égaux.



#2.2.2

#2.2.3
library(leaps)

ozone_pv<-ozone_selected


choix<-regsubsets(maxO3~.,data=ozone_pv,nbest = 1,nvmax = 12)
plot(choix,scale = "bic")
#les variables sélectionnés par le BIC sont T9, Ne9, maxO3v, ventSud.
#la variabble pluieSec n'est pas selectionnée.



