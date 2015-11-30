##################
##  title: "R_TP2"
##################


# Exemple d'école
## Simulation
n=100
Z0=rep(1,n)
Z1=c(1:n)
Z2=Z1^2
Z3=Z1^3
Z4=Z1^4
Z5=sqrt(Z1)
Z6=1/Z1
Z7=log(Z1)



sigma=20
eps=rnorm(n,0,sigma)


beta=c(5,0,-0.03,0.0002,0,0,0,-3)
X=matrix(c(Z0,Z1,Z2,Z3,Z4,Z5,Z6,Z7),ncol=8)
Y=X%*%beta+eps
data=data.frame(Y=Y,Z0=Z0,Z1=Z1,Z2=Z2,Z3=Z3,Z4=Z4,Z5=Z5,Z6=Z6,Z7=Z7)
plot(Z1,Y,xlim = c(0,150),col="red")


lines(Z1,X%*%beta, col="black")


fit_null = lm(Y ~ 1, data = data)#modele nul
summary(fit_null)$coefficients
add1(fit_null,scope= ~1+Z1+Z2+Z3+Z4+Z5+Z6+Z7,test = "F")

add1(update(fit_null,~.+Z1),scope=~Z1+Z2+Z3+Z4+Z5+Z6+Z7,test = "F")

add1(update(fit_null,~.+Z1+Z4),scope=~Z1+Z2+Z3+Z4+Z5+Z6+Z7,test = "F")

add1(update(fit_null,~.+Z1+Z4+Z3),scope=~Z1+Z2+Z3+Z4+Z5+Z6+Z7,test = "F")


add1(update(fit_null,~.+Z1+Z4+Z3+Z2),scope=~Z1+Z2+Z3+Z4+Z5+Z6+Z7,test = "F")

model_ace<-lm(Y~1+Z1+Z2+Z3+Z4,data=data)
summary(model_ace)
coeff_ace<-model_ace$coefficients
f_fit_ace<-function(x) coeff_ace[1]+coeff_ace[2]*x^1+coeff_ace[3]*x^2+coeff_ace[4]*x^3+coeff_ace[5]*x^4
curve(f_fit_ace,add=T,col="darkgreen")






####################fonction par sélection de F-test décendante.

fit_full<-lm(Y~.,data=data)#model complet
summary(fit_full)$coefficients


drop1(fit_full, test = "F")
drop1(update(fit_full, ~ . -Z4), test = "F")
drop1(update(fit_full, ~ . -Z6-Z4), test = "F")
drop1(update(fit_full, ~ . -Z6-Z4-Z7), test = "F")
drop1(update(fit_full, ~ . -Z6-Z4-Z7-Z1), test = "F")
drop1(update(fit_full, ~ . -Z6-Z4-Z7-Z1-Z5), test = "F")

model_dec<-lm(Y~1+Z2+Z3,data=data)
summary(model_dec)
coeff_dec<-model_dec$coefficients

f_fit_dec<-function(x) coeff_dec[1]+coeff_dec[2]*x^2+coeff_dec[3]*x^3
curve(f_fit_dec,add=T,col="blue")

##############BIC
library(leaps)
choix <- regsubsets(Y~.,data = data,method = "backward") 
plot(choix,scale="bic")
summary(choix)$cp
summary(choix)$which[4,]

model_bic<-lm(Y~1+Z3+Z4,data=data)
summary(model_bic)
coeff_bic<-model_bic$coefficients
f_fit_bic<-function(x) coeff_bic[1]+coeff_bic[3]*x^4+coeff_bic[2]*x^3
curve(f_fit_bic,add=T,col="green")
#############################

###########################AIC forward automatique
model.aic.both <- step(fit_null, direction = "forward" ,
                       scope =   ~ Z0+Z1+Z2+Z3+Z4+Z5+Z6+Z7, trace = TRUE)
model_aic<-lm(Y~1+Z3+Z4+Z5+Z7,data=data)
summary(model_aic)
coeff_aic<-model_aic$coefficients
f_fit_aic<-function(x) coeff_aic[1]+coeff_aic[2]*x^3+coeff_aic[3]*(x^4)+coeff_aic[4]*sqrt(x)+coeff_aic[5]*log(x)
curve(f_fit_aic,add=T,col="orange")



#nouveau jeu de données



id=c(100:150)
new=data.frame(Z1=id,Z2=id^2,Z3=id^3,Z4=id^4,Z5=sqrt(id),Z6=1/id,Z7=log(id))
lines(new$Z1,(5-0.03*new$Z2+0.0002*new$Z3-3*new$Z7) , col="black")

########################comparaisons des plots

plot(Z1,Y,xlim = c(0,150),col="red",main="comparaison de selections des variables")
curve(f_fit_dec,add=T,col="blue")
curve(f_fit_bic,add=T,col="green")
curve(f_fit_aic,add=T,col="orange")
lines(Z1,X%*%beta, col="black")
lines(new$Z1,(5-0.03*new$Z2+0.0002*new$Z3-3*new$Z7) , col="black")
legend("topright",c("dec",'bic',"aic","vraie"),col = c("blue","green","orange","black"),lty = rep(1,4))







# Ozone
## reprendre vos codes précédents


# Simulations
n=50
p=30
puis=c(0:9)
puis_petit=c(10:28)
beta=c(0.95^(puis),0^puis_petit)
X=cbind(matrix(rnorm(n*(p-1)),ncol=(p-1)))
epsilon=rnorm(n)
beta0=2
Y=beta0+X%*%beta+epsilon



# this functions gives a covariate matrix with gaussian entries
# with covariance of the covariates equal to r^(|i-j|) 
# as this is a standard example for LASSO benchmark 
covariates.matrix <- function(n, p, r=0.5) {
  V <- matrix(0, p, p) 
  for(i in 1:p) {
    for(j in 1:p) {
      V[i, j] <- r^(abs(i-j))
    }	
  }
  eig <- eigen(V)
  sqroot <- eig$vectors %*% diag(sqrt(eig$values)) %*% t(eig$vectors)
  matrix(rnorm(n*p), n, p) %*% sqroot
}