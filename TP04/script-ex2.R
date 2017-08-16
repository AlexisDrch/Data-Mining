
setwd("/Users/alexisdurocher/Documents/YouTheaSea/Compiegne2016/P17/SY09/TP04/")
source("fonctions/anadisc.R")
source("fonctions/logistic-2.R")
source("fonctions/mvdnorm.R")
source("fonctions/prob.ad.R")
source("fonctions/prob.log.R")
source("fonctions/prob.log2.R")
source("fonctions/separ1.R")
source("tree.R")

library(MASS)


scriptSepare <- function(t, X, z, function_name) {
  erreurTest = c() # error ratio on the testing set
  for (i in 1 : t) {
    # Data separation.
    data.separated = separ1(X, z)
    Xapp <- data.separated$Xapp
    zapp <- data.separated$zapp
    Xtst <- data.separated$Xtst
    ztst <- data.separated$ztst
    ntst = length(ztst)
    
    
    modele.app <- match.fun(function_name)
    if (function_name == "tree.app") {
      tr <- tree.app(Xapp, zapp)
      rpart.plot(tr)
        
      summary(tr)
      zTestcalcule <- tree.val(tr, Xtst)
    } else if (function_name == "log.app" || function_name == "log2.app") {
      # cas de la regression
      # apprentissage du modèle
      param <- modele.app(Xapp, zapp, T, 1e-5)
      # estimation des zTest
      zTestcalcule = log.val(param$beta, Xtst)$pred
    } else if (function_name == "log.app.quadra")
    {
      # cas de la regression
      # apprentissage du modèle
      param <- modele.app(Xapp, zapp, F, 1e-5)
      # estimation des zTest
      zTestcalcule = log.val.quadra(param$beta, Xtst, param$nbVar)$pred
    }
    else {
      # cas de l'ad
      # apprentissage du modèle 
      param <- modele.app(Xapp, zapp)
      print(param)
      # estimation des zTest
      zTestcalcule = ad.val(param, Xtst)$pred
    }
    # taux d'erreur de test
    erreurTest = c(erreurTest, 1 - sum(ztst == zTestcalcule) / ntst)
  }
  
  resultat <- erreurTest
  
  resultat
}

###ESTIMATEUR ###
estimateur <- function(X,z)
{
  n = dim(X)[1]
  p = dim(X)[2]
  
  X_class1 <- X[which(z == 1),]
  X_class2 <- X[which(z == 2),]
  
  n1 <- dim(X_class1)[1]
  n2 <- dim(X_class2)[1]
  
  #calculate pi
  pi1 <- n1 / n
  pi2 <- n2 / n
  
  #calculate mu
  mu1<-colMeans(X_class1)
  mu2<-colMeans(X_class2)
  
  sigma1 <- matrix(0,nrow=p,ncol=p)
  sigma2 <- matrix(0,nrow=p,ncol=p)
  sigma1<-var(X_class1) 
  sigma2<-var(X_class2)
  
  res <- list()
  res$pi1 <- pi1
  res$pi2 <- pi2
  res$mu1 <- mu1
  res$mu2 <- mu2
  res$sigma1 <- sigma1
  res$sigma2 <- sigma2
  
  return (res)	
}

###ESTIME TAUX ERREUR : intervalle de confiance de Student ###
estimeTauxErreur = function(t, erreurTest) {
  res <- NULL
  epsilonBarreTst = mean(erreurTest)
  # variance empirique
  setoile2 = sum((erreurTest - epsilonBarreTst) ^ 2) / (t - 1)
  setoile = sqrt(setoile2)
  left = epsilonBarreTst - 2.093 * (setoile / sqrt(t))
  right = epsilonBarreTst + 2.093 * (setoile / sqrt(t))
  
  res$leftTst <- left
  res$rightTst <- right
  
  res
}

# Synth1-1000
data1 <- read.csv("donnees/Synth1-1000.csv")
X <- data1[,1:2]
z<- data1[,3]
t<- 20
param <- estimateur(X, z)
plot(X, bg=c("red","blue")[z], pch=c(21,22)[z],  main = "Données Synth1-1000") # visualisation


# Synth2-1000
data2 <- read.csv("donnees/Synth2-1000.csv")
X <- data2[,1:2]
z<- data2[,3]
t<- 20
param <- estimateur(X, z)
plot(X, bg=c("red","blue")[z], pch=c(21,22)[z],  main = "Données Synth2-1000") # visualisation


# Synth3-1000
data3 <- read.csv("donnees/Synth3-1000.csv")
X <- data3[,1:2]
z<- data3[,3]
t<- 20
param <- estimateur(X, z)
plot(X, bg=c("red","blue")[z], pch=c(21,22)[z],  main = "Données Synth3-1000") # visualisation

#Pima
Donn <- read.csv("donnees/Pima.csv", header=T)
X <- Donn[,1:7]
z <- Donn[,8]
t<- 100
param <- estimateur(X, z)

# ACP sur pima
corr.acp <- Donn
corr.acp=scale(corr.acp,scale=F)#centre la matrice
corr.v<-cov.wt(corr.acp,method="ML")$cov#renvoi la matrice de covariance
corr.v.d<-eigen(corr.v)#renvoi les valeurs propres et les vecteurs propres
(corr.v.d$values[1]/sum(corr.v.d$values))*100#pourcentage d'inertie expliqué de la premiere valeur propre
vecteur.p<-corr.v.d$vectors
C<-as.matrix(corr.acp)%*%vecteur.p#calcul de la matrice des composantes principales
plot(C, bg=c("red","blue")[z], pch=c(21,22)[z], xlab="Axe1",ylab="Axe2",axes=T, main = "BCW représenté dans le 1er plan factoriel") # visualisation
summary(C)
param <- estimateur(C[,1:2], z)


#B-w-c
Donn <- read.csv("donnees/bcw.csv", header=T)
X <- Donn[,1:9]
z <- Donn[,10]
t<- 100


source("fonctions/perform.R")

# adq
erreurTest <- scriptSepare(t, X, z, "adq.app")
muErreurTest <- mean(erreurTest)
icErreurTest <- estimeTauxErreur(t, erreurTest)
# adl 
erreurTest <- scriptSepare(t, X, z, "adl.app")
muErreurTest <- mean(erreurTest)
icErreurTest <- estimeTauxErreur(t, erreurTest)
# nba 
erreurTest <- scriptSepare(t, X, z, "nba.app")
muErreurTest <- mean(erreurTest)
icErreurTest <- estimeTauxErreur(t, erreurTest)
# log reg
erreurTest <- scriptSepare(t, X, z, "log.app")
muErreurTest <- mean(erreurTest)
icErreurTest <- estimeTauxErreur(t, erreurTest)
# log reg 2
grille <- onSquare(X)
X2 <- as.matrix(grille)
erreurTest <- scriptSepare(t, X2, z, "log.app")
muErreurTest <- mean(erreurTest)
icErreurTest <- estimeTauxErreur(t, erreurTest)
# tree 
erreurTest <- scriptSepare(t, X, z, "tree.app")
muErreurTest <- mean(erreurTest)
icErreurTest <- estimeTauxErreur(t, erreurTest)
summary(tr)




