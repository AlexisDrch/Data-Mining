setwd("/Users/alexisdurocher/Documents/YouTheaSea/Compiegne2016/P17/SY09/TP04/")
source("fonctions/anadisc.R")
source("fonctions/logistic.R")
source("fonctions/mvdnorm.R")
source("fonctions/prob.ad.R")
source("fonctions/prob.log.R")
source("fonctions/prob.log2.R")
source("fonctions/separ1.R")
library(MASS)
library(scatterplot3d)
df <- read.csv("donnees/spam.csv")
summary(df)
X <- df[,1:58]
z<- df[,59]

## NETTOYAGE DONNEES 
#1 ere étape : ACP
X.acp <- X
X.acp=scale(X.acp,scale=T) #centre la matrice
X.v<-cov.wt(X.acp,method="ML")$cov #renvoi la matrice de covariance
X.eigen<-eigen(X.v) #renvoi les valeurs propres et les vecteurs propres

# pourcentage Inertie expliquée
(X.eigen$values[1]/sum(X.eigen$values))*100 #  1 er axe
((X.eigen$values[1]+X.eigen$values[2])/sum(X.eigen$values))*100 #  1 er plan (2 axes)
((X.eigen$values[1]+X.eigen$values[2]+X.eigen$values[3])/sum(X.eigen$values))*100 #  1 er plan 3D 
# : > 99% on choisira les 3 premiers axes du coup

vecteur.p<-X.eigen$vectors
C<-as.matrix(X.acp)%*%vecteur.p #calcul de la matrice des composantes principales

# 3D Scatterplot
scatterplot3d(C[,1],C[,2],C[,3],color=c("red","blue")[z], main="3D Scatterplot")

# conclusion du plot : outlier obvious : to remove (bcp trop bas sur C3)
df.acp <- data.frame(C[,1],C[,2],C[,3])
df.acp.c <- subset(df.acp, df.acp[,3]>-4000)
z.c <- subset(z, df.acp[,3]>-4000)
# better distributed
scatterplot3d(df.acp.c[,1],df.acp.c[,2],df.acp.c[,3],color=c("red","blue")[z.c], main="3D Scatterplot")

# tu dois pouvoir faire des trucs cools avec ça now :)
t = 20


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


# adq
erreurTest <- scriptSepare(t, df.acp.c, z.c, "adq.app")
muErreurTest <- mean(erreurTest)
icErreurTest <- estimeTauxErreur(t, erreurTest)