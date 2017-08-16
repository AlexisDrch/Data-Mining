setwd("/Users/alexisdurocher/Documents/YouTheaSea/Compiegne2016/P17/SY09/TP04/")
source("fonctions/anadisc.R")
#source("fonctions/logistic.R")
source("fonctions/logistic-2.R")
source("fonctions/mvdnorm.R")
source("fonctions/prob.ad.R")
source("fonctions/prob.log.R")
source("fonctions/prob.log2.R")
source("fonctions/separ1.R")
source("fonctions/perform.R")
source("tree.R")
library(MASS)
library(scatterplot3d)
df <- read.csv("donnees/spam.csv")
summary(df)
X <- df[,2:58]
z<- df[,59]
t <- 20

# Pb de discrimination, juge de paix = résultat de classifieur
# 1. Sur dimension p, avec individus centré-réduit
  # attention : garder centrage (moy de Xapp et sigma App) pour effectuer meme translation sur Xtst
  # permet de tasser les classes et prendre en considération les étendues 
  # = > nous donne erreur de réference // taux à essayer d'améliorer (Tref)

# 2. ACP
  # attention : ACP est outil de visualisation, non de discrimination.
  # ACP sur Xapp (pas sur X) !! 
  # Axes factoriels représentatifs ne sont pas forcement ceux significativement discriminants.
  
    # a . Commencer sans réduction 
    # --> 2 axes / 3 axes arrivent déjà à 99%. Un peu violent. (donne U)

    # pourcentage Inertie expliquée
    #(Xapp.eigen$values[1]/sum(Xapp.eigen$values))*100 # 1 axe = 95.60
    #((Xapp.eigen$values[1]+Xapp.eigen$values[2])/sum(Xapp.eigen$values))*100 #  2 axes = 99.79
    #((Xapp.eigen$values[1]+Xapp.eigen$values[2]+Xapp.eigen$values[3])/sum(Xapp.eigen$values))*100 # 3 axes = 99.99 

    # Trouver Ctest = Xtst * U
    # Tester erreur quand même et voir si T trouvé est < Tref (juge de paix).
  
    # b. Reduire matrice
    # ACP : besoin de plus d'axes pour atteindre 99%. (donne U)
    # Tester erreur : < Tref ? (juge de paix)

# 3. Arbre sur Xapp
  # Tracer importances des variables en pourcentage par variable.
  # garder celles avant que importance converge.
  # Tester erreur : < Tref ? (juge de paix)
  # 26, 46, 27, 23, 19, 5, 24, 57, 25, 56, 21, 55, 16, 7, 53, 52
   
  #df = cbind(z, X)
  #names(df)[1] = "label"
  #df$label = as.factor(df$label)
  #rforest = randomForest(label ~ ., data=df, importance = T)
  #rforest.mGini <- as.matrix(sort(rforest$importance[,4]))
  #plot(rforest.mGini,col= "blue",  pch = 19, cex = 1, lty = "solid", lwd = 2, main = "Influence de chaque variable sur la baisse de l'indice de Gini")
  #text(rforest.mGini, ylab ="MeanDecreaseGini", ylim = c(10, 100), labels = rownames(rforest.mGini), cex= 0.7, pos=1, col= "orange")
  #abline(h = 38, col = "red")


# Data separation


#plot(Xtst[,1:2], color=c("red","blue")[ztst], main="3D Scatterplot")
# 3D Scatterplot
#scatterplot3d(Xtst.C[,1], Xtst.C[,2], Xtst.C[,3], color=c("red","blue")[ztst], main="3D Scatterplot")

### 2. ACP  ###
  # adq
  erreurTest <- scriptSepareACP(t, X, z, "adq.app",16)
  erreurTest <- erreurTest[!is.na(erreurTest)]
  muErreurTest <- mean(erreurTest)
  muErreurTest
  icErreurTest <- estimeTauxErreur(t, erreurTest)
  icErreurTest
  # adl 
  erreurTest <- scriptSepareACP(t, X, z, "adl.app",16)
  erreurTest <- erreurTest[!is.na(erreurTest)]
  muErreurTest <- mean(erreurTest)
  muErreurTest
  icErreurTest <- estimeTauxErreur(t, erreurTest)
  icErreurTest
  # nba 
  erreurTest <- scriptSepareACP(t, X, z, "nba.app",16)
  erreurTest <- erreurTest[!is.na(erreurTest)]
  muErreurTest <- mean(erreurTest)
  muErreurTest
  icErreurTest <- estimeTauxErreur(t, erreurTest)
  icErreurTest
  # log reg
  erreurTest <- scriptSepareACP(t, X, z, "log.app",16)
  erreurTest <- erreurTest[!is.na(erreurTest)]
  muErreurTest <- mean(erreurTest)
  muErreurTest
  icErreurTest <- estimeTauxErreur(t, erreurTest)
  icErreurTest
  # log reg 2
  X2 <- onSquare(X)
  X2 <- as.matrix(X2)
  erreurTest <- scriptSepareACP(3, X2, z, "log.app",16)
  erreurTest <- erreurTest[!is.na(erreurTest)]
  muErreurTest <- mean(erreurTest)
  muErreurTest
  icErreurTest <- estimeTauxErreur(t, erreurTest)
  icErreurTest
  # tree

  erreurTest <- scriptSepareACP(t, X, z, "tree.app",16)
  muErreurTest <- mean(erreurTest)
  muErreurTest
  erreurTest <- erreurTest[!is.na(erreurTest)]
  icErreurTest <- estimeTauxErreur(t, erreurTest)
  icErreurTest
  #summary(tr) 
  
  # conclusion du plot : outlier obvious : to remove (bcp trop bas sur C3)
  #df.acp <- data.frame(C[,1],C[,2],C[,3])
  #df.acp.c <- subset(df.acp, df.acp[,3]>-4000)
  #z.c <- subset(z, df.acp[,3]>-4000)
  # better distributed
  #scatterplot3d(df.acp.c[,1],df.acp.c[,2],df.acp.c[,3],color=c("red","blue")[z.c], main="3D Scatterplot")
  
 
    Xdata <- cbind(X[,26],X[,46],X[,27],X[,23],X[,19],X[,5],X[,24],X[,57],X[,25],X[56],X[,21],X[,55],X[,16],X[,7],X[,53],X[,52])
    zdata <- z
# evaluation des performances
  # adq
  erreurTest <- scriptSepare(t, Xdata, zdata, "adq.app")
  erreurTest <- erreurTest[!is.na(erreurTest)]
  muErreurTest <- mean(erreurTest)
  muErreurTest
  icErreurTest <- estimeTauxErreur(t, erreurTest)
  icErreurTest
  # adl 
  erreurTest <- scriptSepare(t, Xdata, zdata, "adl.app")
  erreurTest <- erreurTest[!is.na(erreurTest)]
  muErreurTest <- mean(erreurTest)
  muErreurTest
  icErreurTest <- estimeTauxErreur(t, erreurTest)
  icErreurTest
  # nba 
  erreurTest <- scriptSepare(t, Xdata, zdata, "nba.app")
  erreurTest <- erreurTest[!is.na(erreurTest)]
  muErreurTest <- mean(erreurTest)
  muErreurTest
  icErreurTest <- estimeTauxErreur(t, erreurTest)
  icErreurTest
  # log reg
  erreurTest <- scriptSepare(t, Xdata, zdata, "log.app")
  erreurTest <- erreurTest[!is.na(erreurTest)]
  muErreurTest <- mean(erreurTest)
  muErreurTest
  icErreurTest <- estimeTauxErreur(t, erreurTest)
  icErreurTest
  # log reg 2
  X2 <- onSquare(Xdata)
  X2 <- as.matrix(X2)
  erreurTest <- scriptSepare(3, X2, zdata, "log.app")
  erreurTest <- erreurTest[!is.na(erreurTest)]
  muErreurTest <- mean(erreurTest)
  muErreurTest
  icErreurTest <- estimeTauxErreur(t, erreurTest)
  icErreurTest
  # tree 
  erreurTest <- scriptSepare(t, Xdata, zdata, "tree.app")
  muErreurTest <- mean(erreurTest)
  muErreurTest
  erreurTest <- erreurTest[!is.na(erreurTest)]
  icErreurTest <- estimeTauxErreur(t, erreurTest)
  icErreurTest
  summary(tr)

