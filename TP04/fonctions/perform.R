
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
      #print(param)
      # estimation des zTest
      zTestcalcule = ad.val(param, Xtst)$pred
    }
    # taux d'erreur de test
    erreurTest = c(erreurTest, 1 - sum(ztst == zTestcalcule) / ntst)
  }
  
  resultat <- erreurTest
  
  resultat
}

scriptSepareACP <- function(t, X, z, function_name, acpDim = 3) {
  erreurTest = c() # error ratio on the testing set
  for (i in 1 : t) {
    data.separated = separ1(X, z)
    Xapp <- data.separated$Xapp
    zapp <- data.separated$zapp
    Xtst <- data.separated$Xtst
    ztst <- data.separated$ztst
    ntst = length(ztst)
    Xmoy <- c()
    Xscale <- c()
    
    for( j in 1 : ncol(Xapp))
    {
      Xmoy <- cbind(Xmoy, mean(Xapp[,j]))
      Xscale <- cbind(Xscale, sd(Xapp[,j]))
      Xtst[,j] <- Xtst[,j] - Xmoy[j]
      Xtst[,j] <- Xtst[,j]/ Xscale[j]
    }
    
    # diagonalisation
    Xapp.acp <- Xapp
    Xapp.acp=scale(Xapp.acp,scale=T) #centre la matrice
    Xapp.v<-cov.wt(Xapp.acp,method="ML")$cov 
    Xapp.eigen<-eigen(Xapp.v)
    
    # print cumulate inertie expliquée
    Xapp.values <- as.matrix(Xapp.eigen$values)
    pourcentVars <- apply(Xapp.values, 2, function(x) cumsum((x/sum(Xapp.values))*100))
    #plot(pourcentVars, ylab = '%I', main = "Pourcentage cumulé d'inertie expliqué par axes factoriels")
    #abline(h=50, col = 'purple')
    
    # calcul de composantes principales
    Xapp.U<-Xapp.eigen$vectors
    Xapp.C<-as.matrix(as.matrix(Xapp.acp)%*%Xapp.U) #calcul de la matrice des composantes principales
    # Xapp dans 1 er cube factoriel
    Xapp <- Xapp.C[,1:16]
    
    #fit test data
    Xtst.acp <- Xtst
    #Xtst.acp=scale(Xtst.acp,scale=F) #centre la matrice
    Xtst.C <-as.matrix(Xtst.acp)%*%Xapp.U
    # Xtst dans 1 er cube facrtoriel
    Xtst <- as.data.frame(Xtst.C[,1:16])
    (Xapp.eigen$values[1]/sum(Xapp.eigen$values))*100 # 1 axe = 95.60
    ((Xapp.eigen$values[1]+Xapp.eigen$values[2])/sum(Xapp.eigen$values))*100 #  2 axes = 99.79
    ((Xapp.eigen$values[1]+Xapp.eigen$values[2]+Xapp.eigen$values[3])/sum(Xapp.eigen$values))*100 # 3 axes = 99.99 
    
    #scatterplot3d(Xtst.C[,1], Xtst.C[,2], Xtst.C[,3], xlab = "Uapp_1", ylab = "Uapp_2", zlab = "Uapp_3",color=c("red","blue")[ztst], main="Xtest représenté dans premier cube factoriel")
    
    modele.app <- match.fun(function_name)
    if (function_name == "tree.app") {
      tr <- tree.app(Xapp, zapp)
      rpart.plot(tr)
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
      #print(param)
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



onSquare = function(X) {
  X2 <- X
  for (p in 1:(dim(X)[2]-1))
  {
    for (q in (p+1):dim(X)[2])
    {
      X2 <- cbind(X2, X[,p]*X[,q])
    }
  }
  for (p in 1:dim(X)[2])
  {
    X2 <- cbind(X2, X[,p]^2)
  }
  X2
}