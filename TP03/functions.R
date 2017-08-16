
###CEUC.APP###
ceuc.app <- function(Xapp, zapp)
{
  #On part du tableau individus-variables Xapp.
  #On applique pour chaque colonne (chaque attribut) la fonction moyenne en distinguant les 
  #deux classes differentes renseignees dans le vecteur zapp.
  apply(Xapp, 2, by, zapp, mean)
}


###CEUC.VAL###
ceuc.val <- function(mu, Xtst)
{ 
  # Détermine les distances centre de grav / individus test
  a = distXY(mu,Xtst)
  # garde le centre de grav le plus proche 
  b = apply(a,2,which.min)
  b
}


###DISTXY###
distXY <- function(X, Y)
{
  nx <- dim(X)[1]
  ny <- dim(Y)[1]
  h.x <- rowSums(X^2)
  h.y <- rowSums(Y^2)
  ones.x <- rep(1,nx)
  ones.y <- rep(1,ny)
  D2xy <- h.x%*%t(ones.y) - 2*X %*% t(Y) + ones.x%*%t(h.y)
}



###FRONT.CEUC###
front.ceuc <- function(X, z, mu, discretisation=50)
{
  deltaX <- (max(X[,1])-min(X[,1]))/discretisation
  deltaY <- (max(X[,2])-min(X[,2]))/discretisation
  minX <- min(X[,1])-deltaX
  maxX <- max(X[,1])+deltaX
  minY <- min(X[,2])-deltaY
  maxY <- max(X[,2])+deltaY
  
  # grille d'affichage 
  grilleX <- seq(from=minX,to=maxX,by=deltaX)
  naffX <- length(grilleX)
  grilleY <- seq(from=minY,to=maxY,by=deltaY)
  naffY <- length(grilleY)
  grille <- cbind(rep.int(grilleX,times=rep(naffY,naffX)),rep(grilleY,naffX))
  
  # calcul des valeurs de la fonction 
  valf <- ceuc.val(mu, grille)
  plot(X, col=c("red","green","blue","magenta","orange")[z], asp=1)
  contour(grilleX, grilleY, matrix(valf,nrow=naffX,byrow=T), add=T, drawlabels=FALSE, levels=1.5)
}

###KPPV.TUNE###
kppv.tune <- function(Xapp,zapp,Xval,zval,nppv)
{
  min = 1
  tmp = kppv.val(Xapp,zapp,1,Xval)
  tmp = as.vector(tmp)
  erreur = 0
  # recupere l'erreur de reference (Kopt = 1)
  for (j in 1:length(tmp)){
    if (tmp[j]!=zval[j]){
      erreur = erreur +1
    }
  }
  erreur = erreur / length(zval)
  # calcul erreur avec autre valeur de k
  erreur_min <- erreur
  for (i in nppv){
    tmp = c()
    tmp = kppv.val(Xapp,zapp,i,Xval)
    tmp = as.vector(tmp)
    for (j in 1:length(tmp)){
      if (tmp[j]!=zval[j]){
        erreur = erreur +1
      }
    }
    erreur = erreur / length(zval)
    print(erreur)
    # conserve erreur min et valeur de k opt
    if(erreur <= erreur_min){
      erreur_min <- erreur
      min <- i
    }
  }
  return (min)
}

###KPPV.VAL###
kppv.val <- function(Xapp,zapp,K,Xtst)
{
  # matrice de toutes distances entre indiv de App et Test
  a = distXY(as.matrix(Xapp),as.matrix(Xtst))
  tmp = apply(a,2,order)
  dist = tmp
  for (i in 1:ncol(tmp)){
    for (j in 1:nrow(tmp)){
      # remplace distances par classes correspondantes
      dist[j,i]=zapp[tmp[j,i]]
    }
  }
  # on ne cherche que les K meilleures distances (devenues classes)
  dist <- dist[1:K,]
  if (K>1){
    # round renverra la classe majoritaire parmis les K
    # (en arrondissant la moyenne des K classes voisines de Xtst_j)
    dist = round(apply(dist,2,mean))
  }
  return (dist)
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

filtre <- function(Xapp, zapp)
{
  magicNb <- 2
  Xapp <- as.matrix(Xapp)
  # Nombre d'individus de la classe magicNb
  nb1 <- sum(zapp==magicNb)
  
  # Initialise les variables de résultats
  zres <- rep(magicNb, nb1)
  Xres <- matrix(0, nrow=nb1, ncol=ncol(Xapp))
  k <- 1
  
  for (i in 1:length(zapp)) {
    if (zapp[i]==magicNb) {
      Xres[k, ] <- Xapp[i, ]
      k <- k + 1
    }
  }
  return (list(Xapp=Xres, zapp=zres))
}

###SCRIPT SEPARE pour euclidien###
scriptSepare <- function(t, X, z) {
  erreurApprentissage = c() # error ratio on the learning set
  erreurTest = c() # error ratio on the testing set
  erreurTest2 = c() #error ratio on App Beta
   for (i in 1 : t) {
    # Data separation.
    data.separated = separ1(X, z)
    Xapp <- data.separated$Xapp
    zapp <- data.separated$zapp
    Xtst <- data.separated$Xtst
    ztst <- data.separated$ztst
    n = length(zapp)
    a<-filtre(Xtst,ztst)
    Xtst<-a$Xapp
    ztst<-a$zapp
    
    ntst = length(ztst)
    
    mu = ceuc.app(Xapp, zapp)
    zAppcalcule = ceuc.val(mu, Xapp)
    zTestcalcule = ceuc.val(mu, Xtst)
    
    erreurApprentissage = c(erreurApprentissage, 1 - sum(zapp == zAppcalcule) / n)
    erreurTest = c(erreurTest, 1 - sum(ztst == zTestcalcule) / ntst)
  }
  
  resultat <- NULL
  resultat$erreurApprentissage <- erreurApprentissage
  resultat$erreurTest <- erreurTest
  resultat$erreurTest2 <- erreurTest2
  
  resultat
}

#fonction pour calculer le taux d'erreur ponctuel


###ESTIME TAUX ERREUR : intervalle de confiance de Student ###
estimeTauxErreur = function(t, erreurApprentissage, erreurTest) {
  epsilonBarreApp = mean(erreurApprentissage)
  # variance empirique
  setoile2 = sum((erreurApprentissage - epsilonBarreApp) ^ 2) / (t - 1)
  setoile = sqrt(setoile2)
  left = epsilonBarreApp - 2.093 * (setoile / sqrt(t))
  right = epsilonBarreApp + 2.093 * (setoile / sqrt(t))
  
  res <- NULL
  res$leftApp <- left
  res$rightApp <- right
  
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

###SCRIPT SEPARE KPPV###
scriptSepareKppv <- function(t, X, z) {
  erreurApprentissage = c()
  erreurTest = c()
  for (i in 1 : t) {
    # Data separation.
    donn.sep = separ2(X, z)
    Xapp <- as.matrix(donn.sep$Xapp)
    zapp <- donn.sep$zapp
    Xval <- as.matrix(donn.sep$Xval)
    zval <- donn.sep$zval
    Xtst <- as.matrix(donn.sep$Xtst)
    ztst <- donn.sep$ztst
    n = length(zapp)
    a<-filtre(Xtst,ztst)
    Xtst<-a$Xapp
    ztst<-a$zapp
    
    K = kppv.tune(Xapp, zapp, Xval, zval, c(2, 3, 4, 5, 6, 7, 8, 9, 10))
    
    zpredicted = kppv.val(Xapp, zapp, K, Xapp)
    erreurApprentissage = c(erreurApprentissage, 1 - sum(zapp == zpredicted) / n)
    zpredicted = kppv.val(Xapp, zapp, K, Xtst)
    erreurTest = c(erreurApprentissage, 1 - sum(ztst == zpredicted) / n)
  }
  
  res <- NULL
  res$erreurApprentissage <- erreurApprentissage
  res$erreurTest <- erreurTest
  
  res
}

###SEPAR1#####
separ1 <- function(X, z)
{
  g <- max(z)
  
  Xapp <- NULL
  zapp <- NULL
  Xtst <- NULL
  ztst <- NULL
  
  for (k in 1:g)
  {
    indk <- which(z==k)
    ntot <- length(indk)
    napp <- round(ntot*2/3)
    ntst <- ntot-napp
    
    itot <- sample(indk)
    iapp <- itot[1:napp]
    itst <- itot[(napp+1):ntot]
    
    Xapp <- rbind(Xapp, X[iapp,])
    zapp <- c(zapp, z[iapp])
    Xtst <- rbind(Xtst, X[itst,])
    ztst <- c(ztst, z[itst])
  }
  
  res <- NULL
  res$Xapp <- Xapp
  res$zapp <- zapp
  res$Xtst <- Xtst
  res$ztst <- ztst
  
  res
}


###SEPAR2###
separ2 <- function(X, z)
{
  g <- max(z)
  
  Xapp <- NULL
  zapp <- NULL
  Xval <- NULL
  zval <- NULL
  Xtst <- NULL
  ztst <- NULL
  
  for (k in 1:g)
  {
    indk <- which(z==k)
    ntot <- length(indk)
    napp <- round(ntot/2)
    nval <- round(ntot/4)
    ntst <- ntot-napp-nval
    
    itot <- sample(indk)
    iapp <- itot[1:napp]
    ival <- itot[(napp+1):(napp+nval)]
    itst <- itot[(napp+nval+1):ntot]
    
    Xapp <- rbind(Xapp, X[iapp,])
    zapp <- c(zapp, z[iapp])
    Xval <- rbind(Xval, X[ival,])
    zval <- c(zval, z[ival])
    Xtst <- rbind(Xtst, X[itst,])
    ztst <- c(ztst, z[itst])
  }
  
  res <- NULL
  res$Xapp <- Xapp
  res$zapp <- zapp
  res$Xval <- Xval
  res$zval <- zval
  res$Xtst <- Xtst
  res$ztst <- ztst
  
  res
}

front.kppv <- function(X, z, K, discretisation=50)
{
  deltaX <- (max(X[,1])-min(X[,1]))/discretisation
  deltaY <- (max(X[,2])-min(X[,2]))/discretisation
  minX <- min(X[,1])-deltaX
  maxX <- max(X[,1])+deltaX
  minY <- min(X[,2])-deltaY
  maxY <- max(X[,2])+deltaY
  
  # grille d'affichage 
  grilleX <- seq(from=minX,to=maxX,by=deltaX)
  naffX <- length(grilleX)
  grilleY <- seq(from=minY,to=maxY,by=deltaY)
  naffY <- length(grilleY)
  grille <- cbind(rep.int(grilleX,times=rep(naffY,naffX)),rep(grilleY,naffX))
  
  # calcul des valeurs de la fonction 
  valf <- kppv.val(X, z, K, grille)
  plot(X, col=c("red","green","blue","magenta","orange")[z], asp=1)
  contour(grilleX, grilleY, matrix(valf,nrow=naffX,byrow=T), add=T, drawlabels=FALSE, levels=1.5)
}
