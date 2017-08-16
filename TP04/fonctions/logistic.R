
log.app <- function(Xapp, zapp, intr, epsi)
{
  n <- dim(Xapp)[1]
  p <- dim(Xapp)[2]
  
  Xapp <- as.matrix(Xapp)
  
  # on ajoute la coordonnée à l'origine (1)
  if (intr == T)
  {
    Xapp <- cbind(rep(1,n),Xapp)
    p <- p + 1
  }
  
  # one-hot encoding
  targ <- matrix(as.numeric(zapp),nrow=n)
  targ[which(targ==2),] <- 0
  
  tXap <- t(Xapp)
  
  # beta_0 = vecteur null
  beta <- matrix(0,nrow=p,ncol=1)
  bold <- matrix(1,nrow=p,ncol=1)
  
  conv <- F
  iter <- 0
  while (norm(beta-bold)>=epsi)
  {
    iter <- iter + 1
    # old beta
    bold <- beta
    #print(beta)
    prob <- postprob(beta, Xapp)
    
    MatW <- diag(apply(prob, 1, function(x) x*(1-x)))
    # prepare new beta
    beta <-beta <- bold+ginv(tXap%*%MatW%*%Xapp)%*%tXap%*%(targ-t(prob))
    # verifie si seuil de convergence est dépassé
    print(norm(beta-bold))
    if (norm(beta-bold)<epsi)
    {
      print("DONE")
      #conv <- T
     # break
    }
  }
  
  prob <- postprob(beta, Xapp)
  out <- NULL
  out$beta <- beta
  out$iter <- iter
  #out$logL <- 
  
  out
}


log.val <- function(beta, Xtst)
{
  m <- dim(Xtst)[1]
  p <- dim(beta)[1]
  pX <- dim(Xtst)[2]
  
  Xtst <- as.matrix(Xtst)
  
  if (pX == (p-1))
  {
    Xtst  <- cbind(rep(1,m),Xtst)
  }
  # on calcule les probas conditionelles des deux classes 
  prob <- cbind(postprob(beta, Xtst), 1-postprob(beta, Xtst))
  pred <- max.col(prob)
  
  out <- NULL
  out$prob <- prob
  out$pred <- pred
  
  return(out)
}



postprob <- function(beta, X){
  X <- as.matrix(X)
  beta <- as.matrix(beta)
  prob <- exp(t(beta) %*% t(X))/(1+exp(t(beta) %*% t(X)))
}
