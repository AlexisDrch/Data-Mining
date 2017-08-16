log.app <- function(Xapp, zapp, intr, epsi)
{
  n <- dim(Xapp)[1]
  p <- dim(Xapp)[2]
  
  Xapp <- as.matrix(Xapp)
  
  if (intr == T)
  {
    Xapp <- cbind(rep(1,n),Xapp)
    p <- p + 1
  }
  
  targ <- matrix(as.numeric(zapp),nrow=n)
  targ[which(targ==2),] <- 0
  tXap <- t(Xapp)
  
  beta <- matrix(0,nrow=p,ncol=1)
  
  conv <- F
  iter <- 0
  while (conv == F)
  {
    iter <- iter + 1
    bold <- beta
    
    prob <- postprob(beta, Xapp)
    MatW <- diag(as.vector(prob))*diag(as.vector(1-prob))
    
    beta <- bold+ginv(tXap%*%MatW%*%Xapp)%*%tXap%*%(targ-t(prob))
    
    if (norm(beta-bold)<epsi)
    {
      conv <- T
    }
  }
  
  prob <- postprob(beta, Xapp)
  out <- NULL
  out$beta <- beta
  out$iter <- iter
  out$logL <- sum(targ%*%log(prob)+(1-targ)%*%log(1-prob))
  
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
  
  prob <-  cbind(t(exp(t(beta) %*% t(Xtst))/(1+exp(t(beta) %*% t(Xtst)))),
                 t(1- exp(t(beta) %*% t(Xtst))/(1+exp(t(beta) %*% t(Xtst)))))
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
