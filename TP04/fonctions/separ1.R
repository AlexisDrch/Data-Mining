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
