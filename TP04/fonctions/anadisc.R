adq.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	param <- NULL
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	for (k in 1:g)
	{
		indk <- which(zapp==k)
		Xappk <- Xapp[as.numeric(rownames(Xapp)) %in% indk,]

		param$MCov[,,k] <- cov(Xappk)
		param$mean[k,] <- colMeans(Xappk)
		param$prop[k] <- length(indk)/n
	}

	param
}

adl.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	param <- NULL
	MCov <- array(0, c(p,p))
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	for (k in 1:g)
	{
		indk <- which(zapp==k)
		Xappk <- Xapp[as.numeric(rownames(Xapp)) %in% indk,]

		MCov <- MCov + length(indk)*cov(Xappk)
		param$mean[k,] <- colMeans(Xappk)
		param$prop[k] <- length(indk)/n
	}
	MCov <- MCov/n
	

	for (k in 1:g)
	{
		param$MCov[,,k] <- MCov
	}

	param
}

nba.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	param <- NULL
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	for (k in 1:g)
	{
		indk <- which(zapp==k)
		Xappk <- Xapp[as.numeric(rownames(Xapp)) %in% indk,]

		param$MCov[,,k] <- diag(diag(cov(Xappk)))
		param$mean[k,] <- colMeans(Xappk)
		param$prop[k] <- length(indk)/n
	}

	param
}

ad.val <- function(param, Xtst)
{
	n <- dim(Xtst)[1]
	p <- dim(Xtst)[2]
	g <- length(param$prop)

	out <- NULL

	prob <- matrix(0, nrow=n, ncol=g)

	for (k in 1:g)
	{	
		prob[,k] <- (param$prop[k]*mvdnorm(Xtst, param$mean[k,], param$MCov[,,k]))
	}
	prob <- prob / rowSums(prob, na.rm = FALSE, dims = 1)
	pred <- max.col(prob)

	out$prob <- prob
	out$pred <- pred

	out
}
