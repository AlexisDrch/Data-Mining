setwd("/Users/alexisdurocher/Documents/YouTheaSea/Compiegne2016/P17/SY09/TP04/")
source("fonctions/anadisc.R")
source("fonctions/logistic.R")
source("fonctions/mvdnorm.R")
source("fonctions/prob.ad.R")
source("fonctions/prob.log.R")
source("fonctions/prob.log2.R")
source("fonctions/separ1.R")
library(MASS)
donn <- read.csv("donnees/Synth1-40.csv")
summary(donn)
X <- donn[,1:2]
z<- donn[,3]
data <- separ1(X, z)
Xapp <- as.matrix(data$Xapp)
zapp <- as.matrix(data$zapp)
Xtst <- as.matrix(data$Xtst)
ztst <- as.matrix(data$ztst)

# test  adq sur X
param <- adq.app(X, z)
prob.ad(param, X, z, c(0.4, 0.6, 0.7))


# test  adl sur X
param <- adl.app(X, z)
prob.ad(param, X, z, c(0.4, 0.6, 0.7))

# test  nba sur X
param <- nba.app(X, z)
prob.ad(param, X, z, c(0.4, 0.6, 0.7))

# test log sur X
param <- log.app(X, z, T, 1e-5)
prob.log(param$beta, X, z,  c(0.4, 0.6, 0.7))

#test log2 sur X 
param <- log.app.quadra(X, z, 0, 1e-5) 
prob.log2(param$beta,X, z,  c(0.4, 0.6, 0.7))


