setwd("/Users/alexisdurocher/Documents/YouTheaSea/Compiegne2016/P17/SY09/TP03/")

source("fonctionsFinalesLaura.R")

synth2_1000 <- read.csv("donnees/Synth2-1000.csv")
synth1_40 <- read.csv("donnees/Synth1-40.csv")
synth1_100 <- read.csv("donnees/Synth1-100.csv")
synth1_500 <- read.csv("donnees/Synth1-500.csv")
synth1_1000 <- read.csv("donnees/Synth1-1000.csv")



#####1.2.1##############
#######################


### QUESTION 1###

#synth1_40 <- read.table("Synth1-40.txt", header=F)
X_40 <- synth1_40[,1:2]
z_40 <- synth1_40[,3]
E_40 = estimateur(X_40,z_40)
E_40

#synth1_100 <- read.table("Synth1-100.txt", header=F)
X_100 <- synth1_100[,1:2]
z_100 <- synth1_100[,3]
E_100 = estimateur(X_100,z_100)
E_100

#synth1_500 <- read.table("Synth1-500.txt", header=F)
X_500 <- synth1_500[,1:2]
z_500 <- synth1_500[,3]
E_500 = estimateur(X_500,z_500)
E_500

#synth1_1000 <- read.table("Synth1-1000.txt", header=F)
X_1000 <- synth1_1000[,1:2]
z_1000 <- synth1_1000[,3]
E_1000 = estimateur(X_1000,z_1000)
E_1000

###QUESTION 2###

#1er jeu

t=20
X <- synth1_40[,1:2]
z <- synth1_40[,3]
n = length(z)
errors = scriptSepare(t, X, z)
errorConfidenceCeuc = estimeTauxErreur(t, errors$erreurApprentissage, errors$erreurTest)
errors
moyErrApp <- mean(errors$erreurApprentissage)
moyErrTst <- mean(errors$erreurTest)
moyErrApp
moyErrTst
errorConfidenceCeuc

#2eme jeu

t=20
X <- synth1_100[,1:2]
z <- synth1_100[,3]
n = length(z)
errors = scriptSepare(t, X, z)
errorConfidenceCeuc = estimeTauxErreur(t, errors$erreurApprentissage, errors$erreurTest)
errors
moyErrApp <- mean(errors$erreurApprentissage)
moyErrTst <- mean(errors$erreurTest)
moyErrApp
moyErrTst
errorConfidenceCeuc

#3eme jeu

t=20
X <- synth1_500[,1:2]
z <- synth1_500[,3]
n = length(z)
errors = scriptSepare(t, X, z)
errorConfidenceCeuc = estimeTauxErreur(t, errors$erreurApprentissage, errors$erreurTest)
errors
moyErrApp <- mean(errors$erreurApprentissage)
moyErrTst <- mean(errors$erreurTest)
moyErrApp
moyErrTst
errorConfidenceCeuc

#4eme jeu
X <- synth1_1000[,1:2]
z <- synth1_1000[,3]
n = length(z)
errors = scriptSepare(t, X, z)
errorConfidenceCeuc = estimeTauxErreur(t, errors$erreurApprentissage, errors$erreurTest)
errors
moyErrApp <- mean(errors$erreurApprentissage)
moyErrTst <- mean(errors$erreurTest)
moyErrApp
moyErrTst
errorConfidenceCeuc

##################################### Question 3 ##########################################

#donn <- read.table("Synth1-40.txt", header=F)
X <- synth1_40[,1:2]
z <- synth1_40[,3]
data_sep <- separ1(X,z)
Xapp <- data_sep$Xapp
zapp <- data_sep$zapp
Xtst <- data_sep$Xtst
ztst <- data_sep$ztst
n = length(zapp)
t=20
Kopt = kppv.tune(Xapp,zapp,Xapp,zapp,1:10)
Kopt


#donn <- read.table("Synth1-100.txt", header=F)
X <- synth1_100[,1:2]
z <- synth1_100[,3]
data_sep <- separ1(X,z)
Xapp <- data_sep$Xapp
zapp <- data_sep$zapp
Xtst <- data_sep$Xtst
ztst <- data_sep$ztst
n = length(zapp)
t=20
Kopt = kppv.tune(Xapp,zapp,Xapp,zapp,1:10)


#donn <- read.table("Synth1-500.txt", header=F)
X <- synth1_500[,1:2]
z <- synth1_500[,3]
data_sep <- separ1(X,z)
Xapp <- data_sep$Xapp
zapp <- data_sep$zapp
Xtst <- data_sep$Xtst
ztst <- data_sep$ztst
n = length(zapp)
t=20
Kopt = kppv.tune(Xapp,zapp,Xapp,zapp,1:10)


#donn <- read.table("Synth1-1000.txt", header=F)
X <- synth1_1000[,1:2]
z <- synth1_1000[,3]
data_sep <- separ1(X,z)
Xapp <- data_sep$Xapp
zapp <- data_sep$zapp
Xtst <- data_sep$Xtst
ztst <- data_sep$ztst
n = length(zapp)
t=20
Kopt = kppv.tune(Xapp,zapp,Xapp,zapp,1:10)

##################################### Question 4 ##########################################


#1er jeu
X <- synth1_40[,1:2]
z <- synth1_40[,3]
t = 20
n = length(z)
errorsKppv = scriptSepareKppv(t, X, z)
errorConfidenceKppv = estimeTauxErreur(t, errorsKppv$erreurApprentissage, errorsKppv$erreurTest)
errorsKppv
moyApp <- mean(errorsKppv$erreurApprentissage)
moyApp
moyTest <- mean(errorsKppv$erreurTest)
moyTest
errorConfidenceKppv


#2eme jeu
X <- synth1_100[,1:2]
z <- synth1_100[,3]
t = 20
n = length(z)
errorsKppv = scriptSepareKppv(t, X, z)
errorConfidenceKppv = estimeTauxErreur(t, errorsKppv$erreurApprentissage, errorsKppv$erreurTest)
errorsKppv
moyApp <- mean(errorsKppv$erreurApprentissage)
moyApp
moyTest <- mean(errorsKppv$erreurTest)
moyTest
errorConfidenceKppv

#3eme jeu
X <- synth1_500[,1:2]
z <- synth1_500[,3]
t = 20
n = length(z)
errorsKppv = scriptSepareKppv(t, X, z)
errorConfidenceKppv = estimeTauxErreur(t, errorsKppv$erreurApprentissage, errorsKppv$erreurTest)
errorsKppv
moyApp <- mean(errorsKppv$erreurApprentissage)
moyApp
moyTest <- mean(errorsKppv$erreurTest)
moyTest
errorConfidenceKppv

#4eme jeu
X <- synth1_1000[,1:2]
z <- synth1_1000[,3]
t = 20
n = length(z)
errorsKppv = scriptSepareKppv(t, X, z)
errorConfidenceKppv = estimeTauxErreur(t, errorsKppv$erreurApprentissage, errorsKppv$erreurTest)
errorsKppv
moyApp <- mean(errorsKppv$erreurApprentissage)
moyApp
moyTest <- mean(errorsKppv$erreurTest)
moyTest
errorConfidenceKppv

#####1.2.2##############
#######################

###Question 1###

X_2_1000 <- synth2_1000[,1:2]
z_2_1000 <- synth2_1000[,3]
E_2_1000 = estimateur(X_2_1000,z_2_1000)
E_2_1000

###Question 2###
#1er jeu

t=20
X <- synth2_1000[,1:2]
z <- synth2_1000[,3]
n = length(z)
errors = scriptSepare(t, X, z)
errorConfidenceCeuc = estimeTauxErreur(t, errors$erreurApprentissage, errors$erreurTest)
errors
moyErrApp <- mean(errors$erreurApprentissage)
moyErrTst <- mean(errors$erreurTest)
moyErrApp
moyErrTst
errorConfidenceCeuc

###Question 3###
X <- synth2_1000[,1:2]
z <- synth2_1000[,3]
n = length(z)
data_sep <- separ1(X,z)
Xapp <- data_sep$Xapp
zapp <- data_sep$zapp
Xtst <- data_sep$Xtst
ztst <- data_sep$ztst

mu <- ceuc.app(Xapp, zapp)
front.ceuc(Xapp, zapp, mu, 1000)

###Question 4###
X <- synth2_1000[,1:2]
z <- synth2_1000[,3]
n = length(z)
t = 20
errorsKppv = scriptSepareKppv(t, X, z)
errorConfidenceKppv = estimeTauxErreur(t, errorsKppv$erreurApprentissage, errorsKppv$erreurTest)
errorsKppv
moyApp <- mean(errorsKppv$erreurApprentissage)
moyApp
moyTest <- mean(errorsKppv$erreurTest)
moyTest
errorConfidenceKppv
# tracer
data_sep <- separ2(X,z)
Xapp <- data_sep$Xapp
zapp <- data_sep$zapp
Xtst <- data_sep$Xtst
ztst <- data_sep$ztst
Xval <- data_sep$Xval
zval <- data_sep$zval
Kopt <- kppv.tune(Xapp, zapp, Xval, zval, c(1,3,5,7,9))
Kopt
front.kppv(Xapp, zapp, Kopt, 1000)



#####1.2.3##############
#######################

####PIMA###
###Question 2###
Pima <- read.csv("donnees/Pima.csv")
X <- Pima[, 1 : 7]
z <- Pima[, 8]
E_Pima = estimateur(X,z)
E_Pima

t = 20
errors = scriptSepare(t, X, z)
errorConfidenceCeuc = estimeTauxErreur(t, errors$erreurApprentissage, errors$erreurTest)
errors
moyErrApp <- mean(errors$erreurApprentissage)
moyErrTst <- mean(errors$erreurTest)
moyErrApp
moyErrTst
errorConfidenceCeuc

####QUESTION 4####
X <- Pima[, 1 : 7]
z <- Pima[, 8]
n = length(z)
t = 20
errorsKppv = scriptSepareKppv(t, X, z)
errorConfidenceKppv = estimeTauxErreur(t, errorsKppv$erreurApprentissage, errorsKppv$erreurTest)
errorsKppv
moyApp <- mean(errorsKppv$erreurApprentissage)
moyApp
moyTest <- mean(errorsKppv$erreurTest)
moyTest
errorConfidenceKppv

#####BREASTCANCER###
###Question 2###
Breastcancer <- read.csv("donnees/Breastcancer.csv")
X <- Breastcancer[, 1 : 9]
z <- Breastcancer[, 10]
E_Breast = estimateur(X,z)
E_Breast
t = 20

errors = scriptSepare(t, X, z)
errorConfidenceCeuc = estimeTauxErreur(t, errors$erreurApprentissage, errors$erreurTest)
errors
moyErrApp <- mean(errors$erreurApprentissage)
moyErrTst <- mean(errors$erreurTest)
moyErrApp
moyErrTst
errorConfidenceCeuc


####QUESTION 4####
X <- Breastcancer[, 1 : 9]
z <- Breastcancer[, 10]
t = 20
errorsKppv = scriptSepareKppv(t, X, z)
errorConfidenceKppv = estimeTauxErreur(t, errorsKppv$erreurApprentissage, errorsKppv$erreurTest)
errorsKppv
n = length(z)
moyApp <- mean(errorsKppv$erreurApprentissage)
moyApp
moyTest <- mean(errorsKppv$erreurTest)
moyTest
errorConfidenceKppv