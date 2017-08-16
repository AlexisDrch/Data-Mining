library(rpart)
#library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)

tree.app <- function(Xapp, zapp) {
  df =as.data.frame(cbind(as.factor(zapp), Xapp))
  names(df)[1] = "label"
  names(df)[2] = "V1"
  names(df)[3] = "V2"
  names(df)[4] = "V3"
  names(df)[5] = "V4"
  names(df)[6] = "V5"
  names(df)[7] = "V6"
  names(df)[8] = "V7"
  names(df)[9] = "V8"
  names(df)[10] = "V9"
  names(df)[11] = "V10"
  names(df)[12] = "V11"
  names(df)[13] = "V12"
  names(df)[14] = "V13"
  names(df)[15] = "V14"
  names(df)[16] = "V15"
  names(df)[17] = "V16"
  df$label = as.factor(df$label)
  tree = rpart(label ~ ., data=df, method="class", control=rpart.control(minsplit=0.0001, cp=0, xval=20))
  treeOptimal = prune(tree, cp=tree$cptable[which.min(tree$cptable[,4]),1])
  treeOptimal
}

tree.val <- function(ad, Xtst) {
  predicted = predict(ad, Xtst, type="class")
  predicted
}

rforest.app <- function(Xapp, zapp) {
  df = cbind(zapp, Xapp)
  names(df)[1] = "label"
  df$label = as.factor(df$label)
  rforest = randomForest(label ~ ., data=df, importance = T)
  rforest
}

rforest.val <- function(ad, Xtst) {
  predicted = predict(ad, Xtst, type="response")
  print(predicted)
  predicted
}