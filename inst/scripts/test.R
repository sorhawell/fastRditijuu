library("fastRditijuu")
mean(1:7)
do.call(mean,list(1:7))     #local

doClust(mean,list(c(1:7)),packages="roxygen2")  #on DTU cluster


#run function that refer to an external variable, include variable in scope
a=4
doClust(function(x) x+a ,list(c(1:7)),packages="roxygen2",globalVar = list(a=a))  #on DTU cluster

#if one dont care about transfer speed, one can just export enire global environment
a=4
b=3
doClust(function(x) x+a ,list(c(1:7)),packages="roxygen2",globalVar = mget(ls()))  #on DTU cluster


doClust(mean,list(c(1:7)),packages="randomForest")  #on DTU cluster

readPrint()                 #get print

#test locally
out = fastRditijuu:::doBatchJob(1:250,sum,max.nodes = 3)
out = lapply(1:10, function(x) x+1)
out = lply(X=1:250,FUN=sum,max.nodes=4,local=T)
out = lply(X=1:250,FUN=function(x) x+1,max.nodes=2)
out = lply(X=1:250,FUN=function(x) x+1,max.nodes=80)

a=1
out = lply(X=1:250,FUN=function(x) x+a,max.nodes=4,globalVar = list(a=1))

#try run single model
X = data.frame(replicate(15,rnorm(1000)))
y = with(X,X1*X2+X3)
model = doClust("randomForest",list(x=X,y=y,ntree=1000),packages="randomForest")  #on DTU cluster
library(randomForest)
print(model)
preds = predict(model,X)


#try run 48 models
out = lply(1:5,function(mtry) {
  cat("does y exist?", exists("y"))
  print(ls())
  #randomForest(x=X2,y=y,mtry=mtry)
  },
           globalVar=list(X2=X,y=y),
           packages=c("randomForest"),
           user="sowe",local=F)
out = fastRditijuu:::doBatchJob(1:15,function(mtry) mtry+1,
                                X=X,y=y,packages="randomForest")

