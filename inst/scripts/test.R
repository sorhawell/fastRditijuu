library("fastRditijuu")
mean(1:7)
do.call(mean,list(1:7))     #local

doClust(mean,list(c(1:7)),packages="roxygen2")  #on DTU cluster


#run function that refer to an external variable, include variable in scope
a=4
system.time({doClust(function(x) x+a ,list(c(1:7)),globalVar = list(a=a))})  #on DTU cluster


#if one dont care about transfer speed, one can just export enire global environment
a=4
b=3
doClust(function(x) x+a ,list(c(1:7)),globalVar = mget(ls()))  #on DTU cluster

#require(install if neccessary) package on server side
doClust(function(x) x ,list(c(1:7)),packages="AUC")  #on DTU cluster
doClust(function(x) remove.packages("AUC"),42)  #on DTU cluster

#test locally

#doBatchJobs is the internal handler for any iteration call
#it performs it calls the BatchJobs package write jobs to qsub and collect results afterwards
#doBatchJobs on top of BatchJobs has its own job array handler (more jobs on save slave), and
#the global environment of the slave can be modified. Useful for lapply(lply) call, that scope
#out of the supplied function.


out = fastRditijuu:::doBatchJob(1:250,sum,max.nodes = 3)

out = lapply(1:10, function(x) x+1)
out = lply(X=1:250,FUN=sum,max.nodes=4,local=T)
out = lply(X=1:250,FUN=function(x) x+1,max.nodes=2)
out = lply(X=1:250,FUN=function(x) x+1,max.nodes=80)

a=1
out = lply(X=1:250,FUN=function(x) x+a,max.nodes=2,globalVar = list(a=a))

#try run single model

model = doClust("randomForest",list(x=X,y=y,ntree=1000),packages="randomForest")  #on DTU cluster
library(randomForest)
print(model)
preds = predict(model,X)


#try run 48 models
library("fastRditijuu")
library(randomForest)
X = data.frame(replicate(24,rnorm(500)))
y = with(X,X1*X2+X3)


myFunc =  function(mtry) tail(randomForest(x=X,y=y,mtry=mtry)$rsq,1)

#run 10 times local
out = lapply(X=rep(1:5,2),fun=myFunc)
plot(rep(1:5,2),out)

#run 1000 times on cluster
out = lply(X=rep(1:24,50),fun=myFunc,
           #that extra stuff you need to mention
           globalVar=list(X=X,y=y),  #gotta mention global variables
           packages=c("randomForest"), #mention packages to be installed and/or loaded
           user="sowe",                #mention user name, remember to set up private/public key
           max.nodes = 80)             #optional limit to certain number of nodes
                                       # ... do not set higher than 80.


plot(rep(1:24,50),unlist(out),col="#23232313")

out = fastRditijuu:::doBatchJob(1:15,function(mtry) mtry+1,
                                X=X,y=y,packages="randomForest")
lapply(out,class)
