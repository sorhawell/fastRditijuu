library("fastRditijuu")
mean(1:7)
do.call(mean,list(1:7))     #local

doClust(mean,1:10,"sowe",keyPath = "C:/Users/sowe/.ssh/fastr2.ppk")



X = data.frame(replicate(6,rnorm(1500)))
y = apply(X,1,sum)
system.time({
  out = doClust(function(x) mclapply(x,function(x) tail(randomForest(X,y)$rsq,1),mc.cores=4),
  1:24,"sowe",keyPath = "C:/Users/sowe/.ssh/fastr2.ppk",packages = c("parallel","randomForest"),
  globalVar = list(X=X,y=y))
})
c

#hello cluster
out = system("timeout 5 ssh sowe@login.gbar.dtu.dk ls",intern=T)
a=2
out = doClust(function(x) x+a ,1:8, globalVar = list(a=a),user="sowe",async = T,qsub.walltime="00:10:00")
result = getResult(out,user="sowe",verbose = T)
cleanUp(user="sowe")

ticket = doClust(function(x) {Sys.sleep(120);'blop'} ,1:8,
                 globalVar = list(a=a),user="sowe",async = T,qsub.walltime="00:00:10")
result = getResult(ticket,user="sowe",verbose = T)


ticket2 = doClust(function(x) {Sys.sleep(120);'blop'} ,1:8,
                 globalVar = list(a=a),user="sowe",async = T,walltime="00:00:55")
result = getResult(ticket2,user="sowe",verbose = T)

a=4
doClust(function(x) x+a,list(c(1:7)),
        user="sowe", globalVar = list(a=a), packages="AUC",async=FALSE)  #on DTU cluster
doClust(remove.packages,"AUC",user="sowe",async = F)  #on DTU cluster

out = lply(X=1:250,FUN=function(x) x+1,max.nodes=80,user="sowe")

#run function that refer to an external variable, include variable in scope
a=4
system.time({doClust(function(x) x+a ,list(c(1:7)),globalVar = list(a=a),user="sowe")})  #on DTU cluster


#if one dont care about transfer speed, one can just export enire global environment
a=4
b=3
doClust(function(x) x+a ,list(c(1:7)),globalVar = mget(ls()),user="sowe")  #on DTU cluster

#require(install if neccessary) package on server side
doClust(function(x) x ,list(c(1:7)),packages="AUC",user="sowe")  #on DTU cluster
doClust(function(x) remove.packages("AUC"),42,user="sowe")  #on DTU cluster

#test locally

#doBatchJobs is the internal handler for any iteration call
#it performs it calls the BatchJobs package write jobs to qsub and collect results afterwards
#doBatchJobs on top of BatchJobs has its own job array handler (more jobs on save slave), and
#the global environment of the slave can be modified. Useful for lapply(lply) call, that scope
#out of the supplied function.


out = fastRditijuu:::doBatchJob(1:250,sum,max.nodes = 3)

out = lapply(1:10, function(x) x+1)
out = lply(X=1:250,FUN=sum,max.nodes=4,local=T)
out = lply(X=1:250,FUN=function(x) x+1,max.nodes=20,user="sowe",host="grid01.compute.dtu.dk",Rscript = F)
cleanUp("sowe")
a=42
ticket = lply(X=1:250, FUN=function(x) x+a,
              max.nodes=40, user="sowe", async=T,globalVar = list(a=a))
result = getResult(ticket,user="sowe",verbose = TRUE)

out = lply(X=1:250,FUN=function(x) x+1,max.nodes=12,user="sowe")

a=1
out = lply(X=1:250,FUN=function(x) x+a,max.nodes=2,globalVar = list(a=a),user="sowe",async=T)
result = getResult(out,user="sowe",verbose = F)

a=1
out = lply(X=list(a=2,b=3,c=5),FUN=function(x) x+a,max.nodes=2,globalVar = list(a=a),user="sowe",async=T)
result = getResult(out,user="sowe",verbose = T)



#try run iterate 1200 RF models asynchonously with lply
library("fastRditijuu")
X = data.frame(replicate(24,rnorm(2500)))
y = apply(X[,1:3],1,sum)
X[] = X[1:3]
X[] = X[] + rnorm(500*24)

#run 1000 times on cluster
ticket = lply(X=rep(1:24,2),function(mtry) tail(randomForest(x=X,y=y,mtry=mtry)$rsq,1),
           #that extra stuff you need to mention
           globalVar=list(X=X,y=y),  #gotta mention global variables
           packages=c("randomForest"), #mention packages to be installed and/or loaded
           user="sowe",                #mention user name, remember to set up private/public key
           #host="grid01.compute.dtu.dk",Rscript=T,
           keyPath ="c:/Users/sowe/.ssh/fastr2.ppk",
           async = T,nCores=1,
           max.nodes = 4,
           qsub.walltime = "00:15:00",
           qsub.proc = 1)             #optional limit to certain number of nodes
                                       # ... do not set higher than 80.
result = getResult(ticket,user="sowe",verbose = TRUE, keyPath ="c:/Users/sowe/.ssh/fastr2.ppk")
plot(rep(1:24,2),unlist(result),col="#23232313",log="x")
cleanUp("sowe")
