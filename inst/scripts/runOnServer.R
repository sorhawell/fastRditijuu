#Soren Welling 2016
#cluster backend script to execute a do.call-like call
#Can be used as a seamless cluster backend for local execution

#this script loads an export object (list)
# first cell a function (or its name)
# second ćell a char vector of needed packages (can be empty)
# remaining cells are arguments passed to function by do.call
# function is executed and return value is saved to .Tempout.rda

print("Printing from R backend on server...")

#get Tempdir.backend file path
args = commandArgs(trailingOnly=TRUE)
Tempdir.backend = args[1]
print(args)
# cat("set backend work directory to: ",Tempdir.backend,"\n")
setwd(Tempdir.backend)

print("Loading exported variables ...")
load(file="Tempexp.rda")

print("Check, install and load packages...")
list.of.packages <- export$packages
#from http://stackoverflow.com/questions/4090169
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos="http://cran.us.r-project.org")
for(aPackage in list.of.packages) require(package=aPackage,character.only = T)
print("following packages loaded on master")
print(search())


##preparation for starting slave nodes, only when using BatchJobs package
if("package:BatchJobs" %in% search()) {
  #source doBatchJob function on server side
  #this wrapper is handling job-arrays (to split a list of jobs in separate job lists)
  #...and global variables (loads an global variable state)
  #...and combines individual slave node results into list of all results
  doBatchJob = function(
    X,FUN,packages=c(),max.nodes=24,globalVar=list(),...) {
    #BatchJobs package only needs to be loaded on master node, not on slaves

    #split jobs into one job-list for each node
    cluster.nodes = min(length(X),max.nodes) #no more nodes required than jobs
    jobArrays = suppressWarnings(split(X,1:cluster.nodes))
    splitKey  = unlist(suppressWarnings(split(1:length(X),1:cluster.nodes)),use.names = FALSE)
    invSplitKey = match(1:length(X),splitKey)
    #suppress warning, when nodes get ueven amount of jobs.

    #Meeseeks box(Rick & Morty reference) executer of job-lists
    wrapB = function(X,FUN,...) {
      print("I'm Mr Meeseeks(a torque/PBS cluster slave), look at me!!!")
      cat("Oh geee, my work directory is",getwd(),"\n")

      #load attach global vars on slave machine
      if(file.exists('globalVar.rda')) {
        print("global variables detected, loading...")
        load('globalVar.rda')
        attach(globalVar)
      }

      #run array of jobs on this slave
      print("Master: Mr Meeseeks, please iterate this job-list with lapply")
      print("Mr Meeseeks: 'Sure can do!!'")
      out = lapply(X,function(X) do.call(eval(FUN),list(X,...)),...)
      print("Mr Meeseeks: Job completed, pooofff!!")
      return(out)
    }

    cluster.functions <- makeClusterFunctionsTorque("~/.fastRditijuu_qsub.tmpl")
    reg <- makeRegistry(id="testBatchJobs",packages=if(length(packages)) packages else character(0L))
    save(reg,file="testBatchJobs-files/reg.rda")
    batchMap(reg,fun=wrapB,X=jobArrays,use.names=T,more.args = c(list(FUN=FUN,...)))
    submitJobs(reg)
    waitForJobs(reg)
    out = unlist(loadResults(reg,1:cluster.nodes),recursive = FALSE)
    out = out[invSplitKey] #re-order jobs by inverted splitKey
    removeRegistry(reg,ask="no")
    return(out)
  }

#place two config files in user root

#   writeLines(text =
# "# Torque/PBS cluster
# cluster.functions <- makeClusterFunctionsTorque('~/.fastRditijuu_qsub.tmpl')",
# con = "~/.BatchJobs.R"
# )

  writeLines(text =
"#!/bin/sh

#PBS -l nodes=1:ppn=1
#PBS -l walltime=00:08:00

echo $CPUTYPE

#PBS -N <%= job.name %>
## merge standard error and output
#PBS -j oe
## direct streams to our logfile
#PBS -o <%= log.file %>


## Run R:
## we merge R output with stdout from PBS, which gets then logged via -o option
R CMD BATCH --no-save --no-restore '<%= rscript %>' /dev/stdout
",
             con = "~/.fastRditijuu_qsub.tmpl"
)

}

#save state of global variables, which can be read individually by slave nodes
if(length(export$globalVar)) {
  cat("save file with these globalVariables",ls() ,"\n")
  globalVar = export$globalVar
  save(globalVar,file="globalVar.rda")
  attach(export$globalVar)
  print("following global variables exported to server side:")
  print(ls())
} else {
  print("no global variables exported to server side, use globalVar")
}

print("calling the function")
out = do.call(eval(export$what),export$arg,quote=T)

print("save output")
saveRDS(out,file="Tempout.rda")

print("Hello Master, this is HAL 9000. Work completed, returning to local.")
print(Sys.time())
