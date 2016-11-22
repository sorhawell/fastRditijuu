#Soren Welling 2016 123
#cluster backend script to execute a do.call-like call
#Can be used as a seamless cluster backend for local execution

#this script loads an export object (list)
# first cell a function (or its name)
# second ćell a char vector of needed packages (can be empty)
# remaining cells are arguments passed to function by do.call
# function is executed and return value is saved to .Tempout.rda


#This script has step 1 to 9
print("Printing from R backend on server...")
print(Sys.time())

##1 set work directory
#get Tempdir.backend file path
args = commandArgs(trailingOnly=TRUE) #get wd from command arg
Tempdir.backend = args[1] #first argument is wd
print(args)
# cat("set backend work directory to: ",Tempdir.backend,"\n")
setwd(Tempdir.backend)
print(paste0("this is wd: ",Tempdir.backend))

##2a load exported variables (all the data/instructions from user computer)
print("Loading exported variables ...")
load(file="Tempexp.rda")

##2b If this script is set to run async, it will submit itself to qsub
#To avoid infinite recursive calling, this script will call itself with the async_stop arg
#Therefore, when this script is run through qsub it will execute and return,
#... and not submit itself again

#If second commandArg is "async_stop" unsubmit
if(!is.null(args[2]) && !is.na(args[2]) && args[2][[1]] == "async_stop") {
  export$async = FALSE
} #turn off submitting itself to qsub

##3 Before loading BatchJobs packages... some config files must be placed in user root
#Place one config file in user root, BatchJobs package will load this config file when loaded
#write qsub file
#3a
#files to submit this script to qsub
if(export$async) {
  writeLines(text = paste0(
    "#!/bin/sh

    #PBS -l nodes=",export$qsub.nodes,":ppn=",export$qsub.proc,"
    #PBS -l walltime=",export$qsub.walltime,"

    echo $CPUTYPE

    #PBS -N <%= job.name %>
    ## merge standard error and output
    #PBS -j oe
    ## direct streams to our logfile
    #PBS -o <%= log.file %>


    ## Run R:
    ##runserver script will submit itself to qsub",
    "\n",
    "cd ", Tempdir.backend,
    "\n",
    "R CMD BATCH --no-save --no-restore '--args ",Tempdir.backend ," async_stop' runOnServer.R /dev/stdout",

    "\n \n"),con = "./fastRditijuu_qsub_async.sh")
} else {
  #write config files for BatchJob package
  if("BatchJobs" %in% export$packages) {
  print("saving file to ")
  writeLines(text =
               "# Torque/PBS cluster
             cluster.functions <- makeClusterFunctionsTorque('./fastRditijuu_qsub_BatchJobs.sh')",
             con = "~/.BatchJobs.R"
  )

  #place one config file in tmp folder, BatchJobs pacakge will use this config file to form qsub's
  writeLines(paste0(text =
"#!/bin/sh
#PBS -l nodes=",export$qsub.nodes,":ppn=",export$qsub.proc,"
#PBS -l walltime=",export$qsub.walltime,"
echo $CPUTYPE
#PBS -N <%= job.name %>
## merge standard error and output
#PBS -j oe
## direct streams to our logfile
#PBS -o <%= log.file %>
## Run R:
## we merge R output with stdout from PBS, which gets then logged via -o option
R CMD BATCH --no-save --no-restore '<%= rscript %>' /dev/stdout
"),con = './fastRditijuu_qsub_BatchJobs.sh'
  )
  }
}

##4 Handle packages, skip if async
if(!export$async) {
  print("Check, install and load packages...")
  list.of.packages <- export$packages
  #from http://stackoverflow.com/questions/4090169
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages,repos="http://cran.us.r-project.org")
  for(aPackage in list.of.packages) require(package=aPackage,character.only = T)
  print("following packages loaded on master")
  print(search())
}


##4b - Define a wrapper function doBatchJob only for BatchJobs
# preparation for starting slave nodes, only when using BatchJobs package
if(!export$async) {
  if("package:BatchJobs" %in% search() && export$async==FALSE) {
    #source doBatchJob function on server side
    #this wrapper is handling job-arrays (to split a list of jobs in separate job lists)
    #...and global variables (loads an global variable state)
    #...and combines individual slave node results into list of all results
    doBatchJob = function(X,FUN,packages=c(),max.nodes=24,globalVar=list(),...) {
      #BatchJobs package only needs to be loaded on master node, not on slaves

      #split jobs into one job-list for each node
      cluster.nodes = min(length(X),max.nodes) #no more nodes required than jobs
      jobArrays = suppressWarnings(split(X,1:cluster.nodes))
      splitKey  = unlist(suppressWarnings(split(1:length(X),1:cluster.nodes)),use.names = FALSE)
      invSplitKey = match(1:length(X),splitKey)
      #suppress warning, when nodes get ueven amount of jobs.

      #Meeseeks box(Rick & Morty reference) executer of job-lists
      wrapMeeseeks = function(X,FUN,...) {
        print("I'm Mr Meeseeks (a torque/PBS cluster slave), look at me!!!")
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

      #cluster.functions = makeClusterFunctionsTorque("./fastRditijuu_qsub.sh") #this file is created below

      #ensure empty package list are character(0) and remove BatchJobs package, as it is not needed on slaves
      packages = if(length(packages)) packages[packages!="BatchJobs"] else character(0L)
      reg <- makeRegistry(id="testBatchJobs",packages=if(length(packages)) packages else character(0L))
      save(reg,file="testBatchJobs-files/reg.rda")
      batchMap(reg,fun=wrapMeeseeks,X=jobArrays,use.names=T,more.args = c(list(FUN=FUN,...)))
      submitJobs(reg)
      print("jobs have been submitted")
      waitForJobs(reg)
      print("All jobs has finished")
      out = unlist(loadResults(reg,1:cluster.nodes),recursive = FALSE)
      out = out[invSplitKey] #re-order jobs by inverted splitKey
      removeRegistry(reg,ask="no")
      return(out)
    }
  }
}

##6 handling global variables
if(!export$async && length(export$globalVar)) { #if any variables
  if("package:BatchJobs" %in% search()) {
    #if job in executed with BatchJobs, save globalVarible to separate file
    cat("Export these variables to slave nodes",names(export$globalVar) ,"\n")
    globalVar = export$globalVar
    save(globalVar,file="globalVar.rda")
    print(names(export$globalVar))
  } else {
    #if job is executed locally on master node in this R environment, attached global variables here.
    attach(export$globalVar)
    print("following global variables are attached to Master node environment:")
    print(names(export$globalVar))
  }
}

##7 - excution on master node
if(!export$async) {
  print("calling the function")
  out = do.call(eval(export$what),export$arg,quote=T)
} else {
  ##runserver script will submit itself to qsub
  jobNumber = system("qsub fastRditijuu_qsub_async.sh",intern=TRUE)
  print("yep we got to here")
  out = list(backend.tmp = getwd(),jobNumber = jobNumber)
  class(out) = "ticket"
}

##8 - saving
print("save output")
saveRDS(out,file="Tempout.rda")

##9 say goodbye
print("Hello Master, this is HAL 9000. Work completed, returning to local.")
print(Sys.time())

if(export$async==FALSE)
writeLines("Hello Master, this is HAL 9000. Work completed, returning to local.",
             con="job_completed.txt")
