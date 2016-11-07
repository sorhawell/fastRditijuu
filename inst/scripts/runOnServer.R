#Soren Welling 2016
#cluster backend script to execute a do.call-like call
#Can be used as a seamless cluster backend for local execution

#this script loads an export object (list)
# first cell a function (or its name)
# second ćell a char vector of needed packages (can be empty)
# remaining cells are arguments passed to function by do.call
# function is executed and return value is saved to .Tempout.rda

print("loading export")
load(file="./.Tempexp.rda")

print(export)

print("check, install and load packages")
list.of.packages <- export$packages
#from http://stackoverflow.com/questions/4090169
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos="http://cran.us.r-project.org")
for(aPackage in list.of.packages) require(package=aPackage,character.only = T)


# #check if fastRditijuu is installed on server
# if(!"fastRditijuu" %in% installed.packages()[,"Package"]) {
#   print("this seems to be first time, installing fastRditijuu on server")
#   if("devtools" %in% installed.packages()[,"Package"]) {
#     #devtools is available use that
#     require(devtools)
#     devtools::install_github("sorhawell/fastRditijuu")
#   } else {
#     #if devtools is not installed, don't mind we use minimal ghit instead
#     install.packages("ghit",repos="http://cran.us.r-project.org")
#     require("ghit")
#     ghit::install_github("sorhawell/fastRditijuu")
#   }
# }
# require("fastRditijuu")

if("package:BatchJobs" %in% search()) {
  #source this wrapper
  doBatchJob = function(
    X,FUN,packages=c(),max.nodes=24,globalVar=list(),...) {
    #BatchJobs package only needs to be loaded on master node, not on slaves

    #split jobs in to one pile for each node
    cluster.nodes = min(length(X),max.nodes) #no more nodes required than jobs
    jobArrays = suppressWarnings(split(X,1:cluster.nodes))
    splitKey  = unlist(suppressWarnings(split(1:length(X),1:cluster.nodes)),use.names = FALSE)
    invSplitKey = match(1:length(X),splitKey)
    #suppress warning, when nodes get ueven amount of jobs.

    #Use BatchJobs package to create
    reg <- makeRegistry(id="testBatchJobs",packages=if(length(packages)) packages else character(0L),
                        src.files = if(length(globalVar)) "./.slaveSource.R" else character(0L))
    save(reg,file="testBatchJobs-files/reg.rda")
    batchMap(reg,fun=lapply,X=jobArrays,use.names=T,more.args = c(list(FUN=FUN,...)))
    submitJobs(reg)
    waitForJobs(reg)
    out = unlist(loadResults(reg,1:cluster.nodes),recursive = FALSE)
    out = out[invSplitKey] #re-order jobs by inverted splitKey
    removeRegistry(reg,ask="no")
    return(out)
  }

  if(length(export$globalVar)) {
    save(export$globalVar,file=".globalVar.rda")
  #write source file for slaves to load global variables
    writeLines( text =
"#source slave
if('globalVar.rda' %in% list.files()) {
  load('./.globalVar.rda')
} else {
  print('friendly warning: globalVar file not found, try execute slave without')
}", con = "./.slaveSource.R")

  }

}


print("following packages loaded on master")
print(search())

print("do job")
out = with(export$globalVar,{
           print(ls())

           do.call(eval(export$what),export$arg,quote=T)
      })

print("save output")
saveRDS(out,file=".Tempout.rda")

print("Hello Master, this is HAL 9000. Work completed, returning to local.")
print(Sys.time())
