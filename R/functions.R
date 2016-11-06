#' do.call with cluster as backend via ssh
#'
#' @param what function or name of function to execute
#' @param arg arguments for function
#' @param user username
#' @param host server (will connect to user@server)
#' @param packages char vector of package names (can be empty)
#' @param Rscript execute by Rscript or R CMD BATCH (former  only supported on gbar, ladder no verbose)
#'
#' @return value return by evaluated function
#' @export
#'
doClust = function(what,arg=list(),user='sowe',host='login.gbar.dtu.dk',packages=c(),
                   Rscript=TRUE,globalVar=list(),sourceSupportFunctions=T) {
  if(!is.list(arg)) arg = list(arg)
  export = c(what=list(substitute(what)),packages=list(packages),arg=list(arg),globalVar=list(globalVar))
  save(export, file="./.Tempexp.rda")

  #export variables
  varPath = "./.Tempexp.rda"
  hostString = paste0(user,"@",host)
  varTransferCall = paste0("scp ",varPath," ",hostString,":",varPath)
  system(varTransferCall)

  #export executer
  runFile = 'runOnServer.R'
  scriptPath = system.file(paste0("/scripts/",runFile),package="fastRditijuu")
  scriptTransferCall = paste0("scp ",scriptPath," ",hostString,":",runFile)
  system(scriptTransferCall)

  #execute script
  if(Rscript) program = "Rscript " else " R CMD BATCH "
  executeCall = paste0('ssh ',hostString,' ". /etc/profile ; ', program,runFile,'"')
  system(executeCall)

  #retrieve result
  outFile = ".Tempout.rda"
  retrieveCall = paste0("timeout 3s scp ", hostString,":",outFile," ",getwd(),"/",outFile)
  system(retrieveCall)
  out = readRDS(file=outFile)
  return(out)
}


#' Internal backend function to perform a qsub batchjob on server
#' @import BatchJobs
#' @return list with results of qsub batchjob
#' @export
#'
doBatchJob = function(X,FUN,packages=c(),max.nodes=24,globalVar=list(),...) {

  #BatchJobs package only needs to be loaded on master node, not on slaves


  #split jobs in to one pile for each node
  cluster.nodes = min(length(X),max.nodes) #no more nodes required than jobs
  jobArrays = suppressWarnings(split(X,1:cluster.nodes))
  splitKey  = unlist(suppressWarnings(split(1:length(X),1:cluster.nodes)),use.names = FALSE)
  invSplitKey = match(1:length(X),splitKey)
  #suppress warning, when nodes get ueven amount of jobs.

  #Use BatchJobs package to create
  reg <- makeRegistry(id="testBatchJobs",packages=if(length(packages)) packages else character(0L))
  save(reg,file="testBatchJobs-files/reg.rda")
  batchMap(reg,fun=lapply,X=jobArrays,use.names=T,
           more.args = list(FUN=FUN,...))
  submitJobs(reg)
  waitForJobs(reg)
  out = unlist(loadResults(reg,1:cluster.nodes),recursive = FALSE)
  out = out[invSplitKey] #re-order jobs by inverted splitKey
  removeRegistry(reg,ask="no")
  return(out)
}

#' Powerful lapply function with a qsub(Torque/PBS)-cluster as backend
#'
#' @param X object to map
#' @param FUN mapper function
#' @param user username for server
#' @param host server adress (will be combined as user@host)
#' @param Rscript if true verbose, however only supported by gbar, nor compute.cluster
#' @param packages required packages on server
#' @param max.nodes maximum nodes reuired, do not set higher than 80
#' @param local should the lply run on local computer (only for debugging/testing)
#' @param ...
#'
#' @return list of results
#' @export
lply = function(X, FUN, user="sowe", host="login.gbar.dtu.dk", Rscript=T,
                packages=c(),max.nodes=24,local=FALSE,export=list(),globalVar=list(),...) {
  if(local) {
    require("BatchJobs")
    out = do.call(what=doBatchJob,args=list(X=X,FUN=FUN,max.nodes=max.nodes,...))
  } else {
    packages = unique(c(packages,"BatchJobs")) #include BatchJobs package on server
    out = doClust(doBatchJob,arg=list(X=X,FUN=FUN,max.nodes=max.nodes,globalVar=globalVar,...),
                  packages=packages,
                  Rscript=Rscript)
  }
  names(out) = names(X) #restore list naming
  return(out)
}




#' Read Rout, useful if Rscript=FALSE to retrieve print
#'
#' @param hostString server to read from
#' @param printFile  print file to read
#'
#' @return lines of print
#' @export
#'
readPrint = function(hostString="sowe@login.gbar.dtu.dk",
                     printFile = 'runOnServer.Rout') {
  retrieveCall = paste0("scp ", hostString,":",printFile," ",getwd(),"/",'.tempPrint.Rout')
  system(retrieveCall,wait=F)
  #system('scp sowe@grid01.compute.dtu.dk:runOnServer.Rout	runOnServer.Rout')
    Sys.sleep(2)
    cat(paste(readLines('.tempPrint.Rout'),collapse="\n"))
    readLines('.tempPrint.Rout')
}



