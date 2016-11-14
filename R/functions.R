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

  #server call#1 create tempoary directory on backend
  print("call server... make temp dir,")
  hostString = paste0(user,"@",host) #make ssh host string
  tempDirCall = paste("ssh",hostString,"'source /etc/profile; mktemp -d ~/tmp/XXXXXXXXXXXX'")
  Tempdir.backend = system(tempDirCall,intern = TRUE)

  #save variables to Rdata file in temp directory on local machine
  export = c(what=list(substitute(what)),packages=list(packages),
             arg=list(arg),globalVar=list(globalVar),Tempdir.backend=list(Tempdir.backend))
  Tempdir.frontend = system("mktemp -d /tmp/XXXXXXXXXXXX",intern = TRUE) #temp directory on local machin
  varFileName = "/Tempexp.rda"
  varPath.frontend = paste0(Tempdir.frontend,varFileName)
  cat("frontend Tempdir ",Tempdir.frontend,"\n")
  cat("backend  Tempdir ",Tempdir.backend ,"\n")
  cat("save variables,")
  save(export, file=varPath.frontend)

  #server call#2, push variables file to server
  varTransferCall = paste0("scp ",varPath.frontend," ",hostString,":",
                           Tempdir.backend,varFileName)
  cat(" transfer variables,")
  system(varTransferCall)

  #server call#3, export executable R script
  runFile = 'runOnServer.R'
  scriptPath = system.file(paste0("/scripts/",runFile),package="fastRditijuu")
  scriptTransferCall = paste0("scp ",scriptPath," ",hostString,":",Tempdir.backend,"/",runFile)
  cat(" transfer executable,")
  system(scriptTransferCall)

  #server call#4, execute script
  if(Rscript) program = "Rscript " else " R CMD BATCH "
  executeCall = paste0('ssh ',hostString,' " source /etc/profile ; ',
                       program,Tempdir.backend,"/",runFile," ",Tempdir.backend,'"')
  cat(" execute! \n")
  system(executeCall)

  #server call #5, retrieve results
  cat("returning from server... fetch results from server,")
  outFile = "Tempout.rda"
  retrieveCall = paste0("scp ", hostString,":",Tempdir.backend ,"/",outFile,
                                          "  ",Tempdir.frontend,"/",outFile)
  system(retrieveCall)
  cat("  read results,")
  out = readRDS(file=paste0(Tempdir.frontend,"/",outFile))
  cat(" delete local temp files,")
  system(paste0("rm -rf ",Tempdir.frontend))
  cat(" delete backend temp files")
  system(paste0("ssh ",hostString," rm -rf ",Tempdir.backend))

  cat(" Finito...\n")
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
  wrapB = function(X,FUN,...) {
    #load attach global vars on slave machine
    load('.globalVar.rda')
    attach(globalVar)
    #run array of jobs on this slave
    lapply(X,function(X) do.call(eval(FUN),list(X,...)),...)
  }
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
                packages=c(),max.nodes=24,local=FALSE,globalVar=list(),...) {
  if(local) {
    require("BatchJobs")
    out = do.call(what=doBatchJob,args=list(X=X,FUN=FUN,max.nodes=max.nodes,
                                            globalVar=globalVar,...))
  } else {
    packages = unique(c(packages,"BatchJobs")) #include BatchJobs package on server
    out = doClust(doBatchJob,arg=list(X=X,FUN=FUN,max.nodes=max.nodes,
                                      packages=packages,...),
                  packages=packages,
                  Rscript=Rscript,
                  globalVar=globalVar)
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


#' Read Rout, useful if Rscript=FALSE to retrieve print
#' @param user server to read from
#' @param host  print file to read
#'
#' @return TRUE/1 if found and deleted
#' @export
#'
cleanUp = function(user="sowe",host='login.gbar.dtu.dk') {
  doClust(function() system("rm -rf testBatchJobs-files"),
          arg=list(),user=user,host=host)
}


