#' do.call with cluster as backend via ssh
#'
#' @param what function or name of function to execute
#' @param arg arguments for function
#' @param user username
#' @param host server (will connect to user@server)
#' @param packages char vector of package names (can be empty)
#' @param Rscript execute by Rscript or R CMD BATCH (former  only supported on gbar, ladder no verbose)
#' @param async return after starting job? returned value is ticked to fetch result when job completed
#'
#' @return value return by evaluated function
#' @export
#'
doClust = function(what,arg=list(),user,host='login.gbar.dtu.dk',packages=c(),
                   Rscript=TRUE,globalVar=list(),sourceSupportFunctions=T,async=FALSE) {
  if(!is.list(arg)) arg = list(arg)

  #server call#1 create tempoary directory on backend
  thisVersion = installed.packages()[installed.packages()[,"Package"]=="fastRditijuu","Version"]
  cat("this is fastRditijuu version: ", thisVersion , "\n")
  print("call server... make temp dir,")
  hostString = paste0(user,"@",host) #make ssh host string
  tempDirCall = paste(
    "timeout 10 ssh",hostString, #ssh the server with 10 sec time out
    "'source /etc/profile; mkdir -p ~/tmp; mktemp -d ~/tmp/XXXXXXXXXXXX'") #source profile and create temp dir

  #make server call#1 one protected by time out and tryCatch
  tryCatch({
    Tempdir.backend = system(tempDirCall,intern = TRUE) #execute, intern=return output
  },
  #if failing, make complete stop
  error=function(e) stop("connection to host failed, check user name and rsa keys for ssh"))
  #tryCatch(stop(e), error = function(e) e, finally = print("Hello"))

  #get only last snip of server return
  cat("message from server:",Tempdir.backend,sep="\n")
  Tempdir.backend = tail(Tempdir.backend,1)
  if(is.null(Tempdir.backend)) stop("no server response returned")

  #check if satus was return instead of backend temp dir
  if(!is.null(attr(Tempdir.backend,"status"))) {
    status = attr(Tempdir.backend,"status")
    if(status==124) stop("server didn't answer for 10 seconds, timeout, status 124
server is either ignoring you or maybe host server is wrong or no internet")
    if(status==255) stop("seems your login was unauthorized, check ssh keys and username, status 255")
    stop(paste("unknown return status code:",status))
  }
  #silly test to gues if return is a valid path on backend
  if(!is.character(Tempdir.backend)) {
    stop(paste("server returns non-char temp path:",Tempdir.backend))
  } else {

    if(length(Tempdir.backend)==0 ||  #if empty
    gregexpr(pattern="/",Tempdir.backend[1])[[1]][1]==-1){ #if no slash
        stop(paste("server returns non-path (has no / in it ??):",Tempdir.backend))
    }
  }




  #save variables to Rdata file in temp directory on local machine
  export = c(what=list(enquote(what)),packages=list(packages),
             arg=list(arg),globalVar=list(globalVar),Tempdir.backend=list(Tempdir.backend),
             async=list(async))
  Tempdir.frontend = system("mkdir -p /tmp; mktemp -d /tmp/XXXXXXXXXXXX",intern = TRUE) #temp directory on local machin
  varFileName = "Tempexp.rda"
  varPath.frontend = paste0(Tempdir.frontend,varFileName)
  cat("frontend Tempdir ",Tempdir.frontend,"\n")
  cat("backend  Tempdir ",Tempdir.backend ,"\n")
  cat("save variables,")
  save(export, file=varPath.frontend)

  #server call#2, push variables file to server
  varTransferCall = paste0("scp ",varPath.frontend," ",hostString,":",
                           Tempdir.backend,"/",varFileName)
  cat(" transfer variables,")

  system(varTransferCall)

  #server call#3, export executable R script
  runFile = 'runOnServer.R'
  scriptPath = system.file(paste0("/scripts/",runFile),package="fastRditijuu")
  scriptTransferCall = paste0("scp ",scriptPath," ",hostString,":",Tempdir.backend,"/",runFile)
  cat(" transfer executable,")
  system(scriptTransferCall)

  #server call#4, execute script
  program = if(Rscript) "Rscript" else paste0(
"R CMD BATCH --no-save --no-restore \'--args ",Tempdir.backend,"\'")
  suffix =  if(Rscript) paste0(" ",Tempdir.backend) else ""

  executeCall = paste0('ssh ',hostString,
    " \"",                         #start collection of lines
    "source /etc/profile ; ",     #source profile script
    "cd ",Tempdir.backend,"; ",   #change to backend temp dir
    program," ./",runFile,suffix,        #execute runFile with Tempdir.backend as arg
    " \"")
  cat(" execute! \n")
  print(executeCall)
  system(executeCall)


  #server call #5, retrieve results

    cat("returning from server...")
    outFile = "Tempout.rda"
    retrieveCall = paste0("scp ", hostString,":",Tempdir.backend ,"/",outFile,
                                            "  ",Tempdir.frontend,"/",outFile)
    system(retrieveCall)
    cat("  read results,")
    out = readRDS(file=paste0(Tempdir.frontend,"/",outFile))
    cat(" delete local temp files,")
    system(paste0("rm -rf ",Tempdir.frontend))

    if(!async) {
    cat(" delete backend temp files,")
    system(paste0("ssh ",hostString," rm -rf ",Tempdir.backend))
  } else {
    cat("keep backend temp... return job ticket,")
  }



  cat(" Finito...\n")
  return(out)
}


#' Internal backend function to perform a qsub batchjob on server
#' @import BatchJobs
#' @param X what to map
#' @param FUN the function
#' @param max.nodes number of max nodes
#' @param global.var lsit with variables attached to global environment on each node
#' @return list with results of qsub batchjob
#' @export
#'
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
#' @param globalVar list of global variables
#' @param async true/false, if torun lply loop asynchronously
#' @param ...
#'
#' @return list of results
#' @export
lply = function(X, FUN, user, host="login.gbar.dtu.dk", Rscript=T,
                packages=c(),max.nodes=24,local=FALSE,globalVar=list(),async=F,...) {
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
                  globalVar=globalVar,
                  user=user,
                  host=host,
                  async,async)
  }
  names(out) = names(X) #restore list naming
  return(out)
}

#' Read Rout, useful if Rscript=FALSE to retrieve print
#'
#' @param ticket
#' @param user
#' @param host
#' @param vebose
#'
#' @return
#' @export
#'
getResult = function(ticket, user, host="login.gbar.dtu.dk",verbose=F) {
  Tempdir.frontend = system("mkdir -p /tmp; mktemp -d /tmp/XXXXXXXXXXXX",intern = TRUE)
  Tempdir.backend = ticket[[1]] #ticket$backend.tmp
  hostString = paste(user,host,sep="@")
  cat("returning from server...")
  outFile = "Tempout.rda"
  retrieveCall = paste0("scp ", hostString,":",Tempdir.backend ,"/",outFile,
                        "  ",Tempdir.frontend,"/",outFile)
  system(retrieveCall)
  cat("  read results,")


  if(verbose) system(paste0(
    "ssh ",hostString," less ",
    Tempdir.backend,
    "/fastRditijuu_qsub_async.sh.o",
     substr(ticket[[2]],1,7)
  ))

  out = readRDS(file=paste0(Tempdir.frontend,"/",outFile))

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


#' Clean up for temp files
#' @param user server to read from
#' @param host  print file to read
#'
#' @return TRUE/1 if found and deleted
#' @export
#'
cleanUp = function(user,host='login.gbar.dtu.dk',printCols=3) {
  hostString = paste0(user,"@",host)
  system(paste0("ssh ",hostString," rm -rf tmp"))
  system(paste0("ssh ",hostString," rm -rf .BatchJobs.R"))
  print("returning content of user root")
  do.call(mapply,list(FUN=paste,split( #sort files in columns
    system(paste0("ssh ",hostString," ls -a"),intern=T)
    ,1:printCols)))
}


