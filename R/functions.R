#' do.call with cluster as backend via ssh
#'
#' @param what function or name of function to execute
#' @param arg arguments for function
#' @param user username
#' @param host server (will connect to user@server)
#' @param keyPath specifiy file and path for private key, if NULL non or system default.
#' @param packages char vector of package names (can be empty)
#' @param Rscript execute by Rscript or R CMD BATCH (former  only supported on gbar, ladder no verbose)
#' @param async return after starting job? returned value is ticked to fetch result when job completed
#' @param qsub.walltime only relevant for async=T or lply, job time limit on Torque('qsub')-cluster
#' @param qsub.proc how many processes to ask for per job, 1 unless using doParallel etc.
#' @param qsub.nodes how many nodes to ask for per job, leave unchanged if in doubt
#'
#' @return value return by evaluated function
#' @export
#'
doClust = function(what,arg=list(),user,host='login.gbar.dtu.dk',keyPath=NULL,packages=c(),
                   Rscript=TRUE,globalVar=list(),async=FALSE,
                   qsub.walltime="00:09:00",qsub.proc=1,qsub.nodes=1,qsub.moreArgs) {
  if(!is.list(arg)) arg = list(arg) #wrap arg in list if not list
  keyPath = if(is.null(keyPath)) "" else paste0(" -i ",keyPath," ")
  lang = if(!Sys.info()['sysname']=='Windows') "bash" else "BATCH" #check OS

  #temp directory on local machin
  if(lang=="BATCH") {

    Tempdir.frontend = tempdir()
    tempFolder = shell("echo %TEMP%",intern=TRUE)
    Tempdir.frontend = paste0(tempFolder,"\\",
                      paste(sample(c(letters,LETTERS),12),collapse=""))
    shell(paste("mkdir",Tempdir.frontend))
  } else {
    Tempdir.frontend = system("mkdir -p /tmp; mktemp -d /tmp/XXXXXXXXXXXX",intern = TRUE)
  }

  #server call#1 create tempoary directory on backend
  thisVersion = installed.packages()[installed.packages()[,"Package"]=="fastRditijuu","Version"]
  cat("this is fastRditijuu version: ", thisVersion , "\n")
  print("call server... make temp dir,")
  hostString = paste0(user,"@",host) #make ssh host string
  if(lang=="bash") {
    tempDirCall = paste(
      "timeout 15 ssh",hostString,keyPath, #ssh the server with 10 sec time out
      "'source /etc/profile; mkdir -p ~/tmp; mktemp -d ~/tmp/XXXXXXXXXXXX'") #source profile and create temp dir
  } else {
    path_maketemp = paste0(Tempdir.frontend,"/putty_maketemp.txt")
    writeLines(
      "source /etc/profile \n mkdir -p ~/tmp \n mktemp -d ~/tmp/XXXXXXXXXXXX", #source profile and create temp dir
      con=path_maketemp)
    tempDirCall = paste0(
      "Plink -ssh ",hostString," -l ",user,keyPath," -m ",path_maketemp
    )
  }

  #make server call#1 one protected by time out and tryCatch
  print(tempDirCall)
  tryCatch({
    if(lang=="bash") {
      Tempdir.backend = system(tempDirCall,intern = TRUE) #execute, intern=return output
    }else {
      Tempdir.backend = shell(tempDirCall,intern = TRUE)
#      return("success")
    }
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
             async=list(async),
             qsub.walltime = list(qsub.walltime),
             qsub.proc     = list(qsub.proc),
             qsub.nodes    = list(qsub.nodes))
  varFileName = "Tempexp.rda"
  varPath.frontend = paste0(Tempdir.frontend,varFileName)
  cat("frontend Tempdir ",Tempdir.frontend,"\n")
  cat("backend  Tempdir ",Tempdir.backend ,"\n")
  cat("save variables,")
  save(export, file=varPath.frontend)

  #server call#2, push variables file to server
  cat(" transfer variables,")
  if(lang=="bash") {
    varTransferCall = paste0("scp ",varPath.frontend," ",hostString,":",
                             Tempdir.backend,"/",varFileName)
    system(varTransferCall)
  } else {
    varTransferCall = paste0("PSCP  ",varPath.frontend," ",hostString,":",
                             Tempdir.backend,"/",varFileName)
    print(varTransferCall)
    shell(varTransferCall)
  }



  #server call#3, export executable R script
  runFile = 'runOnServer.R'
  scriptPath = system.file(paste0("/scripts/",runFile),package="fastRditijuu")
  cat(" transfer executable,")
  if(lang=="bash") {
    scriptTransferCall = paste0("scp ",scriptPath," ",hostString,":",Tempdir.backend,"/",runFile)
    system(scriptTransferCall)
  } else {
    scriptTransferCall = paste0("PSCP ",scriptPath," ",hostString,":",Tempdir.backend,"/",runFile)
    shell(scriptTransferCall)
  }



  #server call#4, execute script
  program = if(Rscript) "Rscript" else paste0(
"R CMD BATCH --no-save --no-restore \'--args ",Tempdir.backend,"\'")
  suffix =  if(Rscript) paste0(" ",Tempdir.backend) else ""
  cat(" execute! \n")
  if(lang=="bash") {
    executeCall = paste0('ssh ',hostString,
      " \"",                         #start collection of lines
      "source /etc/profile ; ",     #source profile script
      "cd ",Tempdir.backend,"; ",   #change to backend temp dir
      program," ./",runFile,suffix,        #execute runFile with Tempdir.backend as arg
      " \"")
    print(executeCall)
    system(executeCall)
  } else {
    path_runfile = paste0(Tempdir.frontend,"/putty_runFile.txt")
    backend.bashcall = paste0(
      "source /etc/profile", "\n",     #source profile script
      "cd ",Tempdir.backend, "\n",   #change to backend temp dir
      program," ",runFile," ",suffix, "\n")
    writeLines(backend.bashcall,con = path_runfile)
    executeCall = paste0('Plink -ssh ',host," -l ",user," -m ",path_runfile)
    print(executeCall)
    shell(executeCall)
  }


  #server call #5, retrieve results
    cat("returning from server...")
    outFile = "Tempout.rda"
    if(lang=="bash") {
      retrieveCall = paste0("scp ", hostString,":",Tempdir.backend ,"/",outFile,
                                              "  ",Tempdir.frontend,"/",outFile)
      system(retrieveCall)
    } else {
      retrieveCall = paste0("PSCP ", hostString,":",Tempdir.backend ,"/",outFile,
                            "  ",Tempdir.frontend,"/",outFile)
      print(retrieveCall)
      shell(retrieveCall)
    }

    cat("  read results,")
    out = readRDS(file=paste0(Tempdir.frontend,"/",outFile))

    if(!async) {
      cat(" delete backend temp files,")
      if(lang=="bash") {
          delCall.backend = paste0("ssh ",hostString," rm -rf ",Tempdir.backend)
          print(delCall.backend)
          system(delCall.backend)
        } else {
          path_delBeckendTemp = paste0(Tempdir.frontend,"/putty_delBackendTemp.txt")
          delCall_string = paste0("rm -rf ",Tempdir.backend)
          writeLines(delCall_string,con=path_delBeckendTemp)
          delCall.backend = paste0("Plink -ssh ",hostString, " -m ",path_delBeckendTemp)
          print(delCall.backend)
          print(delCall_string)
          shell(delCall.backend)
      }
    } else {
      cat("keep backend temp... return job ticket,")
    }

    cat(" delete local temp files,")
    if(lang=="bash") {
      delCall.frontend = paste0("rm -rf ",Tempdir.frontend)
      print(delCall.frontend)
      system(delCall.frontend)
    } else {
      delCall.frontend = unlink(Tempdir.frontend,recursive = TRUE)
    }

  cat(" Finito...\n")
  if(async) class(out) = "ticket"
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
#' @param max.nodes maximum nodes reuired, do not set higher than 80 (79 if async=T)
#' @param local should the lply run on local computer (only for debugging/testing)
#' @param globalVar list of global variables
#' @param async true/false, if to run lply loop asynchronously
#' @param qsub.walltime change wall time qsub, will apply to both master and slaves
#' @param qsub.proc how many processes to ask for per job, 1 unless using doParallel etc.
#' @param qsub.nodes how many nodes to ask for per job, leave unchanged if in doubt
#' @param ...
#'
#' @return list of results
#' @export
lply = function(X, FUN, user, host="login.gbar.dtu.dk", Rscript=T,
                packages=c(),max.nodes=24,local=FALSE,globalVar=list(),async=F,
                qsub.walltime="00:09:00",qsub.proc=1,qsub.nodes=1,...) {
  if(local) {
    # require("BatchJobs")
    # out = do.call(what=doBatchJob,args=list(X=X,FUN=FUN,max.nodes=max.nodes,
    #                                         globalVar=globalVar,...))
    print("local is not supported anymore")
    return(1)
  } else {
    packages = unique(c(packages,"BatchJobs")) #include BatchJobs package on server
    out = doClust('doBatchJob',
      arg=list(
        X=X,
        FUN=FUN,
        max.nodes=max.nodes,
        packages=packages,
        ...),
    packages  = packages,
    Rscript   = Rscript,
    globalVar = globalVar,
    user      = user,
    host      = host,
    async     = async,
    qsub.walltime = qsub.walltime,
    qsub.proc     = qsub.proc,
    qsub.nodes    = qsub.nodes)
  }

  return(out)
}

#' Read Rout, useful if Rscript=FALSE to retrieve print
#'
#' @param ticket the out object from lply or doClust when call with async=TRUE
#' @param user username on server
#' @param host server host address
#' @param verbose if TRUE, prints from R master, can be used to monitor progress and debugging
#'
#' @return Either the result of a finished job, or if not complete yet, then a ticket like object
#' @export
#'
getResult = function(ticket, user, host="login.gbar.dtu.dk",verbose=F) {
  lang = if(!Sys.info()['sysname']=='Windows') "bash" else "BATCH" #check OS

  if(class(ticket)=="ticket") {
    #temp directory on local machin
    if(lang=="BATCH") {
      Tempdir.frontend = tempdir()
      tempFolder = shell("echo %TEMP%",intern=TRUE)
      Tempdir.frontend = paste0(tempFolder,"\\",
                                paste(sample(c(letters,LETTERS),12),collapse=""))
      shell(paste("mkdir",Tempdir.frontend))
    } else {
      Tempdir.frontend = system("mkdir -p /tmp; mktemp -d /tmp/XXXXXXXXXXXX",intern = TRUE)
    }
    Tempdir.backend = ticket[[1]] #ticket$backend.tmp
    hostString = paste(user,host,sep="@")
    cat("contact server...")
    outFile = "Tempout.rda"
    if(lang=="BATCH") {
      retrieveCall = paste0("PSCP ", hostString,":",Tempdir.backend ,"/",outFile,
                            "  ",Tempdir.frontend,"/",outFile)
      status = shell(retrieveCall,intern=T)
    } else {
      retrieveCall = paste0("scp ", hostString,":",Tempdir.backend ,"/",outFile,
                          "  ",Tempdir.frontend,"/",outFile)
      status = system(retrieveCall,intern=T)
    }
    #check if ticket or a result are found, stop if scp return non-zero status
    if(!is.null(attr(status,"status")) && attr(status,"status")!=0) {
      system(paste0("rm -rf ",Tempdir.frontend))
      stop("fastRditijuu: 'Failed to locate job. Maybe this job has been deleted, with cleanUp()?'")
    }

    cat("  read results,")
    out = readRDS(file=paste0(Tempdir.frontend,"/",outFile))

    if(verbose) {
      if(lang=="bash") {
        print("print out file from backend master")
        system(paste0(
          "ssh ",hostString," less ",
          Tempdir.backend,
          "/fastRditijuu_qsub_async.sh.o",
          substr(ticket[[2]],1,7)
        ))
      } else {
        getPrint_path = paste0(Tempdir.frontend,"/","putty_getPrint.txt")
        writeLines(paste0(
          "less ",
          Tempdir.backend,
          "/fastRditijuu_qsub_async.sh.o",
          substr(ticket[[2]],1,7)),con=getPrint_path)
        shell(paste("Plink -ssh",hostString,"-m", getPrint_path))
      }
    }

    #delete frontend
    if(lang=="bash") {
      delCall.frontend = paste0("rm -rf ",Tempdir.frontend)
      print(delCall.frontend)
      system(delCall.frontend)
    } else {
      delCall.frontend = unlink(Tempdir.frontend,recursive = TRUE)
    }

    if(class(out)=="ticket") {
      #out is a ticket, backend master has only saved a ticket file so far
      cat(" job has not finshed yet, ask again later")
      names(out) = ticket$names
      return(NULL)
    } else {
      #since out is not a ticket, the beckend master must have overwritten
      # the ticket file with a result file, return out to user
      cat(" job's done!")
      return(out)
    }

  #ticket is not of class ticket
  } else {
    print("this seems not to be a valid ticket")
    return(NULL)
  }
}

#' Clean up for temp files
#' @param user server to read from
#' @param host  print file to read
#'
#' @return TRUE/1 if found and deleted
#' @export
#'
cleanUp = function(user,host='login.gbar.dtu.dk') {
  lang = if(!Sys.info()['sysname']=='Windows') "bash" else "BATCH" #check OS
  if(lang=="bash") {
    hostString = paste0(user,"@",host)
    system(paste0("ssh ",hostString," rm -rf tmp"))
    system(paste0("ssh ",hostString," rm -rf .BatchJobs.R"))
  } else {
    cleanUp_path = paste0(shell("echo %TEMP%",intern=T),"\\","putty_cleanUp.txt")
    writeLines("
rm -rf tmp
rm -rf .BatchJobs.R
",con=cleanUp_path)
    shell(paste("Plink -ssh",hostString,"-m",cleanUp_path))

  }
}


#' Clean up for temp files
#' @param user server to read from
#' @param host  print file to read
#' @param host  print file to read
#'
#' @return TRUE/1 if found and deleted
#' @export
#'
exchangeRSAkeys = function(
  user,host,
  server.ssh.pub.file = "~/.ssh/authorized_keys",
  keyName = "fastRditijuu") {

  system(paste0("
cd ~/
if [ -d '.ssh' ]
then
    echo '.ssh folder found, placing private key'

else
    echo 'Create folder'
    mkdir ~/.ssh
    chmod 700 ~/.ssh
fi
cd ~/.ssh
ssh-keygen -t rsa -f ",keyName," -N ''"))

pubKey = readLines(paste0(con="~/.ssh/",keyName,".pub"))
print("to send public key to server, copy, paste and run this line in terminal")
cat("\n")
cat(paste0("ssh ",user,"@",host,
              " \" mkdir -p ~/.ssh;",
              "echo \'",pubKey,"\' >> ",server.ssh.pub.file,"\""))


}
