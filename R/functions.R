library(RCurl)

doClust = function(what,arg=list(),user='sowe',host='login.gbar.dtu.dk') {
  export = c(list(what),arg)
  save(export, file="./.Tempexp.rda")
  fileTransferCall = paste0("scp temp2server.rda ",user,"@",host,":./Tempexp.rda")
  system(fileTransferCall)

}


{

}

serverSide = function(export)


sowe2@sowe2-VirtualBox:~$ ssh sowe@login.gbar.dtu.dk ". /etc/profile ; Rscript foo.R"
[1] "hurray this works"
sowe2@sowe2-VirtualBox:~$ ssh sowe@grid01.compute.dtu.dk ". /etc/profile ; Rscript foo.R "
The program 'Rscript' is currently not installed. To run 'Rscript' please ask your administrator to install the package 'r-base-core'
