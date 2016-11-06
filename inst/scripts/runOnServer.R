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


#check if fastRditijuu is installed on server
if(!"fastRditijuu" %in% installed.packages()[,"Package"]) {
  print("this seems to be first time, installing fastRditijuu on server")
  if("devtools" %in% installed.packages()[,"Package"]) {
    #devtools is available use that
    require(devtools)
    devtools::install_github("sorhawell/fastRditijuu")
  } else {
    #if devtools is not installed, don't mind we use minimal ghit instead
    install.packages("ghit")
    require("ghit")
    ghit::install_github("sorhawell/fastRditijuu")
  }
}
require("fastRditijuu")


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
