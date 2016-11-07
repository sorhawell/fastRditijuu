#source slave

if("globalVar.rda" %in% list.files()) {
  load("./globalVar.rda")
} else {
  print("'friendly' warning: globalVar file not found, try execute slave without")
}
