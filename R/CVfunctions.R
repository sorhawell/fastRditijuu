

#' Make indexes for a repeated k-fold cross-validation.
#' @aliases gridSearch
#' @param N number of observations in training set, or a target vector of N elements
#' @param reps number of repetitions
#' @param folds number of folds
#'
#' @return a named list of test index for each fold and repetition
#' @export
#'
#' @examples
#' repfold(N=500,reps=10,folds=10)
repfold = function(N,reps=5,folds=20) {
  if(length(N)>1) N = length(N)
  allFolds = lapply(1:reps,function(thisRep) {
    suppressWarnings(split(sample(N),paste0(
      "R",formatC(thisRep,width=2,flag=0),
      "F",formatC(1:folds,width=2,flag=0),
      sep=".")))
  })
  allFolds = do.call(c,allFolds)
  for(r in 1:reps) for(f in 1:folds) {
    attr(allFolds[[(r-1)*folds+f]],"rep") = r
    attr(allFolds[[(r-1)*folds+f]],"fold") = f
  }
  allFolds
}

#' make a job list for a repeated kfold CV grid search
#' @aliases gridSearch
#' @param N number of observations in training set, or a target vector of N elements
#' @param reps number of repetitions
#' @param folds number of folds
#' @param trainArgs named list of model arguments to span a grid over
#'
#' @return a mappable job list, run with lapply or lply
#' @export
#'
#' @examples grid = makeGrid(N=500,trainArgs = list(mtry=1:5,ntree=1:10))
makeGrid = function(N,reps=10,folds=10,trainArgs = list()) {
  if(length(N)>1) N = length(N) #support to passe target vector instead of N
  allFolds = repfold(N,reps=reps,folds=folds)
  nfoldreps = length(allFolds)
  seeds = runif(nfoldreps)
  for(i in 1:nfoldreps) attr(allFolds[[i]],"seed") = seeds[i]
  allArgSets = expand.grid(trainArgs)
  allArgSets = lapply(1:nrow(allArgSets), function(i) {
    row = allArgSets[i,]
    names(row) = names(allArgSets) #needed when only one arg in argSets
    return(row)
  })
  grid = expand.grid(fold=allFolds,argSet = allArgSets)
  grid = apply(grid,1,c)
  for(i in 1:length(grid)) grid[[i]]$seed = attr(grid[[i]]$fold,"seed")
  attr(grid,"gridDim") = c(folds=folds,reps=reps,sapply(trainArgs,length))
  grid
}


#' Perform one job in a grid
#' @aliases gridSearch
#' @param one one job, as defined by makeGrid
#' @param parent.function.name doOneGrid apply a parent function of this name
#' @param Dim is training set X a list dim=1, or data.frame dim=2
#' @param ... other args passed to model.function
#'
#' @return result from model.function
#' @export
#'
#' @examples
#'library(randomForest)
#'library(fastRditijuu)
#'#have some training data called X and y
#'N=1000
#'var = 8
#'X = data.frame(replicate(var,rnorm(1000)))
#'y = with(X,X1*X2+sin(X3))
#'
#'#design a model function
#'model.function = function(Xtrain,ytrain,Xtest,ytest=NULL,
#'                          trainArgs = list(...)){
#'  std.args = list(x=Xtrain,y=ytrain,mtry=10,ntree=1000,importance=T)
#'  run.args = c(trainArgs,std.args[!names(std.args) %in% names(trainArgs)])
#'  rf2 = do.call(randomForest,run.args)
#'  out = list(
#'    ypred = predict(rf2,Xtest),
#'    ytest = ytest,
#'    explVar.OOB = tail(rf2$rsq,1)
#'  )
#'}
#'
#'#make the a grid here with any combination mtry (1 to 5) and ntree (1:10)
#'trainArgs = list(mtry=1:3,ntree=1:10)
#'folds = 5
#'reps  = 6
#'grid = makeGrid(N=N,reps=reps,folds=folds,trainArgs = trainArgs)
#'length(grid)
#'
#'#map the grid with doOneGrid
#'gridOut = lapply(grid,doOneGrid)
#'
#'#score predictions by some metric, (root mean square error)
#'CVout = sapply(gridOut, function(x) sqrt(sum((x$ypred-x$ytest)^2)))
#'
#'#make array (folds x reps x trainArg1 x trainArg2 x trainArg3 ...)
#'Dims = attr(grid,"gridDim") #grid dims can be found here
#'CVarr = array(CVout, dim=Dims)
#'
#'#reduce array over folds by mean
#'CVarr2 = apply(
#'  X      = CVarr,
#'  MARGIN = which(names(Dims) != "folds"),
#'  FUN    = mean
#')
#'
#'#reduce array over reps by mean
#'CVarr.mean = apply(
#'  X      = CVarr2,
#'  MARGIN = which(names(dim(CVarr2)) %in% names(trainArgs)),
#'  FUN    = mean
#')
#'
#'#reduce array over reps by mean
#'CVarr.sd = apply(
#'  X      = CVarr2,
#'  MARGIN = which(names(dim(CVarr2)) %in% names(trainArgs)),
#'  FUN    = sd
#')
#'
#'#get best model
#'best.ind = which(min(CVarr.mean)==CVarr.mean,arr.ind=T)
#'best.arg = mapply(trainArgs,best.ind,
#'                  FUN=function(thisArg,thisInd) thisArg[thisInd])
#'print(best.arg)
#'
#'#plot train performance
#'
#'for(i in 1:dim(CVarr.mean)[1]) {
#'  if(i==1) {
#'    plot  (trainArgs[[2]],CVarr.mean[i,],type="l",col=i,
#'           ylim=range(CVarr.mean))
#'  } else {
#'    points(trainArgs[[2]],CVarr.mean[i,],type="l",col=i)
#'  }
#'  segments(x0=jitter(trainArgs[[2]]),
#'           y0=CVarr.mean[i,]+CVarr.sd[i,],
#'           y1=CVarr.mean[i,]-CVarr.sd[i,], col =i)
#'}
#'

#'
doOneGrid = function(one,parent.function.name="model.function",Dim=2,verbose=F,...) {
  #assuming following variables in parent env
  #X,y,model.function)
  test.ind = 1:length(y) %in%  one$fold

    ytest = y[test.ind ];  ytrain = y[!test.ind ]
  if(Dim==1) {
    Xtest = X[test.ind ];  Xtrain = X[!test.ind ]
  } else {
    Xtest = X[test.ind,];  Xtrain = X[!test.ind,]
  }

  theseArgs = list(
      X=Xtrain,y=ytrain,Xtest=Xtest,ytest=ytest,
      trainArgs = one$argSet,...)
  if(!is.null(one$seed)) set.seed(one$seed)
  thisModelOut = do.call(get("model.function"),theseArgs)
}


#deprecated...
#
# doGrid = function(X,y,folds.list,model.function,nProc=1,trainArgsGrid,seeds=NULL) {
#   if(is.null(seeds)) seeds = runif(length(folds.list))
#   if(nProc==1) {
#     allFolds = list()
#     for(i in 1:length(folds.list)) {
#       print(names(folds.list)[i])
#       test.ind = 1:length(y) %in% folds.list[[i]]
#       Xtest = X[test.ind]; Xtrain = X[!test.ind]
#       ytest = y[test.ind]; ytrain = y[!test.ind]
#
#       #do grid for this fold
#       thisFold = list()
#       for(j in 1:length(trainArgsGrid)) {
#         theseArgs = list(
#           X=Xtrain,y=ytrain,Xtest=Xtest,ytest=ytest,
#           trainArgs = trainArgsGrid[[j]])
#         set.seed(seeds[i])
#         thisModelOut = do.call(model.function,theseArgs)
#         thisFold[[j]] = thisModelOut
#       }
#       allFolds[[i]] = thisFold
#
#     }
#   }
#   out
# }
#
#
# doOneFold = function(aFold,trainArgs,X,y,model.function) {
#   test.ind = 1:length(y) %in% aFold
#   Xtest = X[test.ind ]; Xtrain = X[!test.ind ]
#   ytest = y[test.ind ]; ytrain = y[!test.ind ]
#   args = list(
#     X=Xtrain,y=ytrain,Xtest=Xtest,ytest=ytest,
#     trainArgs = trainArgs)
#   out = do.call(model.function,args)
# }
#
# doOneFold.fix = function(aFold) {
#   #assuming following arguments in env
#   #trainArgs,X,y,model.function) {
#   test.ind = 1:length(y) %in% aFold
#   Xtest =  X[test.ind ]; Xtrain = X[!test.ind ]
#   ytest =  y[test.ind ]; ytrain = y[!test.ind ]
#   args = list(
#     Xtrain=Xtrain,ytrain=ytrain,Xtest=Xtest,ytest=ytest,
#     trainArgs = trainArgs)
#   out = do.call(model.function,args)
# }
#
#
