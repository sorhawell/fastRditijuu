% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{doClust}
\alias{doClust}
\title{do.call with cluster as backend via ssh}
\usage{
doClust(what, arg = list(), conf = NULL, user = NULL,
  host = "login.gbar.dtu.dk", transferHost = NULL, keyPath = NULL,
  packages = c(), Rscript = TRUE, globalVar = list(), async = FALSE,
  nCores = 1, qsub.walltime = "00:09:00", qsub.proc = 1, qsub.nodes = 1,
  qsub.moreArgs = NULL)
}
\arguments{
\item{what}{function or name of function to execute}

\item{arg}{arguments for function}

\item{user}{username}

\item{host}{server (will connect to user@server)}

\item{transferHost}{server to transfer files through, will use transfer.gbar.dtu.dk or host if NULL}

\item{keyPath}{specifiy file and path for private key, if NULL non or system default.}

\item{packages}{char vector of package names (can be empty)}

\item{Rscript}{execute by Rscript or R CMD BATCH (former  only supported on gbar, ladder no verbose)}

\item{async}{return after starting job? returned value is ticked to fetch result when job completed}

\item{nCores}{number of cores on each node}

\item{qsub.walltime}{only relevant for async=T or lply, job time limit on Torque('qsub')-cluster}

\item{qsub.proc}{how many processes to ask for per job, 1 unless using doParallel etc.}

\item{qsub.nodes}{how many nodes to ask for per job, leave unchanged if in doubt}
}
\value{
value return by evaluated function
}
\description{
do.call with cluster as backend via ssh
}
