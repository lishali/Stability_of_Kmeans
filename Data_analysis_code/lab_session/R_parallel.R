require(parallel) # one of the core R packages
require(doParallel)
# require(multicore); require(doMC) # alternative to parallel/doParallel
# require(Rmpi); require(doMPI) # to use Rmpi as the back-end
library(foreach)
library(iterators)

taskFun <- function() {
  mn <- mean(rnorm(1e+07))
  return(mn)
}
nCores <- 8 # manually for non-cluster machines
# nCores <- as.numeric(Sys.getenv('NSLOTS')) # for use on cluster
registerDoParallel(nCores)
# registerDoMC(nCores) # alternative to registerDoParallel cl <-
# startMPIcluster(nCores); registerDoMPI(cl) # when using Rmpi as the
# back-end
out <- foreach(i = 1:100) %dopar% {
  cat("Starting ", i, "th job.\n", sep = "")
  outSub <- taskFun()
  cat("Finishing ", i, "th job.\n", sep = "")
  outSub # this will become part of the out object
}




