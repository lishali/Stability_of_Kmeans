# Example based on Chris Paciorek's tutorial:
# http://www.stat.berkeley.edu/scf/paciorek-parallelWorkshop.pdf
#
# Note: there is a conflict between openBLAS, the multi-threaded linear
# algebra package, and foreach.  It can cause linear algebra operations
# within a foreach loop to hang
# If your system uses openBLAS (note that the SCF computers do),
# then before running R, execute the command:
#
# export OMP_NUM_THREADS=1
#
# This command sets an environment variable that tells BLAS to only
# use a single thread.
library('iterators')
library('foreach')
library('parallel')
library('doParallel')



# Draw a random dense linear model (some boring data)
GenerateData <- function() {
  p <- 100
  n <- 1000
  x <- matrix(rnorm(p*n),nrow = n, ncol = p) # we first generate p*n random normally distributed points, then insert it into a n by p matrix
  beta <- rt(p, df = 3) #this is drawing from the student t distribution  (randomly generated from student-t)
  #p was the number of observations, and df= degrees of freedom.  
  y <- x %*% beta #interesting, we multiply by the elements of the t-districution.  
  return(list(y = y, x = x))
}

# Set the number of cores.  On a UNIX machine, you can
# look at /proc/cpuinfo.  On a mac, you can look at the system
# profiler.
ncores = 4
registerDoParallel(ncores)

# How many times to fit the linear model.
repetions <- 500

# At this point, open up a terminal and run top to watch
# what is going on.

# Serial execution of lm
start.time <- Sys.time()
serial.results <- list()
for (i in 1:repetions) {
  data.list <- GenerateData()
  serial.results[[i]] <- lm(y ~ x, data.list)$coefficients
}
duration.1 <- Sys.time() - start.time

# Parallel execution of lm
#and the only difference is a little bit in the code....
start.time <- Sys.time()
parallel.results <- foreach(i = 1:repetions) %dopar% {
  data.list <- GenerateData()
  return(lm(y ~ x, data.list)$coefficients)
}
duration.2 <- Sys.time() - start.time
#wow, there were 4 R sessions and it's done!
print(duration.1)
print(duration.2)

#wow, parallelize = the key!!!