# Example usage of RCpp.

library('Rcpp')
library('microbenchmark')

working.directory <- file.path("/Users/LishaLi/Desktop/215A/Lab3")


sourceCpp(file.path(working.directory, 'Rcpp_demo.cpp'))

x = rnorm(1e7)
y = rnorm(1e7)
z <- cbind(x, y)

# Define our own R distance function
DistanceR <- function(x, y) {
  return(sqrt(sum((x - y) ^ 2)))
}

# Test our R version vs. our C++ version, with a strangely underperforming
# openMP function to boot.
#microbenchmark evaluates the timing 
microbenchmark(DistanceR(x, y),
               DistanceCPP(x, y), times = 5)

# All the answers are the same:
DistanceCPP(x, y)

DistanceR(x, y)

