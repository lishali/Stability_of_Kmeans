library('Rcpp')
library('microbenchmark')
library('dplyr')

working.directory <- file.path("/Users/LishaLi/Desktop/215A/Lab3")



sourceCpp(file.path(working.directory, 'cor_sim_function.cpp')) #c++ function I wrote to calculate correlation similarity and dot products between matrices (sum of element-wise product)



# we will just create two long matrices, z and c


a = matrix(rbinom(1e8, 1, 0.5), ncol=sqrt(1e8))
b = matrix(rbinom(1e8, 1, 0.5), ncol=sqrt(1e8))

dot_product(a,b) #this is imported from C++ code 

correlation_similarity(a,b) #this is imported from C++ code 




cor.sim <- function(X, Y) {
  num <- dot_product(X,Y)
  denom <- sqrt(dot_product(X,X))*sqrt(dot_product(Y,Y))
  return (num/denom)
}

R.answer <- cor.sim(a,b)
R.answer
#good, both R and C++ answers are the same
#let's see which is faster:
microbenchmark(correlation_similarity(a,b), cor.sim(a,b), times = 5)
# no difference





