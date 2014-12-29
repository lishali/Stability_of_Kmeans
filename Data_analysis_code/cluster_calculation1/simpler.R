library('Rcpp')
library('microbenchmark')
library('dplyr')

library(foreach)
require(doParallel)
require(parallel) 
library(iterators)


working.directory <- file.path("~/Documents/cluster")
lingBinary <- read.csv(file="lingBinary.csv")

############################################################################################
############################################################################################
#OUTLINE OF FUNCTIONS (details found in section where functions are defined. Code line given)

# 1) correlation similiarity cor.sim(input: two dataframes, with the cluster information.  output: similarity between the two subsamples)
#line: 33.  Calls on cluster_adjacency.cpp file. 

# 2) Df.subsample (input: dataframe, number of subsamples, subsample size (in fraction of dataframe size).  output: list of dataframes, each dataframe being a subsample of original)
#line: 84 
#PARALLIZED with different random seeds. 

#3) k.means (input: dataframe, number of subsamples, cluster size, subsample size.  output, a list of dataframes that are the subsamples of dataframe)
#PARALLIZED. 

# 3) k.stabalize (input: dataframe, number of subsamples, cluster size, subsample size.  output, list of correlation measures for each pair of subsamples for a given k)
#this function calls on all 3 functions above.
#line: 110
#PARALLIZED. 

# 4) Shell script to parallet k. stablize for k in 1:k.max
#line: 180
############################################################################################
############################################################################################
#Correlation Similarity function: 

#we outsource the dotproduct definition to C++ file below
sourceCpp(file.path(working.directory, 'cluster_adjacency.cpp')) 

#more documentation in the cluster_adjacency.cpp file, however  The main benefit of this function, aside from using forloops in C++, is that we never form adjaceny matrices for each subset of variables (of the cluster information).  We simply access the dataframe to determine the total sum. 

cor.sim <- function(X, Y) {
  num <- dot_product2(X,Y) #this is neccessary as we are only computing the dot product of the lower trangular matrix.
  denom <- sqrt(dot_product2(X,X))*sqrt(dot_product2(Y,Y))
  return (num/denom)
}


############################################################################################
############################################################################################


# a function to pick my n subsamples:

Df.subsample <-function(Df, n.subsample, ratio.subsample){
  #Data.frame is the binarized dataframe, we assume that a function already created an ID column
  #n.subsample is the number of subsamples
  #size.subsample is the size of the subsample given in percentage.
  
  #Errors
  if (ratio.subsample > 1) stop("Please give subsample size in terms of ratio of the total size.")
  if (n.subsample < 0) stop("n.subsample negative value: invalid.")
  if (ratio.subsample < 0) stop("ratio.subsample negative value: invalid.")
  
  foreach(i=1:n.subsample) %dopar% {
    set.seed(i)
    sample_frac(Df, size = ratio.subsample, replace = FALSE) #sample rows of dataframe without replacement
    
  }
  
} 

############################################################################################
############################################################################################
k.max =3
Df <- lingBinary[1:12,1:12]
number.samples=10
ratio.sub = 0.8

k.means <- function(Df, k, num.samples, ratio.subsample){
  list.subsamples <- Df.subsample(Df, num.samples, ratio.subsample)
  
  foreach(j=1:num.samples)%dopar%{ #I will calculate kmeans for each of the num.samples number of subsamples.   
    #I will use my Df.subsample function to obtain a list of dataframes of subsamples of the ratio.subsample size:
    #each item in the list is a dataframe
    subsample <- list.subsamples[[j]] #the jth element of this list is the jth subsample dataframe
    k.means <- kmeans(subsample[7:ncol(Df)],centers = k, nstart=20, iter.max=1000) #this was hardcoded in since the lingBinarize dataset's questions start on column 7 to the last ncol(Df), and this is the same for its subsamples
    k.means <- k.means$cluster #this step extracts the cluster vector from the kmeans() output k.means$cluster.  
    k.means <- as.data.frame(k.means) #we need to use semi_joins so coerce into a dataframe, the rownames will still carry the ids of the subsamples.  
    k.means$ID <- as.numeric(rownames(k.means)) #it's hard to semi_join by rownames, so I will create a new column called ID that copies the rownames info
    
    k.means
  }
  
}

parallel.results <- foreach(i = 1:k.max) %dopar% { #will specify k.max
  # Make sure that each process has its own random number stream.
  data.list <- k.means(Df, i, number.samples, ratio.sub)
  sapply(1:length(data.list), 
         function (x) write.csv(data.list[[x]], file=sprintf("what%d.cvs", x) )   )
  return(data.list)
}
