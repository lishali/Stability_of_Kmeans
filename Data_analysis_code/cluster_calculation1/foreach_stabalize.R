library('Rcpp')
library('microbenchmark')
library('dplyr')

library(foreach)
require(doParallel)
require(parallel) 
library(iterators)


working.directory <- file.path("/accounts/grad/janetlishali/Documents/cluster")
lingBinary <- read.csv(file="lingBinary.csv")

############################################################################################
############################################################################################
#OUTLINE OF FUNCTIONS (details found in section where functions are defined. Code line given)

# 1) cluster adjacency (input:dataframe with column 'k.means', output: matrix indicating whether ith point and jth point are in the same cluster, output: lower triangular matrix of this cluster information, since it is symmetric, only lower-triangular is neccessary)
#line: 37.  Calls on cluster.adjacency.cpp file

# 2) correlation similiarity (input: two cluster adjacency matrices.  output: similarity between the two adjacencies)
#line: 63.  Calls on cor_sim_function.cpp file. 

# 3) Df.subsample (input: dataframe, number of subsamples, subsample size (in fraction of dataframe size).  output: list of dataframes, each dataframe being a subsample of original)
#line: 84

# 4) stabalize.k (input: dataframe, number of subsamples, cluster size, subsample size.  output, list of correlation measures for each unique pair of subsamples for a given k)
#this function calls on all 3 functions above.
#line: 110

# 5) Shell script to parallet stablize.k for k in 1:k.max
#line: 180
############################################################################################
############################################################################################


#CLUSTER ADJACENCY FUNCTION

#cluster matrix: Given a dataframe with a column called "k.means" that tells us which cluster each row is in, we need to obtain a cluster ajaceny matrix, where entry (i,j) tells us if i and j are in the same cluster:

#I outsourced my forloops to create an adjacency matrix for the cluster infomortion to C++.  The cluster.adjaceny() fucntion is listed below
sourceCpp(file.path(working.directory, 'cluster_adjacency.cpp')) 

#we actually do not need to create the matrix below.

cluster.adjacency <- function(Df){
  #Df is the dataframe we are forming this information from
  
  #Error and Warnings:
  if ("k.means" %in% colnames(Df) == FALSE) stop("Please rename dataframe column where cluster info is kept as `k.means'")   #our C++ function assumes that column giving cluster number is called k.means 
  
  #call on C++ function to generate lower triangular part of the symmetric cluster matrix
  adj.cluster.matrix <- cluster_lowertri(Df) #using our C++ function for the forloops
  # we only need the lower diagonal in order to computer correlation similarity, due to symmetry, so we'll only return that.
  return(adj.cluster.matrix)
}

############################################################################################
############################################################################################

#Correlation Similarity function: 

sourceCpp(file.path(working.directory, 'cor_sim_function.cpp')) #c++ function I wrote to calculate correlation similarity and dot products between matrices (sum of element-wise product)

#sanity tests for the C++ functions are done in sanitytest_cpp.R.  
#We also compare the performance of the cor.sim function below, which calls on cor_sim_function.cpp, rather then the one completely written in C++.  Since the last step amounts to dividing two doubles, there is no real difference as detected by microbenchmark. 


cor.sim <- function(X, Y) {
  num <- dot_product2(X,Y) #this is neccessary as we are only computing the dot product of the lower trangular matrix.
  denom <- sqrt(dot_product2(X,X)- num.row)*sqrt(dot_product2(Y,Y))
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


k.means <- function(Df, k, num.samples, ratio.subsample){
  list.subsamples <- Df.subsample(Df, num.samples, ratio.subsample)

  foreach(j=1:num.samples)%dopar%{ #I will calculate kmeans for each of the num.samples number of subsamples.   
    #I will use my Df.subsample function to obtain a list of dataframes of subsamples of the ratio.subsample size:
    #each item in the list is a dataframe
    set.seed(j)
    subsample <- list.subsamples[[j]] #the jth element of this list is the jth subsample dataframe
    k.means <- kmeans(subsample[7:ncol(Df)],centers = k, nstart=40, iter.max=5000) #this was hardcoded in since the lingBinarize dataset's questions start on column 7 to the last ncol(Df), and this is the same for its subsamples
    k.means <- k.means$cluster #this step extracts the cluster vector from the kmeans() output k.means$cluster.  
    k.means <- as.data.frame(k.means) #we need to use semi_joins so coerce into a dataframe, the rownames will still carry the ids of the subsamples.  
    k.means$ID <- as.numeric(rownames(k.means)) #it's hard to semi_join by rownames, so I will create a new column called ID that copies the rownames info
    
    k.means
  }
}


############################################################################################
############################################################################################

k.stabalize <- function(Df, k, num.samples, ratio.subsample){ 
list <- k.means(Df, k, num.samples, ratio.subsample)
#correlationSim <- list()

  foreach(m=1:length(list), .combine = "c") %:% # we loops through all unique pairs (order does not matter since cor.sim(X,Y)=cor.sim(Y,X))
    foreach(l=1:m) %dopar% {  #note that we need only look up to m-1 since we don't want to compare the same elements of the list (so m != l)
   
        subsample1 <- arrange(semi_join(list[[m]], list[[l]], by= "ID"), ID) #this restricts our attention to common points of list[[m]] and list[[l]].  The semi_join means subsample1 only has cluster info for b[[m]]
        subsample2 <- arrange(semi_join(list[[l]], list[[m]], by= "ID"), ID) #this will consider clusters in b[[l]], for common points of list[[m]] and list[[l]]
        #now we apply the cluster.adjacency() to subsample1 and subsample2, outputting two matrices we can feed into cor.sim() (this will be fast since I wrote the forloops of both functions in C++)
        
        cor.sim(X,Y)
        #recall that correlationSim is a list defined for each k from 1 to k.max, so this is the list of n^2-n pairs (n number of subsamples) for which we have a similiarty measure

    }
}
############################################################################################
############################################################################################




nCores <- as.numeric(Sys.getenv('NSLOTS')) #QUESTIONS, for the SCF clusters, how many cores can I use? should I use? 
registerDoParallel(nCores)


k.max = 12
Df <- lingBinary
number.samples =25
ratio.sub = 0.8

# i need to remember to delete the 1 correlations from each iteration

#there will be a diagonal number of 1s. 

parallel.results <- foreach(i = 1:k.max) %dopar% { #will specify k.max
  # Make sure that each process has its own random number stream.
  data.list <- k.stabalize(Df, i, number.samples, ratio.sub)
  filename <- sprintf("parallel%d.csv", i)
  write.csv(data.list, file=filename, quote=F, row.names=F)
  return(filename)
}

