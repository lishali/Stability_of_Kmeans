############################################################################################
############################################################################################
#we write a series of functions to compute a distribution of similiarity measures for each size k clustering.  The distirubtion is computed between pairs of subsamples from the dataset.  Details of the function struction follows, as well as approximate code line # where each function can be found.
############################################################################################
############################################################################################
#OUTLINE OF FUNCTIONS (details found in section where functions are defined. Code line given)

# 1) k.means():
#input: dataframe, number of subsamples, cluster size, subsample size.  output: a list of dataframes that are the subsamples of dataframe
#PARALLIZED. 33

# 2) Df.subsample()
#input: dataframe, number of subsamples, subsample size (in fraction of dataframe size).  output: list of dataframes, each dataframe being a subsample of original
#line: 60
#PARALLIZED with different random seeds.

#3) correlation similiarity : cor.sim()
#input: two dataframes, with the cluster information.  output: similarity between the two subsamples
#line: 86.  Calls on cluster_adjacency.cpp file

# 4) k.stabalize (input: dataframe, number of subsamples, cluster size, subsample size.  output, list of correlation measures for each pair of subsamples for a given k)
#this function calls on all 3 functions above.
#line: 103
#PARALLIZED. 

# 4) parallelize k. stablize for k in 1:k.max
#remember to "export OMP_NUM_THREADS=1" on command line when submitting shell script to execute optimize.R
#line: 126
############################################################################################
############################################################################################
library('Rcpp')
library('microbenchmark')
library('dplyr')

library(foreach)
require(doParallel)
require(parallel)
library(iterators)


nCores <- as.numeric(Sys.getenv('NSLOTS'))
registerDoParallel(nCores)

working.directory <- file.path("~/Documents/cluster")
lingBinary <- read.csv(file="lingBinary.csv")
############################################################################################
############################################################################################

#Correlation Similarity function: 

#we outsource the dotproduct function to C++ , imported below:
sourceCpp(file.path(working.directory, 'cluster_adjacency.cpp')) 

#more documentation in the cluster_adjacency.cpp file, however  The main benefit of this function, aside from using forloops in C++, is that we never form adjaceny matrices for each subset of variables (of the cluster information).  We simply access the dataframe to determine the total sum.

cor.sim <- function(X, Y) {
  #Errors
  if (nrow(X) !==nrow(Y)) stop("X and Y are not in comparable form, please take inner join of their rowid")
  if ("k.means" %in% colnames(X)==FALSE) stop("X does not have cluster information column named as k.means")
  if ("k.means" %in% colnames(Y)==FALSE) stop("Y does not have cluster information column named as k.means")
  
  num <- dot_product2(X,Y) #this is neccessary as we are only computing the dot product of the lower trangular matrix, but can retreive the total sum by symmetry.
  denom <- sqrt(dot_product2(X,X))*sqrt(dot_product2(Y,Y))
  return (num/denom)
}
# I also wrote a version of this in C++, but after testing on microbenchmark, there was no measurable difference, so to keep readability, I keep the cor.sim written in R.

############################################################################################
############################################################################################


# a function to pick my n subsamples and gather them into a list (of dataframes):

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
    set.seed(j)
    #I will use my Df.subsample function to obtain a list of dataframes of subsamples of the ratio.subsample size:
    #each item in the list is a dataframe
    subsample <- list.subsamples[[j]] #the jth element of this list is the jth subsample dataframe
    k.means <- kmeans(subsample[7:ncol(Df)],centers = k, nstart=25, iter.max=1000) #this was hardcoded in since the lingBinarize dataset's questions start on column 7 to the last ncol(Df), and this is the same for its subsamples
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

  foreach(m=1:length(list), .combine = 'cbind') %:% # we loops through all unique pairs (order does not matter since cor.sim(X,Y)=cor.sim(Y,X))
    foreach(l=1:m) %dopar% {  #note that we need only look up to m-1 since we don't want to compare the same elements of the list please see remark below prefaced by REMARK:
   
        subsample1 <- arrange(semi_join(list[[m]], list[[l]], by= "ID"), ID) #this restricts our attention to common points of list[[m]] and list[[l]].  The semi_join means subsample1 only has cluster info for b[[m]]
        subsample2 <- arrange(semi_join(list[[l]], list[[m]], by= "ID"), ID) #this will consider clusters in b[[l]], for common points of list[[m]] and list[[l]]
        #now we apply the cluster.adjacency() to subsample1 and subsample2, outputting two matrices we can feed into cor.sim() (this will be fast since I wrote the forloops of both functions in C++)
        
        cor.sim(subsample1,subsample2)
        #recall that correlationSim is a list defined for each k from 1 to k.max, so this is the list of n^2-n pairs (n number of subsamples) for which we have a similiarty measure

    }
}
#REMARK: it was syntactically difficult to delete the pairs of form (X,X)...that is, ones on the diagonal for which it is trivial that cor.sim(X,X) = 1. This created lists of different length which resisted being saved.  So I had to manually removd these later before plotting the histograms , not too hard as we know there are n_rows nonunique pairs giving us cor.sim of 1)
############################################################################################
############################################################################################




k.max =14
Df <- lingBinary
number.samples 40
ratio.sub = 0.8

# i need to remember to delete the 1 correlations from each iteration

#there will be a diagonal number of 1s. 

parallel.results <- foreach(i = 2:k.max) %dopar% { #will specify k.max
  # Make sure that each process has its own random number stream.
  data.list <- k.stabalize(Df, i, number.samples, ratio.sub)
  filename <- sprintf("work%d.csv", i)
  write.csv(data.list, file=filename, quote=F, row.names=F)
  return(filename)
}

