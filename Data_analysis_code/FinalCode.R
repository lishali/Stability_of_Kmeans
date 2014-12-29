library('Rcpp')
library('microbenchmark')
library('dplyr')

library(foreach)
require(doParallel)
require(parallel) 
library(iterators)


working.directory <- file.path("/Users/LishaLi/Desktop/215A/Lab3")
load("~/Desktop/215A/Lab3/lingBinary.RData")

lingBinary <- tbl_df(lingBinary) #this is the dataset I use to evaluate the stability of the k.means algorithm
write.csv(lingBinary, file="lingBinary.csv")
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
  num.row <- nrow(X)
  num <- 2*dot_product(X,Y)-num.row #this is neccessary as we are only computing the dot product of the lower trangular matrix. 
  denom <- sqrt(2*dot_product(X,X)- num.row)*sqrt(2*dot_product(Y,Y)-num.row)
  return (num/denom)
}


############################################################################################
############################################################################################


# a function to pick my n% subsamples: 

Df.subsample <-function(Df, n.subsample, ratio.subsample){
  #Data.frame is the binarized dataframe, we assume that a function already created an ID column
  #n.subsample is the number of subsamples
  #size.subsample is the size of the subsample given in percentage.
  
  #Errors
  if (ratio.subsample > 1) stop("Please give subsample size in terms of ratio of the total size.")
  if (n.subsample < 0) stop("n.subsample negative value: invalid.")
  if (ratio.subsample < 0) stop("ratio.subsample negative value: invalid.")
  
  #Subsample index
  sample.df.list = list() #initialize list of dataframes
  
  for (i in 1:n.subsample) {
    sample <- sample_frac(Df, size = ratio.subsample, replace = FALSE) #sample rows of dataframe without replacement
    sample.df.list[[length(sample.df.list)+1]] <- sample #append this subsample dataframe to list
  }
  
  return(sample.df.list) #return my list of clustered dfs so I can compute that correlation matrix  
  
} 

############################################################################################
############################################################################################

#Remark: I know that this algorithm is most efficient for hierarchical methods, since only one clustering call is needed per subsample (since clusters for different ks can be read off from the same tree)
#however, I used kmeans the most for my Lab2, so I will use this now to verify its stability
# I could probaby have parallelized this function in addition to the last, however, since I am appending things to a list, it was safer to do these steps in seriel to avoid overwriting.  Each forloop call in this function also uses my C++ functions, so the loss in doing it in seriel is not monumental.

k.stabalize <- function(Df, k, num.samples, ratio.subsample){ 
  #K.means.stablize will return the following list of the correlation similiarity. 
  correlationSim <- list()
  
  #Df is my original lingbinarize dataset
  #k is the number of clusters I want to calculate the similarity distribution for
  #num.samples is the number of random subsamples from the dataset Df
  #ratio.subsample is the size of each subsample expressed as a fraction
  
  #Errors and Warnings
  if (is.data.frame(Df) == FALSE) stop("Please give dataframe for Df")
  if (ratio.subsample > 1 ) stop("ratio.subsample must be a double between 0 and 1")

  #Other warnings can be given by the four functions called by k.means.stablize.  They are:
  #cor.sim (correlation similarity measure that calls on C++ code--written above)
  #Df.subsample (function written above, given a dataframe, it creates a list of dataframes of subsamples of the original dataframe)
  #cluster.adjacency (creates cluster adjaceny matrix to feed to cor.sim, also calls on C++ code--written above)
  #kmeans() drawn from R {stats} package
  
  list <- list()  # I initialize a list of kmeans results of the subsamples, this will not be returned, but is needed for the function calls that give us our correlation similarity
  list.subsamples <- Df.subsample(Df, num.samples, ratio.subsample) #my lsit of dataframes of subsampels
  for (j in 1:num.samples){ #I will calculate kmeans for each of the num.samples number of subsamples.   
    #I will use my Df.subsample function to obtain a list of dataframes of subsamples of the ratio.subsample size:
     #each item in the list is a dataframe
    subsample <- list.subsamples[[j]] #the jth element of this list is the jth subsample dataframe
    k.means <- kmeans(subsample[7:ncol(Df)],centers = k, nstart=40, iter.max=5000) #this was hardcoded in since the lingBinarize dataset's questions start on column 7 to the last ncol(Df), and this is the same for its subsamples
    k.means <- k.means$cluster #this step extracts the cluster vector from the kmeans() output k.means$cluster.  
    k.means <- as.data.frame(k.means) #we need to use semi_joins so coerce into a dataframe, the rownames will still carry the ids of the subsamples.  
    k.means$ID <- as.numeric(rownames(k.means)) #it's hard to semi_join by rownames, so I will create a new column called ID that copies the rownames info
    
    list[[length(list)+1]] <- k.means #apending my new subsample cluster dataframe to the list  (this is a list of dataframes)
    
    #now we will call on elements of list to make our list of the correlation similarities: 
    #given k, I now create list of the (k^2-k)/2 comparisons.  
  } #this bracket signals the end of the "for (j in 1:num.samples)" for loop. 
  #At this point, we have created list of kmeans clusters for a particular.  We proceed by calculating correaltion similiarities between each pair of items in our list 
  
  for (m in 1:length(list)){ # we loops through all unique pairs (order does not matter since cor.sim(X,Y)=cor.sim(Y,X))
    for (l in 1:m){  #note that we need only look up to m-1 since we don't want to compare the same elements of the list (so m != l)
      if (m != l){
        
      
      subsample1 <- arrange(semi_join(list[[m]], list[[l]], by= "ID"), ID) #this restricts our attention to common points of list[[m]] and list[[l]].  The semi_join means subsample1 only has cluster info for b[[m]]
      subsample2 <- arrange(semi_join(list[[l]], list[[m]], by= "ID"), ID) #this will consider clusters in b[[l]], for common points of list[[m]] and list[[l]]
      #now we apply the cluster.adjacency() to subsample1 and subsample2, outputting two matrices we can feed into cor.sim() (this will be fast since I wrote the forloops of both functions in C++)
      
      #note that cluster.adjacency hardcodes the column name "k.means", which must be modified if we did not name the cluster column "k.means"  This can be found in the C++ code "cluster_adjacency.cpp"
      
      X <- cluster.adjacency(subsample1) #this creates an adjacency matrix for clusters in list[[k]] for points common to list[[k]] and list[[l]]
      Y <- cluster.adjacency(subsample2) #like above but for list[[l]]
      
      correlationSim[[length(correlationSim)+1]] <- cor.sim(X,Y) #value is appended to last in the list.
      #recall that correlationSim is a list defined for each k from 1 to k.max, so this is the list of n^2-n pairs (n number of subsamples) for which we have a similiarty measure
      }
    }
  }
  return(correlationSim)
}

############################################################################################
############################################################################################
#finally, putting it all together!



nCores <- as.numeric(Sys.getenv('NSLOTS')) #QUESTIONS, for the SCF clusters, how many cores can I use? should I use? 
registerDoParallel(nCores)


k.max = 12
Df <- lingBinary
num.samples = 25
ratio.subsample = 0.9


parallel.results <- foreach(i = 1:k.max) %dopar% { #will specify k.max
  # Make sure that each process has its own random number stream.
  set.seed(i)
  data.list <- k.stabalize(Df, i, num.samples, ratio.subsample) 
  filename <- sprintf("K_cluster%d.cvs", i)
  write.csv(data.list, file=filename,
            quote=F, row.names=F)
  return(filename)
}

#questions, RNG seed only placed above, do I need it for my subsamples?
#how will the cluster know what the files I draw from, shoudl they be in a certain format?
# Can I submit this entire R file? 
#where would I set the address?



############################################################################################
############################################################################################
