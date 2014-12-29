K.means.stabalize <- function(Df, k.max, num.samples, ratio.subsample){ 
  #K.means.stablize will return the following list of lists of the correlation similiarity.  The first level of list is indexed by k, the cluster size, the second gives the correlation similiarty for each k
  correlationSim <- list()
  
  #Df is my original lingbinarize dataset
  #k.max is the maximum number of clusters I want to calculate the similarity distribution for
  #num.samples is the number of random subsamples from the dataset Df
  #ratio.subsample is the size of each subsample expressed as a fraction
  
  #Errors and Warnings
  if (is.data.frame(Df) == FALSE) stop("Please give dataframe for Df")
  if (is.integer(k.max) == FALSE) stop("k.max must be an integer")
  if (ratio.subsample > 1 ) stop("ratio.subsample must be a double between 0 and 1")
  if (is.integer(num.samples) == FALSE) stop("num.samples must be an integer")
  
  #Other warnings can be given by the four functions called by k.means.stablize.  They are:
  #cor.sim (correlation similarity measure that calls on C++ code--written above)
  #Df.subsample (function written above, given a dataframe, it creates a list of dataframes of subsamples of the original dataframe)
  #cluster.adjacency (creates cluster adjaceny matrix to feed to cor.sim, also calls on C++ code--written above)
  #kmeans() drawn from R {stats} package
  
  #the following is a intermediate list "b" that is required to compute the correlation similiarity for each k
  for (i in 2:k.max){  #I will calculate the similarity distribution between pairs on kmeans applied for clusters for k = 2 to k.max 
    b <- paste("cluster.list", i)  # a "b" list is created for each k
    b <- list() #need to asign the pasted name variable b because R would be confused otherwise on why I am assigning an empty list to literal paste("cluster.list", i)
    
    #for each k, I will also intial a list 
    correlationSim_k <- list() # this I will feed to correlationSim after each loop of the outter forloop ((i in 2:k.max))
    
    
    for (j in 1:num.samples){ #I will calculate kmeans for each of the num.samples number of subsamples.   
      
      #I will use my Df.subsample function to obtain a list of dataframes of subsamples of the ratio.subsample size:
      list.subsamples <- Df.subsample(Df, num.samples, ratio.subsample) #each item in the list is a dataframe
      subsample <- list.subsamples[[j]] #the jth element of this list is the jth subsample dataframe
      k.means <- kmeans(subsample[7:474]) #this was hardcoded in since the lingBinarize dataset's questions start on column 7 to the last 474, and this is the same for its subsamples
      k.means <- k.means$cluster #this step extracts the cluster vector from the kmeans() output k.means$cluster.  
      k.means <- as.data.frame(k.means) #we need to use semi_joins so coerce into a dataframe, the rownames will still carry the ids of the subsamples.  
      k.means$ID <- rownames(k.means) #it's hard to semi_join by rownames, so I will create a new column called ID that copies the rownames info
      
      b[[length(b)+1]]] <- k.means #apending my new subsample cluster dataframe to the "b" list
#recall that b was the variable assigned to paste("cluster.list", i), so there is one b list per cluster size (k) 

#now we will call on elements of list b to make our list of the correlation similarities: 
#for each k, I now create list of the (k^2-k)/2 comparisons.  Hence, for each k, I will have a list of values

    } #this bracket signals the end of the "for (j in 1:num.samples)" for loop. At this point, we have created "b"L list of kmeans clusters for a particular k. We proceed by calculating correaltion similiarities between each pair of items in our list 'b'

for (m in 1:length(b)){ # we loops through all unique pairs (order does not matter since cor.sim(X,Y)=cor.sim(Y,X))
  for (l in 1:m-1){  #note that we need only look up to m-1 since we don't want to compare the same elements of the list (so m != l)
    subsample1 <- group_by(semi_join(b[[m]], b[[l]], by= ID), ID) #this restricts our attention to common points of b[[m]] and b[[l]].  The semi_join means subsample1 only has cluster info for b[[m]]
    subsample2 <- group_by(semi_join(b[[l]], b[[m]], by= ID), ID) #this will consider clusters in b[[l]], for common points of b[[m]] and b[[l]]
    #now we apply the cluster.adjacency() to subsample1 and subsample2, outputting two matrices we can feed into cor.sim() (this will be fast since I wrote the forloops of both functions in C++)
    
    #note that cluster.adjacency hardcodes the column name "k.means", which must be modified if we did not name the cluster column "k.means"  This can be found in the C++ code "cluster_adjacency.cpp"
    
    X <- cluster.adjacency(subsample1) #this creates an adjacency matrix for clusters in b[[k]] for points common to b[[k]] and b[[l]]
    Y <- cluster.adjacency(subsample2) #like above but for b[[l]]
    
    CorSim <- cor.sim(X,Y) #here we can apply the cor.sim function defined using C++, which outputs a double betwen 0 and 1
    
    
    correlationSim_k[[length(correlationSim_k)+1]] <- CorSim #value is appended to last in the list.
    #recall that correlationSim is a list defined for each k from 1 to k.max, so this is the list of n^2-n pairs (n number of subsamples) for which we have a similiarty measure
  }
}
correlationSim[[length(correlationSim)+1]] <- correlationSim_k #at the end of each k loop, I will have a list correlationSim_k that I append to my master list CorrelationSim
#the above will indeed be a list of lists
  }
return(correlationSim)
}
