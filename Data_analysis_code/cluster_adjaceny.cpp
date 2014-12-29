#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]
IntegerMatrix cluster_lowertri(Rcpp::DataFrame Df) {
  // we assume the cluster row is named cluster, this can be written into the function
  int num_var = Df.nrows() ; //number of variables
  Rcpp::IntegerMatrix adj_cluster(num_var, num_var); //=  matrix(num_var, num_var) ;
  Rcpp::IntegerVector cluster = Df["cluster"] ;
  for (int i = 0; i < num_var; i++) { //now we loop through all tuples (var_1, var_2)
    for (int j = 0; j < i; j++) { //since the matrix is symmetric, we need only calculuate the lower diagonal
      if ( cluster[i] == cluster[j]) { // (i,j) entry is an indicator for whether variable i and variable j are in the same cluster
        adj_cluster(i,j) = 1 ;
      }
   }
  }
  return adj_cluster ;
}

