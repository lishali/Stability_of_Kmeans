#include <Rcpp.h>
using namespace Rcpp;

//the following function takes in two data frames, Df1 and Df2 and calculuates the element wise product and sums this into a total.  
// Df1 and Df2 are dataframes for subsamples of the variables in lingData, and we assume they contain a column called "k.means", which gives the cluster number for each point
//to apply dot_product2 (function defined below) we need to only consider the common variables in Df1 and Df2.  This is also assumed, and later on in R, we make sure our dataframes are in this form before giving them to dot_product2 are arguments
//Since the adjacency information is symmetric, we take a further shortcut by just calculating the sum for the lower triangle, taking twice this amount and subtracing away the diagonal (which is a sum of n_row 1s)

//The main benefit of this function, aside from using forloops in C++, is that we never form adjaceny matrices for each subset of variables (of the cluster information).  We simply access the dataframe to determine the total sum. 

// [[Rcpp::export]]
int dot_product2(Rcpp::DataFrame Df1, Rcpp::DataFrame Df2) {
    //we will assumed each Df is a subsample, and already has column "k.means"
    int num_row = Df1.nrows() ;
    int total = 0 ;
    Rcpp::IntegerVector clusterD1 = Df1["k.means"] ;
    Rcpp::IntegerVector clusterD2 = Df2["k.means"] ;
    for (int j=0 ; j < num_row; j++){
        for (int i = 0; i < j+1; i++){
            if (clusterD1[i]==clusterD1[j] and clusterD2[i]==clusterD2[j]){
                total += 1 ;
            }
        }
    }
    return 2*total-num_row ; //this is because our matrices are symmetric, so pointwise product of the two matrices is symmetric, so we only computed the lower triangular sum of this dot product, including the diagonal.  Thus to get the total, we times 2 and subtract the overcounted diagonal (num_row). 
}

