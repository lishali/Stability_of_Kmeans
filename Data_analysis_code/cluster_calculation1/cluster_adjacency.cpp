#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
int dot_product2(Rcpp::DataFrame Df1, Rcpp::DataFrame Df2) {
    //we assumed each Df already has column "k.means"
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

