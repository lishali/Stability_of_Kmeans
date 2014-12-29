// X is a dataset  

#include <Rcpp.h> 
#include <iostream>

// [[Rcpp::export]]
int dot_product(Rcpp::IntegerMatrix X, Rcpp::IntegerMatrix Y) {
//symmetric, so make code more effic
// our function simply calculates the matrix dot product (sum of the element-wise product)
int x_row = X.nrow() ;
int x_col = X.ncol() ;
int y_row = Y.nrow() ;
int y_col = Y.ncol() ;

if (x_row != y_col or x_col != y_col or x_row != x_col or y_col != y_row) {
     Rcpp::Rcout << "Error: X and Y are not the same dimensions and/or X, Y are not square matrices.\n";
}

int total = 0 ;
    for ( int j = 0; j < x_row; j++ ){
       for (int i = 0; i < j+1; i++) {
        total += X(j,i)*Y(j,i) ; 
     }
    }
//because our cluster adjaceny matrices X and Y are both symmetric, so their pointwise sum in the lower trianglar is the same as that in the upper. 
return total ;
}

// now my correlation similiarty function is simply the dot product f X and Y normalized by the L^2 norm of X times the L^2 norm of Y. 
//geometrically speaking, if we interpret the columns as points in n-dim vector space, the correlation similarity is the angle between the two lines of least squares best fit
// I will use a double because it has more precision 




