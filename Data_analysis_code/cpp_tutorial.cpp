// now let's learn how to create a CPP file and allow it to be sourced in R
//awesome tutorial at : http://adv-r.had.co.nz/Rcpp.html


// to run the code below, we need to type into the console: 

//sourceCpp("/Users/LishaLi/Desktop/215A/Lab3/cpp_tutorial.cpp")


//If calling from another file, we also just write sourceCpp("path/to/file.cpp")

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double meanC(NumericVector x) {
  int n = x.size();
  double total = 0;
  
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total / n;
}

// below we can run R commands in here  
/*** R
library(microbenchmark)
x <- runif(1e5)
microbenchmark(
  mean(x),
  meanC(x)
)
*/

//R sugar makes the syntax of C++ much more similiar to R

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector pdistC2(double x, NumericVector ys) {
  return sqrt(pow((x - ys), 2));
}

//Sugar functions: 
// abs(), acos(), asin(), atan(), beta(), ceil(), 
//ceiling(), choose(), cos(), cosh(), digamma(), 
//exp(), expm1(), factorial(), floor(), gamma(), 
//lbeta(), lchoose(), lfactorial(), lgamma(), log(),
//log10(), log1p(), pentagamma(), psigamma(), 
//round(), signif(), sin(), sinh(), sqrt(), tan(), 
//tanh(), tetragamma(), trigamma(), trunc()
//mean(), min(), max(), sum(), sd(), and (for vectors) var()
//match(), self_match(), which_max(), which_min()
//duplicated(), unique()

//using iterators to make things easier STL: 

// [[Rcpp::export]]
double sum3(NumericVector x) {
  double total = 0;
  
  NumericVector::iterator it;
  for(it = x.begin(); it != x.end(); ++it) {
    total += *it;
  }
  return total;
}

// GIBBS SAMPLER!  








