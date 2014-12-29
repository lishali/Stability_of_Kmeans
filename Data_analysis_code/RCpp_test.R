library(Rcpp)
library('microbenchmark')


cppFunction('int add(int x, int y, int z) {
int sum = x+ y + z;
return sum;
}')
add(1,2,4)

#comparison: 

oneR <- function() 1L
cppFunction('int oneC() {
            return 1;
            }')

x <- 123456789
y <-987654321
z <- 2389193847193847193874

microbenchmark(oneR(), oneC(), times=5)


signR <- function(x) {
  if (x > 0) {
    1
  } else if (x == 0) {
    0
  } else {
    -1
  }
}

cppFunction('int signC(int x) {
  if (x > 0) {
    return 1;
  } else if (x == 0) {
    return 0;
  } else {
    return -1;
  }
}')

microbenchmark(signC(-11241349), signR(-11241349), times=5)
#forloops are not your friend in R

sumR <- function(x) {
  total <- 0
  for (i in seq_along(x)) {
    total <- total + x[i]
  }
  total
}

cppFunction('double sumC(NumericVector x) {


  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total;
}')

#In C++, vector indices start at 0. I’ll say this again because it’s so important:
#IN C++, VECTOR INDICES START AT 0! 

#check it out!

x <- runif(1e3) # this gives a vector of random numbers of the size of the argument
microbenchmark(
  sum(x), 
  sumC(x), 
  sumR(x)
)

#clearly sum(x) is already optimized in C, but sumC does so much better than sumR 



#now let's try a function that also spits out vectors: 

cppFunction('NumericVector pdistC(double x, NumericVector ys) {
  int n = ys.size();
  NumericVector out(n);

  for(int i = 0; i < n; ++i) {
    out[i] = sqrt(pow(ys[i] - x, 2.0));
  }
  return out;
}')

#the R version: 
pdistR <- function(x, ys) {
  sqrt((x - ys) ^ 2)
}


#rowsums

cppFunction('NumericVector rowSumsC(NumericMatrix x) {
  int nrow = x.nrow(), ncol = x.ncol();
  NumericVector out(nrow);

  for (int i = 0; i < nrow; i++) {
    double total = 0;
    for (int j = 0; j < ncol; j++) {
      total += x(i, j);
    }
    out[i] = total;
  }
  return out;
}')
set.seed(1014)
x <- matrix(sample(100), 10)


