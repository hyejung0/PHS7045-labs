//PHS7045 Lab 5
//Question1

#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List ps_match1(const NumericVector & x) {
  // const = means we are keeping x as constant without being anble to modify.
  // & = c++ it will create a pointer (reference) to the original x. Without &, it will create a new copy of x to work with.
  int n=x.size();
  
  IntegerVector indices(n);
  NumericVector values(n);
  
  for (int i = 0; i < n; ++i) {

    int best_n = 0;
    double best_dist = std::numeric_limits<double>::max(); //std is a library, and numeric_limits is a library
    
    for (int j = 0; j < n; ++j) {
      
      if(i==j)
        continue; //if we are comparing it to itself, we don't want it.
      
      double current_dist = abs(x[i] - x[j]);
      if (current_dist<best_dist) {
        best_dist=current_dist;
        best_n=j;
      }
      

    }

    indices[i]=best_n;
    values[i] = x[best_n];    

  }
  
  
  return List::create(
    _["match_id"] = indices,
    _["match_x"] = values
  );
  // return List::create(); //return empty list to check if the c++ code is running fine as we build it.
  
}

/*** R

ps_matchR <- function(x) {
  
  match_expected <- as.matrix(dist(x))
  diag(match_expected) <- .Machine$integer.max
  indices <- apply(match_expected, 1, which.min)
  
  list(
    match_id = as.integer(unname(indices)),
    match_x  = x[indices]
  )
  
}
set.seed(1231)
x<-runif(5)
ps_matchR(x)
ps_match1(x)
*/
