#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


//' Binary search cpp version
//'
//' @param x A double to be find
//' @param y A double vector
//' @return A integer of position
//' @author Zhaowei
//' @export
// [[Rcpp::export]]
int binary_search_cpp(double x, NumericVector y) {
  int startIdx = 0;
  int endIdx = y.size() - 1;

  while (startIdx <= endIdx) {
    int midIdx = (startIdx + endIdx) >> 1;
    double midVal = y[midIdx];

    if (midVal < x) {
      startIdx = midIdx + 1;
    } else if (midVal > x) {
      endIdx = midIdx - 1;
    } else if (fabs(x - midVal) < 1e-8) {
         return midIdx;
    }
  }
  return startIdx;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
binary_search_cpp(9.6, 1:10)
binary_search_cpp(1.2, 1:10)
binary_search_cpp(5.0, 1:10)
binary_search_cpp(0.6, 1:10)
binary_search_cpp(10.6, 1:10)
*/
