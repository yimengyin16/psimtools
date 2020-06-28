#include <Rcpp.h>
using namespace Rcpp;

// This is a simple function using Rcpp that creates an R list
// containing a character vector and a numeric vector.
//
// Learn more about how to use Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//
// and browse examples of code using Rcpp at:
//
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
List rcpp_hello() {
  CharacterVector x = CharacterVector::create("foo", "bar");
  NumericVector y   = NumericVector::create(0.0, 1.0);
  List z            = List::create(x, y);
  return z;
}



//' Account balances
//'
//' @param balance vector to store balances
//' @param C Cash flow
//' @param r Returns
//' @return Balances
//' @export
// [[Rcpp::export]]
NumericVector fnC(NumericVector balance, NumericVector C, NumericVector r) {
	int N = balance.size();
	NumericVector out(N);

	out[0] = balance[0];

	for(int j = 1; j < N; ++j ) {
		out[j] = (out[j-1] + C[j-1]) * (1 + r[j-1]);
	}

	return out;
}



