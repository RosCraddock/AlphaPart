#include "AlphaPartDrop.h"

SEXP AlphaPartDrop(SEXP c1_, SEXP c2_, SEXP nI_, SEXP nP_, SEXP nT_, SEXP y_, SEXP P_, SEXP Px_, SEXP upgCon_, SEXP nGP_)
{
  using namespace Rcpp ;
  //' @export

  // --- Temp ---
      
  int i, j, k, t, p, pt, mt;
  
  // --- Inputs ---
      
  double c1 = Rcpp::as<double>(c1_);  
  double c2 = Rcpp::as<double>(c2_); 
  int nI = Rcpp::as<int>(nI_); 
  int nP = Rcpp::as<int>(nP_);
  int nT = Rcpp::as<int>(nT_);
  int nGP = Rcpp::as<int>(nGP_);
  Rcpp::NumericMatrix ped(y_);
  Rcpp::IntegerVector P(P_);  
  Rcpp::IntegerVector Px(Px_);
  Rcpp::NumericMatrix upgCon(upgCon_);
  
  // --- Outputs ---
      
  Rcpp::NumericMatrix pa(nI+1, nT*nGP);    // parent average
  Rcpp::NumericMatrix  w(nI+1, nT*nGP);    // Mendelian sampling
  Rcpp::NumericMatrix xa(nI+1, nP*nT); // Parts

  // --- Compute ---
      
  for(i = 1; i < nI+1; i++) {
    for(t = 0; t < nT; t++) {
      // Parent average (PA)
      if (ped(i, 1) == 0 && ped(i,2) == 0){ // as all UPG assigned will be to both founders
        pa(i, t) = upgCon(i, t);
      }
      else {
        pa(i, t) = c1 * ped(ped(i, 1), 3+t) +
          c2 * ped(ped(i, 2), 3+t);
      }
      
    
      // Mendelian sampling (MS)
      w(i, t) = ped(i, 3+t) - pa(i, t);
      
      // Gamete Partitioning
      if (nGP == 3) {
        pt = t + nT; // paternal trait index
        mt = t + 2*nT; // maternal trait index
        if (ped(i, 1) == 0 && ped(i,2) == 0){
          pa(i, pt) = c1*upgCon(i, t);
          pa(i, mt) = c2*upgCon(i, t);
        } 
        else {
          pa(i, pt) = c1 * ped(ped(i,1), 3+pt) + c1 * ped(ped(i,1), 3+mt);
          pa(i, mt) = c2 * ped(ped(i,2), 3+pt) + c2 * ped(ped(i,2), 3+mt);
        }
        w(i, mt) = ped(i, 3+mt) - pa(i, mt);
        w(i, pt) = ped(i, 3+pt) - pa(i, pt);
      }
    
      // Parts

      // ... for the MS part
      j = Px[t] + P[i];
      xa(i, j) = w(i, t);

      // ... for the PA parts
      for(p = 0; p < nP; p++) {
        j = Px[t] + p;
        xa(i, j) += c1 * xa(ped(i, 1), j) +
                    c2 * xa(ped(i, 2), j);
      }
      
      // collect the upg contributions
      if (ped(i,1) != 0 || ped(i,2) != 0){
        upgCon(i, t) = c1 * upgCon(ped(i, 1), t) +
          c2 * upgCon(ped(i, 2), t); 
      }
    }
  }
  
  // --- Return ---

  return Rcpp::List::create(Rcpp::Named("pa", pa),
                            Rcpp::Named("w",  w),
                            Rcpp::Named("xa", xa),
                            Rcpp::Named("upgCon", upgCon));
}
