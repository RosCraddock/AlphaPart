#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix AlphaPartDropGroup(double c1, double c2, int nI, int nP, int nT, int nG,
                                 NumericMatrix ped, IntegerVector P, IntegerVector Px,
                                 IntegerVector g) {
  // --- Temp ---

  int i, j, t, p;

  // --- Outputs ---

  NumericMatrix pa(nI+1, nT);    // Parent average
  NumericMatrix ms(nI+1, nT);    // Mendelian sampling
  NumericMatrix xa(nI+1, nP*nT); // Partitions
  NumericMatrix xg(nG+1, nP*nT); // Partitions for groups
  // NOTE: Rcpp::NumericMatrix is filled by 0s by default

  // TODO: Maybe we want an algorithm that works on one trait at a time to save on memory?
  //       https://github.com/AlphaGenes/AlphaPart/issues/15
  
  // TODO: Pass pedigree by reference to improve memory use #13
  //       https://github.com/AlphaGenes/AlphaPart/issues/13

  // --- Compute ---

  for(i = 1; i < nI+1; i++) {
    for(t = 0; t < nT; t++) {
      // Parent average (PA)
      pa(i, t) = c1 * ped(ped(i, 1), 3+t) +
                 c2 * ped(ped(i, 2), 3+t);

      // Mendelian sampling (MS)
      ms(i, t) = ped(i, 3+t) - pa(i, t);

      // Parts

      // ... for the MS part
      j = Px[t] + P[i];
      xa(i, j) = ms(i, t);

      // ... for the PA part
      for(p = 0; p < nP; p++) {
        j = Px[t] + p;
        xa(i, j) += c1 * xa(ped(i, 1), j) +
                    c2 * xa(ped(i, 2), j);
        // Rprintf("Animal: %i, Trait: %i, Path: %i (%i), Group: %i\\n", i, t, p, j, g(i));
        xg(g(i), j) += xa(i, j);
      }
    }
  }

  // --- Return ---

  return xg;
}
