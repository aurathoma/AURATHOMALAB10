#include <Rcpp.h>
using namespace Rcpp;

/*
 ============================================================
 Helper function: call_quantile
 ------------------------------------------------------------
 Purpose:
 Call an R quantile function (e.g. qnorm, qbeta) dynamically
 using do.call(), passing a probability and distribution
 parameters stored in a List.
 ============================================================
 */
double call_quantile(const Function& qfun,
                     double p,
                     const List& args) {

  List call_args = clone(args);
  call_args["p"] = p;
  Function do_call("do.call");

  return as<double>(do_call(qfun, call_args));
}

/*
 ============================================================
 Helper function: call_density
 ------------------------------------------------------------
 Purpose:
 Call an R density function (e.g. dnorm, dbeta) dynamically
 using do.call(), passing an evaluation point and parameters.
 ============================================================
 */
double call_density(const Function& dfun,
                    double x,
                    const List& args) {

  List call_args = clone(args);
  call_args["x"] = x;
  Function do_call("do.call");

  return as<double>(do_call(dfun, call_args));
}

/*
 ============================================================
 Function: dendiff
 ------------------------------------------------------------
 Purpose:
 Compute the difference in density values at the lower and
 upper candidate endpoints of a Highest Density Interval (HDI).
 ============================================================
 */
double dendiff(double x,
               const Function& dfun,
               const Function& qfun,
               const List& args,
               double alpha) {

  double a2 = alpha - x;
  double qa1 = call_quantile(qfun, x, args);
  double qa2 = call_quantile(qfun, 1.0 - a2, args);
  double d1 = call_density(dfun, qa1, args);
  double d2 = call_density(dfun, qa2, args);

  // Root is found when d1 - d2 = 0 (Equal Density Constraint)[cite: 7]
  return d1 - d2;
}

/*
 ============================================================
 Function: bisect_root
 ------------------------------------------------------------
 Purpose:
 Numerically solve dendiff(x) = 0 using the bisection method
 over the interval x ∈ [0, alpha][cite: 7].
 ============================================================
 */
double bisect_root(const Function& dfun,
                   const Function& qfun,
                   const List& args,
                   double alpha,
                   double tol = 1e-8,
                   int maxiter = 100) {

  double lower = 0.0;
  double upper = alpha;
  double mid = 0.0;

  for (int i = 0; i < maxiter; i++) {
    mid = 0.5 * (lower + upper);
    double fmid = dendiff(mid, dfun, qfun, args, alpha);

    if (std::abs(fmid) < tol)
      return mid;

    double flower = dendiff(lower, dfun, qfun, args, alpha);

    if (flower * fmid < 0)
      upper = mid;
    else
      lower = mid;
  }

  return mid;
}

/*
 ============================================================
 Exported Function: hdicalc_cpp
 ------------------------------------------------------------
 Purpose:
 Compute the Highest Density Interval (HDI) and Equal-Tailed
 Interval (ETI) for any user-specified distribution.
 ============================================================
 */

// [[Rcpp::export]]
List hdicalc_cpp(std::string stem = "norm",
                 List args = List::create(
                   Named("mean") = 10,
                   Named("sd") = 5),
                   double alpha = 0.05) {

  std::string dname = "d" + stem;
  std::string qname = "q" + stem;

  Function dfun(dname);
  Function qfun(qname);

  // Find lower tail probability defining HDI via bisection
  double alpha1 = bisect_root(dfun, qfun, args, alpha);
  double alpha2 = alpha - alpha1;

  // HDI endpoints
  double hdi_lower = call_quantile(qfun, alpha1, args);
  double hdi_upper = call_quantile(qfun, 1.0 - alpha2, args);

  // ETI endpoints
  double eti_lower = call_quantile(qfun, alpha / 2.0, args);
  double eti_upper = call_quantile(qfun, 1.0 - alpha / 2.0, args);

  // Return structured results as an R list
  return List::create(
    Named("alpha")  = alpha,
    Named("alpha1") = alpha1,
    Named("alpha2") = alpha2,
    Named("HDI")    = NumericVector::create(hdi_lower, hdi_upper),
    Named("RHDI")   = hdi_upper - hdi_lower,
    Named("ETI")    = NumericVector::create(eti_lower, eti_upper),
    Named("RETI")   = eti_upper - eti_lower
  );
}
