#include "nmath.h"

double runif(double a, double b)
{
    if (!R_FINITE(a) || !R_FINITE(b) || b < a)	ML_ERR_return_NAN;

    if (a == b)
	    return a;
    else {
	    double u;
	    do {u = unif_rand();} while (u <= 0 || u >= 1);
	    return a + (b - a) * u;
    }
}