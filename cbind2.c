#include <Rdefines.h>

SEXP
R_cbind2(SEXP a, SEXP b)
{
    int n = Rf_length(a), i;
    SEXP ans;
    PROTECT(ans = Rf_allocMatrix(STRSXP, n, 2));
    for(i = 0; i < n ; i++) {
	SET_STRING_ELT(ans, i, STRING_ELT(a, i));
	SET_STRING_ELT(ans, i + n, STRING_ELT(b, i));	
    }
    UNPROTECT(1);
    return(ans);
}
