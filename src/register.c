#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* .Call calls */
extern SEXP _imputeTS_locf(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_imputeTS_locf", (DL_FUNC) &_imputeTS_locf, 2},
    {NULL, NULL, 0}
};

void R_init_imputeTS(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
