#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void R_ut_are_convertible(void *, void *, void *);
extern void R_ut_convert(void *, void *, void *, void *, void *);
extern void R_ut_get_name(void *, void *);
extern void R_ut_get_symbol(void *, void *);
extern void R_ut_has_system(void *);
extern void R_ut_init(void *);
extern void R_ut_is_parseable(void *, void *);
extern void R_ut_set_encoding(void *);

static const R_CMethodDef CEntries[] = {
    {"R_ut_are_convertible", (DL_FUNC) &R_ut_are_convertible, 3},
    {"R_ut_convert",         (DL_FUNC) &R_ut_convert,         5},
    {"R_ut_get_name",        (DL_FUNC) &R_ut_get_name,        2},
    {"R_ut_get_symbol",      (DL_FUNC) &R_ut_get_symbol,      2},
    {"R_ut_has_system",      (DL_FUNC) &R_ut_has_system,      1},
    {"R_ut_init",            (DL_FUNC) &R_ut_init,            1},
    {"R_ut_is_parseable",    (DL_FUNC) &R_ut_is_parseable,    2},
    {"R_ut_set_encoding",    (DL_FUNC) &R_ut_set_encoding,    1},
    {NULL, NULL, 0}
};

void R_init_udunits2(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
