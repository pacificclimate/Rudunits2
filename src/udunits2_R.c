/*
  James Hiebert <hiebert@uvic.ca>
  Pacific Climate Impacts Consortium
  August, 16, 2010

  Functions to support the R interface to the udunits (API version 2) library
*/

#include <R.h>
#include <udunits2.h>
#include <stdio.h> /* FILENAME_MAX */

ut_system *sys = NULL;
static ut_encoding enc;

/* From the enum comments in udunits2.h */
const char * ut_status_strings[] = {
  "Success",
  "An argument violates the function's contract",
  "Unit, prefix, or identifier already exists",
  "No such unit exists",
  "Operating-system error.  See \"errno\".",
  "The units belong to different unit-systems",
  "The operation on the unit(s) is meaningless",
  "The unit-system doesn't have a unit named \"second\"",
  "An error occurred while visiting a unit",
  "A unit can't be formatted in the desired manner",
  "string unit representation contains syntax error",
  "string unit representation contains unknown word",
  "Can't open argument-specified unit database",
  "Can't open environment-specified unit database",
  "Can't open installed, default, unit database",
  "Error parsing unit specification"
};

void handle_error(const char *calling_function) {
  ut_status stat;
  stat = ut_get_status();
  error("Error in function %s: %s", calling_function, ut_status_strings[stat]);
}

void R_ut_init(void) {
  ut_status stat;

  ut_set_error_message_handler(ut_write_to_stderr);
  if (sys != NULL) {
    ut_free_system(sys);
  }
  ut_set_error_message_handler(ut_ignore);
  sys = ut_read_xml(NULL);
  ut_set_error_message_handler(ut_write_to_stderr);
  if (sys == NULL) {
    stat = ut_get_status();
    ut_handle_error_message("Warning in R_ut_init: %s", ut_status_strings[stat]);
    return;
  }
  enc = UT_UTF8;
  return;
}

void R_ut_has_system(int *exists) {
  if (sys != NULL) {
    *exists = 1;
  }
  else {
    *exists = 0;
  }
  return;
}

/* Take an encoding string and set the global var enc */
void R_ut_set_encoding(const char * const *enc_string) {

  size_t length = strlen(*enc_string);

  if (strncmp(*enc_string, "utf8", length) == 0) {
    enc = UT_UTF8;
  }
  else if (strncmp(*enc_string, "ascii", length) == 0) {
    enc = UT_ASCII;
  }
  else if (strncmp(*enc_string, "iso-8859-1", length) == 0 ||
	   strncmp(*enc_string, "latin1", length) == 0) {
    enc = UT_LATIN1;
  }
  else {
    error("Valid encoding string parameters are ('utf8'|'ascii'|'iso-8859-1','latin1')");
  }
  return;
}

void R_ut_is_parseable(char * const *units_string, int *parseable) {
  ut_unit *result;

  if (sys == NULL) {
    R_ut_init();
  }

  ut_trim(*units_string, enc);
  result = ut_parse(sys, *units_string, enc);
  if (result == NULL) {
    *parseable = 0;
  }
  else {
    *parseable = 1;
  }
  ut_free(result);
  return;
}

void R_ut_are_convertible(char * const *ustring1, char * const *ustring2, int *convertible) {
  ut_unit *u1, *u2;

  if (sys == NULL) {
    R_ut_init();
  }

  ut_trim(*ustring1, enc); ut_trim(*ustring2, enc);
  u1 = ut_parse(sys, *ustring1, enc);
  u2 = ut_parse(sys, *ustring2, enc);

  if (!(u1 && u2)) {
    handle_error("R_ut_are_convertible");
  }

  if (ut_are_convertible(u1, u2) == 0) {
    *convertible = 0;
  }
  else {
    *convertible = 1;
  }
  ut_free(u1); ut_free(u2);
  return;
}

void R_ut_convert(const double *x, int *count, char * const *units_from, char * const *units_to, double *rv) {
  ut_unit *from, *to;
  cv_converter *conv;

  if (sys == NULL) {
    R_ut_init();
  }

  ut_trim(*units_from, enc); ut_trim(*units_to, enc);

  from = ut_parse(sys, *units_from, enc);
  if (from == NULL) {
    handle_error("R_ut_convert");
    return;
  }

  to = ut_parse(sys, *units_to, enc);
  if (from == NULL) {
    handle_error("R_ut_convert");
    return;
  }
  conv = ut_get_converter(from, to);
  if (conv == NULL) {
    handle_error("R_ut_convert");
    return;
  }
  cv_convert_doubles(conv, x, (size_t) *count, rv);

  // Cleanup
  cv_free(conv);
  ut_free(to);
  ut_free(from);
  return;
}

void R_ut_get_name(char * const *ustring, char **rstring) {
  ut_unit *u;
  char *trimmed;
  char *s;
  trimmed = ut_trim(*ustring, enc);
  u = ut_parse(sys, trimmed, enc);

  if (!u) {
    handle_error("R_ut_get_name");
  }

  s = (char *) ut_get_name(u, enc); // FIXME: ut_get_name seems to allocate the string... does it need to be free-ed?

  if (s == NULL) return;
  else *rstring = s;

  return;
}

void R_ut_get_symbol(char * const *ustring, char **rstring) {
  ut_unit *u;
  char *trimmed;
  char *s;
  trimmed = ut_trim(*ustring, enc);
  u = ut_parse(sys, trimmed, enc);

  if (!u) {
    handle_error("R_ut_get_symbol");
  }

  s = (char *) ut_get_symbol(u, enc); // FIXME: ut_get_symbol seems to allocate the string... does it need to be free-ed?

  if (s == NULL) return;
  else *rstring = s;

  return;
}
