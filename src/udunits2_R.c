/*
  James Hiebert <hiebert@uvic.ca
  Pacific Climate Impacts Consortium
  August, 16, 2010

  Functions to support the R interface to the udunits (API version 2) library
*/

#include <R.h>
#include <udunits2.h>
#include <stdio.h>

static int module_initialized = 0;
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
  if (! module_initialized) {
    ut_set_error_message_handler(ut_ignore);
    sys = ut_read_xml(NULL);
    ut_set_error_message_handler(ut_write_to_stderr);
    enc = UT_UTF8;
    module_initialized = 1;
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

  R_ut_init();

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

  R_ut_init();

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
  R_ut_init();

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

void test(void) {
  printf("Hello world!\n");
  printf("Yes I am plugged in\n");
  return;
}

int main(void) {
  R_ut_init();
  char name_buf[256];
  char *s;
  int length;
  const char *units_from = "miles";
  const char *units_to = "km";
  const double miles[10] = {1., 2., 3., 4., 5., 6., 7., 8., 9., 10.};
  const double km[10];
  ut_unit *u1, *u2, *u3;

  int i, j, opt;
  int opts[2] = {UT_NAMES, UT_DEFINITION};
  ut_unit *unit_list[3];
  ut_unit *u;

  length = sizeof(miles) / sizeof(double);

  R_ut_convert(miles, &length, (char * const *) &units_from, (char * const *) &units_to, (double *) km);
  printf("km[0] %f, km[5] %f km[9] %f\n", km[0], km[5], km[9]);

  u1 = ut_parse(sys, units_from, enc);
  u2 = ut_parse(sys, units_to, enc);
  u3 = ut_parse(sys, "hour", enc);
  unit_list[0] = u1; unit_list[1] = u2; unit_list[2] = u3;

  for (i=0; i<3; i++) {
    for (j=0; j<2; j++) {
      u = unit_list[i];
      opt = opts[j];

      length = ut_format(u, name_buf, sizeof(name_buf) / sizeof(char), opt);
      if (length != -1) {
	printf("(ut_format ->) %s\n", name_buf);
      }
      else {
	fprintf(stderr, "Error in ut_format: %s\n", ut_status_strings[ut_get_status()]);
      }
    }
    s = (char *) ut_get_name(u, enc);
    if (! s) {
      fprintf(stderr, "Error in ut_get_name: %s\n", ut_status_strings[ut_get_status()]);
    }
    else {
      printf("ut_get_name -> %s\n", s);
    }
    s = (char *) ut_get_symbol(u, enc);
    if (! s) {
      fprintf(stderr, "Error in ut_get_name: %s\n", ut_status_strings[ut_get_status()]);
    }
    else {
      printf("ut_get_symbol -> %s\n", s);
    }
  }
  return 0;
}
