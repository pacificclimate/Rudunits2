AC_INIT([Rudunits2],[0.13.2],[hiebert@uvic.ca])

CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
LDFLAGS=`${R_HOME}/bin/R CMD config LDFLAGS`

AC_LANG(C)

AC_ARG_WITH([udunits2-include],
    AS_HELP_STRING([--with-udunits2-include=DIR],
		[location of the udunits2 header files]),
		[udunits2_include_path=$withval])
if test -n "$udunits2_include_path" ; then
   UD_CPPFLAGS="-I${udunits2_include_path}"
elif test  -n "${UDUNITS2_INCLUDE}" ; then
   UD_CPPFLAGS="-I{UDUNITS2_INCLUDE}"
dnl Try Fedora/Centos/RHEL location
elif test -d /usr/include/udunits2; then
   UD_CPPFLAGS="-I/usr/include/udunits2"
fi

AC_ARG_WITH([udunits2-lib],
    AS_HELP_STRING([--with-udunits2-lib=DIR],
		[location of the udunits2 libraries]),
		[udunits2_lib_path=$withval])
if test -n "$udunits2_lib_path" ; then
   LIBS="-L${udunits2_lib_path} ${LIBS}"
elif test -n "${UDUNITS2_LIBS}" ; then
   LIBS="-L${UDUNITS2_LIBS} ${LIBS}"
fi

AC_CHECK_LIB(expat,XML_ParserCreate,[],[],${LIBS})
dnl if test "${ac_cv_lib_expat_XML_ParserCreate}" == yes; then
dnl    LIBS="${LIBS} -lexpat"
dnl fi

CPPFLAGS="${UD_CPPFLAGS} ${CPPFLAGS}"

UD_ERROR=""
AC_CHECK_HEADER(udunits2.h, [], UD_ERROR="udunits2.h not found")
if test -z "${UD_ERROR}"; then
AC_CHECK_LIB(udunits2, ut_read_xml, [],
             UD_ERROR="libudunits2.a not found", ${LIBS})
fi

if test -n "${UD_ERROR}" ; then
    echo "-----Error: ${UD_ERROR}-----"
    echo "     If the udunits2 library is installed in a non-standard location,"
    echo "     use --configure-args='--with-udunits2-lib=/usr/local/lib' for example,"
    echo "     or --configure-args='--with-udunits2-include=/usr/include/udunits2'"
    echo "     replacing paths with appropriate values for your installation."
    echo "     You can alternatively use the UDUNITS2_INCLUDE and UDUNITS2_LIB"
    echo "     environment variables."
    echo "     If udunits2 is not installed, please install it."
    echo "     It is required for this package."
    exit 1
fi

dnl LIBS="${LIBS} -ludunits2"

AC_SUBST([LIBS])
AC_SUBST([UD_CPPFLAGS])

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
