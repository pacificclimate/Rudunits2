AC_INIT(Rudunits2, 0.5, hiebert@uvic.ca)
AC_PROG_CC
AC_LANG(C)

AC_ARG_WITH([udunits2],
    AS_HELP_STRING([--with-udunits2=PATH_TO_UDUNITS_HOME],
		[The base location of udunits2 libraries.
	  	This path will be appended with (lib|include)]),
          	[udunits2_path=$withval])
if test [ -n "$udunits2_path" ] ; then
   PKG_LIBS="-L${udunits2_path}/lib ${PKG_LIBS}"
   PKG_CFLAGS="-I${udunits2_path}/include ${PKG_CFLAGS}"
fi

AC_ARG_WITH([udunits2-include],
    AS_HELP_STRING([--with-udunits2-include=DIR],
		[location of the udunits2 header files]),
		[udunits2_include_path=$withval])
if test [ -n "$udunits2_include_path" ] ; then
   CFLAGS="-I${udunits2_include_path} ${CLFAGS}"
   PKG_CFLAGS="-I${udunits2_include_path} ${PKG_CLFAGS}"
fi

AC_CHECK_LIB(expat,XML_ParserCreate,[],[],${PKG_LIBS})
if test "${ac_cv_lib_expat_XML_ParserCreate}" == yes; then
   PKG_LIBS="-lexpat ${PKG_LIBS}"
fi

UD_ERROR=""
AC_CHECK_HEADER(udunits2.h, [], UD_ERROR="udunits2.h not found")
AC_CHECK_LIB(udunits2,ut_read_xml,[],UD_ERROR="libununits2.a not found",${PKG_LIBS})

if test "${UD_ERROR}" != ""  ; then
    echo "-----Error: ${UD_ERROR}-----"
    echo "     If the udunits2 library is installed in a non-standard location,"
    echo "     use --configure-args='--with-udunits2=/usr/local/lib' for example,"
    echo "     or --configure-args='--with-udunits2-include=/usr/include/udunits2'"
    echo "     replacing paths with appropriate values for your installation."
    echo "     If udunits2 is not installed, please install it."
    echo "     It is required for this package."
    exit 1
else
    PKG_LIBS="-ludunits2 ${PKG_LIBS}"
    HAVE_LIBUDUNITS2=TRUE
fi

AC_SUBST([HAVE_LIBUDUNITS2])
AC_SUBST([PKG_LIBS])
AC_SUBST([PKG_CFLAGS])

AC_OUTPUT(R/load.R)
AC_OUTPUT(src/Makevars)
