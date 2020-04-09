cmake_minimum_required(VERSION 3.5)
project(swipl-sgml)

include("../cmake/PrologPackage.cmake")
include(CheckTypeSize)

AC_CHECK_HEADERS(malloc.h unistd.h sys/time.h fcntl.h floatingpoint.h io.h)
AC_CHECK_FUNCS(snprintf strerror strtoll)

check_type_size("long" SIZEOF_LONG)

configure_file(config.h.cmake config.h)

set(DTD_FILES HTML4.dcl HTML4.dtd HTML4.soc HTML5.dtd HTMLlat1.ent
    HTMLspec.ent HTMLsym.ent)
prepend(DTD_FILES DTD/ ${DTD_FILES})

swipl_plugin(
    sgml
    MODULE sgml2pl
    C_SOURCES parser.c util.c charmap.c catalog.c model.c xmlns.c utf8.c
	      xml_unicode.c quote.c sgml2pl.c xsd.c error.c
    PL_LIBS sgml.pl xsdp_types.pl iso_639.pl sgml_write.pl xpath.pl
	    pwp.pl c14n2.pl
    PL_LIB_SUBDIR DTD
	PL_LIBS ${DTD_FILES}
    THREADED)

test_libs(sgml sgml_write xsd c14n
	  TEST_DIRS Test testdata)

pkg_doc(sgml
	SECTION
	    xpath.pl pwp.pl
	SUBSECTION
	    c14n2.pl)
