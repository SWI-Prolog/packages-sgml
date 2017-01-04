/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2016, University of Amsterdam
                              VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#define _ISOC99_SOURCE 1		/* fwprintf(), etc prototypes */

#ifdef __WINDOWS__
#include <windows.h>
#endif

#define DTD_MINOR_ERRORS 1		/* get detailed errors */

#include <stdio.h>
#include "dtd.h"
#include "catalog.h"
#include "model.h"
#include "util.h"
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <errno.h>
#include "error.h"
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <wctype.h>

#define streq(s1, s2) (strcmp(s1, s2) == 0)

#define MAX_ERRORS	50
#define MAX_WARNINGS	50

#define ENDSNUL ((size_t)-1)

		 /*******************************
		 *     PARSER CONTEXT DATA	*
		 *******************************/

#define PD_MAGIC	0x36472ba1	/* just a number */

typedef enum
{ SA_FILE = 0,				/* Stop at end-of-file */
  SA_INPUT,				/* Do not complete input */
  SA_ELEMENT,				/* Stop after first element */
  SA_CONTENT,				/* Stop after close */
  SA_DECL				/* Stop after declaration */
} stopat;

typedef enum
{ EM_QUIET = 0,				/* Suppress messages */
  EM_PRINT,				/* Print message */
  EM_STYLE				/* include style-messages */
} errormode;

typedef struct _env
{ term_t	tail;
  struct _env *parent;
} env;


typedef struct _parser_data
{ int	      magic;			/* PD_MAGIC */
  dtd_parser *parser;			/* parser itself */

  int	      warnings;			/* #warnings seen */
  int	      errors;			/* #errors seen */
  int	      max_errors;		/* error limit */
  int	      max_warnings;		/* warning limit */
  errormode   error_mode;		/* how to handle errors */
  int	      positions;		/* report file-positions */
  term_t      exception;		/* pending exception from callback */

  predicate_t on_begin;			/* begin element */
  predicate_t on_end;			/* end element */
  predicate_t on_cdata;			/* cdata */
  predicate_t on_entity;		/* entity */
  predicate_t on_pi;			/* processing instruction */
  predicate_t on_xmlns;			/* xmlns */
  predicate_t on_urlns;			/* url --> namespace */
  predicate_t on_error;			/* errors */
  predicate_t on_decl;			/* declarations */

  stopat      stopat;			/* Where to stop */
  int	      stopped;			/* Environment is complete */

  IOSTREAM*   source;			/* Where we are reading from */

  term_t      list;			/* output term (if any) */
  term_t      tail;			/* tail of the list */
  env	     *stack;			/* environment stack */
  int	      free_on_close;		/* sgml_free parser on close */
} parser_data;


		 /*******************************
		 *	      CONSTANTS		*
		 *******************************/

static functor_t FUNCTOR_and2;
static functor_t FUNCTOR_attribute_value1;
static functor_t FUNCTOR_bar2;
static functor_t FUNCTOR_comma2;
static functor_t FUNCTOR_default1;
static functor_t FUNCTOR_dialect1;
static functor_t FUNCTOR_keep_prefix1;
static functor_t FUNCTOR_document1;
static functor_t FUNCTOR_dtd1;
static functor_t FUNCTOR_dtd2;
static functor_t FUNCTOR_element3;
static functor_t FUNCTOR_entity1;
static functor_t FUNCTOR_equal2;
static functor_t FUNCTOR_file1;
static functor_t FUNCTOR_file4;
static functor_t FUNCTOR_dstream_position4;
static functor_t FUNCTOR_fixed1;
static functor_t FUNCTOR_line1;
static functor_t FUNCTOR_linepos1;
static functor_t FUNCTOR_list1;
static functor_t FUNCTOR_max_errors1;
static functor_t FUNCTOR_syntax_error1;
static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_nameof1;
static functor_t FUNCTOR_notation1;
static functor_t FUNCTOR_omit2;
static functor_t FUNCTOR_opt1;
static functor_t FUNCTOR_plus1;
static functor_t FUNCTOR_rep1;
static functor_t FUNCTOR_sgml_parser1;
static functor_t FUNCTOR_parse1;
static functor_t FUNCTOR_source1;
static functor_t FUNCTOR_content_length1;
static functor_t FUNCTOR_call2;
static functor_t FUNCTOR_cdata1;
static functor_t FUNCTOR_charpos1;
static functor_t FUNCTOR_charpos2;
static functor_t FUNCTOR_ns2;		/* :/2 */
static functor_t FUNCTOR_prefix2;	/* ns/2. This is a bit confusing but ns2 was already taken */
static functor_t FUNCTOR_space1;
static functor_t FUNCTOR_pi1;
static functor_t FUNCTOR_sdata1;
static functor_t FUNCTOR_ndata1;
static functor_t FUNCTOR_number1;
static functor_t FUNCTOR_syntax_errors1;
static functor_t FUNCTOR_xml_no_ns1;
static functor_t FUNCTOR_minus2;
static functor_t FUNCTOR_positions1;
static functor_t FUNCTOR_position1;
static functor_t FUNCTOR_event_class1;
static functor_t FUNCTOR_doctype1;
static functor_t FUNCTOR_allowed1;
static functor_t FUNCTOR_context1;
static functor_t FUNCTOR_defaults1;
static functor_t FUNCTOR_shorttag1;
static functor_t FUNCTOR_case_sensitive_attributes1;
static functor_t FUNCTOR_case_preserving_attributes1;
static functor_t FUNCTOR_system_entities1;
static functor_t FUNCTOR_max_memory1;
static functor_t FUNCTOR_qualify_attributes1;
static functor_t FUNCTOR_encoding1;
static functor_t FUNCTOR_xmlns1;
static functor_t FUNCTOR_xmlns2;

static atom_t ATOM_true;
static atom_t ATOM_false;
static atom_t ATOM_cdata;
static atom_t ATOM_rcdata;
static atom_t ATOM_pcdata;
static atom_t ATOM_empty;
static atom_t ATOM_any;
static atom_t ATOM_position;
static atom_t ATOM_atom;
static atom_t ATOM_string;

#define mkfunctor(n, a) PL_new_functor(PL_new_atom(n), a)

static void
initConstants()
{
  FUNCTOR_sgml_parser1	 = mkfunctor("sgml_parser", 1);
  FUNCTOR_equal2	 = mkfunctor("=", 2);
  FUNCTOR_dtd1		 = mkfunctor("dtd", 1);
  FUNCTOR_element3	 = mkfunctor("element", 3);
  FUNCTOR_entity1	 = mkfunctor("entity", 1);
  FUNCTOR_document1	 = mkfunctor("document", 1);
  FUNCTOR_dtd2		 = mkfunctor("dtd", 2);
  FUNCTOR_omit2		 = mkfunctor("omit", 2);
  FUNCTOR_and2		 = mkfunctor("&", 2);
  FUNCTOR_comma2	 = mkfunctor(",", 2);
  FUNCTOR_bar2		 = mkfunctor("|", 2);
  FUNCTOR_opt1		 = mkfunctor("?", 1);
  FUNCTOR_rep1		 = mkfunctor("*", 1);
  FUNCTOR_plus1		 = mkfunctor("+", 1);
  FUNCTOR_default1	 = mkfunctor("default", 1);
  FUNCTOR_fixed1	 = mkfunctor("fixed", 1);
  FUNCTOR_list1		 = mkfunctor("list", 1);
  FUNCTOR_nameof1	 = mkfunctor("nameof", 1);
  FUNCTOR_notation1	 = mkfunctor("notation", 1);
  FUNCTOR_file1		 = mkfunctor("file", 1);
  FUNCTOR_file4		 = mkfunctor("file", 4);
  FUNCTOR_line1		 = mkfunctor("line", 1);
  FUNCTOR_linepos1	 = mkfunctor("linepos", 1);
  FUNCTOR_dialect1	 = mkfunctor("dialect", 1);
  FUNCTOR_keep_prefix1	 = mkfunctor("keep_prefix", 1);
  FUNCTOR_max_errors1	 = mkfunctor("max_errors", 1);
  FUNCTOR_parse1	 = mkfunctor("parse", 1);
  FUNCTOR_source1	 = mkfunctor("source", 1);
  FUNCTOR_content_length1= mkfunctor("content_length", 1);
  FUNCTOR_call2		 = mkfunctor("call", 2);
  FUNCTOR_cdata1	 = mkfunctor("cdata", 1);
  FUNCTOR_attribute_value1 = mkfunctor("attribute_value", 1);
  FUNCTOR_charpos1	 = mkfunctor("charpos", 1);
  FUNCTOR_charpos2	 = mkfunctor("charpos", 2);
  FUNCTOR_ns2		 = mkfunctor(":", 2);
  FUNCTOR_prefix2	 = mkfunctor("ns", 2);
  FUNCTOR_space1	 = mkfunctor("space", 1);
  FUNCTOR_pi1		 = mkfunctor("pi", 1);
  FUNCTOR_sdata1	 = mkfunctor("sdata", 1);
  FUNCTOR_ndata1	 = mkfunctor("ndata", 1);
  FUNCTOR_number1	 = mkfunctor("number", 1);
  FUNCTOR_syntax_errors1 = mkfunctor("syntax_errors", 1);
  FUNCTOR_syntax_error1  = mkfunctor("syntax_error", 1);
  FUNCTOR_error2         = mkfunctor("error", 2);
  FUNCTOR_xml_no_ns1     = mkfunctor("xml_no_ns", 1);
  FUNCTOR_minus2	 = mkfunctor("-", 2);
  FUNCTOR_position1	 = mkfunctor("position", 1);
  FUNCTOR_positions1	 = mkfunctor("positions", 1);
  FUNCTOR_event_class1	 = mkfunctor("event_class", 1);
  FUNCTOR_doctype1       = mkfunctor("doctype", 1);
  FUNCTOR_allowed1       = mkfunctor("allowed", 1);
  FUNCTOR_context1       = mkfunctor("context", 1);
  FUNCTOR_defaults1	 = mkfunctor("defaults", 1);
  FUNCTOR_shorttag1	 = mkfunctor("shorttag", 1);
  FUNCTOR_case_sensitive_attributes1 = mkfunctor("case_sensitive_attributes", 1);
  FUNCTOR_case_preserving_attributes1 = mkfunctor("case_preserving_attributes", 1);
  FUNCTOR_system_entities1 = mkfunctor("system_entities", 1);
  FUNCTOR_max_memory1	 = mkfunctor("max_memory", 1);
  FUNCTOR_qualify_attributes1 = mkfunctor("qualify_attributes", 1);
  FUNCTOR_encoding1	 = mkfunctor("encoding", 1);
  FUNCTOR_xmlns1	 = mkfunctor("xmlns", 1);
  FUNCTOR_xmlns2	 = mkfunctor("xmlns", 2);
  FUNCTOR_dstream_position4 = PL_new_functor(PL_new_atom("$stream_position"), 4);

  ATOM_true = PL_new_atom("true");
  ATOM_false = PL_new_atom("false");
  ATOM_cdata = PL_new_atom("cdata");
  ATOM_rcdata = PL_new_atom("rcdata");
  ATOM_pcdata = PL_new_atom("#pcdata");
  ATOM_empty = PL_new_atom("empty");
  ATOM_any = PL_new_atom("any");
  ATOM_atom = PL_new_atom("atom");
  ATOM_string = PL_new_atom("string");
  ATOM_position = PL_new_atom("#position");
}

		 /*******************************
		 *	       ACCESS		*
		 *******************************/

static int
unify_parser(term_t parser, dtd_parser *p)
{ return PL_unify_term(parser, PL_FUNCTOR, FUNCTOR_sgml_parser1,
		         PL_POINTER, p);
}


static int
get_parser(term_t parser, dtd_parser **p)
{ if ( PL_is_functor(parser, FUNCTOR_sgml_parser1) )
  { term_t a = PL_new_term_ref();
    void *ptr;

    _PL_get_arg(1, parser, a);
    if ( PL_get_pointer(a, &ptr) )
    { dtd_parser *tmp = ptr;

      if ( tmp->magic == SGML_PARSER_MAGIC )
      { *p = tmp;

        return TRUE;
      }
      return sgml2pl_error(ERR_EXISTENCE, "sgml_parser", parser);
    }
  }

  return sgml2pl_error(ERR_TYPE, "sgml_parser", parser);
}


static int
unify_dtd(term_t t, dtd *dtd)
{ if ( dtd->doctype )
    return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_dtd2,
			 PL_POINTER, dtd,
			 PL_NWCHARS, (size_t)-1, dtd->doctype);
  else
    return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_dtd2,
			 PL_POINTER, dtd,
			 PL_VARIABLE);
}


static int
get_dtd(term_t t, dtd **dtdp)
{ if ( PL_is_functor(t, FUNCTOR_dtd2) )
  { term_t a = PL_new_term_ref();
    void *ptr;

    _PL_get_arg(1, t, a);
    if ( PL_get_pointer(a, &ptr) )
    { dtd *tmp = ptr;

      if ( tmp->magic == SGML_DTD_MAGIC )
      { *dtdp = tmp;

        return TRUE;
      }
      return sgml2pl_error(ERR_EXISTENCE, "dtd", t);
    }
  }

  return sgml2pl_error(ERR_TYPE, "dtd", t);
}


		 /*******************************
		 *	      NEW/FREE		*
		 *******************************/

static foreign_t
pl_new_sgml_parser(term_t ref, term_t options)
{ term_t head = PL_new_term_ref();
  term_t tail = PL_copy_term_ref(options);
  term_t tmp  = PL_new_term_ref();

  dtd *dtd = NULL;
  dtd_parser *p;

  while ( PL_get_list(tail, head, tail) )
  { if ( PL_is_functor(head, FUNCTOR_dtd1) )
    { _PL_get_arg(1, head, tmp);

      if ( PL_is_variable(tmp) )	/* dtd(X) */
      { dtd = new_dtd(NULL);		/* no known doctype */
	dtd->references++;
	unify_dtd(tmp, dtd);
      } else if ( !get_dtd(tmp, &dtd) )
	return FALSE;
    }
  }
  if ( !PL_get_nil(tail) )
    return sgml2pl_error(ERR_TYPE, "list", tail);

  p = new_dtd_parser(dtd);

  return unify_parser(ref, p);
}


static foreign_t
pl_free_sgml_parser(term_t parser)
{ dtd_parser *p;

  if ( get_parser(parser, &p) )
  { free_dtd_parser(p);
    return TRUE;
  }

  return FALSE;
}


static foreign_t
pl_new_dtd(term_t doctype, term_t ref)
{ ichar *dt;
  dtd *dtd;

  if ( !PL_get_wchars(doctype, NULL, &dt, CVT_ATOM|CVT_EXCEPTION) )
    return FALSE;

  if ( !(dtd=new_dtd(dt)) )
    return FALSE;

  dtd->references++;

  return unify_dtd(ref, dtd);
}


static foreign_t
pl_free_dtd(term_t t)
{ dtd *dtd;

  if ( get_dtd(t, &dtd) )
  { free_dtd(dtd);
    return TRUE;
  }

  return FALSE;
}

		 /*******************************
		 *	   DATA EXCHANGE	*
		 *******************************/

static int
put_atom_wchars(term_t t, wchar_t const *s)
{ PL_put_variable(t);
  return PL_unify_wchars(t, PL_ATOM, ENDSNUL, s);
}


		 /*******************************
		 *	    PROPERTIES		*
		 *******************************/

static foreign_t
pl_set_sgml_parser(term_t parser, term_t option)
{ dtd_parser *p;

  if ( !get_parser(parser, &p) )
    return FALSE;

  if ( PL_is_functor(option, FUNCTOR_file1) )
  { term_t a = PL_new_term_ref();
    wchar_t *file;
    dtd_symbol *fs;

    _PL_get_arg(1, option, a);
    if ( !PL_get_wchars(a, NULL, &file, CVT_ATOM|CVT_EXCEPTION) )
      return FALSE;
    fs = dtd_add_symbol(p->dtd, file);	/* symbol will be freed */
    set_file_dtd_parser(p, IN_FILE, fs->name);
  } else if ( PL_is_functor(option, FUNCTOR_line1) )
  { term_t a = PL_new_term_ref();

    _PL_get_arg(1, option, a);
    if ( !PL_get_integer_ex(a, &p->location.line) )
      return FALSE;
  } else if ( PL_is_functor(option, FUNCTOR_linepos1) )
  { term_t a = PL_new_term_ref();

    _PL_get_arg(1, option, a);
    if ( !PL_get_integer_ex(a, &p->location.linepos) )
      return FALSE;
  } else if ( PL_is_functor(option, FUNCTOR_charpos1) )
  { term_t a = PL_new_term_ref();

    _PL_get_arg(1, option, a);
    if ( !PL_get_long_ex(a, &p->location.charpos) )
      return FALSE;
  } else if ( PL_is_functor(option, FUNCTOR_position1) )
  { term_t a = PL_new_term_ref();

    _PL_get_arg(1, option, a);
    if ( PL_is_functor(a, FUNCTOR_dstream_position4) )
    { term_t arg = PL_new_term_ref();

      if ( !PL_get_arg(1,a,arg) || !PL_get_long_ex(arg, &p->location.charpos) ||
	   !PL_get_arg(2,a,arg) || !PL_get_integer_ex(arg,  &p->location.line) ||
	   !PL_get_arg(3,a,arg) || !PL_get_integer_ex(arg,  &p->location.linepos))
	return FALSE;
    } else
      return PL_type_error("stream_position", a);
  } else if ( PL_is_functor(option, FUNCTOR_dialect1) )
  { term_t a = PL_new_term_ref();
    char *s;

    _PL_get_arg(1, option, a);
    if ( !PL_get_atom_chars(a, &s) )
      return sgml2pl_error(ERR_TYPE, "atom", a);

    if ( streq(s, "xml") )
      set_dialect_dtd(p->dtd, DL_XML);
    else if ( streq(s, "xmlns") )
      set_dialect_dtd(p->dtd, DL_XMLNS);
    else if ( streq(s, "sgml") )
      set_dialect_dtd(p->dtd, DL_SGML);
    else if ( streq(s, "html") || streq(s, "html4") )
      set_dialect_dtd(p->dtd, DL_HTML);
    else if ( streq(s, "html5") )
      set_dialect_dtd(p->dtd, DL_HTML5);
    else if ( streq(s, "xhtml") )
      set_dialect_dtd(p->dtd, DL_XHTML);
    else if ( streq(s, "xhtml5") )
      set_dialect_dtd(p->dtd, DL_XHTML5);
    else
      return sgml2pl_error(ERR_DOMAIN, "sgml_dialect", a);
  } else if ( PL_is_functor(option, FUNCTOR_space1) )
  { term_t a = PL_new_term_ref();
    char *s;

    _PL_get_arg(1, option, a);
    if ( !PL_get_atom_chars(a, &s) )
      return sgml2pl_error(ERR_TYPE, "atom", a);

    if ( streq(s, "preserve") )
      p->dtd->space_mode = SP_PRESERVE;
    else if ( streq(s, "default") )
      p->dtd->space_mode = SP_DEFAULT;
    else if ( streq(s, "remove") )
      p->dtd->space_mode = SP_REMOVE;
    else if ( streq(s, "sgml") )
      p->dtd->space_mode = SP_SGML;

    else
      return sgml2pl_error(ERR_DOMAIN, "space", a);
  } else if ( PL_is_functor(option, FUNCTOR_defaults1) )
  { term_t a = PL_new_term_ref();
    int val;

    _PL_get_arg(1, option, a);
    if ( !PL_get_bool(a, &val) )
      return sgml2pl_error(ERR_TYPE, "boolean", a);

    if ( val )
      p->flags &= ~SGML_PARSER_NODEFS;
    else
      p->flags |= SGML_PARSER_NODEFS;
  } else if ( PL_is_functor(option, FUNCTOR_qualify_attributes1) )
  { term_t a = PL_new_term_ref();
    int val;

    _PL_get_arg(1, option, a);
    if ( !PL_get_bool(a, &val) )
      return sgml2pl_error(ERR_TYPE, "boolean", a);

    if ( val )
      p->flags |= SGML_PARSER_QUALIFY_ATTS;
    else
      p->flags &= ~SGML_PARSER_QUALIFY_ATTS;
  } else if ( PL_is_functor(option, FUNCTOR_shorttag1) )
  { term_t a = PL_new_term_ref();
    int val;

    _PL_get_arg(1, option, a);
    if ( !PL_get_bool(a, &val) )
      return sgml2pl_error(ERR_TYPE, "boolean", a);

    set_option_dtd(p->dtd, OPT_SHORTTAG, val);
  } else if ( PL_is_functor(option, FUNCTOR_case_sensitive_attributes1) )
  { term_t a = PL_new_term_ref();
    int val;

    _PL_get_arg(1, option, a);
    if ( !PL_get_bool(a, &val) )
      return sgml2pl_error(ERR_TYPE, "boolean", a);

    set_option_dtd(p->dtd, OPT_CASE_SENSITIVE_ATTRIBUTES, val);
  } else if ( PL_is_functor(option, FUNCTOR_case_preserving_attributes1) )
  { term_t a = PL_new_term_ref();
    int val;

    _PL_get_arg(1, option, a);
    if ( !PL_get_bool(a, &val) )
      return sgml2pl_error(ERR_TYPE, "boolean", a);

    set_option_dtd(p->dtd, OPT_CASE_PRESERVING_ATTRIBUTES, val);
  } else if ( PL_is_functor(option, FUNCTOR_system_entities1) )
  { term_t a = PL_new_term_ref();
    int val;

    _PL_get_arg(1, option, a);
    if ( !PL_get_bool(a, &val) )
      return sgml2pl_error(ERR_TYPE, "boolean", a);

    set_option_dtd(p->dtd, OPT_SYSTEM_ENTITIES, val);
  } else if ( PL_is_functor(option, FUNCTOR_max_memory1) )
  { term_t a = PL_new_term_ref();
    int val;

    _PL_get_arg(1, option, a);
    if ( !PL_get_integer(a, &val) )
      return sgml2pl_error(ERR_TYPE, "integer", a);

    p->max_memory = val;
    if ( p->buffer )
      p->buffer->limit = val;
    if ( p->cdata )
      p->cdata->limit = val;
  } else if ( PL_is_functor(option, FUNCTOR_number1) )
  { term_t a = PL_new_term_ref();
    char *s;

    _PL_get_arg(1, option, a);
    if ( !PL_get_atom_chars(a, &s) )
      return sgml2pl_error(ERR_TYPE, "atom", a);

    if ( streq(s, "token") )
      p->dtd->number_mode = NU_TOKEN;
    else if ( streq(s, "integer") )
      p->dtd->number_mode = NU_INTEGER;
    else
      return sgml2pl_error(ERR_DOMAIN, "number", a);
  } else if ( PL_is_functor(option, FUNCTOR_encoding1) )
  { term_t a = PL_new_term_ref();
    char *val;

    _PL_get_arg(1, option, a);
    if ( !PL_get_atom_chars(a, &val) )
      return sgml2pl_error(ERR_TYPE, "atom", a);
    if ( !xml_set_encoding(p, val) )
      return sgml2pl_error(ERR_DOMAIN, "encoding", a);
  } else if ( PL_is_functor(option, FUNCTOR_doctype1) )
  { term_t a = PL_new_term_ref();
    ichar *s;

    _PL_get_arg(1, option, a);
    if ( PL_is_variable(a) )
    { p->enforce_outer_element = NULL;
    } else
    { if ( !PL_get_wchars(a, NULL, &s, CVT_ATOM) )
	return sgml2pl_error(ERR_TYPE, "atom_or_variable", a);

      p->enforce_outer_element = dtd_add_symbol(p->dtd, s);
    }
  } else if ( PL_is_functor(option, FUNCTOR_xmlns1) )
  { term_t a = PL_new_term_ref();
    ichar ns[1] = {0};
    ichar *uri;

    _PL_get_arg(1, option, a);
    if ( !PL_get_wchars(a, NULL, &uri, CVT_ATOM|CVT_EXCEPTION) )
      return FALSE;

    xmlns_push(p, ns, uri);
  } else if ( PL_is_functor(option, FUNCTOR_xmlns2) )
  { term_t a = PL_new_term_ref();
    ichar *ns, *uri;

    _PL_get_arg(1, option, a);
    if ( !PL_get_wchars(a, NULL, &ns, CVT_ATOM|CVT_EXCEPTION) )
      return FALSE;
    _PL_get_arg(2, option, a);
    if ( !PL_get_wchars(a, NULL, &uri, CVT_ATOM|CVT_EXCEPTION) )
      return FALSE;

    xmlns_push(p, ns, uri);
  } else if ( PL_is_functor(option, FUNCTOR_keep_prefix1) )
  { term_t a = PL_new_term_ref();
    int val;

    _PL_get_arg(1, option, a);
    if ( !PL_get_bool(a, &val) )
      return sgml2pl_error(ERR_TYPE, "boolean", a);
    set_option_dtd(p->dtd, OPT_KEEP_PREFIX, val);
  } else
    return sgml2pl_error(ERR_DOMAIN, "sgml_parser_option", option);

  return TRUE;
}


static dtd_srcloc *
file_location(dtd_parser *p, dtd_srcloc *l)
{ while(l->parent && l->type != IN_FILE)
    l = l->parent;

  return l;
}


static foreign_t
pl_get_sgml_parser(term_t parser, term_t option)
{ dtd_parser *p;

  if ( !get_parser(parser, &p) )
    return FALSE;

  if ( PL_is_functor(option, FUNCTOR_charpos1) )
  { term_t a = PL_new_term_ref();

    _PL_get_arg(1, option, a);
    return PL_unify_integer(a, file_location(p, &p->startloc)->charpos);
  } else if ( PL_is_functor(option, FUNCTOR_line1) )
  { term_t a = PL_new_term_ref();

    _PL_get_arg(1, option, a);
    return PL_unify_integer(a, file_location(p, &p->startloc)->line);
  } else if ( PL_is_functor(option, FUNCTOR_charpos2) )
  { term_t a = PL_new_term_ref();

    if ( PL_get_arg(1, option, a) &&
	 PL_unify_integer(a, file_location(p, &p->startloc)->charpos) &&
	 PL_get_arg(2, option, a) &&
	 PL_unify_integer(a, file_location(p, &p->location)->charpos) )
      return TRUE;
    else
      return FALSE;
  } else if ( PL_is_functor(option, FUNCTOR_file1) )
  { dtd_srcloc *l = file_location(p, &p->location);

    if ( l->type == IN_FILE && l->name.file )
    { term_t a = PL_new_term_ref();

      _PL_get_arg(1, option, a);
      return PL_unify_wchars(a, PL_ATOM, ENDSNUL, l->name.file);
    }
  } else if ( PL_is_functor(option, FUNCTOR_source1) )
  { parser_data *pd = p->closure;

    if ( pd && pd->magic == PD_MAGIC && pd->source )
    { term_t a = PL_new_term_ref();

      _PL_get_arg(1, option, a);
      return PL_unify_stream(a, pd->source);
    }
  } else if ( PL_is_functor(option, FUNCTOR_dialect1) )
  { term_t a = PL_new_term_ref();

    _PL_get_arg(1, option, a);
    switch(p->dtd->dialect)
    { case DL_SGML:
	return PL_unify_atom_chars(a, "sgml");
      case DL_HTML:
	return PL_unify_atom_chars(a, "html");
      case DL_HTML5:
	return PL_unify_atom_chars(a, "html5");
      case DL_XHTML:
	return PL_unify_atom_chars(a, "xhtml");
      case DL_XHTML5:
	return PL_unify_atom_chars(a, "xhtml5");
      case DL_XML:
	return PL_unify_atom_chars(a, "xml");
      case DL_XMLNS:
	return PL_unify_atom_chars(a, "xmlns");
    }
  } else if ( PL_is_functor(option, FUNCTOR_event_class1) )
  { term_t a = PL_new_term_ref();

    _PL_get_arg(1, option, a);
    switch(p->event_class)
    { case EV_EXPLICIT:
	return PL_unify_atom_chars(a, "explicit");
      case EV_OMITTED:
	return PL_unify_atom_chars(a, "omitted");
      case EV_SHORTTAG:
	return PL_unify_atom_chars(a, "shorttag");
      case EV_SHORTREF:
	return PL_unify_atom_chars(a, "shortref");
    }
  } else if ( PL_is_functor(option, FUNCTOR_dtd1) )
  { term_t a = PL_new_term_ref();

    _PL_get_arg(1, option, a);

    return unify_dtd(a, p->dtd);
  } else if ( PL_is_functor(option, FUNCTOR_doctype1) )
  { term_t a = PL_new_term_ref();

    _PL_get_arg(1, option, a);
    if ( p->enforce_outer_element )
      return PL_unify_wchars(a, PL_ATOM, ENDSNUL,
			     p->enforce_outer_element->name);
    else
      return TRUE;			/* leave variable */
  } else if ( PL_is_functor(option, FUNCTOR_allowed1) )
  { term_t tail, head, tmp;
    sgml_environment *env = p->environments;

    if ( !(tail = PL_new_term_ref()) ||
	 !(head = PL_new_term_ref()) ||
	 !(tmp = PL_new_term_ref()) )
      return FALSE;

    _PL_get_arg(1, option, tail);

    if ( env )
    { for( ; env; env = env->parent)
      { dtd_element *buf[256];		/* MAX_VISITED! */
	int n = sizeof(buf)/sizeof(dtd_element *); /* not yet used! */
	int i;

	state_allows_for(env->state, buf, &n);

	for(i=0; i<n; i++)
	{ int rc;

	  if ( buf[i] == CDATA_ELEMENT )
	    rc = PL_put_atom_chars(tmp, "#pcdata");
	  else
	    rc = put_atom_wchars(tmp, buf[i]->name->name);

	  if ( !rc ||
	       !PL_unify_list(tail, head, tail) ||
	       !PL_unify(head, tmp) )
	    return FALSE;
	}

	if ( !env->element->structure ||
	     !env->element->structure->omit_close )
	  break;
      }
    } else if ( p->enforce_outer_element )
    { put_atom_wchars(tmp, p->enforce_outer_element->name);

      if ( !PL_unify_list(tail, head, tail) ||
	   !PL_unify(head, tmp) )
	return FALSE;
    }

    return PL_unify_nil(tail);
  } else if ( PL_is_functor(option, FUNCTOR_context1) )
  { term_t tail = PL_new_term_ref();
    term_t head = PL_new_term_ref();
    term_t tmp = PL_new_term_ref();
    sgml_environment *env = p->environments;

    _PL_get_arg(1, option, tail);

    for( ; env; env = env->parent)
    { put_atom_wchars(tmp, env->element->name->name);

      if ( !PL_unify_list(tail, head, tail) ||
	   !PL_unify(head, tmp) )
	return FALSE;
    }

    return PL_unify_nil(tail);
  } else
    return sgml2pl_error(ERR_DOMAIN, "parser_option", option);

  return FALSE;
}


static int
call_prolog(parser_data *pd, predicate_t pred, term_t av)
{ qid_t qid = PL_open_query(NULL, PL_Q_PASS_EXCEPTION, pred, av);
  int rc = PL_next_solution(qid);

  PL_close_query(qid);

  if ( rc )
  { pd->exception = FALSE;
  } else
  { if ( (pd->exception = PL_exception(0)) )
      pd->stopped = TRUE;
  }

  return rc;
}


static void
end_frame(fid_t fid, term_t ex)
{ if ( ex )
    PL_close_foreign_frame(fid);
  else
    PL_discard_foreign_frame(fid);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
put_url(dtd_parser *p, term_t t, const ichar *url)
    Store the url-part of a name-space qualifier in term.  We call
    xml:xmlns(-Canonical, +Full) trying to resolve the specified
    namespace to an internal canonical namespace.

    We do a little caching as there will generally be only a very
    small pool of urls in use.  We assume the url-pointers we get
    life for the time of the parser.  It might be possible that
    multiple url pointers point to the same url, but this only clobbers
    the cache a little.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define URL_CACHE 4			/* # entries cached */

typedef struct
{ const ichar *url;			/* URL pointer */
  atom_t canonical;
} url_cache;

static url_cache cache[URL_CACHE];

static void
reset_url_cache()
{ int i;
  url_cache *c = cache;

  for(i=0; i<URL_CACHE; i++)
  { c[i].url = NULL;
    if ( c[i].canonical )
      PL_unregister_atom(c[i].canonical);
    c[i].canonical = 0;
  }
}


WUNUSED static int
put_url(dtd_parser *p, term_t t, const ichar *url)
{ parser_data *pd = p->closure;
  fid_t fid;
  int i;

  if ( !pd->on_urlns )
    return put_atom_wchars(t, url);

  for(i=0; i<URL_CACHE; i++)
  { if ( cache[i].url == url )		/* cache hit */
    { if ( cache[i].canonical )		/* and a canonical value */
	return PL_put_atom(t, cache[i].canonical);
      else
	return put_atom_wchars(t, url);
    }
  }
					/* shift the cache */
  i = URL_CACHE-1;
  if ( cache[i].canonical )
    PL_unregister_atom(cache[i].canonical);
  for(i=URL_CACHE-1; i>0; i--)
    cache[i] = cache[i-1];
  cache[0].url = url;
  cache[0].canonical = 0;

  if ( (fid = PL_open_foreign_frame()) )
  { int rc;
    term_t av = PL_new_term_refs(3);
    atom_t a;

    rc = (put_atom_wchars(av+0, url) &&
	  unify_parser(av+2, p));

    if ( rc &&
	 PL_call_predicate(NULL, PL_Q_NORMAL, pd->on_urlns, av) &&
	 PL_get_atom(av+1, &a) )
    { PL_register_atom(a);
      cache[0].canonical = a;
      PL_put_atom(t, a);
    } else if ( rc )
    { rc = put_atom_wchars(t, url);
    }
    PL_discard_foreign_frame(fid);

    return rc;
  }

  return FALSE;
}


WUNUSED static int
put_attribute_name(dtd_parser *p, term_t t, dtd_symbol *nm)
{ const ichar *url, *local;

  if ( p->dtd->dialect == DL_XMLNS )
  { xmlns_resolve_attribute(p, nm, &local, &url);
    if ( url )
    { term_t av;
      return ( (av=PL_new_term_refs(2)) &&
	       put_url(p, av+0, url) &&
	       put_atom_wchars(av+1, local) &&
	       PL_cons_functor_v(t, FUNCTOR_ns2, av) );
    } else
      return put_atom_wchars(t, local);
  } else
    return put_atom_wchars(t, nm->name);
}


WUNUSED static int
put_element_name(dtd_parser *p, term_t t, dtd_element *e)
{ const ichar *url, *local, *prefix;

  if ( p->dtd->dialect == DL_XMLNS )
  { assert(p->environments->element == e);
    xmlns_resolve_element(p, &local, &url, &prefix);

    if ( url )
    { term_t av;
      if ( p->dtd->keep_prefix )
      { /* creates ns(prefix,url):local */
	return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_ns2,
			          PL_FUNCTOR, FUNCTOR_prefix2,
			            PL_NWCHARS, ENDSNUL, prefix ? prefix : L"",
				    PL_NWCHARS, ENDSNUL, url,
				  PL_NWCHARS, ENDSNUL, local);
      } else
      { return ( (av=PL_new_term_refs(2)) &&
                 put_url(p, av+0, url) &&
                 put_atom_wchars(av+1, local) &&
                 PL_cons_functor_v(t, FUNCTOR_ns2, av) );
      }
    } else
      return put_atom_wchars(t, local);
  } else
    return put_atom_wchars(t, e->name->name);
}


static ichar *
istrblank(const ichar *s)
{ for( ; *s; s++ )
  { if ( iswspace(*s) )
      return (ichar *)s;
  }

  return NULL;
}


WUNUSED static int
unify_listval(dtd_parser *p,
	      term_t t, attrtype type, size_t len, const ichar *text)
{ if ( type == AT_NUMBERS && p->dtd->number_mode == NU_INTEGER )
  { wchar_t *e;

#if SIZEOF_LONG == 4 && defined(HAVE_WCSTOLL)
    int64_t v = wcstoll(text, &e, 10);
    if ( (size_t)(e-text) == len && errno != ERANGE )
      return PL_unify_int64(t, v);
#else
    long v = wcstol(text, &e, 10);

    if ( (size_t)(e-text) == len && errno != ERANGE )
      return PL_unify_integer(t, v);
#endif
					/* TBD: Error!? */
  }

  return PL_unify_wchars(t, PL_ATOM, len, text);
}


WUNUSED static int
put_att_text(dtd_parser *p, term_t t, sgml_attribute *a)
{ if ( a->value.textW )
  { PL_put_variable(t);
    return PL_unify_wchars(t, p->att_rep, a->value.number, a->value.textW);
  } else
    return FALSE;
}


WUNUSED static int
put_attribute_value(dtd_parser *p, term_t t, sgml_attribute *a)
{ switch(a->definition->type)
  { case AT_CDATA:
      return put_att_text(p, t, a);
    case AT_NUMBER:
    { if ( !put_att_text(p, t, a) )
	return PL_put_integer(t, a->value.number);
      return TRUE;
    }
    default:				/* multi-valued attribute */
    { if ( a->definition->islist && a->value.textW )
      { term_t tail, head;
	const ichar *val = a->value.textW;
	const ichar *e;

	PL_put_variable(t);
	if ( !(head = PL_new_term_ref()) ||
	     !(tail = PL_copy_term_ref(t)) )
	  return FALSE;

	for(e=istrblank(val); e; val = e+1, e=istrblank(val))
	{ if ( e == val )
	    continue;			/* skip spaces */
	  if ( !PL_unify_list(tail, head, tail) ||
	       !unify_listval(p, head, a->definition->type, e-val, val) )
	    return FALSE;
	}

	return ( PL_unify_list(tail, head, tail) &&
		 unify_listval(p, head, a->definition->type,
			       istrlen(val), val) &&
		 PL_unify_nil(tail) );
      } else
	return put_att_text(p, t, a);
    }
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Produce a tag-location in the format

	start_location=file:char-char
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

WUNUSED static int
put_tag_position(dtd_parser *p, term_t pos)
{ dtd_srcloc *l = &p->startloc;

  if ( l->type == IN_FILE && l->name.file )
  { PL_put_variable(pos);
    return PL_unify_term(pos,
			 PL_FUNCTOR, FUNCTOR_ns2,
			   PL_NWCHARS, wcslen(l->name.file), l->name.file,
			   PL_FUNCTOR, FUNCTOR_minus2,
			 PL_LONG, l->charpos,
			 PL_LONG, p->location.charpos);
  }

  return FALSE;
}



WUNUSED static int
unify_attribute_list(dtd_parser *p, term_t alist,
		     int argc, sgml_attribute *argv)
{ int i;
  term_t tail = PL_copy_term_ref(alist);
  term_t h    = PL_new_term_ref();
  term_t a    = PL_new_term_refs(2);
  parser_data *pd = p->closure;

  for(i=0; i<argc; i++)
  { if ( !put_attribute_name(p, a+0, argv[i].definition->name) ||
	 !put_attribute_value(p, a+1, &argv[i]) ||
	 !PL_cons_functor_v(a, FUNCTOR_equal2, a) ||
	 !PL_unify_list(tail, h, tail) ||
	 !PL_unify(h, a) )
      return FALSE;
  }

  if ( pd->positions && put_tag_position(p, a+1) )
  { PL_put_atom(a, ATOM_position);

    if ( !PL_cons_functor_v(a, FUNCTOR_equal2, a) ||
	 !PL_unify_list(tail, h, tail) ||
	 !PL_unify(h, a) )
      return FALSE;
  }

  if ( PL_unify_nil(tail) )
  { PL_reset_term_refs(tail);

    return TRUE;
  }

  return FALSE;
}



static int
on_begin(dtd_parser *p, dtd_element *e, int argc, sgml_attribute *argv)
{ parser_data *pd = p->closure;

  if ( pd->stopped )
    return TRUE;

  if ( pd->tail )
  { term_t content = PL_new_term_ref();	/* element content */
    term_t alist   = PL_new_term_ref();	/* attribute list */
    term_t et	   = PL_new_term_ref();	/* element structure */
    term_t h       = PL_new_term_ref();

    if ( !h ||
	 !put_element_name(p, h, e) ||
	 !unify_attribute_list(p, alist, argc, argv) ||
	 !PL_unify_term(et,
			PL_FUNCTOR, FUNCTOR_element3,
			  PL_TERM, h,
			  PL_TERM, alist,
			  PL_TERM, content) )
    { pd->exception = PL_exception(0);
      return FALSE;
    }

    if ( PL_unify_list(pd->tail, h, pd->tail) &&
	 PL_unify(h, et) )
    { env *env = sgml_calloc(1, sizeof(*env));

      env->tail   = pd->tail;
      env->parent = pd->stack;
      pd->stack   = env;

      pd->tail = content;
      PL_reset_term_refs(alist);

      return TRUE;
    }

    pd->exception = PL_exception(0);
    return FALSE;
  }

  if ( pd->on_begin )
  { fid_t fid;

    if ( (fid = PL_open_foreign_frame()) )
    { int rc;
      term_t av = PL_new_term_refs(3);

      rc = ( put_element_name(p, av+0, e) &&
	     unify_attribute_list(p, av+1, argc, argv) &&
	     unify_parser(av+2, p) &&
	     call_prolog(pd, pd->on_begin, av)
	   );

      PL_discard_foreign_frame(fid);
      if ( rc )
	return TRUE;
    }

    pd->exception = PL_exception(0);
    return FALSE;
  }

  return TRUE;
}


static int
on_end(dtd_parser *p, dtd_element *e)
{ parser_data *pd = p->closure;

  if ( pd->stopped )
    return TRUE;

  if ( pd->on_end )
  { fid_t fid;

    if ( (fid = PL_open_foreign_frame()) )
    { int rc;
      term_t av = PL_new_term_refs(2);

      rc = ( put_element_name(p, av+0, e) &&
	     unify_parser(av+1, p) &&
	     call_prolog(pd, pd->on_end, av)
	   );

      PL_discard_foreign_frame(fid);
      if ( rc )
	goto ok;
    }

    if ( (pd->exception = PL_exception(0)) )
      return FALSE;
  }

ok:
  if ( pd->tail && !pd->stopped )
  { if ( !PL_unify_nil(pd->tail) )
      return FALSE;
    PL_reset_term_refs(pd->tail);	/* ? */

    if ( pd->stack )
    { env *parent = pd->stack->parent;

      pd->tail = pd->stack->tail;
      sgml_free(pd->stack);
      pd->stack = parent;
    } else
    { if ( pd->stopat == SA_CONTENT )
	pd->stopped = TRUE;
    }
  }

  if ( pd->stopat == SA_ELEMENT && !p->environments->parent )
    pd->stopped = TRUE;

  return TRUE;
}


static int
on_entity(dtd_parser *p, dtd_entity *e, int chr)
{ parser_data *pd = p->closure;

  if ( pd->stopped )
    return TRUE;

  if ( pd->on_entity )
  { fid_t fid;

    if ( (fid=PL_open_foreign_frame()) )
    { int rc;
      term_t av = PL_new_term_refs(2);

      if ( e )
	rc = put_atom_wchars(av+0, e->name->name);
      else
	rc = PL_put_integer(av+0, chr);

      if ( rc )
	rc = ( unify_parser(av+1, p) &&
	       call_prolog(pd, pd->on_end, av)
	     );

      PL_discard_foreign_frame(fid);
      if ( rc )
	return TRUE;
    }

    pd->exception = PL_exception(0);
    return FALSE;
  }

  if ( pd->tail )
  { int rc;
    term_t h = PL_new_term_ref();

    if ( !h ||
	 !PL_unify_list(pd->tail, h, pd->tail) )
    { pd->exception = PL_exception(0);
      return FALSE;
    }

    if ( e )
      rc = PL_unify_term(h,
			 PL_FUNCTOR, FUNCTOR_entity1,
			   PL_CHARS, e->name->name);
    else
      rc = PL_unify_term(h,
			 PL_FUNCTOR, FUNCTOR_entity1,
			   PL_INT, chr);

    PL_reset_term_refs(h);
    if ( !rc )
      pd->exception = PL_exception(0);

    return rc;
  }

  return TRUE;
}


static int
on_data(dtd_parser *p, data_type type, int len, const wchar_t *data)
{ parser_data *pd = p->closure;

  if ( pd->on_cdata )
  { fid_t fid;

    if ( (fid=PL_open_foreign_frame()) )
    { int rc;
      term_t av = PL_new_term_refs(2);

      rc = ( PL_unify_wchars(av+0, PL_ATOM, len, data) &&
	     unify_parser(av+1, p) &&
	     call_prolog(pd, pd->on_cdata, av) );

      PL_discard_foreign_frame(fid);
      if ( rc )
	return TRUE;
    }

    pd->exception = PL_exception(0);
    return FALSE;
  }

  if ( pd->tail && !pd->stopped )
  { term_t h = PL_new_term_ref();

    if ( PL_unify_list(pd->tail, h, pd->tail) )
    { int rval = TRUE;
      term_t a;

      switch(type)
      { case EC_CDATA:
	  a = h;
	  break;
	case EC_SDATA:
	{ term_t d = PL_new_term_ref();

	  a = d;
	  rval = PL_unify_term(h, PL_FUNCTOR, FUNCTOR_sdata1, PL_TERM, d);
	  break;
	}
	case EC_NDATA:
	{ term_t d = PL_new_term_ref();

	  a = d;
	  rval = PL_unify_term(h, PL_FUNCTOR, FUNCTOR_ndata1, PL_TERM, d);
	  break;
	}
	default:
	  rval = FALSE;
	  assert(0);
      }

      if ( rval )
	rval = PL_unify_wchars(a, p->cdata_rep, len, data);

      if ( rval )
      { PL_reset_term_refs(h);
	return TRUE;
      } else
      { pd->exception = PL_exception(0);
      }
    }
  }

  return FALSE;
}


static int
on_cdata(dtd_parser *p, data_type type, int len, const wchar_t *data)
{ return on_data(p, type, len, data);
}


static int
can_end_omitted(dtd_parser *p)
{ sgml_environment *env;

  for(env=p->environments; env; env = env->parent)
  { dtd_element *e = env->element;

    if ( !(e->structure && e->structure->omit_close) )
      return FALSE;
  }

  return TRUE;
}


static int
on_error(dtd_parser *p, dtd_error *error)
{ parser_data *pd = p->closure;
  const char *severity;

  if ( pd->stopped )
    return TRUE;

  if ( pd->stopat == SA_ELEMENT &&
       (error->minor == ERC_NOT_OPEN || error->minor == ERC_NOT_ALLOWED) &&
       can_end_omitted(p) )
  { end_document_dtd_parser(p);
    sgml_cplocation(&p->location, &p->startloc);
    pd->stopped = TRUE;
    return TRUE;
  }

  switch(error->severity)
  { case ERS_STYLE:
      if ( pd->error_mode != EM_STYLE )
	return TRUE;
      severity = "informational";
      break;
    case ERS_WARNING:
      pd->warnings++;
      severity = "warning";
      break;
    case ERS_ERROR:
    default:				/* make compiler happy */
      pd->errors++;
      severity = "error";
      break;
  }

  if ( pd->on_error )			/* msg, parser */
  { fid_t fid;

    if ( (fid=PL_open_foreign_frame()) )
    { int rc;
      term_t av = PL_new_term_refs(3);

      rc = ( PL_put_atom_chars(av+0, severity) &&
	     PL_unify_wchars(av+1, PL_ATOM,
			     wcslen(error->plain_message),
			     error->plain_message) &&
	     unify_parser(av+2, p) &&
	     call_prolog(pd, pd->on_error, av)
	   );
      PL_discard_foreign_frame(fid);
      if ( rc )
	return TRUE;
    }
    pd->exception = PL_exception(0);
    return FALSE;
  } else if ( pd->error_mode != EM_QUIET )
  { fid_t fid;

    if ( (fid=PL_open_foreign_frame()) )
    { int rc = TRUE;
      dtd_srcloc *l = file_location(p, &p->startloc);

      if ( pd->max_errors == 0 )
      { term_t ex = PL_new_term_ref();
	term_t pos = PL_new_term_ref();

	if ( l->name.file )
	  rc = PL_unify_term(pos,
			     PL_FUNCTOR, FUNCTOR_file4,
			       PL_NWCHARS, (size_t)-1, l->name.file,
			       PL_INT,   l->line,
			       PL_INT,   l->linepos,
			       PL_INT64, (int64_t)l->charpos);
	if ( rc )
	  rc = PL_unify_term(ex,
			     PL_FUNCTOR, FUNCTOR_error2,
			       PL_FUNCTOR, FUNCTOR_syntax_error1,
			         PL_NWCHARS, (size_t)-1, error->plain_message,
			       PL_TERM, pos);
	if ( rc )
	  rc = PL_raise_exception(ex);
      } else
      { predicate_t pred = PL_predicate("print_message", 2, "user");
	term_t av = PL_new_term_refs(2);
	term_t src = PL_new_term_ref();
	term_t parser = PL_new_term_ref();

	rc = ( unify_parser(parser, p) &&
	       PL_put_atom_chars(av+0, severity) );

	if ( rc )
	{ if ( l->name.file )
	  { if ( l->type == IN_FILE )
	      rc = put_atom_wchars(src, l->name.file);
	    else
	      rc = put_atom_wchars(src, l->name.entity);
	  } else
	  { PL_put_nil(src);
	  }
	}

	if ( rc )
	  rc = PL_unify_term(av+1,
			     PL_FUNCTOR_CHARS, "sgml", 4,
			       PL_TERM, parser,
			       PL_TERM, src,
			       PL_INT, l->line,
			       PL_NWCHARS, wcslen(error->plain_message),
					   error->plain_message);

	if ( rc )
	  rc = PL_call_predicate(NULL, PL_Q_NODEBUG, pred, av);

	PL_discard_foreign_frame(fid);
      }

      if ( rc )
	return TRUE;
    }

    pd->exception = PL_exception(0);
    return FALSE;
  }

  return TRUE;
}


static int
on_xmlns(dtd_parser *p, dtd_symbol *ns, dtd_symbol *url)
{ parser_data *pd = p->closure;

  if ( pd->stopped )
    return TRUE;

  if ( pd->on_xmlns )
  { fid_t fid;
    term_t av;

    if ( (fid = PL_open_foreign_frame()) &&
	 (av = PL_new_term_refs(3)) )
    { int rc;

      if ( ns )
      { rc = put_atom_wchars(av+0, ns->name);
      } else
      { PL_put_nil(av+0);
	rc = TRUE;
      }

      if ( rc )
      { rc = ( put_atom_wchars(av+1, url->name) &&
	       unify_parser(av+2, p) &&
	       call_prolog(pd, pd->on_xmlns, av)
	     );
      }
      end_frame(fid, pd->exception);
      PL_discard_foreign_frame(fid);
      if ( rc )
	return TRUE;
    }

    pd->exception = PL_exception(0);
    return FALSE;
  }

  return TRUE;
}


static int
on_pi(dtd_parser *p, const ichar *pi)
{ parser_data *pd = p->closure;

  if ( pd->stopped )
    return TRUE;

  if ( pd->on_pi )
  { fid_t fid;

    if ( (fid=PL_open_foreign_frame()) )
    { int rc;
      term_t av = PL_new_term_refs(2);

      rc = ( put_atom_wchars(av+0, pi) &&
	     unify_parser(av+1, p) &&
	     call_prolog(pd, pd->on_pi, av)
	   );

      PL_discard_foreign_frame(fid);
      if ( rc )
	return TRUE;
    }

    pd->exception = PL_exception(0);
    return FALSE;
  }

  if ( pd->tail )
  { term_t h;

    if ( !(h = PL_new_term_ref()) ||
	 !PL_unify_list(pd->tail, h, pd->tail) )
    { pd->exception = PL_exception(0);
      return FALSE;
    }

    if ( !PL_unify_term(h,
			PL_FUNCTOR, FUNCTOR_pi1,
			  PL_NWCHARS, wcslen(pi), pi) )
    { pd->exception = PL_exception(0);
      return FALSE;
    }

    PL_reset_term_refs(h);
  }

  return TRUE;
}


static int
on_decl(dtd_parser *p, const ichar *decl)
{ parser_data *pd = p->closure;

  if ( pd->stopped )
    return TRUE;

  if ( pd->on_decl )
  { fid_t fid;
    term_t av;

    if ( (fid = PL_open_foreign_frame()) &&
	 (av = PL_new_term_refs(2)) )
    { int rc;

      rc = ( put_atom_wchars(av+0, decl) &&
	     unify_parser(av+1, p) &&
	     call_prolog(pd, pd->on_decl, av)
	   );
      end_frame(fid, pd->exception);
      PL_discard_foreign_frame(fid);
      if ( rc )
	return TRUE;
    }

    pd->exception = PL_exception(0);
    return FALSE;
  }

  if ( pd->stopat == SA_DECL )
    pd->stopped = TRUE;

  return TRUE;
}


static int
write_parser(void *h, char *buf, int len)
{ parser_data *pd = h;
  unsigned char *s = (unsigned char *)buf;
  unsigned char *e = s+len;

  if ( !pd->parser || pd->parser->magic != SGML_PARSER_MAGIC )
  { errno = EINVAL;
    return -1;
  }

  if ( (pd->errors > pd->max_errors && pd->max_errors >= 0) || pd->stopped )
  { errno = EIO;
    return -1;
  }

  for(; s<e; s++)
  { putchar_dtd_parser(pd->parser, *s);
    if ( pd->exception )
      break;
  }

  return len;
}


static int
close_parser(void *h)
{ parser_data *pd = h;
  dtd_parser *p;

  if ( !(p=pd->parser) || p->magic != SGML_PARSER_MAGIC )
  { errno = EINVAL;
    return -1;
  }

  if ( pd->tail )
  { if ( !PL_unify_nil(pd->tail) )
      return -1;			/* resource error */
  }

  if ( p->dmode == DM_DTD )
    p->dtd->implicit = FALSE;		/* assume we loaded a DTD */

  if ( pd->free_on_close )
    free_dtd_parser(p);
  else
    p->closure = NULL;

  sgml_free(pd);

  return 0;
}


static IOFUNCTIONS sgml_stream_functions =
{ (Sread_function)  NULL,
  (Swrite_function) write_parser,
  (Sseek_function)  NULL,
  (Sclose_function) close_parser,
		    NULL
};


static parser_data *
new_parser_data(dtd_parser *p)
{ parser_data *pd;

  pd = sgml_calloc(1, sizeof(*pd));
  pd->magic = PD_MAGIC;
  pd->parser = p;
  pd->max_errors = MAX_ERRORS;
  pd->max_warnings = MAX_WARNINGS;
  pd->error_mode = EM_PRINT;
  pd->exception = FALSE;
  p->closure = pd;

  return pd;
}


static foreign_t
pl_open_dtd(term_t ref, term_t options, term_t stream)
{ dtd *dtd;
  dtd_parser *p;
  parser_data *pd;
  IOSTREAM *s;
  term_t tail = PL_copy_term_ref(options);
  term_t option = PL_new_term_ref();

  if ( !get_dtd(ref, &dtd) )
    return FALSE;
  p = new_dtd_parser(dtd);
  p->dmode = DM_DTD;
  pd = new_parser_data(p);
  pd->free_on_close = TRUE;

  while( PL_get_list(tail, option, tail) )
  { if ( PL_is_functor(option, FUNCTOR_dialect1) )
    { term_t a = PL_new_term_ref();
      char *s;

      _PL_get_arg(1, option, a);
      if ( !PL_get_atom_chars(a, &s) )
	return sgml2pl_error(ERR_TYPE, "atom", a);

      if ( streq(s, "xml") )
	set_dialect_dtd(dtd, DL_XML);
      else if ( streq(s, "xmlns") )
	set_dialect_dtd(dtd, DL_XMLNS);
      else if ( streq(s, "sgml") )
	set_dialect_dtd(dtd, DL_SGML);
      else
	return sgml2pl_error(ERR_DOMAIN, "sgml_dialect", a);
    }
  }
  if ( !PL_get_nil(tail) )
    return sgml2pl_error(ERR_TYPE, "list", options);

  s = Snew(pd, SIO_OUTPUT|SIO_FBUF, &sgml_stream_functions);

  if ( !PL_open_stream(stream, s) )
    return FALSE;

  return TRUE;
}


static int
set_callback_predicates(parser_data *pd, term_t option)
{ term_t a = PL_new_term_ref();
  char *fname;
  atom_t pname;
  predicate_t *pp = NULL;		/* keep compiler happy */
  int arity;
  module_t m = NULL;

  _PL_get_arg(2, option, a);
  if ( !PL_strip_module(a, &m, a) )
    return FALSE;
  if ( !PL_get_atom(a, &pname) )
    return sgml2pl_error(ERR_TYPE, "atom", a);
  _PL_get_arg(1, option, a);
  if ( !PL_get_atom_chars(a, &fname) )
    return sgml2pl_error(ERR_TYPE, "atom", a);

  if ( streq(fname, "begin") )
  { pp = &pd->on_begin;			/* tag, attributes, parser */
    arity = 3;
  } else if ( streq(fname, "end") )
  { pp = &pd->on_end;			/* tag, parser */
    arity = 2;
  } else if ( streq(fname, "cdata") )
  { pp = &pd->on_cdata;			/* cdata, parser */
    arity = 2;
  } else if ( streq(fname, "entity") )
  { pp = &pd->on_entity;		/* name, parser */
    arity = 2;
  } else if ( streq(fname, "pi") )
  { pp = &pd->on_pi;			/* pi, parser */
    arity = 2;
  } else if ( streq(fname, "xmlns") )
  { pp = &pd->on_xmlns;			/* ns, url, parser */
    arity = 3;
  } else if ( streq(fname, "urlns") )
  { pp = &pd->on_urlns;			/* url, ns, parser */
    arity = 3;
  } else if ( streq(fname, "error") )
  { pp = &pd->on_error;			/* severity, message, parser */
    arity = 3;
  } else if ( streq(fname, "decl") )
  { pp = &pd->on_decl;			/* decl, parser */
    arity = 2;
  } else
    return sgml2pl_error(ERR_DOMAIN, "sgml_callback", a);

  *pp = PL_pred(PL_new_functor(pname, arity), m);
  return TRUE;
}


static foreign_t
pl_sgml_parse(term_t parser, term_t options)
{ dtd_parser *p;
  parser_data *pd;
  parser_data *oldpd;
  term_t head = PL_new_term_ref();
  term_t tail = PL_copy_term_ref(options);
  IOSTREAM *in = NULL;
  IOSTREAM *release = NULL;
  int recursive;
  int has_content_length = FALSE;
  int64_t content_length = 0;		/* content_length(Len) */
  int count = 0;
  int rc = TRUE;

  if ( !get_parser(parser, &p) )
    return FALSE;

  if ( p->closure )			/* recursive call */
  { recursive = TRUE;

    oldpd = p->closure;
    if ( oldpd->magic != PD_MAGIC || oldpd->parser != p )
      return sgml2pl_error(ERR_MISC, "sgml",
			   "Parser associated with illegal data");

    pd = sgml_calloc(1, sizeof(*pd));
    *pd = *oldpd;
    p->closure = pd;

    in = pd->source;
  } else
  { recursive = FALSE;
    oldpd = NULL;			/* keep compiler happy */

    set_mode_dtd_parser(p, DM_DATA);

    p->on_begin_element = on_begin;
    p->on_end_element   = on_end;
    p->on_entity	= on_entity;
    p->on_pi		= on_pi;
    p->on_data          = on_cdata;
    p->on_error	        = on_error;
    p->on_xmlns		= on_xmlns;
    p->on_decl		= on_decl;
    p->cdata_rep        = PL_ATOM;
    p->att_rep          = PL_ATOM;
    pd = new_parser_data(p);
  }

  while ( PL_get_list(tail, head, tail) )
  { if ( PL_is_functor(head, FUNCTOR_document1) )
    { pd->list  = PL_new_term_ref();
      _PL_get_arg(1, head, pd->list);
      pd->tail  = PL_copy_term_ref(pd->list);
      pd->stack = NULL;
    } else if ( PL_is_functor(head, FUNCTOR_content_length1) )
    { term_t a = PL_new_term_ref();

      _PL_get_arg(1, head, a);
      if ( !PL_get_int64(a, &content_length) )
	return sgml2pl_error(ERR_TYPE, "integer", a);
      has_content_length = TRUE;
    } else if ( PL_is_functor(head, FUNCTOR_call2) )
    { if ( !set_callback_predicates(pd, head) )
	return FALSE;
    } else if ( PL_is_functor(head, FUNCTOR_xml_no_ns1) )
    { term_t a = PL_new_term_ref();
      char *s;

      _PL_get_arg(1, head, a);
      if ( !PL_get_atom_chars(a, &s) )
	return sgml2pl_error(ERR_TYPE, "atom", a);
      if ( streq(s, "error") )
	p->xml_no_ns = NONS_ERROR;
      else if ( streq(s, "quiet") )
	p->xml_no_ns = NONS_QUIET;
      else
	return sgml2pl_error(ERR_DOMAIN, "xml_no_ns", a);
    } else if ( PL_is_functor(head, FUNCTOR_parse1) )
    { term_t a = PL_new_term_ref();
      char *s;

      _PL_get_arg(1, head, a);
      if ( !PL_get_atom_chars(a, &s) )
	return sgml2pl_error(ERR_TYPE, "atom", a);
      if ( streq(s, "element") )
	pd->stopat = SA_ELEMENT;
      else if ( streq(s, "content") )
	pd->stopat = SA_CONTENT;
      else if ( streq(s, "file") )
	pd->stopat = SA_FILE;
      else if ( streq(s, "input") )
	pd->stopat = SA_INPUT;
      else if ( streq(s, "declaration") )
	pd->stopat = SA_DECL;
      else
	return sgml2pl_error(ERR_DOMAIN, "parse", a);
    } else if ( PL_is_functor(head, FUNCTOR_max_errors1) )
    { term_t a = PL_new_term_ref();

      _PL_get_arg(1, head, a);
      if ( !PL_get_integer(a, &pd->max_errors) )
	return sgml2pl_error(ERR_TYPE, "integer", a);
    } else if ( PL_is_functor(head, FUNCTOR_syntax_errors1) )
    { term_t a = PL_new_term_ref();
      char *s;

      _PL_get_arg(1, head, a);
      if ( !PL_get_atom_chars(a, &s) )
	return sgml2pl_error(ERR_TYPE, "atom", a);

      if ( streq(s, "quiet") )
	pd->error_mode = EM_QUIET;
      else if ( streq(s, "print") )
	pd->error_mode = EM_PRINT;
      else if ( streq(s, "style") )
	pd->error_mode = EM_STYLE;
      else
	return sgml2pl_error(ERR_DOMAIN, "syntax_error", a);
    } else if ( PL_is_functor(head, FUNCTOR_positions1) )
    { term_t a = PL_new_term_ref();
      char *s;

      _PL_get_arg(1, head, a);
      if ( !PL_get_atom_chars(a, &s) )
	return sgml2pl_error(ERR_TYPE, "atom", a);

      if ( streq(s, "true") )
	pd->positions = TRUE;
      else if ( streq(s, "false") )
	pd->positions = FALSE;
      else
	return sgml2pl_error(ERR_DOMAIN, "positions", a);
    } else if ( PL_is_functor(head, FUNCTOR_cdata1) )
    { term_t arg = PL_new_term_ref();
      atom_t a;
      _PL_get_arg(1, head, arg);
      if (!PL_get_atom_ex(arg, &a))
	return FALSE;
      if (a == ATOM_atom)
	p->cdata_rep = PL_ATOM;
      else if (a == ATOM_string)
	p->cdata_rep = PL_STRING;
      else
	return sgml2pl_error(ERR_DOMAIN, "representation", a);
    } else if ( PL_is_functor(head, FUNCTOR_attribute_value1) )
    { term_t arg = PL_new_term_ref();
      atom_t a;
      _PL_get_arg(1, head, arg);
      if (!PL_get_atom_ex(arg, &a))
	return FALSE;
      if (a == ATOM_atom)
	p->att_rep = PL_ATOM;
      else if (a == ATOM_string)
	p->att_rep = PL_STRING;
      else
	return sgml2pl_error(ERR_DOMAIN, "representation", a);
    } else if ( PL_is_functor(head, FUNCTOR_source1) )
    { term_t a = PL_new_term_ref();

      _PL_get_arg(1, head, a);
      if ( !PL_get_stream_handle(a, &in) )
	return FALSE;
      release = in;
    }/* else ignored option */
  }
  if ( !PL_get_nil_ex(tail) )
  { if ( release )
      PL_release_stream(release);
    return FALSE;
  }

					/* Parsing input from a stream */
#define CHECKERROR \
    { if ( pd->exception ) \
      { rc = FALSE; \
	goto out; \
      } \
      if ( pd->errors > pd->max_errors && pd->max_errors >= 0 ) \
      { rc = sgml2pl_error(ERR_LIMIT, "max_errors", (long)pd->max_errors); \
	goto out; \
      } \
    }

  if ( pd->stopat == SA_CONTENT && p->empty_element )
    goto out;

  if ( in )
  { int eof = FALSE;

    if ( in->encoding == ENC_OCTET )
      p->encoded = TRUE;		/* parser must decode */
    else
      p->encoded = FALSE;		/* already decoded */

    pd->stopped = FALSE;

    if ( !recursive )
    { pd->source = in;
      begin_document_dtd_parser(p);
    }

    while(!eof)
    { int c, ateof;

      if ( (++count % 8192) == 0 && PL_handle_signals() < 0 )
      { rc = FALSE;
	goto out;
      }

      if ( has_content_length )
      { if ( content_length <= 0 )
	  c = EOF;
	else
	  c = Sgetcode(in);
	ateof = (--content_length <= 0);
      } else
      { c = Sgetcode(in);
	ateof = Sfeof(in);
      }

      if ( ateof )
      { eof = TRUE;
	if ( c == LF )			/* file ends in LF */
	  c = CR;
	else if ( c != CR )		/* file ends in normal char */
	{ if ( has_content_length && in->position )
	  { int64_t bc0 = in->position->byteno;
	    putchar_dtd_parser(p, c);
	    content_length -= in->position->byteno-bc0;
	  } else
	  { putchar_dtd_parser(p, c);
	  }
	  CHECKERROR;
	  if ( pd->stopped )
	    goto stopped;
	  c = CR;
	}
      }

      if ( has_content_length && in->position )
      { int64_t bc0 = in->position->byteno;
	putchar_dtd_parser(p, c);
	content_length -= in->position->byteno-bc0;
      } else
      { putchar_dtd_parser(p, c);
      }
      CHECKERROR;
      if ( pd->stopped )
      { stopped:
	pd->stopped = FALSE;
	if ( pd->stopat != SA_CONTENT )
	  reset_document_dtd_parser(p);	/* ensure a clean start */
	goto out;
      }
    }

    if ( !recursive && pd->stopat != SA_INPUT )
      end_document_dtd_parser(p);
    CHECKERROR;

  out:
    if ( release )
      PL_release_stream(release);

    reset_url_cache();
    if ( pd->tail )
    { if ( !PL_unify_nil(pd->tail) )
	return FALSE;
    }

    if ( recursive )
    { p->closure = oldpd;
    } else
    { p->closure = NULL;
    }

    pd->magic = 0;			/* invalidate */
    sgml_free(pd);

    return rc;
  }

  reset_url_cache();

  return TRUE;
}


		 /*******************************
		 *	  DTD PROPERTIES	*
		 *******************************/

static int put_model(term_t t, dtd_model *m) WUNUSED;

/* doctype(DocType) */

static int
dtd_prop_doctype(dtd *dtd, term_t prop)
{ if ( dtd->doctype )
    return PL_unify_wchars(prop, PL_ATOM, ENDSNUL, dtd->doctype);
  return FALSE;
}


/* elements(ListOfElements) */

WUNUSED static int
make_model_list(term_t t, dtd_model *m, functor_t f)
{ if ( !m->next )
  { return put_model(t, m);
  } else
  { term_t av;

    if ( (av=PL_new_term_refs(2)) &&
	 put_model(av+0, m) &&
	 make_model_list(av+1, m->next, f) &&
	 PL_cons_functor_v(t, f, av) )
    { PL_reset_term_refs(av);
      return TRUE;
    }

    return FALSE;
  }
}


WUNUSED static int
put_model(term_t t, dtd_model *m)
{ int rc = TRUE;
  functor_t f;

  switch(m->type)
  { case MT_PCDATA:
      rc = PL_put_atom(t, ATOM_pcdata);
      goto card;
    case MT_ELEMENT:
      rc = put_atom_wchars(t, m->content.element->name->name);
      goto card;
    case MT_AND:
      f = FUNCTOR_and2;
      break;
    case MT_SEQ:
      f = FUNCTOR_comma2;
      break;
    case MT_OR:
      f = FUNCTOR_bar2;
      break;
    case MT_UNDEF:
    default:
      assert(0);
      f = 0;
      break;
  }

  if ( rc )
  { if ( !m->content.group )
      rc = PL_put_atom(t, ATOM_empty);
    else
      rc = make_model_list(t, m->content.group, f);
  }

card:
  if ( !rc )
    return FALSE;

  switch(m->cardinality)
  { case MC_ONE:
      break;
    case MC_OPT:
      rc = PL_cons_functor_v(t, FUNCTOR_opt1, t);
      break;
    case MC_REP:
      rc = PL_cons_functor_v(t, FUNCTOR_rep1, t);
      break;
    case MC_PLUS:
      rc = PL_cons_functor_v(t, FUNCTOR_plus1, t);
      break;
  }

  return rc;
}


WUNUSED static int
put_content(term_t t, dtd_edef *def)
{ switch(def->type)
  { case C_EMPTY:
      return PL_put_atom(t, ATOM_empty);
    case C_CDATA:
      return PL_put_atom(t, ATOM_cdata);
    case C_RCDATA:
      return PL_put_atom(t, ATOM_rcdata);
    case C_ANY:
      return PL_put_atom(t, ATOM_any);
    default:
      if ( def->content )
	return put_model(t, def->content);

      return TRUE;
  }
}


static int
dtd_prop_elements(dtd *dtd, term_t prop)
{ term_t tail = PL_copy_term_ref(prop);
  term_t head = PL_new_term_ref();
  term_t et   = PL_new_term_ref();
  dtd_element *e;

  for( e=dtd->elements; e; e=e->next )
  { put_atom_wchars(et, e->name->name);
    if ( !PL_unify_list(tail, head, tail) ||
	 !PL_unify(head, et) )
      return FALSE;
  }

  return PL_unify_nil(tail);
}


static int
get_element(dtd *dtd, term_t name, dtd_element **elem)
{ ichar *s;
  dtd_element *e;
  dtd_symbol *id;

  if ( !PL_get_wchars(name, NULL, &s, CVT_ATOM|CVT_EXCEPTION) )
    return FALSE;

  if ( !(id=dtd_find_symbol(dtd, s)) ||
       !(e=id->element) )
    return FALSE;

  *elem = e;
  return TRUE;
}




static int
dtd_prop_element(dtd *dtd, term_t name, term_t omit, term_t content)
{ dtd_element *e;
  dtd_edef *def;
  term_t model = PL_new_term_ref();

  if ( !get_element(dtd, name, &e) || !(def=e->structure) )
    return FALSE;

  if ( !PL_unify_term(omit, PL_FUNCTOR, FUNCTOR_omit2,
		        PL_ATOM, def->omit_open ?  ATOM_true : ATOM_false,
		        PL_ATOM, def->omit_close ? ATOM_true : ATOM_false) )
    return FALSE;

  return ( put_content(model, def) &&
	   PL_unify(content, model) );
}


static int
dtd_prop_attributes(dtd *dtd, term_t ename, term_t atts)
{ dtd_element *e;
  term_t tail = PL_copy_term_ref(atts);
  term_t head = PL_new_term_ref();
  term_t elem = PL_new_term_ref();
  dtd_attr_list *al;

  if ( !get_element(dtd, ename, &e) )
    return FALSE;

  for(al=e->attributes; al; al=al->next)
  { put_atom_wchars(elem, al->attribute->name->name);

    if ( !PL_unify_list(tail, head, tail) ||
	 !PL_unify(head, elem) )
      return FALSE;
  }

  return PL_unify_nil(tail);
}


typedef struct _plattrdef
{ attrtype	type;			/* AT_* */
  const char *	name;			/* name */
  int	       islist;			/* list-type */
  atom_t	atom;			/* name as atom */
} plattrdef;

static plattrdef plattrs[] =
{
  { AT_CDATA,	 "cdata",    FALSE },
  { AT_ENTITY,	 "entity",   FALSE },
  { AT_ENTITIES, "entity",   TRUE },
  { AT_ID,	 "id",	     FALSE },
  { AT_IDREF,	 "idref",    FALSE },
  { AT_IDREFS,	 "idref",    TRUE },
  { AT_NAME,	 "name",     FALSE },
  { AT_NAMES,	 "name",     TRUE },
/*{ AT_NAMEOF,	 "nameof",   FALSE },*/
  { AT_NMTOKEN,	 "nmtoken",  FALSE },
  { AT_NMTOKENS, "nmtoken",  TRUE },
  { AT_NUMBER,	 "number",   FALSE },
  { AT_NUMBERS,	 "number",   TRUE },
  { AT_NUTOKEN,	 "nutoken",  FALSE },
  { AT_NUTOKENS, "nutoken",  TRUE },
  { AT_NOTATION, "notation", FALSE },

  { AT_CDATA,    NULL,       FALSE }
};


static int
unify_attribute_type(term_t type, dtd_attr *a)
{ plattrdef *ad = plattrs;

  for( ; ad->name; ad++ )
  { if ( ad->type == a->type )
    { if ( !ad->atom )
	ad->atom = PL_new_atom(ad->name);

      if ( ad->islist )
	return PL_unify_term(type, PL_FUNCTOR, FUNCTOR_list1,
			     PL_ATOM, ad->atom);
      else
	return PL_unify_atom(type, ad->atom);
    }
  }

  if ( a->type == AT_NAMEOF || a->type == AT_NOTATION )
  { dtd_name_list *nl;
    term_t tail, head, elem;

    if ( !(tail = PL_new_term_ref()) ||
	 !(head = PL_new_term_ref()) ||
	 !(elem = PL_new_term_ref()) ||
	 !PL_unify_functor(type,
			   a->type == AT_NAMEOF ?
			     FUNCTOR_nameof1 :
			     FUNCTOR_notation1) )
      return FALSE;

    _PL_get_arg(1, type, tail);

    for(nl = a->typeex.nameof; nl; nl = nl->next)
    { if ( !put_atom_wchars(elem, nl->value->name) ||
	   !PL_unify_list(tail, head, tail) ||
	   !PL_unify(head, elem) )
	return FALSE;
    }
    return PL_unify_nil(tail);
  }

  assert(0);
  return FALSE;
}



static int
unify_attribute_default(term_t defval, dtd_attr *a)
{ int v;

  switch(a->def)
  { case AT_REQUIRED:
      return PL_unify_atom_chars(defval, "required");
    case AT_CURRENT:
      return PL_unify_atom_chars(defval, "current");
    case AT_CONREF:
      return PL_unify_atom_chars(defval, "conref");
    case AT_IMPLIED:
      return PL_unify_atom_chars(defval, "implied");
    case AT_DEFAULT:
      v = PL_unify_functor(defval, FUNCTOR_default1);
      goto common;
    case AT_FIXED:
      v = PL_unify_functor(defval, FUNCTOR_fixed1);
    common:
      if ( v )
      { term_t tmp;

	if ( !(tmp=PL_new_term_ref()) )
	  return FALSE;

	_PL_get_arg(1, defval, tmp);
	switch( a->type )
	{ case AT_CDATA:
	    return PL_unify_wchars(tmp, PL_ATOM, ENDSNUL, a->att_def.cdata);
	  case AT_NAME:
	  case AT_NMTOKEN:
	  case AT_NAMEOF:
	  case AT_NOTATION:
	    return PL_unify_wchars(tmp, PL_ATOM, ENDSNUL, a->att_def.name->name);
	  case AT_NUMBER:
	    return PL_unify_integer(tmp, a->att_def.number);
	  default:
	    assert(0);
	}
      } else
	return FALSE;
    default:
      assert(0);
      return FALSE;
  }
}


static int
dtd_prop_attribute(dtd *dtd, term_t ename, term_t aname,
		   term_t type, term_t def_value)
{ dtd_element *e;
  ichar *achars;
  dtd_symbol *asym;
  dtd_attr_list *al;


  if ( !get_element(dtd, ename, &e) )
    return FALSE;
  if ( !PL_get_wchars(aname, NULL, &achars, CVT_ATOM|CVT_EXCEPTION) )
    return FALSE;
  if ( !(asym=dtd_find_symbol(dtd, achars)) )
    return FALSE;

  for(al=e->attributes; al; al=al->next)
  { if ( al->attribute->name == asym )
    { if ( unify_attribute_type(type, al->attribute) &&
	   unify_attribute_default(def_value, al->attribute) )
	return TRUE;

      return FALSE;
    }
  }

  return FALSE;
}


static int
dtd_prop_entities(dtd *dtd, term_t list)
{ term_t tail = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();
  term_t et   = PL_new_term_ref();
  dtd_entity *e;

  for( e=dtd->entities; e; e=e->next )
  { put_atom_wchars(et, e->name->name);
    if ( !PL_unify_list(tail, head, tail) ||
	 !PL_unify(head, et) )
      return FALSE;
  }

  return PL_unify_nil(tail);
}


static int
dtd_prop_entity(dtd *dtd, term_t ename, term_t value)
{ ichar *s;
  dtd_entity *e;
  dtd_symbol *id;

  if ( !PL_get_wchars(ename, NULL, &s, CVT_ATOM|CVT_EXCEPTION) )
    return FALSE;

  if ( !(id=dtd_find_symbol(dtd, s)) ||
       !(e=id->entity)  )
    return FALSE;

  switch(e->type)
  { case ET_SYSTEM:
      return PL_unify_term(value, PL_FUNCTOR_CHARS, "system", 1,
			   PL_CHARS, e->exturl);
    case ET_PUBLIC:
      if ( e->exturl )
	return PL_unify_term(value, PL_FUNCTOR_CHARS, "public", 2,
			     PL_CHARS, e->extid,
			     PL_CHARS, e->exturl);
      else
	return PL_unify_term(value, PL_FUNCTOR_CHARS, "public", 2,
			     PL_CHARS, e->extid,
			     PL_VARIABLE);

    case ET_LITERAL:
    default:
      if ( e->value )
      { const char *wrap;

	switch(e->content)
	{ case EC_SGML:     wrap = "sgml"; break;
	  case EC_STARTTAG: wrap = "start_tag"; break;
	  case EC_ENDTAG:   wrap = "end_tag"; break;
	  case EC_CDATA:    wrap = NULL; break;
	  case EC_SDATA:    wrap = "sdata"; break;
	  case EC_NDATA:    wrap = "ndata"; break;
	  case EC_PI:       wrap = "pi"; break;
	  default:	    wrap = NULL; assert(0);
	}

	if ( wrap )
	  return PL_unify_term(value, PL_FUNCTOR_CHARS, wrap, 1,
			       PL_CHARS, e->value);
	else
	  return PL_unify_wchars(value, PL_ATOM, wcslen(e->value), e->value);
      }
  }

  assert(0);
  return FALSE;
}


static int
dtd_prop_notations(dtd *dtd, term_t list)
{ dtd_notation *n;
  term_t tail = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();

  for(n=dtd->notations; n; n=n->next)
  { if ( PL_unify_list(tail, head, tail) &&
	 PL_unify_wchars(head, PL_ATOM, wcslen(n->name->name), n->name->name) )
      continue;

    return FALSE;
  }

  return PL_unify_nil(tail);
}


static int
dtd_prop_notation(dtd *dtd, term_t nname, term_t desc)
{ char *s;
  dtd_symbol *id;

  if ( !PL_get_atom_chars(nname, &s) )
    return sgml2pl_error(ERR_TYPE, "atom", nname);

  if ( (id=dtd_find_symbol(dtd, (ichar *)s)) )
  { dtd_notation *n;

    for(n=dtd->notations; n; n=n->next)
    { if ( n->name == id )
      { term_t tail = PL_copy_term_ref(desc);
	term_t head = PL_new_term_ref();

	if ( n->system )
	{ if ( !PL_unify_list(tail, head, tail) ||
	       !PL_unify_term(head,
			      PL_FUNCTOR_CHARS, "system", 1,
			        PL_CHARS, n->system) )
	    return FALSE;
	}
	if ( n->public )
	{ if ( !PL_unify_list(tail, head, tail) ||
	       !PL_unify_term(head,
			      PL_FUNCTOR_CHARS, "public", 1,
			        PL_CHARS, n->public) )
	    return FALSE;
	}

	return PL_unify_nil(tail);
      }
    }
  }

  return FALSE;
}



typedef struct _prop
{ int (*func)();
  const char *name;
  int arity;
  functor_t functor;
} prop;


static prop dtd_props[] =
{ { dtd_prop_doctype,    "doctype",    1 },
  { dtd_prop_elements,	 "elements",   1 },
  { dtd_prop_element,	 "element",    3 },
  { dtd_prop_attributes, "attributes", 2, },
  { dtd_prop_attribute,	 "attribute",  4, },
  { dtd_prop_entities,	 "entities",   1, },
  { dtd_prop_entity,	 "entity",     2, },
  { dtd_prop_notations,	 "notations",  1, },
  { dtd_prop_notation,	 "notation",   2, },
  { NULL }
};


static void
initprops()
{ static int done = FALSE;

  if ( !done )
  { prop *p;

    done = TRUE;
    for(p=dtd_props; p->func; p++)
      p->functor = PL_new_functor(PL_new_atom(p->name), p->arity);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
dtd_property(DTD, doctype(DocType))
dtd_property(DTD, elements(ListOfNames))
dtd_property(DTD, element(Name, Omit, Model))
dtd_property(DTD, attributes(ElementName, ListOfAttributes)),
dtd_property(DTD, attribute(ElementName, AttributeName, Type, Default))
dtd_property(DTD, entities(ListOfEntityNames))
dtd_property(DTD, entity(Name, Type))
dtd_property(DTD, notations(ListOfNotationNames)
dtd_property(DTD, notation(Name, File))
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
pl_dtd_property(term_t ref, term_t property)
{ dtd *dtd;
  const prop *p;

  initprops();

  if ( !get_dtd(ref, &dtd) )
    return FALSE;

  for(p=dtd_props; p->func; p++)
  { if ( PL_is_functor(property, p->functor) )
    { term_t a = PL_new_term_refs(p->arity);
      int i;

      for(i=0; i<p->arity; i++)
	_PL_get_arg(i+1, property, a+i);

      switch(p->arity)
      { case 1:
	  return (*p->func)(dtd, a+0);
	case 2:
	  return (*p->func)(dtd, a+0, a+1);
	case 3:
	  return (*p->func)(dtd, a+0, a+1, a+2);
	case 4:
	  return (*p->func)(dtd, a+0, a+1, a+2, a+3);
	default:
	  assert(0);
	  return FALSE;
      }
    }
  }

  return sgml2pl_error(ERR_DOMAIN, "dtd_property", property);
}

		 /*******************************
		 *	     CATALOG		*
		 *******************************/

static foreign_t
pl_sgml_register_catalog_file(term_t file, term_t where)
{ wchar_t *fn;
  char *w;
  catalog_location loc;

  if ( !PL_get_wchars(file, NULL, &fn, CVT_ATOM|CVT_EXCEPTION) )
    return FALSE;
  if ( !PL_get_atom_chars(where, &w) )
    return sgml2pl_error(ERR_TYPE, "atom", where);

  if ( streq(w, "start") )
    loc = CTL_START;
  else if ( streq(w, "end") )
    loc = CTL_END;
  else
    return sgml2pl_error(ERR_DOMAIN, "location", where);

  return register_catalog_file(fn, loc);
}


		 /*******************************
		 *	      INSTALL		*
		 *******************************/

extern install_t install_xml_quote(void);
extern install_t install_xsd(void);
#ifdef O_STATISTICS
extern void sgml_statistics(void);
#endif

install_t
install(void)
{ initConstants();

  init_ring();

  PL_register_foreign("new_dtd",	  2, pl_new_dtd,	  0);
  PL_register_foreign("free_dtd",	  1, pl_free_dtd,	  0);
  PL_register_foreign("new_sgml_parser",  2, pl_new_sgml_parser,  0);
  PL_register_foreign("free_sgml_parser", 1, pl_free_sgml_parser, 0);
  PL_register_foreign("set_sgml_parser",  2, pl_set_sgml_parser,  0);
  PL_register_foreign("get_sgml_parser",  2, pl_get_sgml_parser,  0);
  PL_register_foreign("open_dtd",         3, pl_open_dtd,	  0);
  PL_register_foreign("sgml_parse",       2, pl_sgml_parse,
		      PL_FA_TRANSPARENT);
  PL_register_foreign("_sgml_register_catalog_file", 2,
		      pl_sgml_register_catalog_file, 0);

  PL_register_foreign("$dtd_property",	  2, pl_dtd_property, 0);

  install_xml_quote();
  install_xsd();
#ifdef O_STATISTICS
  atexit(sgml_statistics);
#endif
}


install_t
uninstall(void)
{ stop_ring();
}
