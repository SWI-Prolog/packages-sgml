/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2014, University of Amsterdam
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

#ifndef SGML_PARSER_H_INCLUDED
#define SGML_PARSER_H_INCLUDED
#include "util.h"
#include <wchar.h>

		 /*******************************
		 *	      CALL-BACK		*
		 *******************************/

					/* sgml_attribute->flags */
#define SGML_AT_DEFAULT		0x1

typedef struct _sgml_attribute
{ struct				/* so we can free members */
  { wchar_t *textW;			/* UCS textual value */
    long   number;			/* numeric value/length */
  } value;
  dtd_attr *definition;			/* DTD definition */
  unsigned flags;			/* additional flags */
} sgml_attribute;

typedef struct _dtd_parser *dtd_parser_p;

typedef int (*sgml_begin_element_f)(dtd_parser_p parser,
				    dtd_element *e,
				    int argc,
				    sgml_attribute *argv);
typedef int (*sgml_end_element_f)(dtd_parser_p parser,
				  dtd_element *e);
typedef int (*sgml_data_f)(dtd_parser_p parser,
			   data_type type, int len, const wchar_t *text);
typedef int (*sgml_wdata_f)(dtd_parser_p parser,
			   data_type type, int len, const wchar_t *text);
typedef int (*sgml_entity_f)(dtd_parser_p parser,
			     dtd_entity *entity,
			     int chr);
typedef int (*sgml_pi_f)(dtd_parser_p parser, const ichar *pi);
typedef int (*sgml_error_f)(dtd_parser_p parser,
			    dtd_error *error);
typedef int (*sgml_decl_f)(dtd_parser_p parser, const ichar *decl);
#ifdef XMLNS
typedef int (*xmlns_f)(dtd_parser_p parser,
		       dtd_symbol *ns, dtd_symbol *url);
#endif


		 /*******************************
		 *	 PARSER AND STATES	*
		 *******************************/

#define SGML_PARSER_MAGIC	(0x834ab663)

typedef enum
{ S_PCDATA,				/* between declarations */
#ifdef UTF8
  S_UTF8,				/* Loading UTF-8 character */
#endif
  S_CDATA,				/* non-parsed data */
  S_RCDATA,				/* CDATA+entities */
  S_MSCDATA,				/* <![CDATA[...]]> */
  S_EMSCDATA1,				/* Seen ] in S_MSCDATA */
  S_EMSCDATA2,				/* Seen ]] in S_MSCDATA */
  S_ECDATA1,				/* Seen < in CDATA */
  S_ECDATA2,				/* Seen </ in CDATA */
  S_EMSC1,				/* Seen ] in marked section */
  S_EMSC2,				/* Seen ]] in marked section */
  S_PI,					/* Seen <? */
  S_PI2,				/* Seen <?...? */
  S_DECL0,				/* Seen < */
  S_DECL,				/* inside a declaration */
  S_MDECL0,				/* Seen <! */
  S_STRING,				/* inside a "string" or 'string' */
  S_DECLCMT0,				/* Seen <...- */
  S_DECLCMT,				/* Seen <...-- */
  S_DECLCMTE0,				/* Seen <...--..- */
  S_CMTO,				/* Seen <!- */
  S_CMT1,				/* Seen <!-- */
  S_CMT,				/* Seen <!--X... */
  S_CMTE0,				/* Seem <!--...- */
  S_CMTE1,				/* Seem <!--...-- */
  S_GROUP,				/* inside [...] */
  S_PENT,				/* Seen % */
  S_ENT0,				/* Seen & */
  S_ENT,				/* Seen &(#|\w) */
  S_ENTCR				/* Seen &entity<CR> */
} dtdstate;


typedef enum
{ DCL_DTD,				/* DTD Declaration */
  DCL_BEGIN,				/* begin-tag */
  DCL_END				/* end-tag */
} dcl_type;


typedef enum
{ MS_IGNORE,				/* ignore this data */
  MS_INCLUDE,				/* process normally */
  MS_CDATA,				/* pass literally */
  MS_RCDATA				/* replace entities */
} marktype;


typedef enum
{ EV_EXPLICIT,				/* Explicit event */
  EV_OMITTED,				/* Omitted tag event */
  EV_SHORTTAG,				/* SHORTTAG event: <tag/value/ */
  EV_SHORTREF				/* SHORTREF event */
} sgml_event_class;


typedef struct _dtd_marked
{ dtd_symbol *keyword;			/* keyword of the marked section */
  marktype	type;			/* processing type */
  struct _dtd_marked *parent;		/* parent marked section */
} dtd_marked;


typedef enum
{ DM_DTD,				/* DTD mode: no data allowed (?) */
  DM_DATA				/* Environment has only elements */
} data_mode;

#ifdef XMLNS
typedef enum
{ NONS_ERROR = 0,
  NONS_QUIET
} xmlnons;
#endif

typedef struct _sgml_environment
{ dtd_element *element;			/* element that opened the env */
  struct _dtd_state *state;		/* State we are in */
#ifdef XMLNS
  struct _xmlns *xmlns;			/* XML namespace */
  struct _xmlns *thisns;		/* Name space of element */
#endif
#ifdef XMLBASE
  ichar *uri_base;			/* xml:base handling */
#endif
  dtd_space_mode space_mode;		/* How to handle blanks */
  dtd_shortref *map;			/* SHORTREF map */
  struct _sgml_environment *parent;	/* Parent environment */
  int	wants_net;			/* I want a net */
  int	saved_waiting_for_net;		/* saved value of waiting for net */
} sgml_environment;

					/* parser->flags */
#define SGML_PARSER_NODEFS	 0x01	/* don't handle default atts */
#define SGML_PARSER_QUALIFY_ATTS 0x02	/* qualify attributes in XML mode */

typedef struct _dtd_parser
{ unsigned long magic;			/* SGML_PARSER_MAGIC */
  dtd     *dtd;				/* DTD we are building */
  dtdstate state;			/* current state */
  dtdstate cdata_state;			/* S_CDATA/S_RCDATA */
  dtd_marked *marked;			/* marked section stack */
  marktype mark_state;			/* processing mode */
  dtd_element *empty_element;		/* empty of <tag/> seen */
  sgml_environment *environments;	/* Open environments */
  data_mode dmode;			/* How to handle characters */
  int	   first;			/* Just seen <tag> */
  int	   waiting_for_net;		/* waiting for / in <shorttag/mode/ */
  size_t max_memory;			/* maximum buffer size */
  icharbuf *buffer;			/* buffer for temp data */
  ocharbuf *cdata;			/* collected character data */
  int	   blank_cdata;			/* CDATA is all blank */
  int	   cdata_must_be_empty;		/* Only shortrefs allowed here */
  const ichar *etag;			/* name of end-tag in CDATA */
  int	   etaglen;			/* length of end-tag */
  int	   grouplevel;			/* [..] level in declaration */
  int	   saved;			/* saved character */
  dtdstate lit_saved_state;		/* literal saved-state */
  int      encoded;			/* TRUE for binary input */
  dtd_shortref *map;			/* SHORTREF map */
#ifdef UTF8
  int	   utf8_decode;			/* decode UTF-8 sequences? */
  int      utf8_char;			/* building character */
  int	   utf8_left;			/* bytes left */
  dtdstate utf8_saved_state;		/* state from which we come */
#endif
  dtd_srcloc	location;		/* Current location */
  dtd_srcloc	startloc;		/* Start of last markup */
  dtd_srcloc	startcdata;		/* Start of last cdata */
  dtd_symbol   *enforce_outer_element;	/* Outer element to look for */
  sgml_event_class event_class;		/* EV_* */
  xmlnons	xml_no_ns;		/* What if namespace does not exist? */
#ifdef XMLNS
  struct _xmlns *xmlns;			/* Outer xmlns declaration */
#endif

  void *closure;			/* client handle */
  sgml_begin_element_f	on_begin_element; /* start an element */
  sgml_end_element_f	on_end_element;	/* end an element */
  sgml_data_f		on_data;	/* process cdata */
  sgml_entity_f		on_entity;	/* unprocessed entity */
  sgml_pi_f		on_pi;		/* processing instruction */
  sgml_error_f		on_error;	/* handle error */
  sgml_decl_f		on_decl;	/* handle declarations */
#ifdef XMLNS
  xmlns_f		on_xmlns;	/* handle new namespace */
#endif
  unsigned		flags;		/* misc flags */
  int                   cdata_rep;      /* representation for cdata */
  int                   att_rep;        /* representation for attribute values */
} dtd_parser;


#ifdef XMLNS
#include "xmlns.h"
#endif

extern int		gripe(dtd_parser *p, dtd_error_id e, ...);

#define SGML_SUB_DOCUMENT	0x1

#endif /*SGML_PARSER_H_INCLUDED*/

