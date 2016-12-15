/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2015, University of Amsterdam
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

#define DTD_IMPLEMENTATION 1
#include <stdio.h>
#include <wchar.h>
#include "dtd.h"
#include "model.h"
#include "util.h"
#include "catalog.h"
#include "parser.h"
#include <stdlib.h>
#include <assert.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include "utf8.h"
#include <errno.h>
#include <wctype.h>
#include "xml_unicode.h"

#define DEBUG(g) ((void)0)
#define ZERO_TERM_LEN (-1)		/* terminated by nul */

#ifdef __WINDOWS__
#define inline __inline
#define swprintf _snwprintf
#endif


		 /*******************************
		 *	    LOCAL TYPES		*
		 *******************************/

typedef struct locbuf
{ dtd_srcloc start;			/* p->startloc */
  dtd_srcloc here;			/* p->location */
} locbuf;

#define FASTATTRIBUTES	64

typedef struct sgml_attribute_list
{ sgml_attribute *attributes;		/* The attributes */
  size_t	  count;		/* Number of attributes */
  size_t	  allocated;		/* #Allocated attributes */
  sgml_attribute  local[FASTATTRIBUTES];/* Handle fast up to here */
} sgml_attribute_list;


		 /*******************************
		 *	      PROTOYPES		*
		 *******************************/

static const ichar *	itake_name(dtd_parser *p,
				   const ichar *in, dtd_symbol **id);
static const ichar *	itake_entity_name(dtd_parser *p, const ichar *in,
					  dtd_symbol **id);
static const ichar *	itake_namegroup(dtd_parser *p, const ichar *decl,
					dtd_symbol **names, int *n);
static const ichar *	iskip_layout(dtd *dtd, const ichar *in);
static dtd_parser *	clone_dtd_parser(dtd_parser *p);
static void		free_model(dtd_model *m);
static int		process_entity_declaration(dtd_parser *p,
						    const ichar *decl);
static void		free_notations(dtd_notation *n);
static void		free_shortrefs(dtd_shortref *sr);
static int		process_cdata(dtd_parser *p, int last);
static int		process_entity(dtd_parser *p, const ichar *name);
static int		emit_cdata(dtd_parser *p, int last);
static dtd_space_mode	istr_to_space_mode(const ichar *val);
static void		update_space_mode(dtd_parser *p, dtd_element *e,
					  int natts, sgml_attribute *atts);
static dtd_model *	make_model(dtd_parser *p, const ichar *decl,
				   const ichar **end);
static void		for_elements_in_model(dtd_model *m,
					      void (*f)(dtd_element *e,
							void *closure),
					      void *closure);
int			putchar_dtd_parser(dtd_parser *p, int chr);
void			free_dtd_parser(dtd_parser *p);
static const ichar *	isee_character_entity(dtd *dtd, const ichar *in,
					      int *chr);
static int		add_default_attributes(dtd_parser *p, dtd_element *e,
					       sgml_attribute_list *atts);
static int		prepare_cdata(dtd_parser *p);
static void		init_attribute_list(sgml_attribute_list *atts);
static void		clear_attribute_list(sgml_attribute_list *atts);
static sgml_attribute * new_attribute(sgml_attribute_list *atts);


		 /*******************************
		 *	      MACROS		*
		 *******************************/

#define WITH_CLASS(p, c, g) \
	{ sgml_event_class _oc = p->event_class; \
	  p->event_class = c; \
	  g; \
	  p->event_class = _oc; \
	}

		 /*******************************
		 *	     STATISTICS		*
		 *******************************/

#ifdef O_STATISTICS

int edefs_created = 0;
int edefs_freed = 0;
int edefs_implicit = 0;
int edefs_atts = 0;
int edefs_decl = 0;
int dtd_created = 0;
int dtd_freed = 0;

void
sgml_statistics(void)
{ fprintf(stderr, "EDEFS: created %d; freed %d\n", edefs_created, edefs_freed);
  fprintf(stderr, "EDEFS: implicit %d; atts %d; decl %d\n",
	  edefs_implicit, edefs_atts, edefs_decl);
  fprintf(stderr, "DTDs: created: %d; freed: %d\n", dtd_created, dtd_freed);
}

#define STAT(g) g

#else

#define STAT(g) ((void)0)

#endif


		 /*******************************
		 *	   SRC LOCATION		*
		 *******************************/


static void				/* TBD: also handle startloc */
push_location(dtd_parser *p, locbuf *save)
{ save->here  = p->location;
  save->start = p->startloc;

  p->location.parent = &save->here;
  p->startloc.parent = &save->start;
}


static void
pop_location(dtd_parser *p, locbuf *saved)
{ p->location = saved->here;
  p->startloc = saved->start;
}


static inline void
_sgml_cplocation(dtd_srcloc *d, dtd_srcloc *loc)
{ d->type    = loc->type;
  d->name.file = loc->name.file;
  d->line    = loc->line;
  d->linepos = loc->linepos;
  d->charpos = loc->charpos;
					/* but not the parent! */
}

void
sgml_cplocation(dtd_srcloc *d, dtd_srcloc *loc)
{ _sgml_cplocation(d, loc);
}

#define sgml_cplocation(d,s) _sgml_cplocation(d, s)

static void
inc_location(dtd_srcloc *l, int chr)
{ if ( chr == '\n' )
  { l->linepos = 0;
    l->line++;
  } else if ( chr == '\t' )
  { l->linepos |= 7;
  }

  l->linepos++;
  l->charpos++;
}


static void
dec_location(dtd_srcloc *l, int chr)
{ if ( chr == '\n' )
  { l->linepos = 2;			/* not good! */
    l->line--;
  }
  l->linepos--;
  l->charpos--;
}

		 /*******************************
		 *   CLASSIFICATION PRIMITIVES	*
		 *******************************/

static inline int
HasClass(dtd *dtd, wint_t chr, int mask)
{ if ( chr <= 0xff )
    return (dtd->charclass->class[(chr)] & (mask));
  else
  { switch(mask)
    { case CH_NAME:
	return ( xml_basechar(chr) ||
		 xml_digit(chr) ||
		 xml_ideographic(chr) ||
		 xml_combining_char(chr) ||
		 xml_extender(chr)
	       );
      case CH_NMSTART:
	return ( xml_basechar(chr) ||
		 xml_ideographic(chr) );
      case CH_WHITE:
	return FALSE;			/* only ' ' and '\t' */
      case CH_BLANK:
	return iswspace(chr);
      case CH_DIGIT:
	return xml_digit(chr);
      case CH_RS:
      case CH_RE:
	return FALSE;
      default:
	assert(0);
        return FALSE;
    }
  }
}


static const ichar *
isee_func(dtd *dtd, const ichar *in, charfunc func)
{ if ( dtd->charfunc->func[func] == *in )
    return ++in;

  return NULL;
}

		 /*******************************
		 *	      SYMBOLS		*
		 *******************************/

static dtd_symbol_table *
new_symbol_table()
{ dtd_symbol_table *t = sgml_calloc(1, sizeof(*t));
  t->size    = SYMBOLHASHSIZE;
  t->entries = sgml_calloc(t->size, sizeof(dtd_symbol*));

  return t;
}


static void
free_symbol_table(dtd_symbol_table *t)
{ int i;

  for(i=0; i<t->size; i++)
  { dtd_symbol *s, *next;

    for(s=t->entries[i]; s; s=next)
    { next = s->next;

      sgml_free((ichar*)s->name);
      sgml_free(s);
    }
  }

  sgml_free(t->entries);
  sgml_free(t);
}


dtd_symbol *
dtd_find_symbol(dtd *dtd, const ichar *name)
{ dtd_symbol_table *t = dtd->symbols;

  if ( dtd->case_sensitive )
  { int k = istrhash(name, t->size);
    dtd_symbol *s;

    for(s=t->entries[k]; s; s = s->next)
    { if ( istreq(s->name, name) )
	return s;
    }
  } else
  { int k = istrcasehash(name, t->size);
    dtd_symbol *s;

    for(s=t->entries[k]; s; s = s->next)
    { if ( istrcaseeq(s->name, name) )
	return s;
    }
  }

  return NULL;
}


static dtd_symbol *
dtd_find_entity_symbol(dtd *dtd, const ichar *name)
{ dtd_symbol_table *t = dtd->symbols;

  if ( dtd->ent_case_sensitive )
  { int k = istrhash(name, t->size);
    dtd_symbol *s;

    for(s=t->entries[k]; s; s = s->next)
    { if ( istreq(s->name, name) )
	return s;
    }
  } else
  { int k = istrcasehash(name, t->size);
    dtd_symbol *s;

    for(s=t->entries[k]; s; s = s->next)
    { if ( istrcaseeq(s->name, name) )
	return s;
    }
  }

  return NULL;
}


dtd_symbol *
dtd_add_symbol(dtd *dtd, const ichar *name)
{ dtd_symbol_table *t = dtd->symbols;
  int k = istrhash(name, t->size);
  dtd_symbol *s;

  for(s=t->entries[k]; s; s = s->next)
  { if ( istreq(s->name, name) )
      return s;
  }

  s = sgml_calloc(1, sizeof(*s));
  s->name = istrdup(name);
  s->next = t->entries[k];
  t->entries[k] = s;

  return s;
}


		 /*******************************
		 *	    ENTITIES		*
		 *******************************/

static void
free_entity_list(dtd_entity *e)
{ dtd_entity *next;

  for( ; e; e=next)
  { next = e->next;

    if ( e->value )   sgml_free(e->value);
    if ( e->extid )   sgml_free(e->extid);
    if ( e->exturl )  sgml_free(e->exturl);
    if ( e->baseurl ) sgml_free(e->baseurl);

    sgml_free(e);
  }
}


static dtd_entity *
find_pentity(dtd *dtd, dtd_symbol *id)
{ dtd_entity *e;

  for(e = dtd->pentities; e; e=e->next)
  { if ( e->name == id )
      return e;
  }

  return NULL;
}


/* returned path must be freed when done */

static ichar *
entity_file(dtd *dtd, dtd_entity *e)
{ switch(e->type)
  { case ET_SYSTEM:
    case ET_PUBLIC:
    { const ichar *f;

      f = find_in_catalogue(e->catalog_location,
			    e->name->name,
			    e->extid,
			    e->exturl,
			    IS_XML_DIALECT(dtd->dialect));

      if ( f )				/* owned by catalog */
      { ichar *file;

	if ( is_absolute_path(f) || is_url(f) || !e->baseurl )
	  file = istrdup(f);
	else
	  file = localpath(e->baseurl, f);

	return file;
      }
    }
    default:
      return NULL;
  }
}


static const ichar *
entity_value(dtd_parser *p, dtd_entity *e, int *len)
{ ichar *file;

  if ( !e->value && (file=entity_file(p->dtd, e)) )
  { int normalise = (e->content == EC_SGML || e->content == EC_CDATA);
    size_t l;

    e->value = load_sgml_file_to_charp(file, normalise, &l);
    e->length = (long)l;
    sgml_free(file);
  }

  if ( len )
    *len = e->length;

  return e->value;
}


static int
expand_pentities(dtd_parser *p, const ichar *in, int ilen, ichar *out, int len)
{ dtd *dtd = p->dtd;
  int pero = dtd->charfunc->func[CF_PERO]; /* % */
  int ero = dtd->charfunc->func[CF_ERO]; /* & */
  const ichar *s;
  const ichar *end;

  if ( ilen == ZERO_TERM_LEN )
  { end = in + wcslen(in);
  } else
  { end = &in[ilen];
  }

  while(in < end)
  { if ( *in == pero )
    { dtd_symbol *id;

      if ( (s = itake_entity_name(p, in+1, &id)) )
      { dtd_entity *e = find_pentity(dtd, id);
	const ichar *eval;
	int l;

	in = s;
	if ( (s=isee_func(dtd, s, CF_ERC)) ) /* ; is not obligatory? */
	  in = s;

	if ( !e )
	  return gripe(p, ERC_EXISTENCE, L"parameter entity", id->name);

	if ( !(eval = entity_value(p, e, NULL)) )
	  return FALSE;

	if ( !expand_pentities(p, eval, ZERO_TERM_LEN, out, len) )
	  return FALSE;
	l = (int)istrlen(out);		/* could be better */
	out += l;
	len -= l;

	continue;
      }
    }

    if ( --len <= 0 )
    { gripe(p, ERC_REPRESENTATION, L"Declaration too long");
      return FALSE;
    }

    if ( *in == ero && in[1] == '#' )	/* &# */
    { int chr;

      if ( (s=isee_character_entity(dtd, in, &chr)) )
      { if ( chr == 0 )
	{ gripe(p, ERC_SYNTAX_ERROR, L"Illegal character entity", in);
	} else
	{ *out++ = chr;
	  in = s;
	  continue;
	}
      }
    }

    *out++ = *in++;
  }

  *out = '\0';

  return TRUE;
}


static int
char_entity_value(const ichar *decl)
{ if ( *decl == '#' )
  { const ichar *s = decl+1;
    ichar *end;
    long v;

					/* do octal too? */
    if ( s[0] == 'x' || s[0] == 'X' )
      v = wcstoul(s+1, &end, 16);
    else
      v = wcstoul(s, &end, 10);

    if ( *end == '\0' )
    { return (int)v;
    } else if ( istreq(s, L"RS") )
    { return '\n';
    } else if ( istreq(s, L"RE") )
    { return '\r';
    } else if ( istreq(s, L"TAB") )
    { return '\t';
    } else if ( istreq(s, L"SPACE") )
    { return ' ';
    }
  }

  return -1;
}


static const ichar *
isee_character_entity(dtd *dtd, const ichar *in, int *chr)
{ const ichar *s;

  if ( (s=isee_func(dtd, in, CF_ERO)) && *s == '#' )
  { ichar e[32];
    ichar *o = e;
    int v;

    *o++ = *s++;
    while(o < e+sizeof(e)/sizeof(ichar)-1 && HasClass(dtd, *s, CH_NAME))
      *o++ = *s++;
    if ( isee_func(dtd, s, CF_ERC))	/* skip ; */
      s++;

    *o = '\0';
    if ( (v=char_entity_value(e)) >= 0 )
    { *chr = v;
      return s;
    }
  }

  return NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Expand entities in a string.  Used to expand CDATA attribute values.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
expand_entities(dtd_parser *p, const ichar *in, int len, ocharbuf *out)
{ const ichar *s;
  const ichar *end = &in[len];
  dtd *dtd = p->dtd;
  int ero = dtd->charfunc->func[CF_ERO]; /* & */

  while(in < end)
  { if ( *in == ero )
    { const ichar *estart = in;		/* for recovery */
      int chr;

      if ( (s=isee_character_entity(dtd, in, &chr)) )
      { if ( chr == 0 )
	  gripe(p, ERC_SYNTAX_ERROR, L"Illegal character entity", in);

	add_ocharbuf(out, chr);
	in = s;
	continue;
      }

      if ( HasClass(dtd, in[1], CH_NMSTART) )
      { dtd_symbol *id;
	dtd_entity *e;
	const ichar *eval;

	if ( !(in = itake_name(p, in+1, &id)) )
	{ in = estart;
	  goto recover;
	}
	if ( isee_func(dtd, in, CF_ERC) || *in == '\n' )
	  in++;

	if ( !(e = id->entity) && !(e=dtd->default_entity) )
	{ gripe(p, ERC_EXISTENCE, L"entity", id->name);
	  in = estart;
	  goto recover;
	}

	if ( !(eval = entity_value(p, e, NULL)) )
	{ gripe(p, ERC_NO_VALUE, e->name->name);
	  in = estart;
	  goto recover;
	}

	if ( e->content == EC_SGML )
	{ if ( !expand_entities(p, eval, (int)istrlen(eval), out) )
	    return FALSE;
	} else
	{ const ichar *s;

	  for(s=eval; *s; s++)
	    add_ocharbuf(out, *s);
	}

	continue;
      }

      if ( IS_XML_DIALECT(dtd->dialect) )
	gripe(p, ERC_SYNTAX_ERROR, L"Illegal entity", estart);
    }

  recover:

    if ( *in == CR && in[1] == LF )
      in++;

    if ( HasClass(dtd, *in, CH_BLANK) )
    { add_ocharbuf(out, ' ');
      in++;
    } else
    { add_ocharbuf(out, *in++);
    }
  }

  terminate_ocharbuf(out);

  return TRUE;
}



		 /*******************************
		 *	      ELEMENTS		*
		 *******************************/

static dtd_element *
find_element(dtd *dtd, dtd_symbol *id)
{ dtd_element *e;

  if ( id->element )
    return id->element;			/* must check */

  e = sgml_calloc(1, sizeof(*e));
  e->space_mode = SP_INHERIT;
  e->undefined = TRUE;
  e->name = id;
  id->element = e;

  e->next = dtd->elements;
  dtd->elements = e;

  return e;
}


static dtd_edef *
new_element_definition(dtd *dtd)
{ dtd_edef *def = sgml_calloc(1, sizeof(*def));

  STAT(edefs_created++);

  return def;
}


static dtd_element *
def_element(dtd *dtd, dtd_symbol *id)
{ dtd_element *e = find_element(dtd, id);

  if ( !e->structure )
  { e->structure = new_element_definition(dtd);
    e->structure->references = 1;
    e->structure->type = C_EMPTY;
  }

  return e;
}


static void
free_name_list(dtd_name_list *nl)
{ dtd_name_list *next;

  for( ; nl; nl=next)
  { next = nl->next;

    sgml_free(nl);
  }
}


#define REFS_VIRGIN (-42)

static void
free_attribute(dtd_attr *a)
{ if ( a->references == REFS_VIRGIN || --a->references == 0 )
  { switch(a->type)
    { case AT_NAMEOF:
      case AT_NOTATION:
	free_name_list(a->typeex.nameof);
      default:
	;
    }
    switch(a->def)
    { case AT_DEFAULT:
      case AT_FIXED:
      { if ( a->islist )
	  sgml_free(a->att_def.list);
	else if ( a->type == AT_CDATA && a->att_def.cdata )
	  sgml_free(a->att_def.cdata);
      }
      default:
	;
    }

    sgml_free(a);
  }
}


static void
free_attribute_list(dtd_attr_list *l)
{ dtd_attr_list *next;

  for(; l; l=next)
  { next = l->next;

    free_attribute(l->attribute);
    sgml_free(l);
  }
}


static void
free_element_list(dtd_element_list *l)
{ dtd_element_list *next;

  for( ; l; l=next)
  { next = l->next;

    sgml_free(l);
  }
}

static void
free_element_definition(dtd_edef *def)
{ if ( --def->references == 0 )
  { STAT(edefs_freed++);
    if ( def->content )
      free_model(def->content);
    free_element_list(def->included);
    free_element_list(def->excluded);
    free_state_engine(def->initial_state);

    sgml_free(def);
  }
}


static void
free_elements(dtd_element *e)
{ dtd_element *next;

  for( ; e; e=next)
  { next = e->next;

    if ( e->structure )
      free_element_definition(e->structure);
    free_attribute_list(e->attributes);

    sgml_free(e);
  }
}


		 /*******************************
		 *	    ATTRIBUTES		*
		 *******************************/

static dtd_attr *
find_attribute(dtd_element *e, dtd_symbol *name)
{ dtd_attr_list *a;

  for(a=e->attributes; a; a=a->next)
  { if ( a->attribute->name == name )
      return a->attribute;
  }

  return NULL;
}


		 /*******************************
		 *	  PARSE PRIMITIVES	*
		 *******************************/

static const ichar *
iskip_layout(dtd *dtd, const ichar *in)
{ ichar cmt = dtd->charfunc->func[CF_CMT]; /* also skips comment */

  for( ; *in; in++ )
  { if ( HasClass(dtd, *in, CH_BLANK) )
      continue;

    if ( in[0] == cmt && in[1] == cmt )
    { in += 2;

      for( ; *in; in++ )
      { if ( in[0] == cmt && in[1] == cmt )
	  break;
      }
      in++;
      continue;
    }

    return in;
  }

  return in;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
See whether we are looking at identifier   "id". "id" must be lowercase!
This is only used for reserved words,  and parsed case-insentive in both
XML and SGML modes.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static const ichar *
isee_identifier(dtd *dtd, const ichar *in, char *id)
{ in = iskip_layout(dtd, in);

					/* match */
  while (*id && (wint_t)*id == towlower(*in) )
    id++, in++;
  if ( *id == 0 && !HasClass(dtd, *in, CH_NAME) )
    return iskip_layout(dtd, in);

  return NULL;
}


static const ichar *
itake_name(dtd_parser *p, const ichar *in, dtd_symbol **id)
{ ichar buf[MAXNMLEN];
  ichar *o = buf;
  ichar *e = &buf[MAXNMLEN]-1;
  dtd *dtd = p->dtd;

  in = iskip_layout(dtd, in);
  if ( !HasClass(dtd, *in, CH_NMSTART) )
    return NULL;

  if ( dtd->case_sensitive )
  { while( HasClass(dtd, *in, CH_NAME) && o < e )
      *o++ = *in++;
  } else
  { while( HasClass(dtd, *in, CH_NAME) && o < e )
      *o++ = towlower(*in++);
  }

  if ( o == e )
  { gripe(p, ERC_REPRESENTATION, L"NAME too long");
    return NULL;
  }

  *o++ = '\0';

  *id = dtd_add_symbol(dtd, buf);

  return iskip_layout(dtd, in);
}


static const ichar *
itake_entity_name(dtd_parser *p, const ichar *in, dtd_symbol **id)
{ ichar buf[MAXNMLEN];
  ichar *o = buf;
  ichar *e = &buf[MAXNMLEN]-1;
  dtd *dtd = p->dtd;

  in = iskip_layout(dtd, in);
  if ( !HasClass(dtd, *in, CH_NMSTART) )
    return NULL;

  if ( dtd->ent_case_sensitive )
  { while( HasClass(dtd, *in, CH_NAME) && o < e )
      *o++ = *in++;
  } else
  { while( HasClass(dtd, *in, CH_NAME) && o < e )
      *o++ = towlower(*in++);
  }
  if ( o == e )
  { gripe(p, ERC_REPRESENTATION, L"Entity NAME too long");
    return NULL;
  }

  *o++ = '\0';

  *id = dtd_add_symbol(dtd, buf);

  return in;
}


static const ichar *
itake_nmtoken(dtd_parser *p, const ichar *in, dtd_symbol **id)
{ ichar buf[MAXNMLEN];
  ichar *o = buf;
  ichar *e = &buf[MAXNMLEN]-1;
  dtd *dtd = p->dtd;

  in = iskip_layout(dtd, in);
  if ( !HasClass(dtd, *in, CH_NAME) )
    return NULL;
  if ( dtd->case_sensitive )
  { while( HasClass(dtd, *in, CH_NAME) && o < e )
      *o++ = *in++;
  } else
  { while( HasClass(dtd, *in, CH_NAME) && o < e )
      *o++ = towlower(*in++);
  }
  if ( o == e )
  { gripe(p, ERC_REPRESENTATION, L"NMTOKEN too long");
    return NULL;
  }

  *o = '\0';

  *id = dtd_add_symbol(dtd, buf);

  return iskip_layout(dtd, in);
}


static const ichar *
itake_nutoken(dtd_parser *p, const ichar *in, dtd_symbol **id)
{ ichar buf[MAXNMLEN];
  ichar *o = buf;
  ichar *e = &buf[MAXNMLEN]-1;
  dtd *dtd = p->dtd;

  in = iskip_layout(dtd, in);
  if ( !HasClass(dtd, *in, CH_DIGIT) )
    return NULL;

  if ( dtd->case_sensitive )
  { while( HasClass(dtd, *in, CH_NAME) && o < e )
      *o++ = *in++;
  } else
  { while( HasClass(dtd, *in, CH_NAME) && o < e )
      *o++ = towlower(*in++);
  }

  if ( o == e )
  { gripe(p, ERC_REPRESENTATION, L"NUTOKEN too long");
    return NULL;
  }

  *o = '\0';
  if ( o - buf > 8 )
    gripe(p, ERC_LIMIT, L"nutoken length");

  *id = dtd_add_symbol(dtd, buf);

  return iskip_layout(dtd, in);
}


static const ichar *
itake_number(dtd_parser *p, const ichar *in, dtd_attr *at)
{ dtd *dtd = p->dtd;

  in = iskip_layout(dtd, in);

  switch(dtd->number_mode)
  { case NU_TOKEN:
    { ichar buf[MAXNMLEN];
      ichar *o = buf;

      while( HasClass(dtd, *in, CH_DIGIT) )
	*o++ = *in++;
      if ( o == buf )
	return NULL;			/* empty */
      *o = '\0';
      at->att_def.name = dtd_add_symbol(dtd, buf);

      return iskip_layout(dtd, (const ichar *)in);
    }
    case NU_INTEGER:
    { ichar *end;

      at->att_def.number = wcstol(in, &end, 10);
      if ( end > in && errno != ERANGE )
	return iskip_layout(dtd, end);
    }
  }

  return NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get a quoted value. After successful return,  *start points to the start
of the string in the input and  *len   to  the length. The data is *not*
nul terminated.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static const ichar *
itake_string(dtd *dtd, const ichar *in, ichar **start, int *len)
{ in = iskip_layout(dtd, in);

  if ( isee_func(dtd, in, CF_LIT) ||
       isee_func(dtd, in, CF_LITA) )
  { ichar q = *in++;

    *start = (ichar *)in;
    while( *in && *in != q )
      in++;
    if ( *in )
    { *len = (int)(in - (*start));

      return iskip_layout(dtd, ++in);
    }
  }

  return NULL;
}


static const ichar *
itake_dubbed_string(dtd *dtd, const ichar *in, ichar **out)
{ ichar *start;
  int len;
  const ichar *end;

  if ( (end=itake_string(dtd, in, &start, &len)) )
    *out = istrndup(start, len);

  return end;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
itake_url() is used to get the argument of a SYSTEM or 2nd argument of a
PUBLIC reference. Once upon a  time  it   tried  to  tag the argument as
file:<path>, but this job cannot be before   lookup in the catalogue. It
is now the same as itake_dubbed_string(), so we simply call this one.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static const ichar *
itake_url(dtd *dtd, const ichar *in, ichar **out)
{ return itake_dubbed_string(dtd, in, out);
}


static const ichar *
itake_nmtoken_chars(dtd_parser *p, const ichar *in, ichar *out, int len)
{ dtd *dtd = p->dtd;

  in = iskip_layout(dtd, in);
  if ( !HasClass(dtd, *in, CH_NAME) )
    return NULL;
  while( HasClass(dtd, *in, CH_NAME) )
  { if ( --len <= 0 )
      gripe(p, ERC_REPRESENTATION, L"Name token too long");
    *out++ = (dtd->case_sensitive ? *in++ : (ichar)towlower(*in++));
  }
  *out++ = '\0';

  return iskip_layout(dtd, in);
}


/*  There used to be a function

    itake_nonblank_chars(dtd, in, out, len) -> new end

    which
    - skipped layout,
    - copied characters from in[] to out[] until layout or \0 was found,
    - added a terminating \0 to out[],
    - skipped any following layout, and
    - returned the new position.

    That function was only called by get_attribute_value(), which used
    it to parse an unquoted attribute value.  According to SGML, that's
    not right:  unquoted attribute values must look like NMTOKENs (but
    have a different length bound).  In particular, elements like
	<foo a=bar>zoo</foo>
	<foo a=ugh/zip/
    are perfectly legal, so scanning an unquoted attribute value MUST
    stop at a '/' or '>'.  According to HTML practice, pretty much any
    old junk will be accepted, and some HTML parsers will allow bare
    slashes in such an attribute.

    Typical HTML is *so* bad that it doesn't agree with *any* part of
    the HTML specifications (e.g., <FONT> is commonly wrapped around
    block-level elements, which has never been legal).  It's not clear
    that there is much point in trying to accomodate bad HTML; if you
    really need to do that, use the free program HTML Tidy (from the
    http://www.w3c.org/ site) to clean up, and parse its output instead.

    However, in order to break as little as possible, the new (sgml-1.0.14)
    function accepts anything except > / \0 and blanks.

JW: I decided to accept / as part of an unquoted in SGML-mode if
    shorttag is disabled as well as in XML mode if it is not the
    end of the begin-element
*/

static ichar const *
itake_unquoted(dtd_parser *p, ichar const *in, ichar *out, int len)
{ dtd *dtd = p->dtd;
  ichar const end2 = dtd->charfunc->func[CF_ETAGO2];	/* / */
  ichar c;

  /* skip leading layout.  Do NOT skip comments! --x-- is a value! */
  while (c = *in, HasClass(dtd, c, CH_BLANK))
    in++;

  /* copy the attribute to out[] */
  while ( !HasClass(dtd, c, CH_BLANK) &&
	  c != '\0' )
  { if ( c == end2 && (dtd->shorttag ||
		       (in[1] == '\0' && IS_XML_DIALECT(dtd->dialect))) )
      break;

    if ( --len > 0 )
      *out++ = c;
    else if ( len == 0 )
      gripe(p, ERC_REPRESENTATION, L"Attribute too long");
    c = *++in;
  }
  *out = '\0';

  /* skip trailing layout.  While it is kind to skip comments here,
     it is technically wrong to do so.  Tags may not contain comments.
   */

  return iskip_layout(dtd, in);
}


		 /*******************************
		 *		DTD		*
		 *******************************/

dtd *
new_dtd(const ichar *doctype)
{ dtd *dtd = sgml_calloc(1, sizeof(*dtd));

  STAT(dtd_created++);
  dtd->magic	 = SGML_DTD_MAGIC;
  dtd->implicit  = TRUE;
  dtd->dialect   = DL_SGML;
  if ( doctype )
    dtd->doctype = istrdup(doctype);
  dtd->symbols	 = new_symbol_table();
  dtd->charclass = new_charclass();
  dtd->charfunc	 = new_charfunc();
  dtd->space_mode = SP_SGML;
  dtd->ent_case_sensitive = TRUE;	/* case-sensitive entities */
  dtd->shorttag    = TRUE;		/* allow for <tag/value/ */
  dtd->system_entities = FALSE;		/* expand SYSTEM entities */
  dtd->number_mode = NU_TOKEN;

  return dtd;
}


void
free_dtd(dtd *dtd)
{ if ( --dtd->references == 0 )
  { STAT(dtd_freed++);

    if ( dtd->doctype )
      sgml_free(dtd->doctype);

    free_entity_list(dtd->entities);
    free_entity_list(dtd->pentities);
    free_notations(dtd->notations);
    free_shortrefs(dtd->shortrefs);
    free_elements(dtd->elements);
    free_symbol_table(dtd->symbols);
    sgml_free(dtd->charfunc);
    sgml_free(dtd->charclass);
    dtd->magic = 0;

    sgml_free(dtd);
  }
}


static const wchar_t *xml_entities[] =
{ L"lt CDATA \"&#60;\"",		/* < */
  L"gt CDATA \"&#62;\"",		/* > */
  L"amp CDATA \"&#38;\"",		/* & */
  L"apos CDATA \"&#39;\"",		/* ' */
  L"quot CDATA \"&#34;\"",		/* " */
  NULL
};


int
set_dialect_dtd(dtd *dtd, dtd_dialect dialect)
{ if ( dtd->dialect != dialect )
  { dtd->dialect = dialect;

    switch(dialect)
    { case DL_HTML5:
	dtd->encoding = SGML_ENC_UTF8;
      case DL_SGML:
      case DL_HTML:
	dtd->case_sensitive = FALSE;
	dtd->att_case_sensitive = FALSE;
	dtd->space_mode = SP_SGML;
	dtd->shorttag = (dialect == DL_SGML);
	break;
      case DL_XHTML5:
      case DL_XHTML:
      case DL_XML:
      case DL_XMLNS:
      { const ichar **el;
	dtd_parser p;

	dtd->case_sensitive = TRUE;
	dtd->att_case_sensitive = TRUE;
	dtd->encoding = SGML_ENC_UTF8;
	dtd->space_mode = SP_PRESERVE;
	dtd->shorttag = FALSE;

	memset(&p, 0, sizeof(p));
	p.dtd = dtd;
	for(el = xml_entities; *el; el++)
	  process_entity_declaration(&p, *el);

	break;
      }
    }
  }

  return TRUE;
}


int
set_option_dtd(dtd *dtd, dtd_option option, int set)
{ switch(option)
  { case OPT_SHORTTAG:
      dtd->shorttag = set;
      break;
    case OPT_CASE_SENSITIVE_ATTRIBUTES:
      dtd->att_case_sensitive = set;
      break;
    case OPT_CASE_PRESERVING_ATTRIBUTES:
      dtd->att_case_preserving = set;
      dtd->att_case_sensitive = set;
      break;
    case OPT_SYSTEM_ENTITIES:
      dtd->system_entities = set;
      break;
    case OPT_KEEP_PREFIX:
      dtd->keep_prefix = set;
      break;
  }

  return TRUE;
}


static const ichar *
baseurl(dtd_parser *p)
{ if ( p->location.type == IN_FILE && p->location.name.file )
  { return p->location.name.file;
  }

  return NULL;
}


static const ichar *
process_entity_value_declaration(dtd_parser *p,
				 const ichar *decl, dtd_entity *e)
{ dtd *dtd = p->dtd;
  const ichar *s;

  if ( e->type == ET_SYSTEM )
  { if ( (s=itake_url(dtd, decl, &e->exturl)) )
    { e->baseurl = istrdup(baseurl(p));
      return s;
    }

    goto string_expected;
  } else
  { ichar *start; int len;
    ichar val[MAXSTRINGLEN];

    if ( !(s = itake_string(dtd, decl, &start, &len)) )
      goto string_expected;
    decl = s;

    expand_pentities(p, start, len, val, sizeof(val)/sizeof(ichar));

    switch ( e->type )
    { case ET_PUBLIC:
      { e->extid = istrdup(val);
	if ( isee_func(dtd, decl, CF_LIT) ||
	     isee_func(dtd, decl, CF_LITA) )
	{ if ( (s=itake_url(dtd, decl, &e->exturl)) )
	  { e->baseurl = istrdup(baseurl(p));
	    decl = s;
	  }
	}
	return decl;
      }
      case ET_LITERAL:
      { e->value = istrdup(val);
	e->length = (int)wcslen(e->value);
	return decl;
      }
      default:
	assert(0);
	return NULL;
    }
  }

string_expected:
  gripe(p, ERC_SYNTAX_ERROR, L"String expected", decl);
  return NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The sgml-standard tells us to accept the  first definition of an entity,
silently suppressing any further attempt to redefine the entity.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
process_entity_declaration(dtd_parser *p, const ichar *decl)
{ dtd *dtd = p->dtd;
  const ichar *s;
  dtd_symbol *id;
  dtd_entity *e;
  int isparam;
  int isdef = FALSE;
					/* parameter entity */
  if ( (s=isee_func(dtd, decl, CF_PERO)) )
  { isparam = TRUE;
    decl = s;
  } else
    isparam = FALSE;

  if ( !(s = itake_entity_name(p, decl, &id)) )
  { if ( !(s = isee_identifier(dtd, decl, "#default")) )
      return gripe(p, ERC_SYNTAX_ERROR, L"Name expected", decl);
    id = dtd_add_symbol(dtd, (ichar*)"#DEFAULT");
    isdef = TRUE;
  }

  if ( isparam && find_pentity(dtd, id) )
  { gripe(p, ERC_REDEFINED, L"parameter entity", id);
    return TRUE;			/* already defined parameter entity */
  }
  if ( id->entity )
  { gripe(p, ERC_REDEFINED, L"entity", id);
    return TRUE;			/* already defined normal entity */
  }

  decl = iskip_layout(dtd, s);
  e = sgml_calloc(1, sizeof(*e));
  e->name = id;
  e->catalog_location = (isparam ? CAT_PENTITY : CAT_ENTITY);

  if ( (s = isee_identifier(dtd, decl, "system")) )
  { e->type = ET_SYSTEM;
    e->content = EC_SGML;
    decl = s;
  } else if ( (s = isee_identifier(dtd, decl, "public")) )
  { e->type = ET_PUBLIC;
    e->content = EC_SGML;
    decl = s;
  } else
  { e->type = ET_LITERAL;

    if ( !isparam )
    { if ( (s=isee_identifier(dtd, decl, "cdata")) )
      { decl = s;
	e->content = EC_CDATA;
      } else if ( (s=isee_identifier(dtd, decl, "sdata")) )
      { decl = s;
	e->content = EC_SDATA;
      } else if ( (s=isee_identifier(dtd, decl, "pi")) )
      { decl = s;
	e->content = EC_PI;
      } else if ( (s=isee_identifier(dtd, decl, "starttag")) )
      { decl = s;
	e->content = EC_STARTTAG;
      } else if ( (s=isee_identifier(dtd, decl, "endtag")) )
      { decl = s;
	e->content = EC_ENDTAG;
      } else
	e->content = EC_SGML;
    }
  }

  if ( (decl=process_entity_value_declaration(p, decl, e)) )
  { if ( e->type == ET_LITERAL )
    { switch(e->content)
      { case EC_STARTTAG:
	{ ichar *buf = sgml_malloc((e->length + 3)*sizeof(ichar));

	  buf[0] = dtd->charfunc->func[CF_STAGO];
	  istrcpy(&buf[1], e->value);
	  buf[++e->length] = dtd->charfunc->func[CF_STAGC];
	  buf[++e->length] = 0;

	  sgml_free(e->value);
	  e->value = buf;
	  e->content = EC_SGML;

	  break;
	}
	case EC_ENDTAG:
	{ ichar *buf = sgml_malloc((e->length + 4)*sizeof(ichar));

	  buf[0] = dtd->charfunc->func[CF_ETAGO1];
	  buf[1] = dtd->charfunc->func[CF_ETAGO2];
	  istrcpy(&buf[2], e->value);
	  e->length++;
	  buf[++e->length] = dtd->charfunc->func[CF_STAGC];
	  buf[++e->length] = 0;

	  sgml_free(e->value);
	  e->value = buf;
	  e->content = EC_SGML;

	  break;
	}
	default:
	  break;
      }
    } else
    { if ( *decl )
      { dtd_symbol *nname;

	if ( (s=isee_identifier(dtd, decl, "cdata")) )
	{ decl = s;
	  e->content = EC_CDATA;
	} else if ( (s=isee_identifier(dtd, decl, "sdata")) )
	{ decl = s;
	  e->content = EC_SDATA;
	} else if ( (s=isee_identifier(dtd, decl, "ndata")) )
	{ decl = s;
	  e->content = EC_NDATA;
	} else
	  return gripe(p, ERC_SYNTAX_ERROR, L"Bad datatype declaration", decl);

	if ( (s=itake_name(p, decl, &nname)) ) /* what is this? */
	{ decl = s;
	} else
	  return gripe(p, ERC_SYNTAX_ERROR, L"Bad notation declaration", decl);
      }
    }

    if ( *decl )
      return gripe(p, ERC_SYNTAX_ERROR, L"Unexpected end of declaraction", decl);
  }

  if ( isparam )
  { e->next = dtd->pentities;
    dtd->pentities = e;
  } else
  { e->name->entity = e;
    e->next = dtd->entities;
    dtd->entities = e;
  }

  if ( isdef )
    dtd->default_entity = e;

  return TRUE;
}


		 /*******************************
		 *	      NOTATIONS		*
		 *******************************/

static dtd_notation *
find_notation(dtd *dtd, dtd_symbol *name)
{ dtd_notation *n;

  for(n=dtd->notations; n; n = n->next)
  { if ( n->name == name )
      return n;
  }

  return NULL;
}


static void
add_notation(dtd *dtd, dtd_notation *not)
{ dtd_notation **n = &dtd->notations;

  for( ; *n; n = &(*n)->next)
    ;
  *n = not;
}

static int
process_notation_declaration(dtd_parser *p, const ichar *decl)
{ dtd *dtd = p->dtd;
  dtd_symbol *nname;
  const ichar *s;
  ichar *system = NULL, *public = NULL;
  dtd_notation *not;

  if ( !(s=itake_name(p, decl, &nname)) )
    return gripe(p, ERC_SYNTAX_ERROR, L"Notation name expected", decl);
  decl = s;

  if ( find_notation(dtd, nname) )
  { gripe(p, ERC_REDEFINED, L"notation", nname);
    return TRUE;
  }

  if ( (s=isee_identifier(dtd, decl, "system")) )
  { ;
  } else if ( (s=isee_identifier(dtd, decl, "public")) )
  { decl = s;
    if ( !(s=itake_dubbed_string(dtd, decl, &public)) )
      return gripe(p, ERC_SYNTAX_ERROR, L"Public identifier expected", decl);
  } else
    return gripe(p, ERC_SYNTAX_ERROR, L"SYSTEM or PUBLIC expected", decl);

  decl = s;
  if ( (s=itake_dubbed_string(dtd, decl, &system)) )
    decl = s;

  if ( *decl )
    return gripe(p, ERC_SYNTAX_ERROR, L"Unexpected end of declaraction", decl);

  not = sgml_calloc(1, sizeof(*not));
  not->name = nname;
  not->system = system;
  not->public = public;
  not->next = NULL;
  add_notation(dtd, not);

  return TRUE;
}


static void
free_notations(dtd_notation *n)
{ dtd_notation *next;

  for( ; n; n=next)
  { next = n->next;

    sgml_free(n->system);
    sgml_free(n->public);

    sgml_free(n);
  }
}

		 /*******************************
		 *	       SHORTREF		*
		 *******************************/

static void
free_maps(dtd_map *map)
{ dtd_map *next;

  for( ; map; map=next)
  { next = map->next;
    if ( map->from )
      sgml_free(map->from);
    sgml_free(map);
  }
}


static void
free_shortrefs(dtd_shortref *sr)
{ dtd_shortref *next;

  for( ; sr; sr=next)
  { next = sr->next;
    free_maps(sr->map);
    sgml_free(sr);
  }
}


static const ichar *
shortref_add_map(dtd_parser *p, const ichar *decl, dtd_shortref *sr)
{ ichar *start; int len;
  ichar from[MAXMAPLEN];
  ichar *f = from;
  dtd_symbol *to;
  const ichar *s;
  const ichar *end;
  dtd *dtd = p->dtd;
  dtd_map **prev;
  dtd_map *m;

  if ( !(s=itake_string(dtd, decl, &start, &len)) )
  { gripe(p, ERC_SYNTAX_ERROR, L"map-string expected", decl);
    return NULL;
  }
  decl = s;
  if ( !(s=itake_entity_name(p, decl, &to)) )
  { gripe(p, ERC_SYNTAX_ERROR, L"map-to name expected", decl);
    return NULL;
  }
  end = s;

  for(decl=start; len > 0;)
  { if ( *decl == 'B' )		/* blank */
    { if ( decl[1] == 'B' )
      { *f++ = CHR_DBLANK;
	decl += 2;
	len -= 2;
        continue;
      }
      *f++ = CHR_BLANK;
      decl++;
      len--;
    } else
    { *f++ = *decl++;			/* any other character */
      len--;
    }
  }
  *f = 0;

  for(prev=&sr->map; *prev; prev = &(*prev)->next)
    ;

  m = sgml_calloc(1, sizeof(*m));
  m->from = istrdup(from);
  m->len  = (int)istrlen(from);
  m->to   = to;

  *prev = m;

  return end;
}


static dtd_shortref *
def_shortref(dtd_parser *p, dtd_symbol *name)
{ dtd *dtd = p->dtd;
  dtd_shortref *sr, **pr;

  for(pr=&dtd->shortrefs; *pr; pr = &(*pr)->next)
  { dtd_shortref *r = *pr;

    if ( r->name == name )
      return r;
  }

  sr = sgml_calloc(1, sizeof(*sr));
  sr->name = name;
  *pr = sr;

  return sr;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Create an array with TRUE in any character   that can be the last of the
shortref map.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
compile_map(dtd *dtd, dtd_shortref *sr)
{ dtd_map *map;

  for(map = sr->map; map; map = map->next)
  { ichar last = map->from[map->len-1];

    switch( last )
    { case CHR_BLANK:
      case CHR_DBLANK:
      { wint_t i;

	for( i=0; i< SHORTMAP_SIZE; i++)
	{ if ( HasClass(dtd, i, CH_BLANK) )
	    sr->ends[i] = TRUE;
	}
      }

      default:
	sr->ends[last] = TRUE;
    }
  }
}


static int
process_shortref_declaration(dtd_parser *p, const ichar *decl)
{ dtd *dtd = p->dtd;
  ichar buf[MAXDECL];
  dtd_shortref *sr;
  dtd_symbol *name;
  const ichar *s;

  if ( !expand_pentities(p, decl, ZERO_TERM_LEN, buf, sizeof(buf)/sizeof(ichar)) )
    return FALSE;
  decl = buf;

  if ( !(s=itake_name(p, decl, &name)) )
    return gripe(p, ERC_SYNTAX_ERROR, L"Name expected", decl);
  decl = s;

  sr = def_shortref(p, name);
  if ( sr->defined )
  { gripe(p, ERC_REDEFINED, L"shortref", name);

    return TRUE;
  }

  sr->defined = TRUE;

  while( *(decl = iskip_layout(dtd, decl)) != '\0'
	 && (s=shortref_add_map(p, decl, sr)) )
    decl = s;
  compile_map(dtd, sr);

  if ( *decl )
    return gripe(p, ERC_SYNTAX_ERROR, L"Map expected", decl);

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find named name.  The name NULL stands for the #empty map

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static dtd_shortref *
find_map(dtd *dtd, dtd_symbol *name)
{ dtd_shortref *sr;

  if ( !name )
  { static dtd_shortref *empty;

    if ( !empty )
    { empty = sgml_calloc(1, sizeof(*empty));
      empty->name = dtd_add_symbol(dtd, (ichar*)"#EMPTY");
      empty->defined = TRUE;
    }

    return empty;
  }

  for( sr = dtd->shortrefs; sr; sr = sr->next )
  { if ( sr->name == name )
    { if ( !sr->defined )
	break;

      return sr;
    }
  }

  return NULL;
}


static void
set_map_element(dtd_element *e, void *closure)
{ e->map = closure;
}


static int
process_usemap_declaration(dtd_parser *p, const ichar *decl)
{ dtd *dtd = p->dtd;
  ichar buf[MAXDECL];
  dtd_symbol *name;
  const ichar *s;
  dtd_symbol *ename;
  dtd_element *e;
  dtd_shortref *map;

  if ( !expand_pentities(p, decl, ZERO_TERM_LEN, buf, sizeof(buf)/sizeof(ichar)) )
    return FALSE;
  decl = buf;

  if ( !(s=itake_name(p, decl, &name)) )
  { if ( (s=isee_identifier(dtd, decl, "#empty")) )
      name = NULL;
    else
      return gripe(p, ERC_SYNTAX_ERROR, L"map-name expected", decl);
  }

  decl = s;
  if ( !(map = find_map(dtd, name)) )
    map = def_shortref(p, name);	/* make undefined map */

  if ( isee_func(dtd, decl, CF_GRPO) )	/* ( */
  { dtd_model *model;

    if ( (model = make_model(p, decl, &s)) )
    { for_elements_in_model(model, set_map_element, map);
      free_model(model);
      decl = s;
    } else
      return FALSE;
  } else if ( (s=itake_name(p, decl, &ename)) )
  { e = find_element(dtd, ename);
    e->map = map;
    decl = s;
  } else if ( p->environments )
  { if ( !map->defined )
      gripe(p, ERC_EXISTENCE, L"map", name->name);

    p->environments->map = map;
    p->map = p->environments->map;
  } else
    return gripe(p, ERC_SYNTAX_ERROR, L"element-name expected", decl);

  if ( *decl )
    return gripe(p, ERC_SYNTAX_ERROR, L"Unparsed", decl);

  return TRUE;
}


static int
match_map(dtd *dtd, dtd_map *map, ocharbuf *buf)
{ wchar_t *data = buf->data.w;
  wchar_t *e    = data+buf->size-1;
  ichar *m      = map->from+map->len-1;

  while( m >= map->from )
  { if ( e < data )
      return 0;

    if ( *m == *e )
    { m--;
      e--;
      continue;
    }
    if ( *m == CHR_DBLANK )
    { if ( e>data && HasClass(dtd, *e, CH_WHITE) )
	e--;
      else
	return FALSE;
      goto wblank;
    }
    if ( *m == CHR_BLANK )
    { wblank:
      while( e>data && HasClass(dtd, *e, CH_WHITE) )
	e--;
      m--;
      continue;
    }
    return 0;
  }

  return (int)(data+buf->size-1-e);
}


static int
match_shortref(dtd_parser *p)
{ dtd_map *map;

  for(map = p->map->map; map; map = map->next)
  { int len;

    if ( (len=match_map(p->dtd, map, p->cdata)) )
    { p->cdata->size -= len;

      if ( p->cdata_must_be_empty )
      { int blank = TRUE;
	const wchar_t *s;
	int i;

	for(s = p->cdata->data.w, i=0; i++ < p->cdata->size; s++)
	{ if ( !iswspace(*s) )
	  { blank = FALSE;
	    break;
	  }
	}

	p->blank_cdata = blank;
      }

      WITH_CLASS(p, EV_SHORTREF,
		 { sgml_cplocation(&p->startloc, &p->location);
		   p->startloc.charpos -= len;
		   p->startloc.linepos -= len;
		   if ( p->startloc.linepos < 0 )
		   { p->startloc.line--;
		     p->startloc.linepos = 0; /* not correct! */
		   }
		   DEBUG(printf("%d-%d: Matched map '%s' --> %s, len = %d\n",
				p->startloc.charpos,
				p->location.charpos,
				map->from, map->to->name, len));

		   process_entity(p, map->to->name);
		 })			/* TBD: optimise */
      return TRUE;
    }
  }

  return FALSE;
}


		 /*******************************
		 *	       ELEMENTS		*
		 *******************************/

static void
add_submodel(dtd_model *m, dtd_model *sub)
{ dtd_model **d;

  for( d = &m->content.group; *d; d = &(*d)->next )
    ;
  *d = sub;
}


/* for_elements_in_model()
   Walk along the model, calling f(e, closure) for any element found
   in the model.  Used for <!SHORTREF name model>
*/

static void
for_elements_in_model(dtd_model *m,
		      void (*f)(dtd_element *e, void *closure),
		      void *closure)
{ switch(m->type)
  { case MT_SEQ:
    case MT_AND:
    case MT_OR:
    { dtd_model *sub = m->content.group;

      for(; sub; sub = sub->next)
	for_elements_in_model(sub, f, closure);
      break;
    }
    case MT_ELEMENT:
      (*f)(m->content.element, closure);
      break;
    default:
      ;
  }
}


static void
free_model(dtd_model *m)
{ switch(m->type)
  { case MT_SEQ:
    case MT_AND:
    case MT_OR:
    { dtd_model *sub = m->content.group;
      dtd_model *next;

      for(; sub; sub = next)
      { next = sub->next;

	free_model(sub);
      }
    }
    default:
      ;
  }

  sgml_free(m);
}


static dtd_model *
make_model(dtd_parser *p, const ichar *decl, const ichar **end)
{ const ichar *s;
  dtd_model *m = sgml_calloc(1, sizeof(*m));
  dtd_symbol *id;
  dtd *dtd = p->dtd;

  decl = iskip_layout(dtd, decl);

  if ( (s=isee_identifier(dtd, decl, "#pcdata")) )
  { m->type = MT_PCDATA;
    m->cardinality = MC_ONE;		/* actually don't care */
    *end = s;
    return m;
  }

  if ( (s=itake_name(p, decl, &id)) )
  { m->type = MT_ELEMENT;
    m->content.element = find_element(dtd, id);
    decl = s;
  } else
  { if ( !(s=isee_func(dtd, decl, CF_GRPO)) )
    { gripe(p, ERC_SYNTAX_ERROR, L"Name group expected", decl);
      free_model(m);
      return NULL;
    }
    decl = s;

    for(;;)
    { dtd_model *sub;
      modeltype mt;

      if ( !(sub = make_model(p, decl, &s)) )
      { free_model(sub);
	return NULL;
      }
      decl = s;
      add_submodel(m, sub);

      if ( (s = isee_func(dtd, decl, CF_OR)) )
      { decl = s;
	mt = MT_OR;
      } else if ( (s = isee_func(dtd, decl, CF_SEQ)) )
      { decl = s;
	mt = MT_SEQ;
      } else if ( (s = isee_func(dtd, decl, CF_AND)) )
      { decl = s;
	mt = MT_AND;
      } else if ( (s = isee_func(dtd, decl, CF_GRPC)) )
      { decl = s;
	break;
      } else
      { gripe(p, ERC_SYNTAX_ERROR, L"Connector ('|', ',' or '&') expected", decl);
	free_model(m);
	return NULL;
      }
      decl = iskip_layout(dtd, decl);

      if ( m->type != mt )
      { if ( !m->type )
	  m->type = mt;
	else
	{ gripe(p, ERC_SYNTAX_ERROR, L"Different connector types in model", decl);
	  free_model(m);
	  return NULL;
	}
      }
    }
  }

  if ( (s = isee_func(dtd, decl, CF_OPT)) )
  { decl = s;
    m->cardinality = MC_OPT;
  } else if ( (s=isee_func(dtd, decl, CF_REP)) )
  { decl = s;
    m->cardinality = MC_REP;
  } else if ( (s=isee_func(dtd, decl, CF_PLUS)) )
  {					/* ROK: watch out for (x) +(y) */
    if ( isee_func(dtd, iskip_layout(dtd, s), CF_GRPO) == NULL )
    { decl = s;
      m->cardinality = MC_PLUS;
    }
  } else
    m->cardinality = MC_ONE;

  if ( m->type == MT_UNDEF )		/* simplify (e+), etc. */
  { dtd_model *sub = m->content.group;
    modelcard card;

    assert(!sub->next);
    if ( sub->cardinality == MC_ONE )
      card = m->cardinality;
    else if ( m->cardinality == MC_ONE )
      card = sub->cardinality;
    else
    { m->type = MT_OR;
      goto out;
    }

    *m = *sub;
    m->cardinality = card;
    sgml_free(sub);
  }

out:
  *end = iskip_layout(dtd, decl);
  return m;
}


static const ichar *
process_model(dtd_parser *p, dtd_edef *e, const ichar *decl)
{ const ichar *s;
  dtd *dtd = p->dtd;

  decl = iskip_layout(dtd, decl);
  if ( (s = isee_identifier(dtd, decl, "empty")) )
  { e->type = C_EMPTY;
    return s;
  }
  if ( (s = isee_identifier(dtd, decl, "cdata")) )
  { e->type = C_CDATA;
    return s;
  }
  if ( (s = isee_identifier(dtd, decl, "rcdata")) )
  { e->type = C_RCDATA;
    return s;
  }
  if ( (s = isee_identifier(dtd, decl, "any")) )
  { e->type = C_ANY;
    return s;
  }

  e->type = C_PCDATA;
  if ( !(e->content = make_model(p, decl, &decl)) )
    return FALSE;

  return decl;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
See a name-group separator.  As long as we haven't decided, this can be
CF_NG.  If we have decided they must all be the same.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static const ichar *
isee_ngsep(dtd *dtd, const ichar *decl, charfunc *sep)
{ const ichar *s;

  if ( (s=isee_func(dtd, decl, *sep)) )
    return iskip_layout(dtd, s);
  if ( *sep == CF_NG )			/* undecided */
  { static const charfunc ng[] = { CF_SEQ, CF_OR, CF_AND };
    int n;

    for(n=0; n<3; n++)
    { if ( (s=isee_func(dtd, decl, ng[n])) )
      { *sep = ng[n];
        return iskip_layout(dtd, s);
      }
    }
  }

  return NULL;
}



static const ichar *
itake_namegroup(dtd_parser *p, const ichar *decl,
		dtd_symbol **names, int *n)
{ const ichar *s;
  int en = 0;
  dtd *dtd = p->dtd;

  if ( (s=isee_func(dtd, decl, CF_GRPO)) )
  { charfunc ngs = CF_NG;

    for(;;)
    { if ( !(decl=itake_name(p, s, &names[en++])) )
      { gripe(p, ERC_SYNTAX_ERROR, L"Name expected", s);
	return NULL;
      }
      if ( (s=isee_ngsep(dtd, decl, &ngs)) )
      { decl = iskip_layout(dtd, s);
	continue;
      }
      if ( (s=isee_func(dtd, decl, CF_GRPC)) )
      { *n = en;
        decl = s;
	return iskip_layout(dtd, decl);
      }
      gripe(p, ERC_SYNTAX_ERROR, L"Bad name-group", decl);
      return NULL;
    }
  }

  return NULL;
}


typedef struct
{ dtd_symbol **list;
  int size;
} namelist;


static void
add_list_element(dtd_element *e, void *closure)
{ namelist *nl = closure;

  nl->list[nl->size++] = e->name;
}


static const ichar *
itake_el_or_model_element_list(dtd_parser *p,
			       const ichar *decl, dtd_symbol **names, int *n)
{ const ichar *s;
  dtd *dtd = p->dtd;

  if ( isee_func(dtd, decl, CF_GRPO) )
  { dtd_model *model;

    if ( (model = make_model(p, decl, &s)) )
    { namelist nl;

      nl.list = names;
      nl.size = 0;
      for_elements_in_model(model, add_list_element, &nl);
      free_model(model);

      *n = nl.size;
      return s;
    } else
      return NULL;
  } else
  { if ( !(s = itake_name(p, decl, &names[0])) )
    { gripe(p, ERC_SYNTAX_ERROR, L"Name expected", decl);
      return NULL;
    }
    *n = 1;
    return s;
  }
}


static void
add_element_list(dtd_element_list **l, dtd_element *e)
{ dtd_element_list *n = sgml_calloc(1, sizeof(*n));

  n->value = e;

  for( ; *l; l = &(*l)->next )
    ;
  *l = n;
}


static int
process_element_declaraction(dtd_parser *p, const ichar *decl)
{ dtd *dtd = p->dtd;
  ichar buf[MAXDECL];
  const ichar *s;
  dtd_symbol *eid[MAXATTELEM];
  dtd_edef *def;
  int en;
  int i;

					/* expand parameter entities */
  if ( !expand_pentities(p, decl, ZERO_TERM_LEN,
			 buf, sizeof(buf)/sizeof(ichar)) )
    return FALSE;
  decl = buf;

  if ( !(s=itake_el_or_model_element_list(p, decl, eid, &en)) )
    return gripe(p, ERC_SYNTAX_ERROR, L"Name or name-group expected", decl);
  decl = s;
  if ( en == 0 )
    return TRUE;			/* 0 elements */

  STAT(edefs_decl++);
  def = new_element_definition(dtd);
  for(i=0; i<en; i++)
  { find_element(dtd, eid[i]);
    if ( eid[i]->element->structure )
    { if ( eid[i]->element->structure->type != C_EMPTY )
	gripe(p, ERC_SYNTAX_WARNING, L"Redefined element", decl);
      free_element_definition(eid[i]->element->structure);
    }
    eid[i]->element->structure = def;
    eid[i]->element->undefined = FALSE;
  }
  def->references = en;			/* for GC */

					/* omitted tag declarations (opt) */
  if ( (s = isee_identifier(dtd, decl, "-")) )
  { def->omit_close = FALSE;
    goto seeclose;
  } else if ( (s = isee_identifier(dtd, decl, "o")) )
  { def->omit_open = TRUE;

  seeclose:
    decl = s;
    if ( (s = isee_identifier(dtd, decl, "-")) )
    { def->omit_close = FALSE;
    } else if ( (s = isee_identifier(dtd, decl, "o")) )
    { for(i=0; i<en; i++)
	def->omit_close = TRUE;
    } else
      return gripe(p, ERC_SYNTAX_ERROR, L"Bad omit-tag declaration", decl);

    decl = s;
  }

					/* content model */
  if ( !(decl=process_model(p, def, decl)) )
    return FALSE;

					/* in/excluded elements */
  if ( decl[0] == '-' || decl[0] == '+' )
  { dtd_symbol *ng[MAXNAMEGROUP];
    int ns;
    dtd_element_list **l;

    if ( decl[0] == '-' )
      l = &def->excluded;
    else
      l = &def->included;

    decl++;
    if ( (s=itake_namegroup(p, decl, ng, &ns)) )
    { int i;

      decl = s;

      for(i=0; i<ns; i++)
	add_element_list(l, find_element(dtd, ng[i]));
    } else
    { return gripe(p, ERC_SYNTAX_ERROR, L"Name group expected", decl);
    }
  }

  if (*decl)
    return gripe(p, ERC_SYNTAX_ERROR, L"Unexpected end of declaration", decl);

  return TRUE;
}


static void
add_name_list(dtd_name_list **nl, dtd_symbol *s)
{ dtd_name_list *n = sgml_calloc(1, sizeof(*n));

  n->value = s;

  for( ; *nl; nl = &(*nl)->next )
    ;

  *nl = n;
}


static void
set_element_properties(dtd_element *e, dtd_attr *a)
{ if ( istreq(a->name->name, L"xml:space") )
  { switch(a->def)
    { case AT_FIXED:
      case AT_DEFAULT:
	break;
      default:
	return;
    }

    switch (a->type )
    { case AT_NAMEOF:
      case AT_NAME:
      case AT_NMTOKEN:
	e->space_mode = istr_to_space_mode(a->att_def.name->name);
	break;
      case AT_CDATA:
	e->space_mode = istr_to_space_mode((ichar *)a->att_def.cdata);
	break;
      default:
	break;
    }
  }
}


static void
add_attribute(dtd_parser *p, dtd_element *e, dtd_attr *a)
{ dtd_attr_list **l;
  dtd_attr_list *n;

  for(l = &e->attributes; *l; l = &(*l)->next)
  { if ( (*l)->attribute->name == a->name )
    { gripe(p, ERC_REDEFINED, L"attribute", a->name);
      a->references++;			/* attempt to redefine attribute: */
      free_attribute(a);		/* first wins according to standard */

      return;
    }
  }

  n = sgml_calloc(1, sizeof(*n));

  n->attribute = a;
  a->references++;
  *l = n;
  set_element_properties(e, a);
}


static int
process_attlist_declaraction(dtd_parser *p, const ichar *decl)
{ dtd *dtd = p->dtd;
  dtd_symbol *eid[MAXATTELEM];
  int i, en;
  ichar buf[MAXDECL];
  const ichar *s;

					/* expand parameter entities */
  if ( !expand_pentities(p, decl, ZERO_TERM_LEN, buf, sizeof(buf)/sizeof(ichar)) )
    return FALSE;
  decl = iskip_layout(dtd, buf);
  DEBUG(printf("Expanded to %s\n", decl));

  if ( !(decl=itake_el_or_model_element_list(p, decl, eid, &en)) )
    return FALSE;

					/* fetch attributes */
  while(*decl)
  { dtd_attr *at = sgml_calloc(1, sizeof(*at));
    at->references = REFS_VIRGIN;

					/* name of attribute */
    if ( !(s = itake_name(p, decl, &at->name)) )
    { free_attribute(at);
      return gripe(p, ERC_SYNTAX_ERROR, L"Name expected", decl);
    }
    decl = s;

					/* (name1|name2|...) type */
    if ( (s=isee_func(dtd, decl, CF_GRPO)) )
    { charfunc ngs = CF_NG;

      at->type = AT_NAMEOF;
      decl=s;

      for(;;)
      { dtd_symbol *nm;

	if ( !(s = itake_nmtoken(p, decl, &nm)) )
	{ free_attribute(at);
	  return gripe(p, ERC_SYNTAX_ERROR, L"Name expected", decl);
	}
	decl = s;
	add_name_list(&at->typeex.nameof, nm);
	if ( (s=isee_ngsep(dtd, decl, &ngs)) )
	{ decl = s;
	  continue;
	}
	if ( (s = isee_func(dtd, decl, CF_GRPC)) )
	{ decl=s;
	  decl = iskip_layout(dtd, decl);
	  break;
	}
	free_attribute(at);
	return gripe(p, ERC_SYNTAX_ERROR, L"Illegal name-group", decl);
      }
    } else if ( (s=isee_identifier(dtd, decl, "cdata")) )
    { decl = s;
      at->type = AT_CDATA;
    } else if ( (s=isee_identifier(dtd, decl, "entity")) )
    { decl = s;
      at->type = AT_ENTITY;
    } else if ( (s=isee_identifier(dtd, decl, "entities")) )
    { decl = s;
      at->type = AT_ENTITIES;
      at->islist = TRUE;
    } else if ( (s=isee_identifier(dtd, decl, "id")) )
    { decl = s;
      at->type = AT_ID;
    } else if ( (s=isee_identifier(dtd, decl, "idref")) )
    { decl = s;
      at->type = AT_IDREF;
    } else if ( (s=isee_identifier(dtd, decl, "idrefs")) )
    { decl = s;
      at->type = AT_IDREFS;
      at->islist = TRUE;
    } else if ( (s=isee_identifier(dtd, decl, "name")) )
    { decl = s;
      at->type = AT_NAME;
    } else if ( (s=isee_identifier(dtd, decl, "names")) )
    { decl = s;
      at->type = AT_NAMES;
      at->islist = TRUE;
    } else if ( (s=isee_identifier(dtd, decl, "nmtoken")) )
    { decl = s;
      at->type = AT_NMTOKEN;
    } else if ( (s=isee_identifier(dtd, decl, "nmtokens")) )
    { decl = s;
      at->type = AT_NMTOKENS;
      at->islist = TRUE;
    } else if ( (s=isee_identifier(dtd, decl, "number")) )
    { decl = s;
      at->type = AT_NUMBER;
    } else if ( (s=isee_identifier(dtd, decl, "numbers")) )
    { decl = s;
      at->type = AT_NUMBERS;
      at->islist = TRUE;
    } else if ( (s=isee_identifier(dtd, decl, "nutoken")) )
    { decl = s;
      at->type = AT_NUTOKEN;
    } else if ( (s=isee_identifier(dtd, decl, "nutokens")) )
    { decl = s;
      at->type = AT_NUTOKENS;
      at->islist = TRUE;
    } else if ( (s=isee_identifier(dtd, decl, "notation")) )
    { dtd_symbol *ng[MAXNAMEGROUP];
      int ns;

      at->type = AT_NOTATION;
      decl=s;
      if ( (s=itake_namegroup(p, decl, ng, &ns)) )
      { decl = s;

	for(i=0; i<ns; i++)
	  add_name_list(&at->typeex.nameof, ng[i]);
      } else
      { free_attribute(at);
	return gripe(p, ERC_SYNTAX_ERROR, L"name-group expected", decl);
      }
    } else
    { free_attribute(at);
      return gripe(p, ERC_SYNTAX_ERROR, L"Attribute-type expected", decl);
    }

					/* Attribute Defaults */
    if ( (s=isee_identifier(dtd, decl, "#fixed")) )
    { decl = s;
      at->def = AT_FIXED;
    } else if ( (s=isee_identifier(dtd, decl, "#required")) )
    { decl = s;
      at->def = AT_REQUIRED;
    } else if ( (s=isee_identifier(dtd, decl, "#current")) )
    { decl = s;
      at->def = AT_CURRENT;
    } else if ( (s=isee_identifier(dtd, decl, "#conref")) )
    { decl = s;
      at->def = AT_CONREF;
    } else if ( (s=isee_identifier(dtd, decl, "#implied")) )
    { decl = s;
      at->def = AT_IMPLIED;
    } else				/* real default */
      at->def = AT_DEFAULT;

    if ( at->def == AT_DEFAULT || at->def == AT_FIXED )
    { ichar buf[MAXSTRINGLEN];
      ichar *start; int len;
      const ichar *end;

      if ( !(end=itake_string(dtd, decl, &start, &len)) )
      { end=itake_nmtoken_chars(p, decl, buf, sizeof(buf)/sizeof(ichar));
	start = buf;
	len = (int)istrlen(buf);
      }
      if ( !end )
	return gripe(p, ERC_SYNTAX_ERROR, L"Bad attribute default", decl);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note: itake_name(), etc. work on nul-terminated   strings. The result of
itake_string() is a  pointer  in  a   nul-terminated  string  and  these
functions will stop scanning at the  quote   anyway,  so  we can use the
length of the parsed data to verify we parsed all of it.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

      switch(at->type)
      { case AT_CDATA:
	{ at->att_def.cdata = istrndup(start, len);
	  break;
	}
	case AT_ENTITY:
	case AT_NOTATION:
	case AT_NAME:
	{ if ( !(s=itake_name(p, start, &at->att_def.name)) ||
	       (s-start) != len )
	    return gripe(p, ERC_DOMAIN, L"name", decl);
	  break;
	}
	case AT_NMTOKEN:
	case AT_NAMEOF:
	{ if ( !(s=itake_nmtoken(p, start, &at->att_def.name)) ||
	       (s-start) != len )
	    return gripe(p, ERC_DOMAIN, L"nmtoken", decl);
	  break;
	}
	case AT_NUTOKEN:
	{ if ( !(s=itake_nutoken(p, start, &at->att_def.name)) ||
	       (s-start) != len )
	    return gripe(p, ERC_DOMAIN, L"nutoken", decl);
	  break;
	}
	case AT_NUMBER:
	{ if ( !(s=itake_number(p, start, at)) ||
	       (s-start) != len )
	     return gripe(p, ERC_DOMAIN, L"number", decl);
	  break;
	}
	case AT_NAMES:
	case AT_ENTITIES:
	case AT_IDREFS:
	case AT_NMTOKENS:
	case AT_NUMBERS:
	case AT_NUTOKENS:
	{ at->att_def.list = istrndup(buf, len);
	  break;
	}
	default:
	{ free_attribute(at);
	  return gripe(p, ERC_REPRESENTATION, L"No default for type");
	}
      }

      decl = end;
    }

					/* add to list */
    at->references = 0;
    for(i=0; i<en; i++)
    { dtd_element *e = def_element(dtd, eid[i]);

      add_attribute(p, e, at);
    }
  }

  return TRUE;
}

		 /*******************************
		 *    GENERIC TAG PROCESSING	*
		 *******************************/

typedef enum
{ IE_NORMAL,
  IE_INCLUDED,				/* is included */
  IE_EXCLUDED				/* is excluded */
} includetype;


static includetype
in_or_excluded(sgml_environment *env, dtd_element *e)
{ for(; env; env=env->parent)
  { if ( env->element->structure )
    { dtd_edef *def = env->element->structure;
      dtd_element_list *el;

      for(el=def->excluded; el; el=el->next)
      { if ( el->value == e )
	  return IE_EXCLUDED;
      }
      for(el=def->included; el; el=el->next)
      { if ( el->value == e )
	  return IE_INCLUDED;
      }
    }
  }

  return IE_NORMAL;
}


static int
complete(sgml_environment *env)
{ if ( env->element->structure &&
       !env->element->undefined &&
       env->element->structure->type != C_ANY )
  { dtd_edef *def = env->element->structure;

    if ( !same_state(def->final_state, env->state) )
      return FALSE;
  }

  return TRUE;
}


static void
validate_completeness(dtd_parser *p, sgml_environment *env)
{ if ( !complete(env) )
  { wchar_t buf[MAXNMLEN+50];

    swprintf(buf, MAXNMLEN+50, L"Incomplete element: <%s>",
	     env->element->name->name);

    gripe(p, ERC_VALIDATE, buf);		/* TBD: expected */
  }
}


static sgml_environment *
push_element(dtd_parser *p, dtd_element *e, int callback)
{ if ( e != CDATA_ELEMENT )
  { sgml_environment *env = sgml_calloc(1, sizeof(*env));

    emit_cdata(p, FALSE);

    env->element = e;
    env->state = make_state_engine(e);
    env->space_mode = (p->environments ? p->environments->space_mode
				       : p->dtd->space_mode);
    env->parent = p->environments;
    p->environments = env;

    if ( p->dtd->shorttag )
    { env->saved_waiting_for_net = p->waiting_for_net;

      if ( p->event_class == EV_SHORTTAG )
      { p->waiting_for_net = TRUE;
	env->wants_net = TRUE;
      } else
      { env->wants_net = FALSE;
	if ( e->structure && e->structure->omit_close == FALSE )
	  p->waiting_for_net = FALSE;
      }
    }

    if ( e->map )
      p->map = env->map = e->map;
    else if ( env->parent )
      p->map = env->map = env->parent->map;

    p->first = TRUE;
    if ( callback && p->on_begin_element )
    { sgml_attribute_list atts;

      init_attribute_list(&atts);
      if ( !(p->flags & SGML_PARSER_NODEFS) )
	add_default_attributes(p, e, &atts);

      (*p->on_begin_element)(p, e, atts.count, atts.attributes);
      clear_attribute_list(&atts);
    }

    if ( e->structure )
    { if ( e->structure->type == C_CDATA ||
	   e->structure->type == C_RCDATA )
      { p->state = (e->structure->type == C_CDATA ? S_CDATA : S_RCDATA);
	p->cdata_state = p->state;
	p->etag = e->name->name;
	p->etaglen = (int)istrlen(p->etag);
	sgml_cplocation(&p->startcdata, &p->location);
      } else
	p->cdata_state = S_PCDATA;
    }
  }

  return p->environments;
}


static void
free_environment(sgml_environment *env)
{
#ifdef XMLNS
  if ( env->xmlns )
    xmlns_free(env->xmlns);
#endif

  sgml_free(env);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Pop the stack,  closing  all  environment   uptil  `to'.  The  close was
initiated by pushing the element `e'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
pop_to(dtd_parser *p, sgml_environment *to, dtd_element *e0)
{ sgml_environment *env, *parent;

  for(env = p->environments; env != to; env=parent)
  { dtd_element *e = env->element;

    validate_completeness(p, env);
    parent = env->parent;

    if ( e->structure && !e->structure->omit_close )
      gripe(p, ERC_OMITTED_CLOSE, e->name->name);

    if ( e0 != CDATA_ELEMENT )
      emit_cdata(p, TRUE);

    p->first = FALSE;
    p->environments = env;
    if ( p->dtd->shorttag )
      p->waiting_for_net = env->saved_waiting_for_net;

    WITH_CLASS(p, EV_OMITTED,
	       if ( p->on_end_element )
	         (*p->on_end_element)(p, e));
    free_environment(env);
  }
  p->environments = to;
  p->map = to->map;

  return TRUE;
}


static void
allow_for(dtd_element *in, dtd_element *e)
{ dtd_edef *def = in->structure;
  dtd_model *g;

  if ( def->type == C_EMPTY )
  { def->type = C_PCDATA;
    def->content = sgml_calloc(1, sizeof(*def->content));
    def->content->type = MT_OR;
    def->content->cardinality = MC_REP;
  }
  assert(def->content->type == MT_OR);

  g = def->content->content.group;

  if ( e == CDATA_ELEMENT )
  { dtd_model *m;

    for(; g; g = g->next)
    { if ( g->type == MT_PCDATA )
	return;
    }
    m = sgml_calloc(1, sizeof(*m));
    m->type	   = MT_PCDATA;
    m->cardinality = MC_ONE;		/* ignored */
    add_submodel(def->content, m);
  } else
  { dtd_model *m;

    for(; g; g = g->next)
    { if ( g->type == MT_ELEMENT && g->content.element == e )
	return;
    }
    m = sgml_calloc(1, sizeof(*m));
    m->type	   = MT_ELEMENT;
    m->cardinality = MC_ONE;		/* ignored */
    m->content.element = e;
    add_submodel(def->content, m);
  }
}



static int
open_element(dtd_parser *p, dtd_element *e, int warn)
{ if ( !p->environments && p->enforce_outer_element )
  { dtd_element *f = p->enforce_outer_element->element;

    if ( f && f != e )
    { if ( !f->structure ||
	   !f->structure->omit_open )
	gripe(p, ERC_OMITTED_OPEN, f->name->name);

      WITH_CLASS(p, EV_OMITTED,
		 { open_element(p, f, TRUE);
		   if ( p->on_begin_element )
		   { sgml_attribute_list atts;

		     init_attribute_list(&atts);
		     if ( !(p->flags & SGML_PARSER_NODEFS) )
		       add_default_attributes(p, f, &atts);

		     (*p->on_begin_element)(p, f, atts.count, atts.attributes);
		     clear_attribute_list(&atts);
		   }
		 });
    }
  }

					/* no DTD available yet */
  if ( !p->environments && !p->dtd->doctype && e != CDATA_ELEMENT )
  { const ichar *file;

    file = find_in_catalogue(CAT_DOCTYPE, e->name->name, NULL, NULL,
			     IS_XML_DIALECT(p->dtd->dialect));

    if ( file && !is_url(file) )
    { dtd_parser *clone = clone_dtd_parser(p);

      gripe(p, ERC_NO_DOCTYPE, e->name->name, file);

      if ( load_dtd_from_file(clone, file) )
	p->dtd->doctype = istrdup(e->name->name);
      else
	gripe(p, ERC_EXISTENCE, L"file", file);

      free_dtd_parser(clone);
    }
  }

  if ( p->environments )
  { sgml_environment *env = p->environments;

    if ( env->element->undefined )
    { allow_for(env->element, e);	/* <!ELEMENT x - - (model) +(y)> */
      push_element(p, e, FALSE);
      return TRUE;
    }

    if ( env->element->structure &&
	 env->element->structure->type == C_ANY )
    { if ( e != CDATA_ELEMENT && e->undefined )
	gripe(p, ERC_EXISTENCE, L"Element", e->name->name);
      push_element(p, e, FALSE);
      return TRUE;
    }

    switch(in_or_excluded(env, e))
    { case IE_INCLUDED:
        push_element(p, e, FALSE);
	return TRUE;
      case IE_EXCLUDED:
	if ( warn )
	  gripe(p, ERC_NOT_ALLOWED, e->name->name);
	/*FALLTHROUGH*/
      case IE_NORMAL:
	for(; env; env=env->parent)
	{ dtd_state *new;

	  if ( (new = make_dtd_transition(env->state, e)) )
	  { env->state = new;
	    pop_to(p, env, e);
	    push_element(p, e, FALSE);
	    return TRUE;
	  } else
	  { dtd_element *oe[MAXOMITTED]; /* omitted open */
	    int olen;
	    int i;

	    if ( (olen=find_omitted_path(env->state, e, oe)) > 0 )
	    { pop_to(p, env, e);
	      WITH_CLASS(p, EV_OMITTED,
	      for(i=0; i<olen; i++)
	      { env->state = make_dtd_transition(env->state, oe[i]);
		env = push_element(p, oe[i], TRUE);
	      })
	      env->state = make_dtd_transition(env->state, e);
	      push_element(p, e, FALSE);
	      return TRUE;
	    }
	  }

	  if ( !env->element->structure ||
	       !env->element->structure->omit_close )
	    break;
	}
    }

    if ( warn )
    { if ( e == CDATA_ELEMENT )
	gripe(p, ERC_VALIDATE, L"#PCDATA not allowed here");
      else if ( e->undefined )
	gripe(p, ERC_EXISTENCE, L"Element", e->name->name);
      else
	gripe(p, ERC_NOT_ALLOWED, e->name->name);
    }
  }

  if ( warn )
  { push_element(p, e, FALSE);
    return TRUE;
  } else
    return FALSE;
}


static int
close_element(dtd_parser *p, dtd_element *e, int conref)
{ sgml_environment *env;

  for(env = p->environments; env; env=env->parent)
  { if ( env->element == e )		/* element is open */
    { sgml_environment *parent;

      for(env = p->environments; ; env=parent)
      {	dtd_element *ce	= env->element;

	if ( !(conref && env == p->environments) )
	  validate_completeness(p, env);
	parent = env->parent;

	p->first = FALSE;
	if ( p->on_end_element )
	  (*p->on_end_element)(p, env->element);
	free_environment(env);
	p->environments = parent;

	if ( ce == e )			/* closing current element */
	{ p->map = (parent ? parent->map : NULL);
	  return TRUE;
	} else				/* omited close */
	{ if ( ce->structure && !ce->structure->omit_close )
	    gripe(p, ERC_OMITTED_CLOSE, ce->name->name);
	}
      }
    }
  }

  return gripe(p, ERC_NOT_OPEN, e->name->name);
}


static int
close_current_element(dtd_parser *p)
{ if ( p->environments )
  { dtd_element *e = p->environments->element;

    emit_cdata(p, TRUE);
    return close_element(p, e, FALSE);
  }

  return gripe(p, ERC_SYNTAX_ERROR, L"No element to close", "");
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_attribute_value()

Get the value for an attribute.  Once   I  thought  this was simple, but
Richard O'Keefe pointed to the complex   handling of white-space in SGML
attributes. Basically, if the attribute is quoted, we need:

	* If CDATA, map all blank to space characters, then expand
	  entities

	* If !CDATA expand all entities, canonicalise white space by
	  deleting leading and trailing space and squishing multiple
	  space characters to a single (lower for us) case.

This almost, but not completely matches the XML definition. This however
is so complex we will ignore it for now.

[Rewritten by Richard O'Keefe with these addional comments]
Reads a value, the  attribute  name   and  value  indicator  having been
processed already. It calls itake_string() to   read  quoted values, and
itake_unquoted() to read unquoted values.

itake_string(dtd, in, buf, size)
	- skips layout INCLUDING comments,
	- returns NULL if the next character is not ' or ",
	- copies characters from in to buf until a matching ' or " is found,
	- adds a terminating \0,
	- skips more layout INCLUDING comments, and
	- returns the new input position.
It is quite wrong to skip leading comments here.  In the tag

    <foo bar = --ugh-- zoo>

the characters "--ugh--" *are the value*.  They are not a comment.
Comments are not in fact allowed inside tags, unfortunately.
This tag is equivalent to

    <foo bar="--ugh--" something="zoo">

where something is an attribute that has zoo as one of its enumerals.

Because itake_string() is called in many other places, this bug has
not yet been fixed.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static ichar const *
get_attribute_value(dtd_parser *p, ichar const *decl, sgml_attribute *att)
{ ichar tmp[MAXSTRINGLEN];
  ichar *buf = tmp;
  ichar const *s;
  ichar c;
  dtd *dtd = p->dtd;
  ichar const *end;
  ichar *start; int len;

  enum
  { DIG_FIRST = 8,		/* any token start with digit? */
    NAM_FIRST = 4,		/* any token start with non-digit name char? */
    NAM_LATER = 2,		/* any token have non-digit name char later? */
    ANY_OTHER = 1,		/* any token have illegal character? */
    YET_EMPTY = 0
  }
  token = YET_EMPTY;

  att->value.textW = NULL;		/* UCS text */
  att->value.number = 0;
  att->flags = 0;

  end = itake_string(dtd, decl, &start, &len);

  if ( end != NULL )
  { ocharbuf out;

    init_ocharbuf(&out, p->max_memory);
    expand_entities(p, start, len, &out);

    if ( att->definition->type == AT_CDATA )
    { malloc_ocharbuf(&out);

      att->value.number = out.size;
      att->value.textW  = out.data.w;

      return end;
    } else
    { ichar *d;

      buf = out.data.w;

      /* canonicalise blanks */
      s = buf;
      while ((c = *s++) != '\0' && HasClass(dtd, c, CH_BLANK))
	;
      d = buf;
      while ( c != '\0' )
      { token |= HasClass(dtd, c, CH_DIGIT) ? DIG_FIRST
	  : HasClass(dtd, c, CH_NAME) ? NAM_FIRST : /* oops! */ ANY_OTHER;
	if ( d != buf )
	  *d++ = ' ';
	if ( dtd->att_case_sensitive )
	{ *d++ = c;
	  while ((c = *s++) != '\0' && !HasClass(dtd, c, CH_BLANK))
	  { token |= HasClass(dtd, c, CH_DIGIT) ? 0
	      : HasClass(dtd, c, CH_NAME) ? NAM_LATER : /* oops! */ ANY_OTHER;
	    *d++ = c;
	  }
	} else
	{ *d++ = towlower(c);
	  while ((c = *s++) != '\0' && !HasClass(dtd, c, CH_BLANK))
	  { token |= HasClass(dtd, c, CH_DIGIT) ? 0
	      : HasClass(dtd, c, CH_NAME) ? NAM_LATER : /* oops! */ ANY_OTHER;
	    *d++ = towlower(c);
	  }
	}
	while (c != '\0' && HasClass(dtd, c, CH_BLANK))
	  c = *s++;
      }
      *d = '\0';
    }
  } else
  { end = itake_unquoted(p, decl, tmp, sizeof(tmp)/sizeof(ichar));
    if (end == NULL)
      return NULL;

    s = buf;
    c = *s++;
    if (c != '\0')
    { token |= HasClass(dtd, c, CH_DIGIT) ? DIG_FIRST
	: HasClass(dtd, c, CH_NAME) ? NAM_FIRST : /* oops! */ ANY_OTHER;
      while ((c = *s++) != 0)
      { token |= HasClass(dtd, c, CH_DIGIT) ? 0
	  : HasClass(dtd, c, CH_NAME) ? NAM_LATER : /* oops! */ ANY_OTHER;
      }
    }
    if ( token == YET_EMPTY || (token & ANY_OTHER) != 0)
      gripe(p, ERC_SYNTAX_WARNING, L"Attribute value requires quotes", buf);

    if (!dtd->att_case_sensitive && att->definition->type != AT_CDATA)
      istrlower(buf);
  }

  switch (att->definition->type)
  { case AT_NUMBER:		/* number */
      if (token != DIG_FIRST)
      { gripe(p, ERC_SYNTAX_WARNING, L"NUMBER expected", decl);
      } else if (dtd->number_mode == NU_INTEGER)
      { (void) istrtol(buf, &att->value.number);
      } else
      { att->value.textW  = istrdup(buf);
	att->value.number = (long)istrlen(buf);
      }
      return end;
    case AT_CDATA:		/* CDATA attribute */
      att->value.textW  = istrdup(buf);
      att->value.number = (long)istrlen(buf);
      return end;
    case AT_ID:		/* identifier */
    case AT_IDREF:		/* identifier reference */
    case AT_NAME:		/* name token */
    case AT_NOTATION:		/* notation-name */
      if (token == YET_EMPTY || (token & (DIG_FIRST | ANY_OTHER)) != 0)
	gripe(p, ERC_SYNTAX_WARNING, L"NAME expected", decl);
      break;
    case AT_NAMEOF:		/* one of these names */
    case AT_NMTOKEN:		/* name-token */
      if (token == YET_EMPTY || (token & ANY_OTHER) != 0)
	gripe(p, ERC_SYNTAX_WARNING, L"NMTOKEN expected", decl);
      if ( att->definition->type == AT_NAMEOF )
      { dtd_name_list *nl;

	for(nl=att->definition->typeex.nameof; nl; nl = nl->next)
	{ if ( dtd->att_case_preserving )
	  { if ( istrcaseeq(nl->value->name, buf) )
	      goto passed;
	  } else
	  { if ( istreq(nl->value->name, buf) )
	      goto passed;
	  }
	}
	gripe(p, ERC_SYNTAX_WARNING, L"unexpected value", decl);
      }
      break;
    case AT_NUTOKEN:		/* number token */
      if ((token & (NAM_FIRST | ANY_OTHER)) != 0)
	gripe(p, ERC_SYNTAX_WARNING, L"NUTOKEN expected", decl);
      break;
    case AT_ENTITY:		/* entity-name */
      if (token == YET_EMPTY || (token & (DIG_FIRST | ANY_OTHER)) != 0)
	gripe(p, ERC_SYNTAX_WARNING, L"entity NAME expected", decl);
      break;
    case AT_NAMES:		/* list of names */
    case AT_IDREFS:		/* list of identifier references */
      if (token == YET_EMPTY || (token & (DIG_FIRST | ANY_OTHER)) != 0)
	gripe(p, ERC_SYNTAX_WARNING, L"NAMES expected", decl);
      break;
    case AT_ENTITIES:		/* entity-name list */
      if (token == YET_EMPTY || (token & (DIG_FIRST | ANY_OTHER)) != 0)
	gripe(p, ERC_SYNTAX_WARNING, L"entity NAMES expected", decl);
      break;
    case AT_NMTOKENS:		/* name-token list */
      if (token == YET_EMPTY || (token & ANY_OTHER) != 0)
	gripe(p, ERC_SYNTAX_WARNING, L"NMTOKENS expected", decl);
      break;
    case AT_NUMBERS:		/* number list */
      if (token != DIG_FIRST)
	gripe(p, ERC_SYNTAX_WARNING, L"NUMBERS expected", decl);
      break;
    case AT_NUTOKENS:
      if ((token & (NAM_FIRST | ANY_OTHER)) != 0)
	gripe(p, ERC_SYNTAX_WARNING, L"NUTOKENS expected", decl);
      break;
    default:
      assert(0);
      return NULL;
  }

passed:
  att->value.textW  = istrdup(buf);	/* TBD: more validation */
  att->value.number = (long)istrlen(buf);
  return end;
}


static const ichar *
process_attributes(dtd_parser *p, dtd_element *e, const ichar *decl,
		   sgml_attribute_list *atts)
{ dtd *dtd = p->dtd;

  decl = iskip_layout(dtd, decl);
  while(decl && *decl)
  { dtd_symbol *nm;
    const ichar *s;

    if ( (s=itake_nmtoken(p, decl, &nm)) )
    { decl = s;

      if ( (s=isee_func(dtd, decl, CF_VI)) ) /* name= */
      { dtd_attr *a;
	sgml_attribute *ap;

	if ( !HasClass(dtd, nm->name[0], CH_NMSTART) )
	  gripe(p, ERC_SYNTAX_WARNING,
		"Illegal start of attribute-name", decl);

	decl = s;
	if ( !(a=find_attribute(e, nm)) )
	{ a = sgml_calloc(1, sizeof(*a));

	  a->name = nm;
	  a->type = AT_CDATA;
	  a->def  = AT_IMPLIED;
	  add_attribute(p, e, a);

	  if ( !e->undefined &&
	       !(IS_XML_DIALECT(dtd->dialect) &&
		 (istreq(L"xmlns", nm->name) ||
		  istrprefix(L"xmlns:", nm->name))) &&
	       !(IS_HTML5_DIALECT(dtd->dialect) &&
		 istrprefix(L"data-", nm->name)) )
	    gripe(p, ERC_NO_ATTRIBUTE, e->name->name, nm->name);
	}
	ap = new_attribute(atts);
	ap->definition = a;
	if ( (decl=get_attribute_value(p, decl, ap)) )
	  continue;
	else
	  atts->count--;
      } else if ( e->structure )
      { dtd_attr_list *al;		/* value shorthand */

	for(al=e->attributes; al; al=al->next)
	{ dtd_attr *a = al->attribute;

	  if ( a->type == AT_NAMEOF || a->type == AT_NOTATION )
	  { dtd_name_list *nl;

	    for(nl=a->typeex.nameof; nl; nl = nl->next)
	    { if ( nl->value == nm )
	      { sgml_attribute *ap;

		if ( IS_XML_DIALECT(dtd->dialect) )
		  gripe(p, ERC_SYNTAX_WARNING,
			"Value short-hand in XML mode", decl);
		ap = new_attribute(atts);
		ap->flags	 = 0;
		ap->definition   = a;
		ap->value.textW  = istrdup(nm->name);
		ap->value.number = (long)istrlen(nm->name);
		goto next;
	      }
	    }
	  }
	}
	gripe(p, ERC_NO_ATTRIBUTE_VALUE, e->name->name, nm->name);
	decl = s;
      } else
      { gripe(p, ERC_SYNTAX_ERROR, L"Bad attribute", decl);
	decl = s;
      }
    } else
    { return decl;
    }

  next:
    ;
  }

  return decl;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
sgml_add_default_attributes()

This function adds attributes for omitted  default and fixed attributes.
These attributes are added to  the  end   of  the  attribute  list.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
add_default_attributes(dtd_parser *p, dtd_element *e,
		       sgml_attribute_list *atts)
{ dtd_attr_list *al;

  if ( e == CDATA_ELEMENT )
    return TRUE;

  for(al=e->attributes; al; al=al->next)
  { dtd_attr *a = al->attribute;

    switch(a->def)
    { case AT_REQUIRED:			/* TBD: check if present */
      case AT_CURRENT:			/* TBD: register in DTD and reuse */
      case AT_CONREF:
      case AT_IMPLIED:
	goto next;
      case AT_FIXED:
      case AT_DEFAULT:
      { int i;
	sgml_attribute *ap;

	for(i=0, ap=atts->attributes; i<atts->count; i++, ap++)
	{ if ( ap->definition == a )
	    goto next;
	}

	ap = new_attribute(atts);
        ap->definition   = a;
	ap->value.textW  = NULL;
	ap->value.number = 0;
	ap->flags        = SGML_AT_DEFAULT;

	switch(a->type)
	{ case AT_CDATA:
	    ap->value.textW = a->att_def.cdata;
	    ap->value.number = (long)istrlen(ap->value.textW);
	    break;
	  case AT_NUMBER:
	    if ( p->dtd->number_mode == NU_TOKEN )
	    { ap->value.textW  = (ichar*)a->att_def.name->name;
	      ap->value.number = (long)istrlen(ap->value.textW);
	    } else
	    { ap->value.number = a->att_def.number;
	    }
	    break;
	  default:
	    if ( a->islist )
	    { ap->value.textW = a->att_def.list;
	    } else
	    { ap->value.textW = (ichar*)a->att_def.name->name;
	    }
	    ap->value.number = (long)istrlen(ap->value.textW);
	}
      }
    }
  next:;
  }

  return TRUE;
}


static void
free_attribute_values(int argc, sgml_attribute *argv)
{ int i;

  for(i=0; i<argc; i++, argv++)
  { if ( (argv->flags & SGML_AT_DEFAULT) )
      continue;				/* shared with the DTD */

    if ( argv->value.textW )
      sgml_free(argv->value.textW);
  }
}


static void
clear_attribute_list(sgml_attribute_list *atts)
{ free_attribute_values(atts->count, atts->attributes);

  if ( atts->attributes != atts->local )
    sgml_free(atts->attributes);
}


static void
init_attribute_list(sgml_attribute_list *atts)
{ atts->attributes = atts->local;
  atts->count = 0;
  atts->allocated = FASTATTRIBUTES;
}

static sgml_attribute *
new_attribute(sgml_attribute_list *atts)
{ for(;;)
  { if ( atts->count < atts->allocated )
      return &atts->attributes[atts->count++];

    if ( atts->attributes == atts->local )
    { size_t bytes = sizeof(*atts->attributes)*atts->allocated;
      atts->attributes = sgml_malloc(bytes*2);
      memcpy(atts->attributes, atts->local, bytes);
      atts->allocated *= 2;
    } else
    { size_t bytes = sizeof(*atts->attributes)*atts->allocated;
      atts->attributes = sgml_realloc(atts->attributes, bytes*2);
      atts->allocated *= 2;
    }
  }
}


static int
process_begin_element(dtd_parser *p, const ichar *decl)
{ dtd *dtd = p->dtd;
  dtd_symbol *id;
  const ichar *s;

  if ( (s=itake_name(p, decl, &id)) )
  { sgml_attribute_list atts;
    dtd_element *e = find_element(dtd, id);
    int empty = FALSE;
    int conref = FALSE;
    int rc = TRUE;

    if ( !e->structure )
    { dtd_edef *def;
      e->undefined = TRUE;
      STAT(edefs_implicit++);
      def_element(dtd, id);
      def = e->structure;
      def->type = C_EMPTY;
    }

    open_element(p, e, TRUE);

    decl=s;
    init_attribute_list(&atts);
    if ( (s=process_attributes(p, e, decl, &atts)) )
      decl=s;

    if ( IS_XML_DIALECT(dtd->dialect) )
    { if ( (s=isee_func(dtd, decl, CF_ETAGO2)) )
      { empty = TRUE;			/* XML <tag/> */
	decl = s;
      }
#ifdef XMLNS
      if ( dtd->dialect == DL_XMLNS )
	update_xmlns(p, e, atts.count, atts.attributes);
#endif
      update_space_mode(p, e, atts.count, atts.attributes);
    } else				/* SGML, HTML */
    { int i;

      if ( (s=isee_func(dtd, decl, CF_ETAGO2)) )
      { if ( !IS_HTML_DIALECT(dtd->dialect) )
	  gripe(p, ERC_SYNTAX_WARNING, L"Empty tag (<../>) in SGML mode", decl);

	empty = TRUE;			/* HTML5 <tag/> */
	decl = s;
      }

      for(i=0; i<atts.count; i++)
      { if ( atts.attributes[i].definition->def == AT_CONREF )
	{ empty = TRUE;
	  conref = TRUE;
	}
      }
    }
    if ( *decl )
      gripe(p, ERC_SYNTAX_ERROR, L"Bad attribute list", decl);

    if ( !(p->flags & SGML_PARSER_NODEFS) )
      add_default_attributes(p, e, &atts);

    if ( empty ||
	 (IS_SGML_DIALECT(dtd->dialect) &&
	  e->structure &&
	  e->structure->type == C_EMPTY &&
	  !e->undefined) )
      p->empty_element = e;
    else
      p->empty_element = NULL;

    if ( p->on_begin_element )
      rc = (*p->on_begin_element)(p, e, atts.count, atts.attributes);

    clear_attribute_list(&atts);

    if ( p->empty_element )
    { p->empty_element = NULL;
      close_element(p, e, conref);
      if ( conref )	/* might be S_CDATA due to declared content */
	p->cdata_state = p->state = S_PCDATA;
    }

    return rc;
  }

  return gripe(p, ERC_SYNTAX_ERROR, L"Bad open-element tag", decl);
}


static int
process_end_element(dtd_parser *p, const ichar *decl)
{ dtd *dtd = p->dtd;
  dtd_symbol *id;
  const ichar *s;

  emit_cdata(p, TRUE);
  if ( (s=itake_name(p, decl, &id)) && *s == '\0' )
    return close_element(p, find_element(dtd, id), FALSE);

  if ( p->dtd->shorttag && *decl == '\0' ) /* </>: close current element */
    return close_current_element(p);

  return gripe(p, ERC_SYNTAX_ERROR, L"Bad close-element tag", decl);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
process_net(dtd_parser *p)
    We've seen a / of a shorttag element.  Close this one.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
process_net(dtd_parser *p)
{ sgml_environment *env;

  prepare_cdata(p);
  for(env = p->environments; env; env=env->parent)
  { if ( env->wants_net )
    { sgml_environment *parent;

      pop_to(p, env, NULL);		/* close parents */
      validate_completeness(p, env);
      parent = env->parent;

      emit_cdata(p, TRUE);
      p->first = FALSE;

      if ( p->on_end_element )
      { WITH_CLASS(p, EV_SHORTTAG,
		   (*p->on_end_element)(p, env->element));
      }

      free_environment(env);
      p->environments = parent;
      p->map = (parent ? parent->map : NULL);

      return TRUE;
    }
  }

  return FALSE;
}


static int				/* <!DOCTYPE ...> */
process_doctype(dtd_parser *p, const ichar *decl, const ichar *decl0)
{ dtd *dtd = p->dtd;
  dtd_symbol *id;
  const ichar *s;
  dtd_entity *et = NULL;

  if ( !(s=itake_name(p, decl, &id)) )
    return gripe(p, ERC_SYNTAX_ERROR, L"Name expected", decl);
  decl = s;

  if ( (s=isee_identifier(dtd, decl, "system")) )
  { et = sgml_calloc(1, sizeof(*et));
    et->type = ET_SYSTEM;
    decl = s;
  } else if ( (s=isee_identifier(dtd, decl, "public")) )
  { et = sgml_calloc(1, sizeof(*et));
    et->type = ET_PUBLIC;
    decl = s;
  } else if ( isee_func(dtd, decl, CF_DSO) )
    goto local;				/* <!DOCTYPE type [ ... */

  if ( et )
  { et->name = id;
    et->catalog_location = CAT_DOCTYPE;
    if ( !(s=process_entity_value_declaration(p, decl, et)) )
      return FALSE;
    decl = s;
  }

  if ( !dtd->doctype )			/* i.e. anonymous DTD */
  { ichar *file;

    dtd->doctype = istrdup(id->name);	/* Fill it */
    if ( et )
      file = entity_file(dtd, et);
    else
      file = istrdup(find_in_catalogue(CAT_DOCTYPE,
				       dtd->doctype, NULL, NULL,
				       IS_XML_DIALECT(dtd->dialect)));

    if ( !file )
    { gripe(p, ERC_EXISTENCE, L"DTD", dtd->doctype);
    } else if ( !is_url(file) )
    { dtd_parser *clone = clone_dtd_parser(p);

      if ( !load_dtd_from_file(clone, file) )
	gripe(p, ERC_EXISTENCE, L"file", file);
      free_dtd_parser(clone);
      sgml_free(file);
    }
  }

  if ( et )
    free_entity_list(et);

local:
  if ( (s=isee_func(dtd, decl, CF_DSO)) ) /* [...] */
  { int grouplevel = 1;
    data_mode oldmode   = p->dmode;
    dtdstate  oldstate  = p->state;
    int	      olddecode = p->utf8_decode;
    locbuf oldloc;
    const ichar *q;
    icharbuf *saved_ibuf = p->buffer;

    push_location(p, &oldloc);
					/* try to find start-location. */
					/* fails if there is comment before */
					/* the []! */
    sgml_cplocation(&p->location, &p->startloc);
    inc_location(&p->location, '<');
    for(q=decl0; q < s; q++)
      inc_location(&p->location, *q);
    p->dmode = DM_DTD;
    p->state = S_PCDATA;
    p->buffer = new_icharbuf(p->max_memory);
    p->utf8_decode = FALSE;

    for( ; *s; s++ )
    { if ( isee_func(dtd, s, CF_LIT) ||	/* skip quoted strings */
	   isee_func(dtd, s, CF_LITA) )
      { ichar q = *s;

	putchar_dtd_parser(p, *s++);	/* pass open quote */

	for( ; *s && *s != q; s++ )
	  putchar_dtd_parser(p, *s);

	if ( *s == q )			/* pass closing quote */
	  putchar_dtd_parser(p, *s);
	continue;
      }

      if ( isee_func(dtd, s, CF_DSO) )
	grouplevel++;
      else if ( isee_func(dtd, s, CF_DSC) && --grouplevel == 0 )
	break;
      putchar_dtd_parser(p, *s);
    }
    p->dtd->implicit = FALSE;

    p->state       = oldstate;
    p->dmode       = oldmode;
    p->utf8_decode = olddecode;
    free_icharbuf(p->buffer);
    p->buffer = saved_ibuf;
    pop_location(p, &oldloc);
  }

  p->enforce_outer_element = id;	/* make this the outer element */

  return TRUE;
}


static void
init_decoding(dtd_parser *p)
{
#ifdef UTF8
  int decode;
  dtd *dtd = p->dtd;

  if ( dtd->encoding == SGML_ENC_UTF8 &&
       p->encoded    == TRUE )
    decode = TRUE;
  else
    decode = FALSE;

  if ( p->utf8_decode != decode )
  { DEBUG(fprintf(stderr, "%s UTF-8 decoding on %p\n",
		  decode ? "Enable" : "Disable",
		  p));

    p->utf8_decode = decode;
  }
#endif
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
xml_set_encoding() is the public interface to   set the encoding for the
parser.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int				/* strcasecmp() with C locale */
posix_strcasecmp(const char *s1, const char *s2)
{ for(; *s1 && *s2; s1++, s2++)
  { int c1 = *s1&0xff;
    int c2 = *s2&0xff;

    if ( c1 >= 'A' && c1 <= 'Z' ) c1 += 'a'-'A';
    if ( c2 >= 'A' && c2 <= 'Z' ) c2 += 'a'-'A';

    if ( c1 != c2 )
      return c1-c2;
  }

  return *s1 - *s2;
}


int
xml_set_encoding(dtd_parser *p, const char *enc)
{ dtd *dtd = p->dtd;

  if ( posix_strcasecmp(enc, "iso-8859-1") == 0 )
  { dtd->encoding = SGML_ENC_ISO_LATIN1;
  } else if ( posix_strcasecmp(enc, "us-ascii") == 0 )
  { dtd->encoding = SGML_ENC_ISO_LATIN1;	/* doesn't make a difference */
  } else if ( posix_strcasecmp(enc, "utf-8") == 0 )
  { dtd->encoding = SGML_ENC_UTF8;
  } else
    return FALSE;

  init_decoding(p);
  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
set_encoding() sets the encoding from the encoding="..." field of the
XML header.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
set_encoding(dtd_parser *p, const ichar *enc)
{ char buf[32];
  char *e = buf+sizeof(buf)-1;
  char *o;
  const ichar *i;

  for(i=enc, o=buf; *i; )
  { if ( *i < 128 && o < e )
    { *o++ = (char)*i++;
    } else
    { goto error;
    }
  }
  *o = '\0';

  if ( !xml_set_encoding(p, buf) )
  { error:
    gripe(p, ERC_EXISTENCE, L"character encoding", enc);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Process <? ... ?>

Should deal with character encoding for XML documents.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
process_pi(dtd_parser *p, const ichar *decl)
{ const ichar *s;
  dtd *dtd = p->dtd;

  if ( (s=isee_identifier(dtd, decl, "xml")) ) /* <?xml version="1.0"?> */
  { decl = s;

    switch(dtd->dialect)
    { case DL_SGML:
	set_dialect_dtd(dtd, DL_XML);
        break;
      case DL_HTML:
	set_dialect_dtd(dtd, DL_XHTML);
        break;
      case DL_HTML5:
	set_dialect_dtd(dtd, DL_XHTML5);
        break;
      case DL_XHTML:
      case DL_XHTML5:
      case DL_XML:
      case DL_XMLNS:
	break;
    }

    while(*decl)
    { dtd_symbol *nm;

      if ( (s=itake_name(p, decl, &nm)) &&
	   (s=isee_func(dtd, s, CF_VI)) )		/* = */
      { ichar *start;
	int len;
	ichar buf[MAXSTRINGLEN];
	const ichar *end;

	if ( !(end=itake_string(dtd, s, &start, &len)) )
	{ end=itake_nmtoken_chars(p, s, buf, sizeof(buf)/sizeof(ichar));
	  start = buf;
	  len = (int)istrlen(buf);
	}

	if ( end )
	{ decl = end;

	  if ( istrcaseeq(nm->name, L"encoding") )
	  { ichar tmp[32];

	    if ( len < (int)(sizeof(tmp)/sizeof(ichar)-1) )
	    { istrncpy(tmp, start, len);
	      tmp[len] = 0;

	      set_encoding(p, tmp);
	    } else
	    { gripe(p, ERC_SYNTAX_ERROR, L"Unterminated encoding?", decl);
	    }
	  }

	  /* fprintf(stderr, "XML %s = %s\n", nm->name, buf); */

	  continue;
	}
      }

      gripe(p, ERC_SYNTAX_ERROR, L"Illegal XML parameter", decl);
      break;
    }

    return TRUE;
  }

  if ( p->on_pi )
    (*p->on_pi)(p, decl);

  return FALSE;				/* Warn? */
}


static int
process_sgml_declaration(dtd_parser *p, const ichar *decl)
{ return gripe(p, ERC_SYNTAX_WARNING, L"Ignored <!SGML ...> declaration", NULL);
}


static int
process_declaration(dtd_parser *p, const ichar *decl)
{ const ichar *s;
  dtd *dtd = p->dtd;

  if ( p->dmode != DM_DTD )
  { if ( (s=isee_func(dtd, decl, CF_ETAGO2)) ) /* </ ... > */
    { return process_end_element(p, s);
    } else if ( HasClass(dtd, *decl, CH_NAME) ) /* <letter */
    { return process_begin_element(p, decl);
    }
  }

  if ( (s=isee_func(dtd, decl, CF_MDO2)) ) /* <! ... >*/
  { decl = s;

    if ( p->on_decl )
      (*p->on_decl)(p, decl);

    if ( (s = isee_identifier(dtd, decl, "entity")) )
      process_entity_declaration(p, s);
    else if ( (s = isee_identifier(dtd, decl, "element")) )
      process_element_declaraction(p, s);
    else if ( (s = isee_identifier(dtd, decl, "attlist")) )
      process_attlist_declaraction(p, s);
    else if ( (s = isee_identifier(dtd, decl, "notation")) )
      process_notation_declaration(p, s);
    else if ( (s = isee_identifier(dtd, decl, "shortref")) )
      process_shortref_declaration(p, s);
    else if ( (s = isee_identifier(dtd, decl, "usemap")) )
      process_usemap_declaration(p, s);
    else if ( (s = isee_identifier(dtd, decl, "sgml")) )
      process_sgml_declaration(p, s);
    else if ( (s = isee_identifier(dtd, decl, "doctype")) )
    { if ( p->dmode != DM_DTD )
	process_doctype(p, s, decl-1);
    } else
    { s = iskip_layout(dtd, decl);

      if ( *s )
	gripe(p, ERC_SYNTAX_ERROR, L"Invalid declaration", s);
    }

    return TRUE;
  }

  return gripe(p, ERC_SYNTAX_ERROR, L"Invalid declaration", decl);
}

		 /*******************************
		 *	  STREAM BINDING	*
		 *******************************/

void
set_file_dtd_parser(dtd_parser *p, input_type type, const ichar *name)
{ p->location.type      = type;
  p->location.name.file = name;
  p->location.line      = 1;
  p->location.linepos   = 0;
  p->location.charpos   = 0;
}


static void
set_src_dtd_parser(dtd_parser *p, input_type type, const ichar *name)
{ p->location.type        = type;
  p->location.name.entity = name;
  p->location.line        = 1;
  p->location.linepos     = 0;
  p->location.charpos     = 0;
}


void
set_mode_dtd_parser(dtd_parser *p, data_mode m)
{ p->dmode = m;				/* DM_DTD or DM_DATA */
  p->state = S_PCDATA;
  p->blank_cdata = TRUE;
}


dtd_parser *
new_dtd_parser(dtd *dtd)
{ dtd_parser *p = sgml_calloc(1, sizeof(*p));

  if ( !dtd )
    dtd = new_dtd(NULL);
  dtd->references++;

  p->magic       = SGML_PARSER_MAGIC;
  p->dtd	 = dtd;
  p->state	 = S_PCDATA;
  p->mark_state	 = MS_INCLUDE;
  p->dmode       = DM_DTD;
  p->encoded	 = TRUE;		/* encoded octet stream */
  p->buffer	 = new_icharbuf(0);
  p->cdata	 = new_ocharbuf(0);
  p->event_class = EV_EXPLICIT;
  set_src_dtd_parser(p, IN_NONE, NULL);

  return p;
}


static dtd_parser *
clone_dtd_parser(dtd_parser *p)
{ dtd_parser *clone = sgml_calloc(1, sizeof(*p));

  *clone = *p;
  clone->dtd->references++;
  clone->environments =	NULL;
  clone->marked	      =	NULL;
  clone->etag	      =	NULL;
  clone->grouplevel   =	0;
  clone->state	      =	S_PCDATA;
  clone->mark_state   =	MS_INCLUDE;
  clone->dmode	      =	DM_DTD;
  clone->buffer	      =	new_icharbuf(clone->max_memory);
  clone->cdata	      =	new_ocharbuf(clone->max_memory);

  return clone;
}


void
free_dtd_parser(dtd_parser *p)
{ free_icharbuf(p->buffer);
  free_ocharbuf(p->cdata);
#ifdef XMLNS
  xmlns_free(p->xmlns);
#endif
  free_dtd(p->dtd);

  sgml_free(p);
}


static int
process_chars(dtd_parser *p, input_type in, const ichar *name, const ichar *s)
{ locbuf old;

  push_location(p, &old);
  set_src_dtd_parser(p, in, name);
  empty_icharbuf(p->buffer);		/* dubious */
  for(; *s; s++)
    putchar_dtd_parser(p, *s);
  pop_location(p, &old);

  return TRUE;
}


static int
process_include(dtd_parser *p, const ichar *entity_name)
{ dtd_symbol *id;
  dtd_entity *pe;
  dtd *dtd = p->dtd;

  if ( (id=dtd_find_entity_symbol(dtd, entity_name)) &&
       (pe=find_pentity(p->dtd, id)) )
  { ichar *file;

    if ( (file = entity_file(dtd, pe)) )
    { int rc = sgml_process_file(p, file, SGML_SUB_DOCUMENT);
      sgml_free(file);

      return rc;
    } else
    { const ichar *text = entity_value(p, pe, NULL);

      if ( !text )
	return gripe(p, ERC_NO_VALUE, pe->name->name);

      return process_chars(p, IN_ENTITY, entity_name, text);
    }
  }

  return gripe(p, ERC_EXISTENCE, L"parameter entity", entity_name);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Process <![ KEYWORD [

Switches ->mark_state according to KEYWORD. Processes the rest in normal
S_PCDATA style, which pops the mark-stack on seeing ]]>

For the purpose of <!DOCTYPE spec [additions]> we switch to S_GROUP if
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
process_marked_section(dtd_parser *p)
{ ichar buf[MAXDECL];
  dtd *dtd = p->dtd;
  const ichar *decl = p->buffer->data;
  const ichar *s;

  if ( (decl=isee_func(dtd, decl, CF_MDO2)) && /* ! */
       (decl=isee_func(dtd, decl, CF_DSO)) && /* [ */
       expand_pentities(p, decl, ZERO_TERM_LEN, buf, sizeof(buf)/sizeof(ichar)) )
  { dtd_symbol *kwd;

    decl = buf;
    if ( (s=itake_name(p, decl, &kwd)) &&
	 isee_func(dtd, s, CF_DSO) )	/* [ */
    { dtd_marked *m = sgml_calloc(1, sizeof(*m));

      m->keyword = kwd;			/* push on the stack */
      m->parent = p->marked;
      p->marked = m;

      if ( istrcaseeq(kwd->name, L"IGNORE") )
	m->type = MS_IGNORE;
      else if ( istrcaseeq(kwd->name, L"INCLUDE") )
	m->type = MS_INCLUDE;
      else if ( istrcaseeq(kwd->name, L"TEMP") )
	m->type = MS_INCLUDE;
      else if ( istrcaseeq(kwd->name, L"CDATA") )
	m->type = MS_CDATA;
      else if ( istrcaseeq(kwd->name, L"RCDATA") )
	m->type = MS_RCDATA;
      else
	m->type = MS_INCLUDE;		/* default */

      empty_icharbuf(p->buffer);
      if ( m->type == MS_CDATA )
	p->state = S_MSCDATA;
      else
	p->state = S_PCDATA;
      if ( p->mark_state != MS_IGNORE )
	p->mark_state = m->type;
    }
  } else
  { decl = p->buffer->data;

    if ( (decl=isee_func(dtd, decl, CF_MDO2)) && /* ! */
	 !isee_func(dtd, decl, CF_DSO) ) /* [ */
    { p->state = S_GROUP;
      p->grouplevel = 1;
    }
  }
}


static void
pop_marked_section(dtd_parser *p)
{ dtd_marked *m = p->marked;

  if ( m )
  { p->marked = m->parent;
    sgml_free(m);
    p->mark_state = (p->marked ? p->marked->type : MS_INCLUDE);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Update the space-mode for the current element.  The space mode defines
how spaces are handled in the CDATA output.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static dtd_space_mode
istr_to_space_mode(const ichar *val)
{ if ( istreq(val, L"default") )
    return SP_DEFAULT;
  if ( istreq(val, L"preserve") )
    return SP_PRESERVE;
  if ( istreq(val, L"sgml") )
    return SP_SGML;
  if ( istreq(val, L"remove") )
    return SP_REMOVE;

  return SP_INHERIT;			/* interpret as error */
}


static void
update_space_mode(dtd_parser *p, dtd_element *e,
		  int natts, sgml_attribute *atts)
{ for( ; natts-- > 0; atts++ )
  { const ichar *name = atts->definition->name->name;

    if ( istreq(name, L"xml:space") &&
	 atts->definition->type == AT_CDATA &&
	 atts->value.textW )
    { dtd_space_mode m = istr_to_space_mode(atts->value.textW);

      if ( m != SP_INHERIT )
	p->environments->space_mode = m;
      else
	gripe(p, ERC_EXISTENCE, L"xml:space-mode", atts->value.textW);

      return;
    }
  }

  if ( e->space_mode != SP_INHERIT )
    p->environments->space_mode = e->space_mode;
}


static void
empty_cdata(dtd_parser *p)
{ if ( p->dmode == DM_DATA )
  { empty_ocharbuf(p->cdata);
    p->blank_cdata = TRUE;
    p->cdata_must_be_empty = FALSE;
  }
}


static void
cb_cdata(dtd_parser *p, ocharbuf *buf, int offset, int size)
{ if ( p->on_data )
    (*p->on_data)(p, EC_CDATA, size, buf->data.w+offset);
}


static int
emit_cdata(dtd_parser *p, int last)
{ dtd *dtd = p->dtd;
  locbuf locsafe;
  ocharbuf *cdata = p->cdata;
  int offset = 0;
  int size = cdata->size;

  if ( size == 0 )
    return TRUE;			/* empty or done */

  push_location(p, &locsafe);
  sgml_cplocation(&p->location, &p->startloc);   /* start of markup */
  sgml_cplocation(&p->startloc, &p->startcdata); /* real start of CDATA */

  if ( p->environments )
  { switch(p->environments->space_mode)
    { case SP_SGML:
      case SP_DEFAULT:
	if ( p->first )
	{ wint_t c = fetch_ocharbuf(cdata, offset);

	  if ( HasClass(dtd, c, CH_RE) )
	  { inc_location(&p->startloc, c);
	    offset++;
	    size--;
	    c = fetch_ocharbuf(cdata, offset);
	  }

	  if ( HasClass(dtd, c, CH_RS) )
	  { inc_location(&p->startloc, c);
	    offset++;
	    size--;
	  }
	}
	if ( last && size > 0 )
	{ wint_t c = fetch_ocharbuf(cdata, offset+size-1);

	  if ( HasClass(dtd, c, CH_RS) )
	  { dec_location(&p->location, c);
	    size--;
	    poke_ocharbuf(cdata, offset+size, '\0');
	    if ( size > 0 )
	      c = fetch_ocharbuf(cdata, offset+size-1);
	    else
	      c = 0;			/* HasClass(CH_RE) must fail */
	  }
	  if ( HasClass(dtd, c, CH_RE) )
	  { dec_location(&p->location, c);
	    size--;
	    poke_ocharbuf(cdata, offset+size, '\0');
	  }
	}
	if ( p->environments->space_mode == SP_DEFAULT )
	{ int o = 0;
	  int i;

	  for(i=0; i<size; i++)
	  { wint_t c = fetch_ocharbuf(cdata, offset+i);

	    if ( HasClass(dtd, c, CH_BLANK) )
	    { for(i++; i<size; i++)
	      { wint_t c = fetch_ocharbuf(cdata, offset+i);

		if ( !HasClass(dtd, c, CH_BLANK) )
		  break;
	      }
	      i--;
	      poke_ocharbuf(cdata, o++, ' ');
	      continue;
	    }
	    poke_ocharbuf(cdata, o++, c);
	  }
	  poke_ocharbuf(cdata, o, '\0');
	  offset = 0;			/* wrote new output from offset=0 */
	  size = o;
	}
	break;
      case SP_REMOVE:
      { int o = 0;
	int i;
	int end = 0;

	for(i=0; i<size; i++)
	{ wint_t c = fetch_ocharbuf(cdata, offset+i);

	  if ( HasClass(dtd, c, CH_BLANK) )
	    inc_location(&p->startloc, c);
	  else
	    break;
	}

	if ( i<size )
	{ for(; i<size; i++)
	  { wint_t c = fetch_ocharbuf(cdata, offset+i);

	    if ( HasClass(dtd, c, CH_BLANK) )
	    { i++;

	      while(i<size && HasClass(dtd,
				       (wint_t)fetch_ocharbuf(cdata, offset+i),
				       CH_BLANK))
		i++;
	      i--;
	      poke_ocharbuf(cdata, o++, ' ');
	      continue;
	    }
	    poke_ocharbuf(cdata, o++, c);
	    end = o;
	  }
	}
					/* TBD: adjust end */
	poke_ocharbuf(cdata, end, '\0');
	size = end;
	break;
      }
      case SP_PRESERVE:
	break;
      case SP_INHERIT:
	assert(0);
	return FALSE;
    }
  }

  if ( size == 0 )
  { pop_location(p, &locsafe);
    empty_cdata(p);

    return TRUE;
  }

  assert(size > 0);

  if ( !p->blank_cdata )
  { if ( p->cdata_must_be_empty )
    { gripe(p, ERC_NOT_ALLOWED_PCDATA, p->cdata); /* TBD: now passes buffer! */
    }
    cb_cdata(p, cdata, offset, size);
  } else if ( p->environments )
  { sgml_environment *env = p->environments;
    dtd_state *new;

				/* If an element is not in the DTD we must */
				/* assume mixed content and emit spaces */

    if ( (new=make_dtd_transition(env->state, CDATA_ELEMENT)) )
    { env->state = new;
      cb_cdata(p, cdata, offset, size);
    } else if ( env->element->undefined &&
		p->environments->space_mode == SP_PRESERVE )
    { cb_cdata(p, cdata, offset, size);
    }
  }

  pop_location(p, &locsafe);

  empty_cdata(p);

  return TRUE;
}


static int
prepare_cdata(dtd_parser *p)
{ if ( p->cdata->size == 0 )
    return TRUE;

  terminate_ocharbuf(p->cdata);

  if ( p->mark_state == MS_INCLUDE )
  { dtd *dtd = p->dtd;

    if ( p->environments )		/* needed for <img> <img> */
    { dtd_element *e = p->environments->element;

      if ( e->structure && e->structure->type == C_EMPTY && !e->undefined )
	close_element(p, e, FALSE);
    }

    if ( p->blank_cdata == TRUE )
    { int blank = TRUE;
      int i;

      for(i=0; i<p->cdata->size; i++)
      { wint_t c = fetch_ocharbuf(p->cdata, i);

	if ( !HasClass(dtd, c, CH_BLANK) )
	{ blank = FALSE;
	  break;
	}
      }

      p->blank_cdata = blank;
      if ( !blank )
      { if ( p->dmode == DM_DTD )
	  gripe(p, ERC_SYNTAX_ERROR, L"CDATA in DTD", p->cdata->data);
	else
	  open_element(p, CDATA_ELEMENT, TRUE);
      }
    }
  }

  return TRUE;
}


static int
process_cdata(dtd_parser *p, int last)
{ prepare_cdata(p);

  return emit_cdata(p, last);
}


static int
process_entity(dtd_parser *p, const ichar *name)
{ if ( name[0] == '#' )			/* #charcode: character entity */
  { int v = char_entity_value(name);

    if ( v <= 0 )
      return gripe(p, ERC_SYNTAX_ERROR, L"Bad character entity", name);

    add_ocharbuf(p->cdata, v);
  } else
  { dtd_symbol *id;
    dtd_entity *e;
    dtd *dtd = p->dtd;
    int len;
    const ichar *text;
    const ichar *s;
    int   chr;
    ichar *file;

    if ( !(id=dtd_find_entity_symbol(dtd, name)) ||
	 !(e=id->entity) )
    { if ( dtd->default_entity )
	e = dtd->default_entity;
      else
	return gripe(p, ERC_EXISTENCE, L"entity", name);
    }

    if ( !e->value &&
	 e->content == EC_SGML &&
	 (file=entity_file(p->dtd, e)) )
    { int rc;

      if ( dtd->system_entities )
      { empty_icharbuf(p->buffer);		/* dubious */
	rc = sgml_process_file(p, file, SGML_SUB_DOCUMENT);
      } else
      { gripe(p, ERC_ET_SYSTEM, file);
	rc = TRUE;
      }
      sgml_free(file);
      return rc;
    }

    if ( !(text = entity_value(p, e, &len)) )
      return gripe(p, ERC_NO_VALUE, e->name->name);

    switch ( e->content )
    { case EC_SGML:
      case EC_CDATA:
	if ( (s=isee_character_entity(dtd, text, &chr)) && *s == '\0' )
	{ if ( chr == 0 )
	    return gripe(p, ERC_SYNTAX_ERROR, L"Illegal character entity", text);

	  if ( p->blank_cdata == TRUE &&
	       !HasClass(dtd, (wint_t)chr, CH_BLANK) )
	  { p->cdata_must_be_empty = !open_element(p, CDATA_ELEMENT, FALSE);
	    p->blank_cdata = FALSE;
	  }

	  add_ocharbuf(p->cdata, chr);
	  return TRUE;
	}
	if ( e->content == EC_SGML )
	{ locbuf oldloc;
	  int decode = p->utf8_decode;

	  push_location(p, &oldloc);
	  p->utf8_decode = FALSE;
	  set_src_dtd_parser(p, IN_ENTITY, e->name->name);
	  empty_icharbuf(p->buffer);		/* dubious */
	  for(s=text; *s; s++)
	    putchar_dtd_parser(p, *s);
	  p->utf8_decode = decode;
	  pop_location(p, &oldloc);
	} else if ( *text )
	{ const ichar *o;

	  if ( p->blank_cdata == TRUE )
	  { p->cdata_must_be_empty = !open_element(p, CDATA_ELEMENT, FALSE);
	    p->blank_cdata = FALSE;
	  }

	  for(o=text; *o; o++)
	    add_ocharbuf(p->cdata, *o);
	}
	break;
      case EC_SDATA:
      case EC_NDATA:
	process_cdata(p, FALSE);
	if ( p->on_data )
	  (*p->on_data)(p, e->content, len, text);
	break;
      case EC_PI:
	process_cdata(p, FALSE);
	if ( p->on_pi )
	  (*p->on_pi)(p, text);
      case EC_STARTTAG:
#if 0
	prepare_cdata(p);
	process_begin_element(p, text);
#endif
	break;
      case EC_ENDTAG:
#if 0
	prepare_cdata(p);
	process_end_element(p, text);
#endif
	break;
    }

    return TRUE;
  }

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Deal with end of input.  We should give a proper error message depending
on the state and the start-location of the error.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
end_document_dtd_parser(dtd_parser *p)
{ int rval;

  switch(p->state)
  { case S_RCDATA:
    case S_CDATA:
    case S_PCDATA:
      rval = TRUE;
      break;
    case S_CMT:
    case S_CMT1:
    case S_CMTE0:
    case S_CMTE1:
    case S_DECLCMT0:
    case S_DECLCMT:
    case S_DECLCMTE0:
      rval = gripe(p, ERC_SYNTAX_ERROR,
		   L"Unexpected end-of-file in comment", L"");
      break;
    case S_ECDATA1:
    case S_ECDATA2:
    case S_EMSC1:
    case S_EMSC2:
    case S_DECL0:
    case S_DECL:
    case S_MDECL0:
    case S_STRING:
    case S_CMTO:
    case S_GROUP:
    case S_PENT:
    case S_ENT:
    case S_ENT0:
      rval = gripe(p, ERC_SYNTAX_ERROR,
		   L"Unexpected end-of-file", L"");
      break;
#ifdef UTF8
    case S_UTF8:
      rval = gripe(p, ERC_SYNTAX_ERROR,
		   L"Unexpected end-of-file in UTF-8 sequence", L"");
      break;
#endif
    case S_MSCDATA:
    case S_EMSCDATA1:
    case S_EMSCDATA2:
      rval = gripe(p, ERC_SYNTAX_ERROR,
		   L"Unexpected end-of-file in CDATA marked section", L"");
      break;
    case S_PI:
    case S_PI2:
      rval = gripe(p, ERC_SYNTAX_ERROR,
		   L"Unexpected end-of-file in processing instruction", L"");
      break;
    default:
      rval = gripe(p, ERC_SYNTAX_ERROR,
		   L"Unexpected end-of-file in ???", L"");
      break;
  }

  if ( p->dmode == DM_DATA )
  { sgml_environment *env;

    if ( p->cdata->size > 0 &&
	 fetch_ocharbuf(p->cdata, p->cdata->size-1) == CR )
      del_ocharbuf(p->cdata);

    process_cdata(p, TRUE);

    if ( (env=p->environments) )
    { dtd_element *e;

      while(env->parent)
	env = env->parent;

      pop_to(p, env, CDATA_ELEMENT);
      e = env->element;
      if ( e->structure && !e->structure->omit_close )
	gripe(p, ERC_OMITTED_CLOSE, e->name->name);
      close_element(p, e, FALSE);
    }
  }

  return rval;
}


int
begin_document_dtd_parser(dtd_parser *p)
{ init_decoding(p);

  return TRUE;
}


void
reset_document_dtd_parser(dtd_parser *p)
{ if ( p->environments )
  { sgml_environment *env, *parent;

    for(env = p->environments; env; env=parent)
    { parent = env->parent;

      free_environment(env);
    }

    p->environments = NULL;
  }

  while(p->marked)
    pop_marked_section(p);

  empty_icharbuf(p->buffer);
  empty_ocharbuf(p->cdata);

  p->mark_state	   = MS_INCLUDE;
  p->state	   = S_PCDATA;
  p->grouplevel	   = 0;
  p->blank_cdata   = TRUE;
  p->event_class   = EV_EXPLICIT;
  p->dmode	   = DM_DATA;

  begin_document_dtd_parser(p);
}



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Set the UTF-8 state
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef UTF8
static void
process_utf8(dtd_parser *p, int chr)
{ int bytes;
  int mask;

  for( bytes=1, mask=0x20; chr&mask; bytes++, mask >>= 1 )
    ;
  mask--;				/* 0x20 --> 0x1f */

  p->utf8_saved_state = p->state;		/* state to return to */
  p->state = S_UTF8;
  p->utf8_char = chr & mask;
  p->utf8_left = bytes;
}
#endif


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
add_cdata() adds a character to the output  data. It also maps \r\n onto
a single \n for Windows newline conventions.

There is a problem here in shortref  handling. We open the CDATA_ELEMENT
as soon as we find a character as   this may open other elements through
omitted tags and thus install a new shortref map.

If, at a later stage, all CDATA read sofar turns out to be a shortref we
have  incorrectly  opened   the   CDATA_ELEMENT.    As   `undoing'   the
open_element() is not an option (it may  already have caused `events' on
omitted tags) we are in trouble.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
add_cdata(dtd_parser *p, int chr)
{ if ( p->mark_state == MS_INCLUDE )
  { ocharbuf *buf = p->cdata;

    if ( p->blank_cdata == TRUE &&
	 !HasClass(p->dtd, (wint_t)chr, CH_BLANK) )
    { p->cdata_must_be_empty = !open_element(p, CDATA_ELEMENT, FALSE);
      p->blank_cdata = FALSE;
    }

    if ( chr == '\n' )			/* insert missing CR */
    { int sz;

      if ( (sz=buf->size) == 0 ||
	   fetch_ocharbuf(buf, sz-1) != CR )
	add_cdata(p, CR);
    }

    add_ocharbuf(buf, chr);

    if ( p->map &&
	 chr <= 0xff && p->map->ends[chr] &&
	 match_shortref(p) )
      return;

    if ( chr == '\n' )			/* dubious.  Whould we do that */
    { int sz;				/* here or in space-handling? */

      if ( (sz=buf->size) > 1 &&
	   fetch_ocharbuf(buf, sz-1) == LF &&
	   fetch_ocharbuf(buf, sz-2) == CR )
      { poke_ocharbuf(buf, sz-2, LF);
	buf->size--;
      }
    }
  }
}


static void
add_verbatim_cdata(dtd_parser *p, int chr)
{ if ( p->mark_state != MS_IGNORE )
  { ocharbuf *buf = p->cdata;

    if ( p->blank_cdata == TRUE &&
	 !HasClass(p->dtd, (wint_t)chr, CH_BLANK) )
    { p->cdata_must_be_empty = !open_element(p, CDATA_ELEMENT, FALSE);
      p->blank_cdata = FALSE;
    }

    if ( chr == '\n' && buf->size > 0 &&
	 fetch_ocharbuf(buf, buf->size-1) == '\r' )
      buf->size--;

    add_ocharbuf(buf, chr);
  }
}


/* We discovered illegal markup and now process it as normal CDATA
*/

static void
recover_parser(dtd_parser *p)
{ const ichar *s;

  terminate_icharbuf(p->buffer);
  add_cdata(p, p->saved);
  for(s=p->buffer->data; *s; s++)
    add_cdata(p, *s);
  p->state = S_PCDATA;
}


static inline void
setlocation(dtd_srcloc *d, dtd_srcloc *loc, int line, int lpos)
{ d->line    = line;
  d->linepos = lpos;
  d->charpos = loc->charpos - 1;
  d->type    = loc->type;
  d->name    = loc->name;
}


int
putchar_dtd_parser(dtd_parser *p, int chr)
{ dtd *dtd = p->dtd;
  const ichar *f = dtd->charfunc->func;
  int line = p->location.line;
  int lpos = p->location.linepos;

  p->location.charpos++;		/* TBD: actually `bytepos' */

  if ( p->buffer->limit_reached )
  { return gripe(p, ERC_RESOURCE, L"input buffer");
  }
  if ( p->cdata->limit_reached )
  { return gripe(p, ERC_RESOURCE, L"CDATA buffer");
  }

#ifdef UTF8
  if ( p->state == S_UTF8 )
  { if ( (chr & 0xc0) != 0x80 )	/* TBD: recover */
      gripe(p, ERC_SYNTAX_ERROR, L"Bad UTF-8 sequence", L"");
    p->utf8_char <<= 6;
    p->utf8_char |= (chr & ~0xc0);
    if ( --p->utf8_left == 0 )
    { chr = p->utf8_char;
      p->state = p->utf8_saved_state;
    } else
    { return TRUE;
    }
  } else if ( ISUTF8_MB(chr) && p->utf8_decode )
  { process_utf8(p, chr);
    return TRUE;
  }
#endif

  if ( f[CF_RS] == chr )
  { p->location.line++;
    p->location.linepos = 0;
  } else
  { if ( f[CF_RE] == chr )
      p->location.linepos = 0;
    else
      p->location.linepos++;
  }

reprocess:
  switch(p->state)
  { case S_PCDATA:
    { if ( f[CF_MDO1] == chr )		/* < */
      { setlocation(&p->startloc, &p->location, line, lpos);
	p->state = S_DECL0;
	empty_icharbuf(p->buffer);
	return TRUE;
      }
      if ( p->dmode == DM_DTD )
      { if ( f[CF_PERO] == chr )	/* % */
	{ setlocation(&p->startloc, &p->location, line, lpos);
	  p->state = S_PENT;
	  return TRUE;
	}
      } else
      { if ( f[CF_ERO] == chr )		/* & */
	{ setlocation(&p->startloc, &p->location, line, lpos);
	  p->state = S_ENT0;
	  return TRUE;
	}
      }

      if ( p->marked && f[CF_DSC] == chr ) /* ] in marked section */
      { empty_icharbuf(p->buffer);
	p->state = S_EMSC1;
	p->saved = chr;			/* for recovery */
	return TRUE;
      }

      if ( p->waiting_for_net && f[CF_ETAGO2] == chr ) /* shorttag */
      { p->waiting_for_net = FALSE;
	setlocation(&p->startloc, &p->location, line, lpos);
	process_net(p);
	return TRUE;
      }

					/* Real character data */
      if ( p->cdata->size == 0 )
        setlocation(&p->startcdata, &p->location, line, lpos);

      add_cdata(p, chr);
      return TRUE;
    }
    case S_ECDATA2:			/* Seen </ in CDATA/RCDATA */
    { if ( f[CF_MDC] == chr &&
	   p->etaglen == p->buffer->size &&
	   istrncaseeq(p->buffer->data, p->etag, p->etaglen) )
      { p->cdata->size -= p->etaglen+2;	/* 2 for </ */
	terminate_ocharbuf(p->cdata);
	terminate_icharbuf(p->buffer);
	if ( p->mark_state == MS_INCLUDE )
	{ process_cdata(p, TRUE);
	  process_end_element(p, p->buffer->data);
	  empty_cdata(p);
	}
	empty_icharbuf(p->buffer);
	p->cdata_state = p->state = S_PCDATA;
      } else
      { add_verbatim_cdata(p, chr);
	if ( p->etaglen < p->buffer->size ||
	     !HasClass(dtd, (wint_t)chr, CH_NAME))
	{ empty_icharbuf(p->buffer);	/* mismatch */
	  p->state = p->cdata_state;
	} else
	  add_icharbuf(p->buffer, chr);
      }
      return TRUE;
    }
    case S_ECDATA1:			/* seen < in CDATA */
    { add_verbatim_cdata(p, chr);
      if ( f[CF_ETAGO2] == chr )	/* / */
      { empty_icharbuf(p->buffer);
	p->state = S_ECDATA2;
      } else if ( f[CF_ETAGO1] != chr )	/* <: do not change state */
	p->state = p->cdata_state;
      return TRUE;
    }
    case S_RCDATA:
    { if ( f[CF_ERO] == chr ) /* & */
      { setlocation(&p->startloc, &p->location, line, lpos);
	p->state = S_ENT0;
	return TRUE;
      }
      /*FALLTHROUGH*/
    }
    case S_CDATA:
    { add_verbatim_cdata(p, chr);

      if ( f[CF_MDO1] == chr )		/* < */
      { setlocation(&p->startloc, &p->location, line, lpos);
	p->state = S_ECDATA1;
      }

					/* / in CDATA shorttag element */
      if ( p->waiting_for_net && f[CF_ETAGO2] == chr )
      { setlocation(&p->startloc, &p->location, line, lpos);
	p->cdata->size--;
	terminate_ocharbuf(p->cdata);
	terminate_icharbuf(p->buffer);
	if ( p->mark_state == MS_INCLUDE )
	{ process_cdata(p, TRUE);
	  process_net(p);
	  empty_cdata(p);
	}
	empty_icharbuf(p->buffer);
	p->cdata_state = p->state = S_PCDATA;
      }

      return TRUE;
    }
    case S_MSCDATA:
    { add_verbatim_cdata(p, chr);
      if ( f[CF_DSC] == chr )		/* ] */
        p->state = S_EMSCDATA1;
      return TRUE;
    }
    case S_EMSCDATA1:
    { add_verbatim_cdata(p, chr);
      if ( f[CF_DSC] == chr )		/* ]] */
        p->state = S_EMSCDATA2;
      else
        p->state = S_MSCDATA;
      return TRUE;
    }
    case S_EMSCDATA2:
    { add_verbatim_cdata(p, chr);
      if ( f[CF_MDC] == chr )		/* ]]> */
      { p->cdata->size -= 3;		/* Delete chars for ]] */
	pop_marked_section(p);
	p->state = S_PCDATA;
      } else if ( f[CF_DSC] != chr )	/* if ]]], stay in this state */
        p->state = S_MSCDATA;
      return TRUE;
    }
    case S_EMSC1:
    { if ( f[CF_DSC] == chr )		/* ]] in marked section */
      { p->state = S_EMSC2;
	return TRUE;
      } else
      { add_icharbuf(p->buffer, chr);
	recover_parser(p);
	return TRUE;
      }
    }
    case S_EMSC2:
    { if ( f[CF_MDC] == chr )		/* ]]> in marked section */
      { pop_marked_section(p);
	p->state = S_PCDATA;
	return TRUE;
      } else
      { add_icharbuf(p->buffer, chr);
	recover_parser(p);
	return TRUE;
      }
    }
    case S_PENT:			/* %parameter entity; */
    { if ( f[CF_ERC] == chr )
      { p->state = S_PCDATA;
	terminate_icharbuf(p->buffer);
	if ( p->mark_state == MS_INCLUDE )
	{ process_include(p, p->buffer->data);
	}
	empty_icharbuf(p->buffer);
	return TRUE;
      }
      if ( HasClass(dtd, (wint_t)chr, CH_NAME) )
      { add_icharbuf(p->buffer, chr);
	return TRUE;
      }

      terminate_icharbuf(p->buffer);
      return gripe(p, ERC_SYNTAX_ERROR,
		   L"Illegal parameter entity", p->buffer->data);
    }
    case S_ENT0:			/* Seen & */
    { if ( chr == '#' || HasClass(dtd, (wint_t)chr, CH_NAME) )
      { empty_icharbuf(p->buffer);
	add_icharbuf(p->buffer, chr);
	p->state = S_ENT;
      } else
      {	if ( IS_XML_DIALECT(dtd->dialect) )
	{ wchar_t buf[3];
	  buf[0] = '&';
	  buf[1] = chr;
	  buf[2] = '\0';
	  gripe(p, ERC_SYNTAX_ERROR, L"Illegal entity", buf);
	}

	add_cdata(p, f[CF_ERO]);
	p->state = p->cdata_state;
	goto reprocess;
      }

      return TRUE;
    }
    case S_ENT:				/* &entity; */
    { if ( HasClass(dtd, (wint_t)chr, CH_NAME) )
      { add_icharbuf(p->buffer, chr);
	return TRUE;
      }

      terminate_icharbuf(p->buffer);
      p->state = p->cdata_state;
      if ( p->mark_state == MS_INCLUDE )
      { process_entity(p, p->buffer->data);
      }
      empty_icharbuf(p->buffer);

      if ( chr == CR )
	p->state = S_ENTCR;
      else if ( f[CF_ERC] != chr && chr != '\n' )
	goto reprocess;

      return TRUE;
    }
    case S_ENTCR:			/* seen &entCR, eat the LF */
    { p->state = p->cdata_state;
      if ( chr != LF )
	goto reprocess;

      return TRUE;
    }
    case S_DECL0:			/* Seen < */
    { if ( f[CF_ETAGO2] == chr )	/* </ */
      { add_icharbuf(p->buffer, chr);
	p->state = S_DECL;
      } else if ( HasClass(dtd, (wint_t)chr, CH_NAME) ) /* <letter */
      { add_icharbuf(p->buffer, chr);
	p->state = S_DECL;
      } else if ( f[CF_MDO2] == chr )	/* <! */
      { p->state = S_MDECL0;
      } else if ( f[CF_PRO2] == chr )	/* <? */
      { p->state = S_PI;
      } else				/* recover */
      { add_cdata(p, f[CF_MDO1]);
	add_cdata(p, chr);
	p->state = S_PCDATA;
      }

      return TRUE;
    }
    case S_MDECL0:			/* Seen <! */
    { if ( f[CF_CMT] == chr )		/* <!- */
      { p->state = S_CMTO;
	return TRUE;
      }
      add_icharbuf(p->buffer, f[CF_MDO2]);
      add_icharbuf(p->buffer, chr);
      p->state = S_DECL;
      return TRUE;
    }
    case S_DECL:			/* <...> */
    { if ( f[CF_MDC] == chr )		/* > */
      { prepare_cdata(p);
	p->state = S_PCDATA;
	terminate_icharbuf(p->buffer);
	if ( p->mark_state == MS_INCLUDE )
	{ process_declaration(p, p->buffer->data);
	}
	empty_icharbuf(p->buffer);
	return TRUE;
      }
      if ( dtd->shorttag && f[CF_ETAGO2] == chr && p->buffer->size > 0 )
      { prepare_cdata(p);
	p->state = S_PCDATA;
	terminate_icharbuf(p->buffer);
	if ( p->mark_state == MS_INCLUDE )
	{ WITH_CLASS(p, EV_SHORTTAG,
		     process_declaration(p, p->buffer->data));
	}
	empty_icharbuf(p->buffer);
	p->waiting_for_net = TRUE;
	return TRUE;
      }

      add_icharbuf(p->buffer, chr);

      if ( f[CF_LIT] == chr )		/* " */
      { p->state = S_STRING;
	p->saved = chr;
	p->lit_saved_state = S_DECL;
      } else if ( f[CF_LITA] == chr )	/* ' */
      { p->state = S_STRING;
	p->saved = chr;
	p->lit_saved_state = S_DECL;
	return TRUE;
      } else if ( f[CF_CMT] == chr &&	/* - */
		  p->buffer->data[0] == f[CF_MDO2] ) /* Started <! */
      { p->state = S_DECLCMT0;
      } else if ( f[CF_DSO] == chr )	/* [: marked section */
      { terminate_icharbuf(p->buffer);

	process_marked_section(p);
      }

      return TRUE;
    }
    case S_DECLCMT0:			/* <...- */
    { if ( f[CF_CMT] == chr )
      { p->buffer->size--;
	p->state = S_DECLCMT;
      } else
      { add_icharbuf(p->buffer, chr);
	p->state = S_DECL;
      }
      return TRUE;
    }
    case S_DECLCMT:			/* <...--.. */
    { if ( f[CF_CMT] == chr )
	p->state = S_DECLCMTE0;
      return TRUE;
    }
    case S_DECLCMTE0:			/* <...--..- */
    { if ( f[CF_CMT] == chr )
	p->state = S_DECL;
      else
	p->state = S_DECLCMT;
      return TRUE;
    }
    case S_PI:
    { add_icharbuf(p->buffer, chr);
      if ( f[CF_PRO2] == chr )		/* <? ... ? */
	p->state = S_PI2;
      if ( f[CF_PRC] == chr )		/* no ? is ok too (XML/SGML) */
	goto pi;
      return TRUE;
    }
    case S_PI2:
    { if ( f[CF_PRC] == chr )
      { pi:
	process_cdata(p, FALSE);
	p->state = S_PCDATA;
	p->buffer->size--;
	terminate_icharbuf(p->buffer);
	if ( p->mark_state == MS_INCLUDE )
	{ process_pi(p, p->buffer->data);
	}
	empty_icharbuf(p->buffer);
	return TRUE;
      }
      add_icharbuf(p->buffer, chr);
      p->state = S_PI;
      return TRUE;
    }
    case S_STRING:
    { add_icharbuf(p->buffer, chr);
      if ( chr == p->saved )
	p->state = p->lit_saved_state;
      return TRUE;
    }
    case S_CMTO:			/* Seen <!- */
    { if ( f[CF_CMT] == chr )		/* - */
      { p->state = S_CMT1;
	return TRUE;
      } else
      { add_cdata(p, f[CF_MDO1]);
	add_cdata(p, f[CF_MDO2]);
	add_cdata(p, f[CF_CMT]);
	add_cdata(p, chr);
	p->state = S_PCDATA;
	return TRUE;
      }
    }
    case S_CMT1:			/* <!-- */
    { p->state = S_CMT;
      return TRUE;
    }
    case S_CMT:
    { if ( f[CF_CMT] == chr )
	p->state = S_CMTE0;		/* <!--...- */
      return TRUE;
    }
    case S_CMTE0:			/* <!--... -- */
    { if ( f[CF_CMT] == chr )
	p->state = S_CMTE1;
      else
	p->state = S_CMT;
      return TRUE;
    }
    case S_CMTE1:			/* <!--...-- seen */
    { if ( f[CF_MDC] == chr )		/* > */
      { if ( p->on_decl )
	  (*p->on_decl)(p, (ichar*)L"");
	p->state = S_PCDATA;
      } else
      { if ( IS_XML_DIALECT(dtd->dialect) )
	  gripe(p, ERC_SYNTAX_ERROR, L"Illegal comment", L"");
	if ( f[CF_CMT] != chr )
	  p->state = S_CMT;
      }
      return TRUE;
    }
    case S_GROUP:			/* [...] in declaration */
    { add_icharbuf(p->buffer, chr);
      if ( f[CF_DSO] == chr )
      { p->grouplevel++;
      } else if ( f[CF_DSC] == chr )
      { if ( --p->grouplevel == 0 )
	  p->state = S_DECL;
      } else if ( f[CF_LIT] == chr )	/* " */
      { p->state = S_STRING;
	p->saved = chr;
	p->lit_saved_state = S_GROUP;
      } else if ( f[CF_LITA] == chr )	/* ' */
      { p->state = S_STRING;
	p->saved = chr;
	p->lit_saved_state = S_GROUP;
	return TRUE;
      }
      return TRUE;
    }
#ifdef UTF8
    case S_UTF8:
#endif
    default:
      assert(0);
      return FALSE;
  }
}


		 /*******************************
		 *	     TOPLEVEL		*
		 *******************************/

int
load_dtd_from_file(dtd_parser *p, const ichar *file)
{ FILE *fd;
  int rval;
  data_mode   oldmode  = p->dmode;
  dtdstate    oldstate = p->state;
  locbuf      oldloc;

  push_location(p, &oldloc);
  p->dmode = DM_DTD;
  p->state = S_PCDATA;
  empty_icharbuf(p->buffer);		/* dubious */
  set_file_dtd_parser(p, IN_FILE, file);

  if ( (fd = wfopen(file, "rb")) )
  { int chr;

    while( (chr = getc(fd)) != EOF )
      putchar_dtd_parser(p, chr);

    fclose(fd);

    p->dtd->implicit = FALSE;
    rval = TRUE;
  } else
    rval = FALSE;

  pop_location(p, &oldloc);
  p->dmode = oldmode;
  p->state = oldstate;

  return rval;
}


dtd *
file_to_dtd(const ichar *file, const ichar *doctype, dtd_dialect dialect)
{ dtd_parser *p = new_dtd_parser(new_dtd(doctype));

  set_dialect_dtd(p->dtd, dialect);

  if ( load_dtd_from_file(p, file) )
  { dtd *dtd = p->dtd;

    dtd->references++;			/* avoid deletion */
    free_dtd_parser(p);
    return dtd;
  } else
  { free_dtd_parser(p);

    return NULL;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SGML sees a file as

[<LF>]Line 1<CR>
 <LF> Line 2<CR>

I.e. the newline  appearing  just  before   the  end-of-file  should  be
ignored. In addition, Unix-style files are   mapped  to CR-LF. Thanks to
Richard O'Keefe.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
sgml_process_stream(dtd_parser *p, FILE *fd, unsigned flags)
{ int p0, p1;

  if ( (p0 = getc(fd)) == EOF )
    return TRUE;
  if ( (p1 = getc(fd)) == EOF )
  { putchar_dtd_parser(p, p0);
    return end_document_dtd_parser(p);
  }

  for(;;)
  { int p2 = getc(fd);

    if ( p2 == EOF )
    { putchar_dtd_parser(p, p0);
      if ( p1 != LF )
	putchar_dtd_parser(p, p1);
      else if ( p0 != CR )
	putchar_dtd_parser(p, CR);

      if ( flags & SGML_SUB_DOCUMENT )
	return TRUE;
      else
	return end_document_dtd_parser(p);
    }

    putchar_dtd_parser(p, p0);
    p0 = p1;
    p1 = p2;
  }
}


int
sgml_process_file(dtd_parser *p, const ichar *file, unsigned flags)
{ FILE *fd;
  int rval;
  locbuf oldloc;

  push_location(p, &oldloc);
  set_file_dtd_parser(p, IN_FILE, file);
  if ( !(flags & SGML_SUB_DOCUMENT) )
    set_mode_dtd_parser(p, DM_DATA);

  if ( (fd = wfopen(file, "rb")) )
  { rval = sgml_process_stream(p, fd, flags);
    fclose(fd);
  } else
    rval = FALSE;

  pop_location(p, &oldloc);

  return rval;
}



		 /*******************************
		 *	       ERRORS		*
		 *******************************/

static wchar_t *
format_location(wchar_t *s, size_t len, dtd_srcloc *l)
{ int first = TRUE;

  if ( !l || l->type == IN_NONE )
    return s;

  for( ; l && l->type != IN_NONE;
         l = l->parent, first = FALSE )
  { if ( !first )
    { swprintf(s, len, L" (from ");
      s += wcslen(s);
    }

    switch(l->type)
    { case IN_NONE:
	assert(0);
      case IN_FILE:
	swprintf(s, len, L"%ls:%d:%d", l->name.file, l->line, l->linepos);
        break;
      case IN_ENTITY:
        swprintf(s, len, L"&%ls;%d:%d", l->name.entity, l->line, l->linepos);
        break;
    }

    s += wcslen(s);
    if ( !first )
    { *s++ = L')';
    }
  }

  *s++ = L':';
  *s++ = L' ';

  return s;
}


static void
format_message(dtd_error *e)
{ wchar_t buf[1024];
  wchar_t *s;
  int prefix_len;
  int left;

  switch(e->severity)
  { case ERS_ERROR:
      wcscpy(buf, L"Error: ");
      break;
    case ERS_WARNING:
      wcscpy(buf, L"Warning: ");
      break;
    default:
      buf[0] = '\0';
  }
  s = buf+wcslen(buf);

  s = format_location(s, 1024-(s-buf), e->location);
  prefix_len = (int)(s-buf);
  left = 1024-prefix_len;

  switch(e->id)
  { case ERC_REPRESENTATION:
      swprintf(s, left, L"Cannot represent due to %ls", e->argv[0]);
      break;
    case ERC_RESOURCE:
      swprintf(s, left, L"Insufficient %ls resources", e->argv[0]);
      break;
    case ERC_LIMIT:
      swprintf(s, left, L"%ls limit exceeded", e->argv[0]);
      break;
    case ERC_ET_SYSTEM:
      swprintf(s, left, L"SYSTEM entity %ls not allowed.  "
	                L"Use system_entities(true)", e->argv[0]);
      break;
    case ERC_VALIDATE:
      swprintf(s, left, L"%ls", e->argv[0]);
      break;
    case ERC_SYNTAX_ERROR:
      swprintf(s, left, L"%ls", e->argv[0]);
      break;
    case ERC_EXISTENCE:
      swprintf(s, left, L"%ls \"%ls\" does not exist", e->argv[0], e->argv[1]);
      break;
    case ERC_REDEFINED:
      swprintf(s, left, L"Redefined %ls \"%ls\"", e->argv[0], e->argv[1]);
      break;
    default:
      *s = 0;
      ;
  }

  e->message = str2ring(buf);
  e->plain_message = e->message + prefix_len;
}


int
gripe(dtd_parser *p, dtd_error_id e, ...)
{ va_list args;
  wchar_t buf[1024];
  dtd_error error;
  int dtdmode = FALSE;
  void *freeme = NULL;

  va_start(args, e);

  memset(&error, 0, sizeof(error));
  error.minor = e;			/* detailed error code */

  if ( p )
  { error.location = &p->location;
    if ( p->dmode == DM_DTD )
      dtdmode = TRUE;
  } else
  { error.location = NULL;
  }

  switch(e)
  { case ERC_REPRESENTATION:
    case ERC_RESOURCE:
      error.severity = ERS_ERROR;
      error.argv[0]  = va_arg(args, wchar_t *);
      break;
    case ERC_LIMIT:
      error.severity = ERS_WARNING;
      error.argv[0]  = va_arg(args, wchar_t *);
      break;
    case ERC_ET_SYSTEM:
      error.severity = ERS_WARNING;
      error.argv[0]  = va_arg(args, wchar_t *);
      break;
    case ERC_SYNTAX_ERROR:
    case ERC_SYNTAX_WARNING:
    { wchar_t *m       = va_arg(args, wchar_t *);
      const wchar_t *s = va_arg(args, const wchar_t *);

      if ( s && *s )
      { swprintf(buf, 1024, L"%ls, found \"%ls\"", m, str_summary(s, 25));
	error.argv[0] = buf;
      } else
	error.argv[0] = m;

      error.severity = (e == ERC_SYNTAX_WARNING ? ERS_WARNING : ERS_ERROR);
      e = ERC_SYNTAX_ERROR;
      break;
    }
    case ERC_DOMAIN:
    { const wchar_t *expected = va_arg(args, const wchar_t *);
      const wchar_t *found    = str_summary(va_arg(args, const wchar_t *), 25);

      swprintf(buf, 1024, L"Expected type %ls, found \"%ls\"", expected, found);
      error.argv[0] = buf;
      error.severity = ERS_ERROR;
      e = (dtdmode ? ERC_SYNTAX_ERROR : ERC_VALIDATE);
      break;
    }
    case ERC_REDEFINED:
    { dtd_symbol *name;
      error.argv[0] = va_arg(args, wchar_t *); /* type */
      name = va_arg(args, dtd_symbol *); /* name */
      error.argv[1]  = (ichar*)name->name;
      error.severity = ERS_STYLE;
      break;
    }
    case ERC_EXISTENCE:
    { error.argv[0] = va_arg(args, wchar_t *); /* type */
      error.argv[1] = va_arg(args, wchar_t *); /* name */
      error.severity = ERS_ERROR;
      break;
    }
    case ERC_VALIDATE:
    { error.argv[0] = va_arg(args, wchar_t *); /* message */
      error.severity = ERS_WARNING;
      break;
    }
    case ERC_OMITTED_CLOSE:
    { const wchar_t *element = va_arg(args, const wchar_t *);

      swprintf(buf, 1024, L"Inserted omitted end-tag for \"%ls\"", element);
      error.argv[0] = buf;
      error.severity = ERS_WARNING;
      e = ERC_VALIDATE;
      break;
    }
    case ERC_OMITTED_OPEN:
    { const wchar_t *element = va_arg(args, const wchar_t *);

      swprintf(buf, 1024, L"Inserted omitted start-tag for \"%ls\"", element);
      error.argv[0] = buf;
      error.severity = ERS_WARNING;
      e = ERC_VALIDATE;
      break;
    }
    case ERC_NOT_OPEN:
    { const wchar_t *element = va_arg(args, const wchar_t *);

      swprintf(buf, 1024, L"Ignored end-tag for \"%ls\" which is not open",
	       element);
      error.argv[0] = buf;
      error.severity = ERS_WARNING;
      e = ERC_VALIDATE;
      break;
    }
    case ERC_NOT_ALLOWED:
    { const wchar_t *element = va_arg(args, const wchar_t *);

      swprintf(buf, 1024, L"Element \"%ls\" not allowed here", element);
      error.argv[0] = buf;
      error.severity = ERS_WARNING;
      e = ERC_VALIDATE;
      break;
    }
    case ERC_NOT_ALLOWED_PCDATA:
    { const ocharbuf *cdata = va_arg(args, const ocharbuf *);

      swprintf(buf, 1024, L"#PCDATA (\"%ls\") not allowed here",
	       str_summary(cdata->data.w, 25));
      error.argv[0] = buf;
      error.severity = ERS_WARNING;
      e = ERC_VALIDATE;
      break;
    }
    case ERC_NO_ATTRIBUTE:
    { const wchar_t *elem = va_arg(args, wchar_t *); /* element */
      const wchar_t *attr = va_arg(args, wchar_t *); /* attribute */

      swprintf(buf, 1024, L"Element \"%ls\" has no attribute \"%ls\"",
	       elem, attr);
      error.argv[0] = buf;
      error.severity = ERS_WARNING;

      e = ERC_VALIDATE;
      break;
    }
    case ERC_NO_ATTRIBUTE_VALUE:
    { const wchar_t *elem  = va_arg(args, wchar_t *); /* element */
      const wchar_t *value = va_arg(args, wchar_t *); /* attribute value */

      swprintf(buf, 1024, L"Element \"%ls\" has no attribute with value \"%ls\"",
	       elem, value);
      error.argv[0] = buf;
      error.severity = ERS_WARNING;

      e = ERC_VALIDATE;
      break;
    }
    case ERC_NO_VALUE:
    { error.argv[0] = L"entity value";
      error.argv[1] = va_arg(args, wchar_t *); /* entity */

      error.severity = ERS_ERROR;
      e = ERC_EXISTENCE;
      break;
    }
    case ERC_NO_DOCTYPE:
    { const wchar_t *doctype = va_arg(args, wchar_t *); /* element */
      const wchar_t *file    = va_arg(args, wchar_t *); /* DTD file */

      swprintf(buf, 1024, L"No <!DOCTYPE ...>, assuming \"%ls\" from DTD file \"%s\"",
	      doctype, file);
      error.argv[0] = buf;
      error.severity = ERS_WARNING;

      e = ERC_VALIDATE;
      break;
    }
    case ERC_NO_CATALOGUE:
    { char *file = va_arg(args, char *); /* catalogue file */

      error.argv[0] = L"catalogue file";
      freeme = error.argv[1] = utf8towcs(file);
      error.severity = ERS_WARNING;
      e = ERC_EXISTENCE;

      break;
    }
  }

  error.id      = e;
  format_message(&error);

  if ( p && p->on_error )
    (*p->on_error)(p, &error);
  else
    fwprintf(stderr, L"SGML: %ls\n", error.message);

  if ( freeme )
    sgml_free(freeme);

  va_end(args);

  return FALSE;
}
