/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2011, University of Amsterdam
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
#include <stdio.h>
#include <wchar.h>
#include <assert.h>
#include <string.h>
#include <wctype.h>
#include <time.h>
#include "dtd.h"
#include "util.h"
#include "prolog.h"

static int errors;

		 /*******************************
		 *	  PROLOG SYNTAX		*
		 *******************************/

typedef enum
{ AT_LOWER,
  AT_QUOTE,
  AT_FULLSTOP,
  AT_SYMBOL,
  AT_SOLO,
  AT_SPECIAL
} atomtype;


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Contributed by Richard O'Keefe.  Thanks!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
atomType(ichar const *s, int len)
{ static ichar const symbols[] = L"#$&*+-./:<=>?@\\^`~";
  unsigned char const *u = (unsigned char const *)s;

  switch (len)
  { case 0:
      return AT_QUOTE;
    case 1:
      return iswlower(u[0]) ? AT_LOWER
	   : u[0] == '.'    ? AT_FULLSTOP
	   : u[0] == '!'    ? AT_SOLO
	   : u[0] == ';'    ? AT_SOLO
	   : u[0] == ','    ? AT_SOLO
	   :                  AT_QUOTE;
    case 2:
      if (u[0] == '[' && u[1] == ']') return AT_SPECIAL;
      if (u[0] == '{' && u[1] == '}') return AT_SPECIAL;
      break;
    default:
      break;
  }

  if (iswlower(u[0]))
  { do ++u; while (--len > 0 && (iswalnum(*u) || *u == '_'));
    return len == 0 ? AT_LOWER : AT_QUOTE;
  } else if (wcschr(symbols, *u) != NULL)
  { do ++u; while (--len > 0 && wcschr(symbols, *u) != 0);
    return len == 0 ? AT_SYMBOL : AT_QUOTE;
  } else
  { return AT_QUOTE;
  }
}


static const ichar *
atom(const ichar *text)
{ int len = wcslen(text);

  switch(atomType(text, len))
  { case AT_QUOTE:
    case AT_FULLSTOP:
    { ichar *tmp = ringallo((len*2+1)*sizeof(ichar));
      ichar *o = tmp;

      *o++ = '\'';
      for( ; --len >= 0; text++)
      { switch( *text )
	{ case '\n':
	    *o++ = '\\';
	    *o++ = 'n';
	    break;
	  case '\r':
	    *o++ = '\\';
	    *o++ = 'r';
	    break;
	  case '\t':
	    *o++ = '\\';
	    *o++ = 't';
	    break;
	  case '\'':
	    *o++ = '\\';
	  default:
	    *o++ = *text;
	}
      }
      *o++ = '\'';
      *o   = '\0';

      return tmp;
    }
    default:
      return text;
  }
}


static const char *
bool(int val)
{ return val ? "true" : "false";
}


static void
prolog_print_entity(const char *which, dtd_entity *e)
{ switch( e->type )
  { case ET_LITERAL:
      wprintf(L"%s(%ls, %ls).\n",
	      which,
	      atom(e->name->name),
	      atom(e->value));
      break;
    case ET_SYSTEM:
      wprintf(L"%s(%ls, system(%ls)).\n",
	     which,
	     atom(e->name->name),
	     atom(e->exturl));
      break;
    case ET_PUBLIC:
      wprintf(L"%s(%ls, public(%ls, %ls)).\n",
	     which,
	     atom(e->name->name),
	     atom(e->extid),
	     atom(e->exturl));
      break;
  }
}


static void
prolog_print_model(dtd_model *m)
{ dtd_model *sub;
  int n = 0;
  const char *sep;

  switch(m->type)
  { case MT_PCDATA:
      printf("'#pcdata'");
      goto card;
    case MT_ELEMENT:
      wprintf(L"%ls", atom(m->content.element->name->name));
      goto card;
    case MT_AND:
      sep = " & ";
      break;
    case MT_SEQ:
      sep = ", ";
      break;
    case MT_OR:
      sep = "|";
      break;
    case MT_UNDEF:
    default:
      assert(0);
      sep = NULL;			/* should not be used */
      break;
  }

  printf("(");
  for(sub = m->content.group; sub; sub=sub->next)
  { if ( n++ > 0 )
      printf("%s", sep);
    prolog_print_model(sub);
  }
  printf(")");

card:
  switch(m->cardinality)
  { case MC_ONE:
      break;
    case MC_OPT:
      printf("?");
      break;
    case MC_REP:
      printf("*");
      break;
    case MC_PLUS:
      printf("+");
      break;
  }
}


static void
prolog_print_content(dtd_element *e)
{ dtd_edef *def = e->structure;

  switch( def->type )
  { case C_EMPTY:
      printf("empty");
      break;
    case C_CDATA:
      printf("cdata");
      break;
    case C_RCDATA:
      printf("rcdata");
      break;
    case C_ANY:
      printf("any");
      break;
    default:
      if ( def->content )
      { printf("model(");
	prolog_print_model(def->content);
	printf(")");
      } else
      { printf("[]");
	fwprintf(stderr,
		L"Warning: element %s has no content model\n",
		e->name->name);
	errors++;
      }
      break;
  }
}


static ichar *
istrblank(const ichar *s)
{ for( ; *s; s++ )
  { if ( iswspace(*s) )
      return (ichar *)s;
  }

  return NULL;
}


static void
print_listval(attrtype type, int len, const ichar *text)
{ ichar *t = sgml_malloc((len+1)*sizeof(ichar));

  istrncpy(t, text, len);
  t[len] = '\0';

  if ( type == AT_NUMBERS )
    wprintf(L"%ls", t);
  else
    wprintf(L"%ls", atom(t));

  sgml_free(t);
}


static void
prolog_print_attribute(dtd_element *e, dtd_attr *at)
{ wprintf(L"    attribute(%ls, %ls, ",
	 atom(e->name->name), atom(at->name->name));

  switch(at->type)			/* print type */
  { case AT_CDATA:
      printf("cdata");
      break;
    case AT_ENTITY:
      printf("entity");
      break;
    case AT_ENTITIES:
      printf("entities");
      break;
    case AT_ID:
      printf("id");
      break;
    case AT_IDREF:
      printf("idref");
      break;
    case AT_IDREFS:
      printf("list(idref)");
      break;
    case AT_NAME:
      printf("name");
      break;
    case AT_NAMES:
      printf("list(name)");
      break;
    case AT_NMTOKEN:
      printf("nmtoken");
      break;
    case AT_NMTOKENS:
      printf("list(nmtoken)");
      break;
    case AT_NOTATION:
      printf("notation");
      break;
    case AT_NUMBER:
      printf("number");
      break;
    case AT_NUMBERS:
      printf("list(number)");
      break;
    case AT_NAMEOF:
    { dtd_name_list *nl;
      int n = 0;

      printf("nameof([");
      for(nl = at->typeex.nameof; nl; nl = nl->next)
      { if ( n++ > 0 )
	  printf(", ");
	wprintf(L"%ls", atom(nl->value->name));
      }
      printf("])");
    }
      break;
    case AT_NUTOKEN:
      printf("nutoken");
      break;
    case AT_NUTOKENS:
      printf("list(nutoken)");
      break;
  }

  printf(", ");				/* print default */
  switch(at->def)
  { case AT_REQUIRED:
      printf("required");
      break;
    case AT_CURRENT:
      printf("current");
      break;
    case AT_CONREF:
      printf("conref");
      break;
    case AT_IMPLIED:
      printf("implied");
      break;
    case AT_DEFAULT:
    case AT_FIXED:
    { char *f = (at->def == AT_DEFAULT ? "default" : "fixed");

      printf("%s(", f);

      switch( at->type )
      { case AT_CDATA:
	  wprintf(L"%ls", atom(at->att_def.cdata));
	  break;
	case AT_NUMBER:
	  printf("%ld", at->att_def.number);
	  break;
	case AT_NAME:
	case AT_NUTOKEN:
	case AT_NMTOKEN:
	  wprintf(L"%ls", atom(at->att_def.name->name));
	  break;
	default:
	  if ( at->islist )
	  { const ichar *val = at->att_def.list;
	    const ichar *e;
	    int an = 0;

	    printf("[");
	    for(e=istrblank(val); e; val = e+1, e=istrblank(val))
	    { if ( e == val )
		continue;			/* skip spaces */
	      if ( an++ > 0 )
		printf(", ");
	      print_listval(at->type, e-val, val);
	    }
            if ( an++ > 0 )
	      printf(", ");
	    print_listval(at->type, istrlen(val), val);
	    printf("]");
	    break;
	  }
	  assert(0);
      }

      printf(")");
    }
  }

  printf(").\n");
}


static void
prolog_print_element(dtd_element *e, unsigned int flags)
{ ichar nbuf[MAXNMLEN];

  istrcpy(nbuf, e->name->name);
  istrupper(nbuf);

  wprintf(L"\n%% Element <%s>\n", nbuf);

  if ( e->structure )
  { dtd_edef *def = e->structure;

    wprintf(L"element(%ls, omit(%s, %s), ",
	    atom(e->name->name),
	    bool(def->omit_open),
	    bool(def->omit_close));
    prolog_print_content(e);
    printf(").\n");

    if ( def->excluded )
    { dtd_element_list *el;

      for(el = def->excluded; el; el=el->next)
	wprintf(L"exclude(%ls, %ls).\n",
		atom(e->name->name),
		atom(el->value->name->name));
    }
    if ( def->included )
    { dtd_element_list *el;

      for(el = def->included; el; el=el->next)
	wprintf(L"include(%ls, %ls).\n",
		atom(e->name->name),
		atom(el->value->name->name));
    }

    if ( flags & PL_PRINT_ATTRIBUTES )
    { dtd_attr_list *al;

      for(al=e->attributes; al; al=al->next)
	prolog_print_attribute(e, al->attribute);
    }
  } else
  { fwprintf(stderr, L"Warning: element %s has no definition\n",
	     e->name->name);
    errors++;
  }
}


int
prolog_print_dtd(dtd *dtd, unsigned int flags)
{ dtd_entity *et;
  dtd_element *e;
  time_t now;

  if ( !dtd->doctype )
    fprintf(stderr, "DTD has no document type\n");

  time(&now);

  if ( !flags )
    flags = PL_PRINT_ALL;

  errors = 0;

  wprintf(L"/*  This file represents the SGML DOCTYPE \"%s\"\n", dtd->doctype);
  printf("    converted using dtd2pl version %s\n", DTD2PL_VERSION);
  printf("    Conversion date: %s\n\n", ctime(&now));
  printf("    dtd2pl is written by Jan Wielemaker\n");
  printf("    E-mail: jan@swi.psy.uva.nl\n");
  printf("*/\n\n");

  wprintf(L":- module(%s_dtd, []).\n\n", dtd->doctype);
  printf(":- op(100, xf,  ?).\n");
  printf(":- op(100, xf,  +).\n");
  printf(":- op(100, xf,  *).\n");
  printf(":- op(200, xfy, &).\n");

  printf("\n");
  printf(":- discontiguous\n");
  printf("\tattribute/4,\n");
  printf("\telement/3,\n");
  printf("\texclude/2,\n");
  printf("\tinclude/2.\n");

  if ( flags & PL_PRINT_PENTITIES )
  { printf("\n");
    for( et=dtd->pentities; et; et=et->next )
      prolog_print_entity("parameter_entity", et);
  }

  if ( flags & PL_PRINT_ENTITIES )
  { printf("\n");
    for( et=dtd->entities; et; et=et->next )
      prolog_print_entity("entity", et);
  }

  if ( flags & PL_PRINT_ELEMENTS )
  { printf("\n");
    for( e=dtd->elements; e; e=e->next )
      prolog_print_element(e, flags);
  }

  if ( errors )
  { fprintf(stderr, "Warning: DTD contained %d errors\n", errors);
    return FALSE;
  }

  return TRUE;
}

