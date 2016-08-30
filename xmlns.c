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

#include <stdio.h>
#include <stdlib.h>
#include "dtd.h"
#include "parser.h"

#ifdef XMLNS

xmlns *
xmlns_push(dtd_parser *p, const ichar *ns, const ichar *url)
{ sgml_environment *env = p->environments;
  dtd_symbol *n = (*ns ? dtd_add_symbol(p->dtd, ns) : (dtd_symbol *)NULL);
  dtd_symbol *u = dtd_add_symbol(p->dtd, url); /* TBD: ochar/ichar */
  xmlns *x = sgml_malloc(sizeof(*x));

  x->name = n;
  x->url  = u;

  if ( env )
  { if ( p->on_xmlns )
      (*p->on_xmlns)(p, n, u);

    x->next = env->xmlns;
    env->xmlns = x;
  } else
  { x->next = p->xmlns;
    p->xmlns = x;
  }

  return x;
}


void
xmlns_free(xmlns *n)
{ xmlns *next;

  for(; n; n = next)
  { next = n->next;

    sgml_free(n);
  }
}


xmlns *
xmlns_find(dtd_parser *p, dtd_symbol *ns)
{ sgml_environment *env = p->environments;
  xmlns *n;

  for(; env; env = env->parent)
  { for(n=env->xmlns; n; n = n->next)
    { if ( n->name == ns )
	return n;
    }
  }

  for (n=p->xmlns; n; n = n->next)
  { if ( n->name == ns )
      return n;
  }

  return NULL;
}


static ichar *
isxmlns(const ichar *s, int nschr)
{ if ( s[0]=='x' && s[1]=='m' && s[2]=='l' && s[3] =='n'&& s[4]=='s' )
  { if ( !s[5] )
      return (ichar *)s+5;			/* implicit */
    if ( s[5] == nschr )
      return (ichar *)s+6;
  }

  return NULL;
}


void
update_xmlns(dtd_parser *p, dtd_element *e, int natts, sgml_attribute *atts)
{ dtd_attr_list *al;
  int nschr = p->dtd->charfunc->func[CF_NS]; /* : */

  for(al=e->attributes; al; al=al->next)
  { dtd_attr *a = al->attribute;
    const ichar *name = a->name->name;

    if ( (name = isxmlns(name, nschr)) && /* TBD: flag when processing DTD */
	 a->type == AT_CDATA &&
	 (a->def == AT_FIXED || a->def == AT_DEFAULT) )
      xmlns_push(p, name, a->att_def.cdata);
  }

  for( ; natts-- > 0; atts++ )
  { const ichar *name = atts->definition->name->name;

    if ( (name=isxmlns(name, nschr)) &&
	 atts->definition->type == AT_CDATA &&
	 atts->value.textW )
      xmlns_push(p, name, atts->value.textW);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
xmlns_resolve()
    Convert a symbol as returned by the XML level-1.0 parser to its namespace
    tuple {url}localname.  This function is not used internally, but provided
    for use from the call-back functions of the parser.

    It exploits the stack of namespace-environments managed by the parser
    itself (see update_xmlns())
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
xmlns_resolve_attribute(dtd_parser *p, dtd_symbol *id,
			const ichar **local, const ichar **url)
{ dtd *dtd = p->dtd;
  int nschr = dtd->charfunc->func[CF_NS]; /* : */
  ichar buf[MAXNMLEN];
  ichar *o = buf;
  const ichar *s;
  xmlns *ns;

  for(s=id->name; *s; s++)
  { if ( *s == nschr )
    { dtd_symbol *n;

      *o = '\0';
      *local = s+1;
      n = dtd_add_symbol(dtd, buf);

      if ( istrprefix(L"xml", buf) )	/* XML reserved namespaces */
      { *url = n->name;
        return TRUE;
      } else if ( (ns = xmlns_find(p, n)) )
      { if ( ns->url->name[0] )
	  *url = ns->url->name;
	else
	  *url = NULL;
	return TRUE;
      } else
      { *url = n->name;			/* undefined namespace */
	if ( p->xml_no_ns == NONS_QUIET )
	  return TRUE;
	gripe(p, ERC_EXISTENCE, L"namespace", n->name);
	return FALSE;
      }
    }
    *o++ = *s;
  }

  *local = id->name;

  if ( (p->flags & SGML_PARSER_QUALIFY_ATTS) &&
       (ns = p->environments->thisns) && ns->url->name[0] )
    *url = ns->url->name;
  else
    *url = NULL;			/* no default namespace is defined */

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Resolve the namespace for the current  element. This namespace is stored
in the environment as `thisns' and  acts   as  default for resolving the
namespaces of the attributes (see above).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
xmlns_resolve_element(dtd_parser *p, const ichar **local, const ichar **url, const ichar **prefix)
{ sgml_environment *e;

  if ( (e=p->environments) )
  { dtd_symbol *id = e->element->name;
    dtd *dtd = p->dtd;
    int nschr = dtd->charfunc->func[CF_NS]; /* : */
    ichar buf[MAXNMLEN];
    ichar *o = buf;
    const ichar *s;
    xmlns *ns;

    for(s=id->name; *s; s++)
    { if ( *s == nschr )		/* explicit namespace */
      { dtd_symbol *n;

	*o = '\0';
	*local = s+1;
	n = dtd_add_symbol(dtd, buf);
        *prefix = n->name;
	if ( (ns = xmlns_find(p, n)) )
	{ if ( ns->url->name[0] )
	    *url = ns->url->name;
	  else
	    *url = NULL;
	  e->thisns = ns;		/* default for attributes */
	  return TRUE;
	} else
	{ *url = n->name;		/* undefined namespace */
	  e->thisns = xmlns_push(p, n->name, n->name); /* define implicitly */
	  if ( p->xml_no_ns == NONS_QUIET )
	    return TRUE;
	  gripe(p, ERC_EXISTENCE, L"namespace", n->name);
	  return FALSE;
	}
      }
      *o++ = *s;
    }

    *local = id->name;
    *prefix = NULL;
    if ( (ns = xmlns_find(p, NULL)) )
    { if ( ns->url->name[0] )
	*url = ns->url->name;
      else
	*url = NULL;
      e->thisns = ns;
    } else
    { *url = NULL;			/* no default namespace is defined */
      e->thisns = NULL;
    }

    return TRUE;
  } else
    return FALSE;
}


#endif /*XMLNS*/

