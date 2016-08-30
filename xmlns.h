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

#ifndef XMLNS_H_INCLUDED
#define XMLNS_H_INCLUDED

typedef struct _xmlns
{ dtd_symbol *name;			/* Prefix of the NS */
  dtd_symbol *url;			/* pointed-to URL */
  struct _xmlns *next;			/* next name */
} xmlns;

void		xmlns_free(xmlns *list);
xmlns*		xmlns_find(dtd_parser *p, dtd_symbol *ns);
xmlns *		xmlns_push(dtd_parser *p, const ichar *ns, const ichar *url);
void		update_xmlns(dtd_parser *p, dtd_element *e,
			     int natts, sgml_attribute *atts);
int		xmlns_resolve_attribute(dtd_parser *p, dtd_symbol *id,
					const ichar **local, const ichar **url);
int		xmlns_resolve_element(dtd_parser *p,
                                      const ichar **local, const ichar **url, const ichar **prefix);

#endif /*XMLNS_H_INCLUDED*/
