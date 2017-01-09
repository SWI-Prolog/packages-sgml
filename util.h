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

#ifndef DTD_UTIL_H_INCLUDED
#define DTD_UTIL_H_INCLUDED
#include "sgmldefs.h"

#include <stdio.h>
#include <sys/types.h>
#include <wchar.h>

#ifdef _WINDOWS				/* get size_t */
#include <malloc.h>
#endif

typedef struct
{ size_t allocated;
  size_t size;
  size_t limit;
  int    limit_reached;
  ichar *data;
} icharbuf;

typedef struct
{ size_t allocated;
  size_t size;
  size_t limit;
  int limit_reached;
  union
  { wchar_t *w;				/* UCS */
  } data;
  wchar_t localbuf[256];		/* Initial local store */
} ocharbuf;

size_t		istrlen(const ichar *s);
ichar *         istrdup(const ichar *s);
ichar *         istrndup(const ichar *s, int len);
ichar *		istrcpy(ichar *d, const ichar *s);
ichar *		istrcat(ichar *d, const ichar *s);
ichar *		istrncpy(ichar *d, const ichar *s, size_t len);
ichar *		istrupper(ichar *s);
ichar *		istrlower(ichar *s);
int             istrprefix(const ichar *pref, const ichar *s);
int             istreq(const ichar *s1, const ichar *s2);
int             istrcaseeq(const ichar *s1, const ichar *s2);
int		istrncaseeq(const ichar *s1, const ichar *s2, int len);
int             istrhash(const ichar *t, int tsize);
int             istrcasehash(const ichar *t, int tsize);
ichar *		istrchr(const ichar *s, int c);
int		istrtol(const ichar *s, long *val);
void *		sgml_malloc(size_t size);
void *		sgml_calloc(size_t n, size_t size);
void		sgml_free(void *mem);
void *		sgml_realloc(void *old, size_t size);
void		sgml_nomem(void);

#define add_icharbuf(buf, chr) \
	do { if ( buf->size < buf->allocated && chr < 128 ) \
	       buf->data[buf->size++] = chr; \
	     else \
	       __add_icharbuf(buf, chr); \
	   } while(0)

icharbuf *	new_icharbuf(size_t limit);
void		free_icharbuf(icharbuf *buf);
void		__add_icharbuf(icharbuf *buf, int chr);
void		del_icharbuf(icharbuf *buf);
void		terminate_icharbuf(icharbuf *buf);
void		empty_icharbuf(icharbuf *buf);

ocharbuf *	init_ocharbuf(ocharbuf *buf, size_t limit);
ocharbuf *	new_ocharbuf(size_t limit);
void		free_ocharbuf(ocharbuf *buf);
ocharbuf *	malloc_ocharbuf(ocharbuf *buf);
void		add_ocharbuf(ocharbuf *buf, int chr);
void		del_ocharbuf(ocharbuf *buf);
void		terminate_ocharbuf(ocharbuf *buf);
void		empty_ocharbuf(ocharbuf *buf);
#define fetch_ocharbuf(buf, at) ((wint_t)buf->data.w[at])
#define poke_ocharbuf(buf, at, chr) \
	{ buf->data.w[at] = chr; \
	}

void		init_ring(void);
void		stop_ring(void);
const wchar_t *	str_summary(const wchar_t *s, int len);
wchar_t *	str2ring(const wchar_t *in);
void *		ringallo(size_t);
wchar_t *	utf8towcs(const char *in);
char *		wcstoutf8(const wchar_t *in);
ichar *		load_sgml_file_to_charp(const ichar *file, int normalise_rsre,
					size_t *len);
FILE *		wfopen(const wchar_t *name, const char *mode);

#if defined(USE_STRING_FUNCTIONS) && !defined(UTIL_H_IMPLEMENTATION)

#define istrlen(s1)   wcslen((s1))
#define istreq(s1,s2) (wcscmp((s1),(s2))==0)

#endif

#endif /*DTD_UTIL_H_INCLUDED*/
