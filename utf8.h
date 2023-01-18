/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2023, University of Amsterdam
			      SWI-Prolog Solutions b.v.
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

#ifndef UTF8_H_INCLUDED
#define UTF8_H_INCLUDED

#define UNICODE_MAX (0x10FFFF)

#define ISUTF8_MB(c) ((unsigned)(c) >= 0xc0 && (unsigned)(c) <= 0xfd)

#define utf8_get_char(in, chr) \
	(*(in) & 0x80 ? sgml__utf8_get_char(in, chr) \
		      : (*(chr) = *(in), (char *)(in)+1))

extern char *sgml__utf8_get_char(const char *in, int *chr);
#define utf8_get_uchar(in, chr) (unsigned char*)utf8_get_char((char*)(in), chr)

extern char *sgml_utf8_put_char(char *out, int chr);
#define utf8_put_char(out, chr) \
	((chr) < 0x80 ? out[0]=(char)(chr), out+1 \
		      : sgml_utf8_put_char(out, (chr)))

extern size_t sgml_utf8_strlen(const char *s, size_t len);
#define utf8_strlen sgml_utf8_strlen

		 /*******************************
		 *	      UTF-16		*
		 *******************************/

#include <stddef.h>			/* get wchar_t */

/* See https://en.wikipedia.org/wiki/UTF-16#Examples */

#define IS_UTF16_LEAD(c)      ((c) >= 0xD800 && (c) <= 0xDBFF)
#define IS_UTF16_TRAIL(c)     ((c) >= 0xDC00 && (c) <= 0xDFFF)
#define IS_UTF16_SURROGATE(c) ((c) >= 0xD800 && (c) <= 0xDFFF)
#define VALID_CODE_POINT(c)   ((c) >= 0 && (c) <= UNICODE_MAX && !IS_UTF16_SURROGATE(c))

static inline int
utf16_decode(int lead, int trail)
{ int l = (lead-0xD800) << 10;
  int t = (trail-0xDC00);

  return l+t+0x10000;
}

static inline void
utf16_encode(int c, int *lp, int *tp)
{ c -= 0x10000;
  *lp = (c>>10)+0xD800;
  *tp = (c&0X3FF)+0xDC00;
}

static inline wchar_t*
utf16_put_char(wchar_t *out, int chr)
{ if ( chr <= 0xffff )
  { *out++ = chr;
  } else
  { int l, t;

    utf16_encode(chr, &l, &t);
    *out++ = l;
    *out++ = t;
  }

  return out;
}

static inline wchar_t*
put_wchar(wchar_t *out, int chr)
{
#if SIZEOF_WCHAR_T == 2
  return utf16_put_char(out, chr);
#else
  *out++ = chr;
  return out;
#endif
}

static inline const wchar_t*
get_wchar(const wchar_t *in, int *chr)
{
#if SIZEOF_WCHAR_T == 2
  int c = *in++;
  if ( IS_UTF16_LEAD(c) && IS_UTF16_TRAIL(in[0]) )
  { *chr = utf16_decode(c, in[0]);
    in++;
  } else
  { *chr = c;
  }
  return in;
#else
  *chr = *in++;
  return in;
#endif
}

static inline const wchar_t*
get_wchar_r(const wchar_t *in, int *chr)
{
#if SIZEOF_WCHAR_T == 2
  if ( IS_UTF16_TRAIL(in[-1]) && IS_UTF16_LEAD(in[-2]) )
  { *chr = utf16_decode(in[-2], in[-1]);
    in -= 2;
  } else
  { *chr = *--in;
  }
  return in;
#else
  *chr = *--in;
  return in;
#endif
}

static inline size_t
utf8_utf16strlen(const char *s, size_t len)
{
#if SIZEOF_WCHAR_T == 2
  const char *e = s+len;
  size_t n = 0;

  while(s<e)
  { int c;

    s = utf8_get_char(s, &c);
    n++;
    if ( c > 0xffff )
      n++;
  }

  return n;
#else
  return utf8_strlen(s, len);
#endif
}


#endif /*UTF8_H_INCLUDED*/
