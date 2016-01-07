/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, VU University Amsterdam
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

#include <SWI-Prolog.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

#define issign(c)  (c == '-' || c == '+')
#define isdigit(c) (c >= '0' && c <= '9')
#define isexp(c)   (c == 'e' || c == 'E')
#define isdot(c)   (c == '.')

static foreign_t
xsd_number_string(term_t number, term_t string)
{ char *in;
  size_t len;

  if ( PL_get_nchars(string, &len, &in, CVT_ATOM|CVT_STRING|CVT_LIST) )
  { char *s = in;

    if ( strlen(s) == len )			/* no 0-characters */
    { int isfloat = FALSE;

      if ( !strcmp(s, "NaN") == 0 )
      { int decl = 0, dect = 0;

	if ( issign(*s) ) s++;			/* [+-]? */
	if ( strcmp(s, "INF") == 0 )
	  goto ok;

	while(isdigit(*s)) decl++, s++;		/* [0-9]* */
	if ( isdot(*s) )			/* [.]? */
	{ s++;
	  isfloat = TRUE;
	  while(isdigit(*s)) dect++, s++;	/* [0-9]* */
	}
	if ( decl+dect == 0 )
	  goto syntax_error;
	if ( isexp(*s) )
	{ int exp = 0;

	  s++;
	  isfloat = TRUE;
	  if ( issign(*s) ) s++;		/* [+-]? */
	  while(isdigit(*s)) exp++, s++;	/* [0-9]+ */
	  if ( exp == 0 )
	    goto syntax_error;
	}
      }

    ok:
      if ( isfloat )
      { return PL_unify_float(number, strtod(in, NULL));
      } else
      { term_t n = PL_new_term_ref();
	return ( PL_chars_to_term(in, n) &&
		 PL_unify(number, n)
	       );
      }
    } else
    {
    syntax_error:
      return PL_syntax_error("xsd_number", NULL);
    }
  } else if ( PL_get_nchars(number, &len, &in, CVT_NUMBER) )
  { if ( PL_is_float(number) )
    { char buf[32];
      char *s, *e;
      int exp_shift = 0;

      if ( len > 3 && strcmp(&in[len-3], "Inf") == 0 )
	return PL_unify_chars(string, PL_STRING, (size_t)-1,
			      in[0] == '-' ? "-INF" : "INF");
      if ( len > 3 && strcmp(&in[len-3], "NaN") == 0 )
	return PL_unify_chars(string, PL_STRING, (size_t)-1, "NaN");

      assert(len < 32);
      strcpy(buf, in);
      s = buf;
      if ( s[0] == '-' ) s++;
      if ( s[0] == '0' )
      { assert(s[1] == '.');
	s += 2;
	if ( *s == '0' && s[1] )
	{ for(e=s; *e=='0'; e++) exp_shift--;
	  memmove(&s[0], &s[-exp_shift], strlen(&s[-exp_shift])+1);
	}
      } else
      { char *dp = strchr(s, '.');
	if ( dp-s > 1 )
	{ exp_shift = dp-s-1;
	  memmove(&s[2], &s[1], exp_shift);
	  s[1] = '.';
	}
      }

      if ( (e=strchr(buf, 'e')) )
      { *e++ = 'E';
	if ( e[0] == '+' )
	  memmove(&e[0], &e[1], strlen(&e[1])+1);
	if ( exp_shift )
	  sprintf(e, "%d", atoi(e)+exp_shift);
      } else
      { e = &buf[strlen(buf)];

	if ( exp_shift > 0 )
	{ while(e[-1] == '0' && e[-2] != '.')
	    e--;
	}
	sprintf(e, "E%d", exp_shift);
      }

      return PL_unify_chars(string, PL_STRING, (size_t)-1, buf);
    } else
    { return PL_unify_chars(string, PL_STRING, len, in);
    }
  } else if ( !PL_is_variable(number) )
  { return PL_type_error("number", number);
  } else
  { return PL_type_error("text", string);
  }
}


install_t
install_xsd(void)
{ PL_register_foreign("xsd_number_string", 2, xsd_number_string, 0);
}
