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

#define URL_xsd		  "http://www.w3.org/2001/XMLSchema#"

#define issign(c)  (c == '-' || c == '+')
#define isdigit(c) (c >= '0' && c <= '9')
#define isexp(c)   (c == 'e' || c == 'E')
#define isdot(c)   (c == '.')

/* BUG: goes wrong if locale is switched at runtime.  But, doing
   this dynamically is a more than 100% overhead on xsd_number_string/2
   and changing locale after process initialization is uncommon and
   a bad idea anyway.
*/

static int
decimal_dot(void)
{ static int ddot = '\0';

  if ( ddot )
    return ddot;

  char buf[10];
  sprintf(buf, "%f", 1.0);
  ddot = buf[1];

  return ddot;
}


static foreign_t
xsd_number_string(term_t number, term_t string)
{ char *in;
  size_t len;

  if ( PL_get_nchars(string, &len, &in, CVT_ATOM|CVT_STRING|CVT_LIST) )
  { char *s = in;

    if ( strlen(s) == len )			/* no 0-characters */
    { int isfloat = FALSE;
      int hasdot=FALSE;

      if ( strcmp(s, "NaN") != 0 )
      { int decl = 0, dect = 0;

	if ( issign(*s) ) s++;			/* [+-]? */
	if ( strcmp(s, "INF") == 0 )
	{ isfloat = TRUE;
	  goto ok;
	}

	while(isdigit(*s)) decl++, s++;		/* [0-9]* */
	if ( isdot(*s) )			/* [.]? */
	{ s++;
	  isfloat = TRUE;
	  hasdot = TRUE;
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
	if ( *s )
	  goto syntax_error;
      } else
      { isfloat = TRUE;
      }

    ok:
      if ( isfloat )
      { int dot;
	int rc;
	char *end;

	if ( hasdot && (dot=decimal_dot()) != '.' )
	{ char fast[64];
	  char *fs = len < sizeof(fast) ? fast : malloc(len+1);
	  char *o;

	  if ( !fs )
	    return PL_resource_error("memory");
	  for(s=in,o=fs; *s; s++,o++)
	  { if ( (*o=*s) == '.' )
	      *o = dot;
	  }
	  *o = '\0';
	  rc = PL_unify_float(number, strtod(fs, &end));
	  if ( fs != fast )
	    free(fs);
	} else
	{ rc = PL_unify_float(number, strtod(in, &end));
	}
	assert(*end == '\0');

	return rc;
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

		 /*******************************
		 *	   DATE AND TIME	*
		 *******************************/

static functor_t FUNCTOR_date3;
static functor_t FUNCTOR_time3;
static functor_t FUNCTOR_date_time6;
static functor_t FUNCTOR_date_time7;
static functor_t FUNCTOR_month_day2;
static functor_t FUNCTOR_year_month2;
static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_syntax_error1;
static functor_t FUNCTOR_domain_error2;
static functor_t FUNCTOR_xsd_time1;

static atom_t    URL_date;
static atom_t    URL_dateTime;
static atom_t    URL_gDay;
static atom_t    URL_gMonth;
static atom_t    URL_gMonthDay;
static atom_t    URL_gYear;
static atom_t    URL_gYearMonth;
static atom_t    URL_time;

typedef struct
{ int hour;
  int minute;
  int sec_is_float;
  union
  { int i;
    double f;
  } second;
} time;


static int
get_int_arg(term_t t, int n, int *ip)
{ term_t a = PL_new_term_ref();

  _PL_get_arg(n, t, a);
  return PL_get_integer_ex(a, ip);
}


static int
get_int_args(term_t t, int n, int *av)
{ term_t a = PL_new_term_ref();
  int i;

  for(i=0; i<n; i++)
  { _PL_get_arg(i+1, t, a);
    if ( !PL_get_integer_ex(a, &av[i]) )
      return FALSE;
  }

  return TRUE;
}

static int
get_time_args(term_t t, int offset, time *tm)
{ term_t a = PL_new_term_ref();

  _PL_get_arg(offset+1, t, a);
  if ( !PL_get_integer_ex(a, &tm->hour) )
    return FALSE;
  _PL_get_arg(offset+2, t, a);
  if ( !PL_get_integer_ex(a, &tm->minute) )
    return FALSE;
  _PL_get_arg(offset+3, t, a);
  if ( PL_get_integer(a, &tm->second.i) )
  { tm->sec_is_float = FALSE;
  } else if ( PL_get_float_ex(a, &tm->second.f) )
  { tm->sec_is_float = TRUE;
  } else
    return FALSE;

  return TRUE;
}


typedef enum
{ END,
  INT2,
  INT4,
  DECIMAL,
  MINUS,
  PLUS,
  COLON,
  TT,
  TZ
} time_field;


static int
parse_date_parts(const char *in, int *av, size_t avlen)
{ size_t ca = 0;
#define ADDINT(i) \
	do { if ( ca < avlen ) { av[ca++] = (i); } else { return 1; } } while(0)
#define DV(i) (in[i]-'0')

  while(*in)
  { if ( isdigit(in[0]) &&
	 isdigit(in[1]) )
    { if ( isdigit(in[2]) &&
	   isdigit(in[3]) )
      { ADDINT(INT4);
	ADDINT(DV(0)*1000+DV(1)*100+DV(2)*10+DV(3));
	in += 4;
      } else
      { ADDINT(INT2);
	ADDINT(DV(0)*10+DV(1));
	in += 2;
      }
    } else
    { switch(in[0])
      { case '-': ADDINT(MINUS); in++; break;
	case '+': ADDINT(PLUS);  in++; break;
	case ':': ADDINT(COLON); in++; break;
	case 'T': ADDINT(TT);    in++; break;
	case 'Z': ADDINT(TZ);    in++; break;
	case '.': ADDINT(DECIMAL); in++;
        { int v = 0;
	  if ( !isdigit(in[0]) )
	    return 2;
	  while(isdigit(in[0]))
	  { v = v*10+DV(0);
	    in++;
	  }
	  ADDINT(v);
	  break;
	}
	default:
	  return 2;
      }
    }
  }
  ADDINT(END);

#undef DV
#undef ADDINT

  return 0;
}


static int
int_domain(const char *domain, int i)
{ term_t t = PL_new_term_ref();

  return ( PL_put_integer(t, i) && PL_domain_error(domain, t) );
}


static int
float_domain(const char *domain, double f)
{ term_t t = PL_new_term_ref();

  return ( PL_put_float(t, f) && PL_domain_error(domain, t) );
}


static int
valid_year(int i)
{ if ( i != 0 && i >= -9999 && i <= 9999 )
    return TRUE;
  return int_domain("year", i);
}


static int
valid_month(int i)
{ if ( i >= 1 && i <= 12 )
    return TRUE;
  return int_domain("month", i);
}


static int
valid_day(int i)
{ if ( i >= 1 && i <= 31 )
    return TRUE;
  return int_domain("day", i);
}


static int
valid_hour(int i)
{ if ( i >= 0 && i <= 23 )
    return TRUE;
  return int_domain("hour", i);
}


static int
valid_minute(int i)
{ if ( i >= 0 && i <= 59 )
    return TRUE;
  return int_domain("minute", i);
}


static int
valid_second(int i)
{ if ( i >= 0 && i <= 59 )
    return TRUE;
  return int_domain("second", i);
}


static int
valid_second_f(double f)
{ if ( f >= 0.0 && f < 60.0 )
    return TRUE;
  return float_domain("second", f);
}


static int
valid_date(int v[3])
{ return (valid_year(v[0]) && valid_month(v[1]) && valid_day(v[2]));
}


static int
valid_time(const time *t)
{ if ( t->hour == 24 && t->minute == 0 &&
       (t->sec_is_float ? t->second.f == 0.0 : t->second.i == 0) )
    return TRUE;			/* 24:00:00[.0+] */

  if ( valid_hour(t->hour) &&
       valid_minute(t->minute) )
  { if ( t->sec_is_float )
      return valid_second_f(t->second.f);
    else
      return valid_second(t->second.i);
  }

  return FALSE;
}


static int
valid_tz(int hoff, int moff)
{ if ( hoff >= 0 && hoff <= 13 )
    return valid_minute(moff);
  else if ( hoff == 14 && moff == 0 )
    return TRUE;
  else
    return int_domain("tz_hour", hoff);
}


static int
valid_tzoffset(int sec)
{ if ( sec < -12*3600 || sec > 12*3600 )
    return int_domain("tz_offset", sec);
  return TRUE;
}


static int
is_time_seq(const int av[], time *t)
{ if ( av[0] == INT2 && av[2] == COLON &&
       av[3] == INT2 && av[5] == COLON &&
       av[6] == INT2 )
  { t->hour   = av[1];
    t->minute = av[4];
    if ( av[8] == DECIMAL )
    { int v, div = 1;

      t->sec_is_float = TRUE;
      for(v=av[9]; v; v /= 10)
	div *= 10;
      t->second.f = (double)av[7] + (double)av[9]/(double)div;
      return 10;
    } else
    { t->sec_is_float = FALSE;
      t->second.i = av[7];
      return 8;
    }
  }

  return 0;
}


static char *
time_sec_chars(time *t, char *buf)
{ if ( !t->sec_is_float )
  { sprintf(buf, "%02d", t->second.i);
    return buf;
  } else
  { char *s, *e;

    buf[0] = '0';
    sprintf(&buf[1], "%f", t->second.f);
    if ( !isdigit(buf[2]) )
    { buf[2] = '.';
      s = buf;
    } else
    { assert(!isdigit(buf[3]));
      buf[3] = '.';
      s = buf+1;
    }
    e = s+strlen(s);
    while(e[-1] == '0' && e[-2] != '.' )
      e--;
    e[0] = 0;

    return s;
  }
}



/** xsd_time_string(+Term, ?Type, -String) is det.
    xsd_time_string(-Term, ?Type, +String) is det.
*/

static int
is_time_url(atom_t url)
{ return ( url == URL_date ||
	   url == URL_dateTime ||
	   url == URL_gDay ||
	   url == URL_gMonth ||
	   url == URL_gMonthDay ||
	   url == URL_gYear ||
	   url == URL_gYearMonth ||
	   url == URL_time );
}

static int
maybe_invalid_time_url(term_t type)
{ atom_t url;

  if ( PL_get_atom_ex(type, &url) )
  { if ( is_time_url(url) )
      return FALSE;
    return PL_domain_error("xsd_time_url", type);
  }

  return FALSE;
}


static int
mkyear(int v, int sign)
{ if ( sign == 1 )
  { if ( v == 0 )
      return -1;
    return v;
  }
  return -(v+1);
}


static int
unify_parsed_type(term_t t, atom_t type)
{ if ( PL_unify_atom(t, type) )
  { return TRUE;
  } else
  { if ( PL_is_atom(t) )
    { term_t ex;

      return ( (ex=PL_new_term_ref()) &&
	       PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
				   PL_FUNCTOR, FUNCTOR_syntax_error1,
			             PL_FUNCTOR, FUNCTOR_xsd_time1,
			               PL_ATOM, type,
				   PL_VARIABLE) &&
	       PL_raise_exception(ex)
	     );
    }

    return FALSE;
  }
}


static int
incompatible_time_term(term_t term, atom_t type_atom)
{ term_t ex;

  return ( (ex=PL_new_term_ref()) &&
	   PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
			       PL_FUNCTOR, FUNCTOR_domain_error2,
				 PL_FUNCTOR, FUNCTOR_xsd_time1,
				   PL_ATOM, type_atom,
				 PL_TERM, term,
			       PL_VARIABLE) &&
	   PL_raise_exception(ex)
	 );
}

static int
unify_prolog_type(term_t term, term_t type_term, atom_t type_atom)
{ if ( PL_unify_atom(type_term, type_atom) )
  { return TRUE;
  } else
  { if ( PL_is_atom(type_term) )
      incompatible_time_term(term, type_atom);

    return FALSE;
  }
}



static foreign_t
xsd_time_string(term_t term, term_t type, term_t string)
{ if ( PL_is_variable(string) )			/* +, ?, - */
  { char buf[100], b2[20];
    atom_t at = 0;
    int v;

    if ( PL_is_functor(term, FUNCTOR_date3) )	/* date(Y,M,D) */
    { int v[3];
      char *sign = "";

      if ( !get_int_args(term, 3, v) || !valid_date(v) )
	return FALSE;
      at = URL_date;
      if ( v[0] < 0 )
      { sign = "-";
	v[0] = -v[0];
      }
      sprintf(buf, "%s%04d-%02d-%02d", sign, v[0],v[1],v[2]);
    } else if ( PL_is_functor(term, FUNCTOR_date_time6) )
    { int v[3];
      time t;
      char *sign = "";

      if ( !get_int_args(term, 3, v) ||
	   !get_time_args(term, 3, &t) ||
	   !valid_date(v) || !valid_time(&t) )
	return FALSE;
      at = URL_dateTime;
      if ( v[0] < 0 )
      { sign = "-";
	v[0] = -v[0];
      }
      sprintf(buf, "%s%04d-%02d-%02dT%02d:%02d:%s",
	      sign, v[0], v[1], v[2], t.hour, t.minute, time_sec_chars(&t, b2));
    } else if ( PL_is_functor(term, FUNCTOR_date_time7) )
    { int v[3], tz;
      time t;
      char *sign = "";

      if ( !get_int_args(term, 3, v) ||
	   !get_time_args(term, 3, &t) ||
	   !get_int_arg(term, 7, &tz) ||
	   !valid_date(v) || !valid_time(&t) ||
	   !valid_tzoffset(tz) )
	return FALSE;
      at = URL_dateTime;
      if ( v[0] < 0 )
      { sign = "-";
	v[0] = -v[0];
      }
      sprintf(buf, "%s%04d-%02d-%02dT%02d:%02d:%s",
	      sign, v[0], v[1], v[2], t.hour, t.minute, time_sec_chars(&t, b2));
      if ( tz == 0 )
      { strcat(buf, "Z");
      } else
      { char sign = tz < 0 ? '-' : '+';
	int tza   = tz < 0 ? -tz : tz;
	char *out = buf+strlen(buf);
	sprintf(out, "%c%02d:%02d", sign, tza/3600, (tza % 3600)/60);
      }
    } else if ( PL_is_functor(term, FUNCTOR_time3) )
    { time t;

      if ( !get_time_args(term, 0, &t) || !valid_time(&t) )
	return FALSE;
      at = URL_time;
      sprintf(buf, "%02d:%02d:%s",
	      t.hour, t.minute, time_sec_chars(&t, b2));
    } else if ( PL_is_functor(term, FUNCTOR_month_day2) )
    { int v[2];

      if ( !get_int_args(term, 2, v) )
	return FALSE;
      at = URL_gMonthDay;
      sprintf(buf, "%02d-%02d",
	      v[0], v[1]);
    } else if ( PL_is_functor(term, FUNCTOR_year_month2) )
    { int v[2];
      char *sign = "";

      if ( !get_int_args(term, 2, v) )
	return FALSE;
      if ( v[0] < 0 )
      { sign = "-";
	v[0] = -v[0];
      }
      at = URL_gYearMonth;
      sprintf(buf, "%s%04d-%02d", sign, v[0], v[1]);
    } else if ( PL_get_integer(term, &v) )
    { atom_t url;

      if ( !PL_get_atom_ex(type, &url) )
	return FALSE;
      if ( url == URL_gDay )
      { if ( !valid_day(v) )
	  return FALSE;
	sprintf(buf, "%02d", v);
      } else if ( url == URL_gMonth )
      { if ( !valid_month(v) )
	  return FALSE;
	sprintf(buf, "%02d", v);
      } else if ( url == URL_gYear )
      { char *sign = "";
	if ( !valid_year(v) )
	  return FALSE;
	if ( v == -1 )
	{ v = 0;
	} else if ( v < 0 )
	{ sign = "-";
	  v = -v - 1;
	}
	sprintf(buf, "%s%04d", sign, v);
      } else if ( is_time_url(url) )
      { return incompatible_time_term(term, url);
      } else
	return PL_domain_error("xsd_time_url", type);
    } else
    { return PL_domain_error("xsd_time", term);
    }

    if ( at && !unify_prolog_type(term, type, at) )
      return FALSE;

    return PL_unify_chars(string, PL_STRING, (size_t)-1, buf);
  } else					/* -, ?, + */
  { char *in;
    size_t len;
    int avb[30];
    int *av = avb;
    time t;
    int tlen;
    int yearsign = 1;

    if ( !PL_get_nchars(string, &len, &in,
			CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
      return FALSE;

    if ( parse_date_parts(in, avb, sizeof(avb)/sizeof(*avb)) )
      return PL_syntax_error("xsd_time", NULL);

    if ( av[0] == MINUS )
    { av++;
      yearsign = -1;
    }

    if ( av[0] == INT4 && av[2] == MINUS &&
	 av[3] == INT2 && av[5] == MINUS &&
	 av[6] == INT2 )			/* YYYY-MM-DD */
    { int v[3] = {mkyear(av[1],yearsign),av[4],av[7]};

      if ( !valid_date(v) )
	return FALSE;

      if ( av[8] == END )
      { return (unify_parsed_type(type, URL_date) &&
		PL_unify_term(term, PL_FUNCTOR, FUNCTOR_date3,
			      PL_INT, mkyear(av[1],yearsign),
			      PL_INT, av[4], PL_INT, av[7]) );
      } else if ( av[8] == TT &&
		  (tlen=is_time_seq(av+9, &t)) ) /* THH:MM:SS[.DDD] */
      { int at = 9+tlen;
	term_t sec = PL_new_term_ref();

	if ( !valid_time(&t) )
	  return FALSE;

	if ( !(t.sec_is_float ? PL_put_float(sec, t.second.f)
			      : PL_put_integer(sec, t.second.i)) )
	  return FALSE;

	if ( av[at] == END )
	  return (unify_parsed_type(type, URL_dateTime) &&
		  PL_unify_term(term, PL_FUNCTOR, FUNCTOR_date_time6,
				PL_INT, mkyear(av[1],yearsign),
				PL_INT, av[ 4], PL_INT, av[ 7],
				PL_INT, t.hour, PL_INT, t.minute, PL_TERM, sec));
	if ( av[at] == TZ && av[at+1] == END )
	  return (unify_parsed_type(type, URL_dateTime) &&
		  PL_unify_term(term, PL_FUNCTOR, FUNCTOR_date_time7,
				PL_INT, mkyear(av[1],yearsign),
				PL_INT, av[ 4], PL_INT, av[ 7],
				PL_INT, t.hour, PL_INT, t.minute, PL_TERM, sec,
			        PL_INT, 0));
	if ( (av[at] == MINUS || av[at] == PLUS) &&
	     av[at+1] == INT2 && av[at+3] == COLON &&
	     av[at+4] == INT2 && av[at+6] == END )
	{ int tz = av[at+2]*3600 + av[at+5]*60;
	  if ( av[at] == MINUS ) tz = -tz;

	  if ( !valid_tz(av[at+2], av[at+5]) )
	    return FALSE;

	  return (unify_parsed_type(type, URL_dateTime) &&
		  PL_unify_term(term, PL_FUNCTOR, FUNCTOR_date_time7,
				PL_INT, mkyear(av[1],yearsign),
				PL_INT, av[ 4], PL_INT, av[ 7],
				PL_INT, t.hour, PL_INT, t.minute, PL_TERM, sec,
			        PL_INT, tz));
	}
      }
    }

    if ( (tlen=is_time_seq(av, &t)) && av[tlen] == END )
    { term_t sec = PL_new_term_ref();

      if ( !valid_time(&t) )
	return FALSE;
      if ( yearsign == -1 )
	return PL_syntax_error("xsd_time", NULL);

      if ( !(t.sec_is_float ? PL_put_float(sec, t.second.f)
			    : PL_put_integer(sec, t.second.i)) )
	return FALSE;

      return (valid_time(&t) &&
	      unify_parsed_type(type, URL_time) &&
	      PL_unify_term(term, PL_FUNCTOR, FUNCTOR_time3,
			    PL_INT, t.hour, PL_INT, t.minute, PL_TERM, sec) );
    }
    if ( av[0] == INT2 && av[2] == MINUS && av[3] == INT2 && av[5] == END )
    { if ( yearsign == -1 )
	return PL_syntax_error("xsd_time", NULL);

      return (valid_month(av[1]) && valid_day(av[4]) &&
	      unify_parsed_type(type, URL_gMonthDay) &&
	      PL_unify_term(term, PL_FUNCTOR, FUNCTOR_month_day2,
			    PL_INT, av[1], PL_INT, av[4]) );
    }
    if ( av[0] == INT4 && av[2] == MINUS && av[3] == INT2 && av[5] == END )
    { return (valid_year(mkyear(av[1],yearsign)) && valid_month(av[4]) &&
	      unify_parsed_type(type, URL_gYearMonth) &&
	      PL_unify_term(term, PL_FUNCTOR, FUNCTOR_year_month2,
			    PL_INT, mkyear(av[1],yearsign), PL_INT, av[4]) );
    }
    if ( av[0] == INT4 && av[2] == END )
    { return (valid_year(mkyear(av[1],yearsign)) &&
	      unify_parsed_type(type, URL_gYear) &&
	      PL_unify_integer(term, mkyear(av[1],yearsign)));
    }
    if ( av[0] == INT2 && av[2] == END )
    { atom_t url;

      if ( yearsign == -1 )
	return PL_syntax_error("xsd_time", NULL);

      if ( !PL_get_atom_ex(type, &url) )
	return FALSE;
      if ( url == URL_gDay )
	return valid_day(av[1]) && PL_unify_integer(term, av[1]);
      if ( url == URL_gMonth )
	return valid_month(av[1]) && PL_unify_integer(term, av[1]);
      return maybe_invalid_time_url(type);
    }

    return PL_syntax_error("xsd_time", NULL);
  }
}

#define MKFUNCTOR(n, a) \
	FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)
#define MKURL(n) \
	URL_ ## n = PL_new_atom(URL_xsd #n);

install_t
install_xsd(void)
{ MKFUNCTOR(date, 3);
  MKFUNCTOR(date_time, 6);
  MKFUNCTOR(date_time, 7);
  MKFUNCTOR(time, 3);
  MKFUNCTOR(month_day, 2);
  MKFUNCTOR(year_month, 2);
  MKFUNCTOR(error, 2);
  MKFUNCTOR(syntax_error, 1);
  MKFUNCTOR(domain_error, 2);
  MKFUNCTOR(xsd_time, 1);

  MKURL(date);
  MKURL(dateTime);
  MKURL(gDay);
  MKURL(gMonth);
  MKURL(gMonthDay);
  MKURL(gYear);
  MKURL(gYearMonth);
  MKURL(time);

  PL_register_foreign("xsd_number_string", 2, xsd_number_string, 0);
  PL_register_foreign("xsd_time_string",   3, xsd_time_string,	 0);
}
