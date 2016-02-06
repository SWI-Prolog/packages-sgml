/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Richard O'Keefe
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2012, University of Amsterdam
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
#include "util.h"
#include "catalog.h"
#include <stdio.h>
#include <wctype.h>
#include <string.h>
#include <stdlib.h>
#define DTD_MINOR_ERRORS 1
#include <dtd.h>			/* error codes */

#ifdef __WINDOWS__
#define swprintf _snwprintf
#endif

#ifdef _REENTRANT
#include <pthread.h>

static pthread_mutex_t catalog_mutex = PTHREAD_MUTEX_INITIALIZER;
#define LOCK() pthread_mutex_lock(&catalog_mutex)
#define UNLOCK() pthread_mutex_unlock(&catalog_mutex)
#else
#define LOCK()
#define UNLOCK()
#endif

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif
#ifndef MAXLINE
#define MAXLINE 1024
#endif
#ifndef EOS
#define EOS '\0'
#endif
#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#define streq(s1, s2) istreq(s1, s2)
#define uc(p) (*(p))

typedef struct catalogue_item *catalogue_item_ptr;
struct catalogue_item
{ catalogue_item_ptr next;
  int kind;
  ichar const *target;
  ichar const *replacement;
};

static catalogue_item_ptr first_item = 0, last_item = 0;

typedef struct _catalog_file
{ ichar *file;
  struct _catalog_file *next;
  int loaded;				/* did we parse this file? */
  catalogue_item_ptr first_item;	/* List of items in the file */
  catalogue_item_ptr last_item;
} catalog_file;

static catalog_file *catalog;

#ifdef __WINDOWS__
#define isDirSep(c) ((c) == '/' || (c) == '\\')
#define DIRSEPSTR L"\\"
#else
#define isDirSep(c) ((c) == '/')
#define DIRSEPSTR L"/"
#endif

static ichar *
DirName(const ichar *f, ichar *dir)
{ const ichar *base, *p;

  for (base = p = f; *p; p++)
  { if (isDirSep(*p) && p[1] != EOS)
      base = p;
  }
  if (base == f)
  { if (isDirSep(*f))
      istrcpy(dir, DIRSEPSTR);
    else
      istrcpy(dir, L".");
  } else
  { istrncpy(dir, f, base - f);
    dir[base - f] = EOS;
  }

  return dir;
}


int
is_absolute_path(const ichar *name)
{ if (isDirSep(name[0])
#ifdef __WINDOWS__
      || (iswalpha(uc(name)) && name[1] == ':')
#endif
    )
    return TRUE;

  return FALSE;
}

int
is_url(const ichar *name)
{ if ( iswalpha(name[0]) )
  { while(*name && iswalpha(*name))
      name++;
    if ( *name && name[0] == ':' && name[1] == '/' && name[2] == '/' )
      return TRUE;
  }

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
localpath() creates an absolute  path  for   name  relative  to ref. The
returned path must be freed using sgml_free() when done.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ichar *
localpath(const ichar *ref, const ichar *name)
{ ichar *local;

  if (!ref || is_absolute_path(name))
    local = istrdup(name);
  else
  { ichar buf[MAXPATHLEN];

    DirName(ref, buf);
    istrcat(buf, DIRSEPSTR);
    istrcat(buf, name);

    local = istrdup(buf);
  }

  if (!local)
    sgml_nomem();

  return local;
}


int
register_catalog_file_unlocked(const ichar *file, catalog_location where)
{ catalog_file **f = &catalog;
  catalog_file *cf;

  for (; *f; f = &(*f)->next)
  { cf = *f;

    if (istreq(cf->file, file))
      return TRUE;		/* existing, move? */
  }

  cf = sgml_malloc(sizeof(*cf));
  memset(cf, 0, sizeof(*cf));
  cf->file = istrdup(file);
  if (!cf->file)
    sgml_nomem();

  if (where == CTL_END)
  { cf->next = NULL;
    *f = cf;
  } else
  { cf->next = catalog;
    catalog = cf;
  }

  return TRUE;
}


static wchar_t *
wgetenv(const char *name)
{ const char *vs;

  if ( (vs = getenv(name)) )
  { size_t wl = mbstowcs(NULL, vs, 0);

    if ( wl > 0 )
    { wchar_t *ws = sgml_malloc((wl+1)*sizeof(wchar_t));
      mbstowcs(ws, vs, wl+1);

      return ws;
    }
  }

  return NULL;
}


static void
init_catalog()
{ static int done = FALSE;

  LOCK();
  if ( !done++ )
  { ichar *path = wgetenv("SGML_CATALOG_FILES");

    if (!path)
    { UNLOCK();
      return;
    }

    while (*path)
    { ichar buf[MAXPATHLEN];
      ichar *s;

      if ((s = istrchr(path, L':')))
      { istrncpy(buf, path, s - path);
	buf[s - path] = '\0';
	path = s + 1;
	if ( buf[0] )			/* skip empty entries */
	  register_catalog_file_unlocked(buf, CTL_START);
      } else
      { if ( path[0] )			/* skip empty entries */
	  register_catalog_file_unlocked(path, CTL_START);
	break;
      }
    }
  }
  UNLOCK();
}


int
register_catalog_file(const ichar *file, catalog_location where)
{ int rc;

  init_catalog();

  LOCK();
  rc = register_catalog_file_unlocked(file, where);
  UNLOCK();

  return rc;
}


		 /*******************************
		 *     CATALOG FILE PARSING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The code from here to the  end  of   this  file  was  written by Richard
O'Keefe and modified by Jan Wielemaker to fit   in  with the rest of the
parser.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*  OVERRIDE YES/NO
    sets a boolean flag initialised to NO.
    The value of this flag is stored as part of each entry.
    (PUBLIC|DOCTYPE|ENTITY)&YES will match whether a system identifier
    was provided in the source document or not;
    (PUBLIC|DOCTYPE|ENTITY)&NO will only match if a system identifier
    was not provided.
*/

/*  catalogue =
    (   PUBLIC  pubid filename
    |   SYSTEM  sysid filename
    |   DOCTYPE name  filename
    |   ENTITY  name  filename
    |   OVERRIDE YES
    |   OVERRIDE NO
    |   BASE          filename
    |   junk
    )*
*/


/*  Keywords are matched ignoring case.  */

static int
ci_streql(ichar const *a, ichar const *b)
{ return istrcaseeq(a, b);
}

/*  Names may be matched heading case in XML.  */

static int
cs_streql(ichar const *a, ichar const *b)
{ return istreq(a, b);
}

/*  Any other word or any quoted string is reported as CAT_OTHER.
    When we are not looking for the beginning of an entry, the only
    positive outcome is CAT_OTHER.
*/

static int
scan_overflow(size_t buflen)
{ gripe(NULL, ERC_REPRESENTATION, L"token length");

  return EOF;
}

static int
scan(FILE* src, ichar *buffer, size_t buflen, int kw_expected)
{ int c, q;
  ichar *p = buffer, *e = p + buflen - 1;

  for (;;)
  { c = getc(src);
    if (c <= ' ')
    { if (c < 0)
	return EOF;
      continue;
    }
    if (c == '-')
    { c = getc(src);
      if (c != '-')
      { *p++ = '-';
	break;
      }
      for (;;)
      { c = getc(src);
	if (c < 0)
	  return EOF;
	if (c == '-')
	{ c = getc(src);
	  if (c < 0)
	    return EOF;
	  if (c == '-')
	    break;
	}
      }
      continue;
    }
    if (c == '"' || c == '\'')
    { q = c;
      for (;;)
      { c = getc(src);
	if (c < 0)
	  return EOF;
	if (c == q)
	{ *p = '\0';
	  return CAT_OTHER;
	}
	if (p == e)
	  return scan_overflow(buflen);
	*p++ = c;
      }
    }
    break;
  }
  /*  We reach here if there is an unquoted token.   */
  /*  Don't try "PUBLIC--well/sortof--'foo' 'bar'"   */
  /*  because hyphens are allowed in unquoted words  */
  /*  and so are slashes and a bunch of other stuff. */
  /*  To keep this code simple, an unquoted token    */
  /*  ends at EOF, ', ", or layout.                  */
  while (c > ' ' && c != '"' && c != '\'')
  { if (p == e)
      return scan_overflow(buflen);
    *p++ = c;
    c = getc(src);
  }
  *p = '\0';
  if (kw_expected)
  { if (ci_streql(buffer, L"public"))
      return CAT_PUBLIC;
    if (ci_streql(buffer, L"system"))
      return CAT_SYSTEM;
    if (ci_streql(buffer, L"entity"))
      return CAT_ENTITY;
    if (ci_streql(buffer, L"doctype"))
      return CAT_DOCTYPE;
    if (ci_streql(buffer, L"override"))
      return CAT_OVERRIDE;
    if (ci_streql(buffer, L"base"))
      return CAT_BASE;
  }
  return CAT_OTHER;
}

/*  The strings can represent names (taken verbatim),
    system identifiers (ditto), or public identifiers (squished).
    We need to squish, and we need to copy.  When it comes to
    squishing, we don't need to worry about Unicode spaces,
    because public identifiers aren't allow to have any characters
    that aren't in ASCII.
*/

static void
squish(ichar *pubid)
{ ichar const *s = (ichar const *) pubid;
  ichar *d = (ichar *) pubid;
  ichar c;
  int w;

  w = 1;
  while ((c = *s++) != '\0')
  { if (c <= ' ')
    { if (!w)
	*d++ = ' ', w = 1;
    } else
    { *d++ = c, w = 0;
    }
  }
  if (w && d != (ichar *) pubid)
    d--;
  *d = '\0';
}

/*  We represent a catalogue internally by a list of
    (CAT_xxx, string, string)
    triples.
*/

static void
load_one_catalogue(catalog_file * file)
{ FILE *src = wfopen(file->file, "r");
  ichar buffer[2 * FILENAME_MAX];
  ichar base[2 * FILENAME_MAX];
  ichar *p;
  int t;
  catalogue_item_ptr this_item;
  int override = 0;

  if ( !src )
  { gripe(NULL, ERC_NO_CATALOGUE, file->file);
    return;
  }

  (void) istrcpy(base, file->file);
  p = base + istrlen(base);
  while (p != base && !isDirSep(p[-1]))
    p--;

  for (;;)
  { t = scan(src, buffer, sizeof(buffer), 1);
    switch (t)
    { case CAT_BASE:
	if (scan(src, buffer, sizeof(buffer), 0) == EOF)
	  break;
	(void) istrcpy(base, buffer);
	p = base + istrlen(base);
	if (p != base && !isDirSep(p[-1]))
	  *p++ = '/';
	continue;
      case CAT_OVERRIDE:
	if (scan(src, buffer, sizeof(buffer), 0) == EOF)
	  break;
	override = towlower(buffer[0]) == 'y' ? CAT_OVERRIDE : 0;
	continue;
      case CAT_PUBLIC:
      case CAT_SYSTEM:
      case CAT_ENTITY:
      case CAT_DOCTYPE:
	this_item = sgml_malloc(sizeof *this_item);
	if (scan(src, buffer, sizeof buffer, 0) == EOF)
	  break;
	if (t == CAT_PUBLIC)
	  squish(buffer);
	this_item->next = 0;
	this_item->kind = t == CAT_SYSTEM ? t : t + override;
	this_item->target = istrdup(buffer);

	if (scan(src, buffer, sizeof buffer, 0) == EOF)
	  break;

	if (is_absolute_path(buffer) || p == base)
	{ this_item->replacement = istrdup(buffer);
	} else
        { (void) istrcpy(p, buffer);
          this_item->replacement = istrdup(base);
        }

	if (file->first_item == 0)
	{ file->first_item = this_item;
	} else
	{ file->last_item->next = this_item;
	}

	file->last_item = this_item;
	continue;
      case EOF:
	break;
      default:
	continue;
    }
    break;
  }

  fclose(src);
}


/*  To look up a DTD:
    f = find_in_catalogue(CAT_DOCTYPE, name, pubid, sysid, ci);
    If it cannot otherwise be found and name is not null,
    ${name}.dtd will be returned.

    To look up a parameter entity:
    f = find_in_catalogue(CAT_PENTITY, name, pubid, sysid, ci);
    The name may begin with a % but need not; if it doesn't
    a % will be prefixed for the search.
    If it cannot otherwise be found ${name}.pen will be returned.

    To look up an ordinary entity:
    f = find_in_catalogue(CAT_ENTITY, name, pubid, sysid, ci);
    If the name begins with a % this is just like a CAT_PENTITY search.
    If it cannot otherwise be found %{name}.ent will be returned.

    The full catalogue format allows for NOTATION (which we still need
    for XML), SGMLDECL, DTDDECL, and LINKTYPE.  At the moment, only
    notation is plausible.  To handle such things,
    f = find_in_catalogue(CAT_OTHER, name, pubid, sysid, ci);
    If it cannot be found, NULL is returned.

    The name, pubid, and sysid may each be NULL.   It doesn't really
    make sense for them all to be NULL.

    For SGML, name matching (DOCTYPE, ENTITY) should normally ignore
    alphabetic case.  Pass ci=1 to make this happen.  For XML, name
    matching must heed alphabetic case.  Pass ci=0 to make that happen.

    A CAT_DOCTYPE, CAT_ENTITY, or CAT_PENTITY search doesn't really make
    sense withint a name, so if the name should happen to be 0, the search
    kind is converted to CAT_OTHER.
*/

ichar const *
find_in_catalogue(int kind,
		  ichar const *name,
		  ichar const *pubid, ichar const *sysid, int ci)
{ ichar penname[FILENAME_MAX];
  const size_t penlen = sizeof(penname)/sizeof(ichar);
  catalogue_item_ptr item;
  ichar const *result;
  catalog_file *catfile;

  init_catalog();

  if ( name == 0 )
  { kind = CAT_OTHER;
  } else
  { switch (kind)
    { case CAT_OTHER:
      case CAT_DOCTYPE:
	break;
      case CAT_PENTITY:
	if (name[0] != '%')
	{ penname[0] = '%';
	  (void) istrcpy(penname + 1, name);
	  name = penname;
	}
	break;
      case CAT_ENTITY:
	if (name[0] == '%')
	{ kind = CAT_PENTITY;
	}
	break;
      default:
	return 0;
    }
  }

  result = 0;
  for (catfile = catalog;; catfile = catfile->next)
  { if (catfile)
    { if (!catfile->loaded)
      { load_one_catalogue(catfile);
	catfile->loaded = TRUE;
      }
      item = catfile->first_item;
    } else
      item = first_item;

    for (; item != 0; item = item->next)
    { switch (item->kind)
      { case CAT_PUBLIC:
	  if (sysid != 0)
	    break;
	/*FALLTHROUGH*/
	case OVR_PUBLIC:
	  if (pubid != 0 && result == 0 && cs_streql(pubid, item->target))
	    result = item->replacement;
	  break;
	case CAT_SYSTEM:
	  if (sysid != 0 && cs_streql(sysid, item->target))
	    return item->replacement;
	  break;
	case CAT_DOCTYPE:
	  if (sysid != 0)
	    break;
	/*FALLTHROUGH*/
	case OVR_DOCTYPE:
	  if (name != 0 && kind == CAT_DOCTYPE && result == 0
	      && (ci ? ci_streql : cs_streql) (name, item->target))
	    result = item->replacement;
	  break;
	case CAT_ENTITY:
	  if (sysid != 0)
	    break;
	 /*FALLTHROUGH*/ case OVR_ENTITY:
	  if (name != 0 && kind >= CAT_ENTITY && result == 0
	      && (ci ? ci_streql : cs_streql) (name, item->target))
	    result = item->replacement;
	  break;
	default:
	  break;
      }
    }

    if (!catfile)
      break;
  }
  if ( result != 0 )
    return result;
  if ( sysid != 0 )
    return sysid;
  if ( kind == CAT_OTHER || kind == CAT_DOCTYPE )
    return 0;

  if ( istrlen(name)+4+1 > penlen )
  { gripe(NULL, ERC_REPRESENTATION, L"entity name");
    return NULL;
  }

  item = sgml_malloc(sizeof(*item));
  item->next = 0;
  item->kind = kind;
  item->target = istrdup(name);

  switch (kind)
  { case CAT_DOCTYPE:
      (void) swprintf(penname, penlen, L"%ls.dtd", name);
      break;
    case CAT_PENTITY:
      item->kind = CAT_ENTITY;
      (void) swprintf(penname, penlen, L"%ls.pen", name + 1);
      break;
    case CAT_ENTITY:
      (void) swprintf(penname, penlen, L"%ls.ent", name);
      break;
    default:
      abort();
  }

  item->replacement = istrdup(penname);
  if (first_item == 0)
  { first_item = item;
  } else
  { last_item->next = item;
  }
  last_item = item;

  return item->replacement;
}

