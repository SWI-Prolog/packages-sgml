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
#include <string.h>
#include <wchar.h>
#include "dtd.h"
#include "util.h"
#include "prolog.h"

#define streq(s,q) strcmp((s), (q)) == 0

char *program;

static void
usage()
{ fprintf(stderr, "Usage: %s [-xml|sgml] file.dtd\n", program);
}

int
main(int argc, char **argv)
{ dtd_dialect dialect = DL_SGML;

  init_ring();

  program = argv[0];
  argv++;
  argc--;

  while(argc > 0 && argv[0][0] == '-')
  { if ( streq(argv[0], "-xml") )
    { dialect = DL_XML;
      argc--;
      argv++;
    } else if ( streq(argv[0], "-sgml") )
    { dialect = DL_SGML;
      argc--;
      argv++;
    } else
    { usage();
      exit(1);
    }
  }

  if ( argc == 1 )
  { int wl = mbstowcs(NULL, argv[0], 0);

    if ( wl > 0 )
    { wchar_t *ws = malloc((wl+1)*sizeof(wchar_t));
      dtd *dtd;

      mbstowcs(ws, argv[0], wl+1);
      dtd = file_to_dtd(ws, L"test", dialect);

      if ( dtd )
      { prolog_print_dtd(dtd, PL_PRINT_ALL & ~PL_PRINT_PENTITIES);
	return 0;
      }
    } else
    { perror("mbstowcs");
      exit(1);
    }
  }

  usage();
  return 1;
}





