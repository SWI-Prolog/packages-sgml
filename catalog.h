/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
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

#ifndef CATALOG_H_INCLUDED
#define CATALOG_H_INCLUDED
#include "util.h"

/*  When we look for a token, we skip layout characters and comments.
    There there is nothing left, we return EOF.
    If we are looking for the beginning of an entry, the possibilities
    are then
*/

#define CAT_OTHER    (0)    /* token + parameter of find... */
#define CAT_SYSTEM   (1)    /* token only */
#define CAT_PUBLIC   (2)    /* token only */
#define CAT_DOCTYPE  (3)    /* token + parameter of find... */
#define CAT_ENTITY   (4)    /* token + parameter of find... */
#define CAT_PENTITY  (5)    /*         parameter of find... only */
#define CAT_OVERRIDE (5)    /* token only */
#define CAT_BASE     (6)    /* token only */
#define OVR_PUBLIC   (CAT_OVERRIDE + CAT_PUBLIC)
#define OVR_DOCTYPE  (CAT_OVERRIDE + CAT_DOCTYPE)
#define OVR_ENTITY   (CAT_OVERRIDE + CAT_ENTITY)


typedef enum
{ CTL_START,
  CTL_END
} catalog_location;

int	register_catalog_file(const ichar *file, catalog_location where);
int	is_absolute_path(const ichar *iname);
int	is_url(const ichar *iname);
ichar   *localpath(const ichar *ref, const ichar *name);
ichar const *find_in_catalogue(
    int         kind,
    ichar const *name,
    ichar const *pubid,
    ichar const *sysid,
    int         ci
);

#endif /*CATALOG_H_INCLUDED*/
