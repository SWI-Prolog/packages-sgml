/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2013, University of Amsterdam
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

#ifndef SGMLDEFS_H_INCLUDED
#define SGMLDEFS_H_INCLUDED

#ifdef HAVE_CONFIG_H
#include <config.h>
#else
#ifdef __WINDOWS__
#define HAVE_MALLOC_H 1
#define HAVE_IO_H 1
#endif
#endif

#ifdef HAVE_DMALLOC_H
#include <dmalloc.h>
#endif

#define UTF8 1				/* Include UTF-8 decoding */
#define XMLNS 1				/* support XML namespaces */

#include <wchar.h>

		 /*******************************
		 *    INPUT/OUTPUT CHARACTERS	*
		 *******************************/

typedef wchar_t ichar;			/* input character */

#define SHORTMAP_SIZE	256		/* shortmaps in 0..255 */

#define USE_STRING_FUNCTIONS 1		/* use built-in str* functions */


		 /*******************************
		 *	       LIMITS		*
		 *******************************/

#define INPUT_CHARSET_SIZE	256	/* for now */
#define SYMBOLHASHSIZE		256
#define MAXSTRINGLEN	       4096
#define MAXNMLEN		256
#define MAXDECL		      10240
#define MAXATTELEM		256	/* #elements in one ATTLIST */
#define MAXNAMEGROUP		256	/* #names in a (group) */
#define MAXMAPLEN		 32	/* max sequence length for SHORTREF */
#define SHORTENTITYFILE		100	/* short external entities in mem */


		 /*******************************
		 *	    CONSTANTS		*
		 *******************************/

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#define LF 10
#define CR 13

#endif /*SGMLDEFS_H_INCLUDED*/
