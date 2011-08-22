/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef PROLOG_H_INCLUDED
#define PROLOG_H_INCLUDED

#define DTD2PL_VERSION "0.1, March 2000"

#define PL_PRINT_PENTITIES	0x001
#define PL_PRINT_ENTITIES	0x002
#define PL_PRINT_ELEMENTS	0x004
#define PL_PRINT_ATTRIBUTES	0x008

#define PL_PRINT_ALL (PL_PRINT_PENTITIES| \
		      PL_PRINT_ENTITIES| \
		      PL_PRINT_ELEMENTS| \
		      PL_PRINT_ATTRIBUTES)

int            prolog_print_dtd(dtd *dtd, unsigned int flags);

#endif /*PROLOG_H_INCLUDED*/
