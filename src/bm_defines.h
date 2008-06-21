/*
 *  bigmemory: an R package for managing massive matrices using C,
 *  with support for shared memory.
 *
 *  Copyright (C) 2008 John W. Emerson and Michael J. Kane
 *
 *  This file is part of bigmemory.
 *
 *  bigmemory is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published
 *  by the Free Software Foundation; either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#ifndef BIGMEMORY_DEFINES_H
#define BIGMEMORY_DEFINES_H

#include <Rdefines.h>

#define NA_CHAR CHAR_MIN
#define NA_SHORT SHRT_MIN
#define R_INT_MIN (1+INT_MIN)
#define R_INT_MAX INT_MAX
#define R_SHORT_MIN (1+SHRT_MIN)
#define R_SHORT_MAX SHRT_MAX
#define R_CHAR_MIN (1+CHAR_MIN)
#define R_CHAR_MAX CHAR_MAX
#define R_DOUBLE_MIN R_NegInf
#define R_DOUBLE_MAX R_PosInf

#ifdef HAVE_LONG_DOUBLE
# define LDOUBLE long double
#else
# define LDOUBLE double
#endif

#endif //BIGMEMORY_DEFINES_H
