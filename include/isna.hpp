/*
 *  bigmemory: an R package for managing massive matrices using C,
 *  with support for shared memory.
 *
 *  Copyright (C) 2009 John W. Emerson and Michael J. Kane
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

#ifndef ISNA_HPP
#define ISNA_HPP

#include "bigmemoryDefines.h"

inline bool isna( const char val ) { return NA_CHAR == val; };
inline bool isna( const short val ) { return NA_SHORT == val; };
inline bool isna( const int val ) { return NA_INTEGER == val; };
inline bool isna( const double val ) { return isnan(val); };

inline bool neginf( const char val ) {return false;};
inline bool neginf( const short val ) {return false;};
inline bool neginf( const int val ) {return false;};
#ifdef _MSC_VER
inline bool isinf( const double val ) {return !_finite(val);}
#endif
inline bool neginf( const double val ) {return isinf(val) && val < 0;};

inline bool posinf( const char val );
inline bool posinf( const short val );
inline bool posinf( const int val );
inline bool posinf( const double val );

#endif //ISNA_HPP
