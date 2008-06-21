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

#include <limits.h>
#include <math.h>
#include "util.h"

bool isna( const char val ) { return NA_CHAR == val; }
bool isna( const short val ) { return NA_SHORT == val; }
bool isna( const int val ) { return NA_INTEGER == val; }
bool isna( const double val ) { return isnan(val); }

bool neginf( const char val ) {return false;}
bool neginf( const short val ) {return false;}
bool neginf( const int val ) {return false;}
bool neginf( const double val ) {return isinf(val) && val < 0;}

bool posinf( const char val ) {return false;};
bool posinf( const short val ) {return false;};
bool posinf( const int val ) {return false;};
bool posinf( const double val ) {return isinf(val) && val >= 0;}

