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

#ifndef BIGMEMORY_UTIL_HPP
#define BIGMEMORY_UTIL_HPP

#include "bm_defines.h"

bool isna( const char val );
bool isna( const short val );
bool isna( const int val );
bool isna( const double val );

bool neginf( const char val );
bool neginf( const short val );
bool neginf( const int val );
bool neginf( const double val );

bool posinf( const char val );
bool posinf( const short val );
bool posinf( const int val );
bool posinf( const double val );

template<typename T>
struct NewVec;

template<>
struct NewVec<int>
{SEXP operator()(long n) const {return NEW_INTEGER(n);};};

template<>
struct NewVec<double>
{SEXP operator()(long n) const {return NEW_NUMERIC(n);};};

template<typename T>
struct VecPtr;

template<>
struct VecPtr<int>
{int* operator()(SEXP vec) const {return INTEGER_DATA(vec);};};

template<>
struct VecPtr<double>
{double* operator()(SEXP vec) const {return NUMERIC_DATA(vec);};};

#endif // BIGMEMORY_UTIL_HPP
