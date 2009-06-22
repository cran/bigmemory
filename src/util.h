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

#ifndef BIGMEMORY_UTIL_HPP
#define BIGMEMORY_UTIL_HPP

#include <vector>
#include <string>
#include <Rdefines.h>

using namespace std;

vector<string> RChar2StringVec( SEXP charVec );

vector<string> RChar2StringVec( SEXP charVec, 
  const vector<unsigned long> &indices );

SEXP String2RChar(const std::string &str);

std::string RChar2String(SEXP str);

SEXP StringVec2RChar( const vector<string> &strVec );

template<typename T>
SEXP StringVec2RChar( const vector<string> &strVec,
  T indices, const unsigned long indicesLength )
{
  if (strVec.empty())
    return NULL_USER_OBJECT;
  SEXP ret = PROTECT(allocVector(STRSXP, indicesLength));
  unsigned long i;
  for (i=0; i < indicesLength; ++i)
  {
    SET_STRING_ELT(ret, i, 
      mkChar(strVec[static_cast<unsigned long>(indices[i])-1].c_str()));
  }
  UNPROTECT(1);
  return ret;
}


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
