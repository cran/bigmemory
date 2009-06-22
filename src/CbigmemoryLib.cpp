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


#include <iostream> // hack to make sure we are using the right "length" 
                    // function
#include "BigMatrix.h"
#include "BigMatrixAccessor.hpp"
#include "BigMemoryDefines.h"
#include "isna.hpp"

#include <math.h>
#include <R.h>
#include <Rdefines.h>

// ---------------------------------------------------------------------------
// NEW

// NEW_INTEGER, INTEGER_DATA, etc... are macros, not functions; we provide
// simple wrappers to facilitate our use of function templates.

//SEXP NEW_INTEGERF(int i) { return(NEW_INTEGER(i)); }
//SEXP NEW_NUMERICF(int i) { return(NEW_NUMERIC(i)); }
//int* INTEGER_DATAF(SEXP x) { return(INTEGER_DATA(x)); }
//double* NUMERIC_DATAF(SEXP x) { return(NUMERIC_DATA(x)); }

// These typedefs were needed in conjunction with the new functions, above,
// to facilitate passing these functions as arguments to functions.  This is
// just to aid readability.

//typedef SEXP (*sexpptrfun)(int);
//typedef int* (*intptrfun)(SEXP);
//typedef double* (*doubleptrfun)(SEXP);

// The following functions (tmin, tmax, etc...) were originally from
// summary.c, but template-ified.

// --------------------- min -------------------------------------------

template<typename T>
Rboolean tmin(T *x, long n, int *value, Rboolean narm, T NA_VALUE)
{
  long i;
  int s = 0 /* -Wall */;
  Rboolean updated = (Rboolean)FALSE;

  for (i = 0; i < n; i++) {
//    if (x[i] != NA_VALUE && !isnan((double)x[i])) {
    if (!isna(x[i])) {
      if (!updated || s > x[i]) {
        s = x[i];
        if (!updated) updated = (Rboolean)TRUE;
      }
    }
    else if (!narm) {
      *value = NA_VALUE;
      return((Rboolean)TRUE);
    }
  }
  *value = s;
  return(updated);
}

Rboolean tmin(double *x, long n, double *value,
                      Rboolean narm, double NA_VALUE)
{
  double s = 0.0 /* -Wall */;
  long i;
  Rboolean updated = (Rboolean)FALSE;

  /* s = R_PosInf; */
  for (i = 0; i < n; i++) {
    if (ISNAN(x[i])) {/* Na(N) */
      if (!narm) {
        if(s != NA_REAL) s = x[i]; /* so any NA trumps all NaNs */
        if(!updated) updated = (Rboolean)TRUE;
      }
    }
    else if (!updated || x[i] < s) {  /* Never true if s is NA/NaN */
      s = x[i];
      if(!updated) updated = (Rboolean)TRUE;
    }
  }
  *value = s;

  return(updated);
}

// --------------------- max -------------------------------------------

template<typename T>
Rboolean tmax(T *x, long n, int *value, Rboolean narm, T NA_VALUE)
{
  long i;
  int s = 0 /* -Wall */;
  Rboolean updated = (Rboolean)FALSE;

  for (i = 0; i < n; i++) {
//    if (x[i] != NA_VALUE) {
    if (!isna(x[i])) {
      if (!updated || s < x[i]) {
        s = x[i];
        if(!updated) updated = (Rboolean)TRUE;
      }
    } else if (!narm) {
      *value = NA_VALUE;
      return((Rboolean)TRUE);
    }
  }
  *value = s;

  return(updated);
}

Rboolean tmax(double *x, int n, double *value, Rboolean narm,
              double NA_VALUE)
{
  double s = 0.0 /* -Wall */;
  long i;
  Rboolean updated = (Rboolean)FALSE;

  for (i = 0; i < n; i++) {
    if (ISNAN(x[i])) {/* Na(N) */
      if (!narm) {
        if(s != NA_REAL) s = x[i]; /* so any NA trumps all NaNs */
        if(!updated) updated = (Rboolean)TRUE;
      }
    }
    else if (!updated || x[i] > s) {  /* Never true if s is NA/NaN */
      s = x[i];
      if(!updated) updated = (Rboolean)TRUE;
    }
  }
  *value = s;

  return(updated);
}

// --------------------- sum -------------------------------------------

template<typename T>
Rboolean tsum(T *x, long n, double *value, Rboolean narm, T NA_VALUE)
{
  LDOUBLE s = 0.0;
  long i;
  Rboolean updated = (Rboolean)FALSE;

  for (i = 0; i < n; i++) {
    if (!isna(x[i])) {
      if (!updated) updated = (Rboolean)TRUE;
      s += x[i];
    } else if (!narm) {
      if (!updated) updated = (Rboolean)TRUE;
      *value = NA_REAL;
      return(updated);
    }
  }
  // Note the change from the standard isum, always returning double now.
  *value = s;

  return(updated);
}

Rboolean tsum(double *x, int n, double *value, Rboolean narm,
              double NA_VALUE)
{
  LDOUBLE s = 0.0;
  long i;
  Rboolean updated = (Rboolean)FALSE;

  for (i = 0; i < n; i++) {
    if (!ISNAN(x[i]) || !narm) {
      if(!updated) updated = (Rboolean)TRUE;
      s += x[i];
    }
  }
  *value = s;

  return(updated);
}

// --------------------- prod -------------------------------------------

template<typename T>
Rboolean tprod(T *x, int n, double *value, Rboolean narm, T NA_VALUE)
{
  LDOUBLE s = 1.0;
  long i;
  Rboolean updated = (Rboolean)FALSE;

  for (i = 0; i < n; i++) {
    if (!isna(x[i])) {
      s *= x[i];
      if(!updated) updated = (Rboolean)TRUE;
    }
    else if (!narm) {
      if(!updated) updated = (Rboolean)TRUE;
      *value = NA_REAL;
      return(updated);
    }

    if(ISNAN(s)) {  /* how can this happen? */
      *value = NA_REAL;
      return(updated);
    }
  }
  *value = s;

  return(updated);
}

Rboolean tprod(double *x, long n, double *value, Rboolean narm,
               double NA_VALUE)
{
  LDOUBLE s = 1.0;
  long i;
  Rboolean updated = (Rboolean)FALSE;

  for (i = 0; i < n; i++) {
    if (!ISNAN(x[i]) || !narm) {
      if(!updated) updated = (Rboolean)TRUE;
      s *= x[i];
    }
  }
  *value = s;

  return(updated);
}

// --------------------- mean -------------------------------------------

template<typename T>
Rboolean tmean(T *x, long n, double *value, Rboolean narm, T NA_VALUE)
{
  LDOUBLE s = 0.0;
  long i;
  Rboolean updated = (Rboolean)TRUE;

  for (i = 0; i < n; i++) {
    if (!isna(x[i])) 
			s += x[i];
    else if (!narm) {
      *value = NA_REAL;
      return(updated);
    }
  }
  s /= (LDOUBLE)n;
  *value = (double) s;

  return(updated);
}

Rboolean tmean(double *x, long n, double *value, Rboolean narm,
               double NA_VALUE)
{
  LDOUBLE s = 0.0, t = 0.0;
  long i;
  Rboolean updated = (Rboolean)TRUE;

  for (i = 0; i < n; i++) {
    if (!ISNAN(x[i])) s += x[i];
    else if (!narm) {
      *value = NA_REAL;
      return((Rboolean)TRUE);
    }
  }
  s /= (LDOUBLE)n;
  if (R_FINITE((double)s)) {
    for (i = 0; i < n; i++) t += (x[i] - s);
    s += t / ((LDOUBLE)n);   
  } 

  *value = (double) s;

  return(updated);
}

// --------------------- var -------------------------------------------
// If this works for all 4 types, could we simplify in other cases, too???

template<typename T>
Rboolean tvar(T *x, long n, double *value, Rboolean narm, T NA_VALUE)
{
  tmean(x, n, value, narm, NA_VALUE);
  double avg = *value;

  long i;
  Rboolean updated = (Rboolean)TRUE;
  
  double sum=0.0;
  double addNum;
  for (i=0; i < n; ++i) {
    addNum = (double)x[i];
    if (isna(addNum)) {
      if (narm) continue;
      else {
        *value = NA_REAL;
        return updated;
      }
    }
    sum += (addNum - avg) * (addNum - avg);
  }
  *value = sum/((double)n-1.0);
  return(updated);

}

// This CALL_FUN macro really just does an apply on the specified columns.

#define CALL_FUN(fun)                                                        \
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatrixAddr);            \
  if (pMat->separated_columns())                                             \
  {                                                                          \
    SepBigMatrixAccessor<dataT> Mat(*pMat);                                  \
    long i=0;                                                                \
    for (i=0; i < nCols; ++i) {                                              \
      fun(Mat[(long)pCols[i]-1], pMat->nrow(), &pRet[i], \
                  (Rboolean)LOGICAL_VALUE(narm), NA_VALUE);                  \
    }                                                                        \
  }                                                                          \
  else                                                                       \
  {                                                                          \
    BigMatrixAccessor<dataT> Mat(*pMat);                                     \
    long i=0;                                                                \
    for (i=0; i < nCols; ++i) {                                              \
      fun(Mat[(long)pCols[i]-1], pMat->nrow(), &pRet[i], \
                    (Rboolean)LOGICAL_VALUE(narm), NA_VALUE);                \
    }                                                                        \
  }

template<typename dataT, typename retT>
void CMinCol(SEXP bigMatrixAddr, retT *pRet, double *pCols, long nCols,
             SEXP narm, dataT NA_VALUE)
{
  CALL_FUN(tmin);
}

template<typename dataT, typename retT>
void CMaxCol(SEXP bigMatrixAddr, retT *pRet, double *pCols, long nCols,
             SEXP narm, dataT NA_VALUE)
{
  CALL_FUN(tmax);
}

template<typename dataT, typename retT>
void CSumCol(SEXP bigMatrixAddr, retT *pRet, double *pCols, long nCols,
             SEXP narm, dataT NA_VALUE)
{
  CALL_FUN(tsum);
}

template<typename dataT, typename retT>
void CProdCol(SEXP bigMatrixAddr, retT *pRet, double *pCols, long nCols,
             SEXP narm, dataT NA_VALUE)
{
  CALL_FUN(tprod);
}

template<typename dataT, typename retT>
void CMeanCol(SEXP bigMatrixAddr, retT *pRet, double *pCols, long nCols,
             SEXP narm, dataT NA_VALUE)
{
  CALL_FUN(tmean);
}

template<typename dataT, typename retT>
void CVarCol(SEXP bigMatrixAddr, retT *pRet, double *pCols, long nCols,
             SEXP narm, dataT NA_VALUE)
{
  CALL_FUN(tvar);
}


// ----------------------------------------- externs below --------------

extern "C"
{

// The following line was taken from summary.c
#define R_INT_MIN (1+INT_MIN)

// IS THE FOLLOWING USED?
#define _(x) x

#define mainsetup()                                              \
  SEXP ret = R_NilValue;                                         \
  double *pCols = NUMERIC_DATA(col);                             \
  long nCols = GET_LENGTH(col);                                  \
  int mt = INTEGER_VALUE(matType);

#define casesetup(TYPE, NEW_TYPE, TYPE_DATA)                     \
    ret = PROTECT(NEW_TYPE(nCols));                              \
    TYPE *pRet = TYPE_DATA(ret);

SEXP CMinColmain(SEXP matType, SEXP bigMatrixAddr, SEXP col, SEXP narm)
{
  mainsetup();
  switch (mt) {
    case 1: {
        casesetup(int, NEW_INTEGER, INTEGER_DATA);
        CMinCol<char, int>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_CHAR);
      } break;
    case 2: {
        casesetup(int, NEW_INTEGER, INTEGER_DATA);
        CMinCol<short, int>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_SHORT);
      } break;
    case 4: {
        casesetup(int, NEW_INTEGER, INTEGER_DATA);
        CMinCol<int, int>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_INTEGER);
      } break;
    case 8: {
        casesetup(double, NEW_NUMERIC, NUMERIC_DATA);
        CMinCol<double, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_REAL);
      } break;
  }
  UNPROTECT(1);
  return(ret);
}

SEXP CMaxColmain(SEXP matType, SEXP bigMatrixAddr, SEXP col, SEXP narm)
{
  mainsetup();
  switch (mt) {
    case 1: {
        casesetup(int, NEW_INTEGER, INTEGER_DATA);
        CMaxCol<char, int>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_CHAR);
      } break;
    case 2: {
        casesetup(int, NEW_INTEGER, INTEGER_DATA);
        CMaxCol<short, int>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_SHORT);
      } break;
    case 4: {
        casesetup(int, NEW_INTEGER, INTEGER_DATA);
        CMaxCol<int, int>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_INTEGER);
      } break;
    case 8: {
        casesetup(double, NEW_NUMERIC, NUMERIC_DATA);
        CMaxCol<double, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_REAL);
      } break;
  }
  UNPROTECT(1);
  return(ret);
}

SEXP CSumColmain(SEXP matType, SEXP bigMatrixAddr, SEXP col, SEXP narm)
{
  mainsetup();
  casesetup(double, NEW_NUMERIC, NUMERIC_DATA);
  switch (mt) {
    case 1: {
        CSumCol<char, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_CHAR);
      } break;
    case 2: {
        CSumCol<short, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_SHORT);
      } break;
    case 4: {
        CSumCol<int, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_INTEGER);
      } break;
    case 8: {
        CSumCol<double, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_REAL);
      } break;
  }
  UNPROTECT(1);
  return(ret);
}

SEXP CProdColmain(SEXP matType, SEXP bigMatrixAddr, SEXP col, SEXP narm)
{
  mainsetup();
  casesetup(double, NEW_NUMERIC, NUMERIC_DATA);
  switch (mt) {
    case 1: {
        CProdCol<char, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_CHAR);
      } break;
    case 2: {
        CProdCol<short, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_SHORT);
      } break;
    case 4: {
        CProdCol<int, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_INTEGER);
      } break;
    case 8: {
        CProdCol<double, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_REAL);
      } break;
  }
  UNPROTECT(1);
  return(ret);
}

SEXP CMeanColmain(SEXP matType, SEXP bigMatrixAddr, SEXP col, SEXP narm)
{
  mainsetup();
  casesetup(double, NEW_NUMERIC, NUMERIC_DATA);
  switch (mt) {
    case 1: {
        CMeanCol<char, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_CHAR);
      } break;
    case 2: {
        CMeanCol<short, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_SHORT);
      } break;
    case 4: {
        CMeanCol<int, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_INTEGER);
      } break;
    case 8: {
        CMeanCol<double, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_REAL);
      } break;
  }
  UNPROTECT(1);
  return(ret);
}

SEXP CVarColmain(SEXP matType, SEXP bigMatrixAddr, SEXP col, SEXP narm)
{
  mainsetup();
  casesetup(double, NEW_NUMERIC, NUMERIC_DATA);
  switch (mt) {
    case 1: {
        CVarCol<char, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_CHAR);
      } break;
    case 2: {
        CVarCol<short, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_SHORT);
      } break;
    case 4: {
        CVarCol<int, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_INTEGER);
      } break;
    case 8: {
        CVarCol<double, double>(bigMatrixAddr, pRet, pCols, nCols, narm, NA_REAL);
      } break;
  }
  UNPROTECT(1);
  return(ret);
}

} // extern "C"

