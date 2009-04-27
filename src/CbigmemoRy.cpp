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

#include <string>
#include <fstream>
#include <sstream>
#include <iostream>
#include <algorithm>

#include "BigMatrix.h"
#include "BigMatrixAccessor.hpp"
#include "util.h"
#include "isna.hpp"

#include <stdio.h>
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <stdlib.h>
#include <sys/types.h>


template<typename T>
string ttos(T i)
{
  stringstream s;
  s << i;
  return s.str();
}

template<typename CType, typename RType, typename BMAccessorType>
void SetMatrixElements( BigMatrix *pMat, SEXP col, SEXP row, SEXP values,
  double NA_C, double C_MIN, double C_MAX, double NA_R)
{
  BMAccessorType mat( *pMat );
  double *pCol = NUMERIC_DATA(col);
  long nCol = GET_LENGTH(col);
  double *pRow = NUMERIC_DATA(row);
  long nRow = GET_LENGTH(row);
  bool outOfRange=false;
  VecPtr<RType> vec_ptr;
  RType *pVals = vec_ptr(values);
  long i=0;
  long j=0;
  long k=0;
  long lcol=0;
  long lrow=0;
  for (i=0; i < nCol; ++i)
  {
    for (j=0; j < nRow; ++j)
    {
      lcol = static_cast<long>(pCol[i])-1;
      lrow = static_cast<long>(pRow[j])-1;
      if (pVals[k] < C_MIN || pVals[k] > C_MAX)
      {
        if (!isna(pVals[k]))
        {
          outOfRange=true;
        }
        mat[lcol][lrow] = static_cast<CType>(NA_C);
      }
      else
      {
        mat[lcol][lrow] = static_cast<CType>(pVals[k]);
      }
      ++k;
    }
    if (outOfRange)
    {
      warning("Some of the values were out of range, they will be set to NA.");
    }
  }
}

template<typename CType, typename BMAccessorType>
void SetAllMatrixElements( BigMatrix *pMat, SEXP value,
  double NA_C, double C_MIN, double C_MAX, double NA_R)
{
  BMAccessorType mat( *pMat );
  double val = NUMERIC_VALUE(value);
  unsigned long i=0;
  unsigned long j=0;
  unsigned long ncol = pMat->ncol();
  unsigned long nrow = pMat->nrow();

  bool isValNA=false; 
  bool outOfRange=false;
  if (val < C_MIN || val > C_MAX)
  { 
    isValNA=true;
    if (!isna(val))
    {
      outOfRange=true;
    }
  }
  for (i=0; i < ncol; ++i)
  {
    for (j=0; j < nrow; ++j)
    {
      if (isValNA)
      {
        mat[i][j] = static_cast<CType>(NA_C);
      }
      else
      {
        mat[i][j] = static_cast<CType>(val);
      }
    }
    if (outOfRange)
    {
      warning("Some of the values were out of range, they will be set to NA.");
    }
  }
}

bool TooManyRIndices( long val )
{
  return val > (powl(2, 31)-1);
}

template <typename T, typename VecType>
long EqAtIndex( const T &val, const VecType &vec )
{
  unsigned long i;
  for (i=0; i < vec.size(); ++i)
  {
    if (val == vec[i])
    {
      return i;
    }
  }
  return -1;
}

void MakeIndicesNumeric(SEXP indices, double *&pIndices, long &numIndices, 
  BigMatrix *pMat, bool &newIndices, bool &zeroIndices, bool isCol)
{
  long protectCount=0;
  if (indices == NULL_USER_OBJECT)
  {
    if ( TooManyRIndices( (isCol ? pMat->ncol() : pMat->nrow()) ) )
    {
      printf("Too many indices\n");
      pIndices=NULL;
      return;
    }
    newIndices=true;
    pIndices = new double[ (isCol ? pMat->ncol() : pMat->nrow()) ];
    long i;
    for (i=0; i < (isCol ? pMat->ncol() : pMat->nrow()); ++i)
    {
      pIndices[i] = i+1;
    }
    numIndices = (isCol ? pMat->ncol() : pMat->nrow());
  }
  else if (IS_NUMERIC(indices) || IS_INTEGER(indices))
  {
    if (IS_INTEGER(indices))
    {
      indices = PROTECT(AS_NUMERIC(indices));
      ++protectCount;
    }
    pIndices = NUMERIC_DATA(indices);
    long negIndexCount=0;
    long posIndexCount=0;
    long zeroIndexCount=0;
    long i,j;
    for (i=0; i < numIndices; ++i)
    {
      if (static_cast<long>(pIndices[i]) == 0)
      {
        ++zeroIndexCount;
      }
      if (static_cast<long>(pIndices[i]) < 0)
      {
        ++negIndexCount;
      }
      if (static_cast<long>(pIndices[i]) > 0)
      {
        ++posIndexCount;
      }
      if ( labs(static_cast<long>(pIndices[i])) > 
        (isCol ? pMat->ncol() : pMat->nrow()) )
      {
        // TODO: See if there is a C stop function in the R libraries.
        error("Index out of bounds\n");
        pIndices = NULL;
        return;
      }
    }
    if ( (zeroIndexCount == numIndices) && (numIndices > 0) )
    {
      printf("Setting zero indices true %ld\n", numIndices);
      zeroIndices=true;
      return;
    }
    if (posIndexCount > 0 && negIndexCount > 0)
    {
      // TODO: See if there is a C stop function in the R libraries.
      error("You can't have positive and negative indices\n");
      pIndices = NULL;
      return;
    }
    if (zeroIndexCount > 0)
    {
      newIndices = true;
      double *newPIndices = new double[posIndexCount];
      j=0;
      for (i=0; i < numIndices; ++i)
      {
        if (static_cast<long>(pIndices[i]) != 0)
        {
          newPIndices[j++] = pIndices[i]; 
        }
      }
      newPIndices = pIndices;
      numIndices = posIndexCount;
    }
    if (negIndexCount > 0)
    {
      // It might be better to use a data-structure other than a vector
      // (sequential ordering).
      typedef std::vector<long> Indices;
      Indices ind(0, (isCol ? pMat->ncol() : pMat->nrow()));
      for (i=1; i <= static_cast<long>(ind.size()); ++i)
      {
        ind[i] = i;
      }
      Indices::iterator it;
      for (i=0; i < numIndices; ++i)
      {
        it = std::lower_bound(ind.begin(), ind.end(), 
					static_cast<long>(-1*pIndices[i]));
        if ( it != ind.end() && 
					*it == -1*static_cast<long>(pIndices[i]) )
        {
          ind.erase(it);
        }
      }
      if (newIndices)
      {
        delete [] pIndices;
      }
      if (TooManyRIndices(ind.size()))
      {
        // TODO: A better error message?
        printf("Too many indices\n");
        pIndices=NULL;
        return;
      }
      newIndices=true;
      numIndices = ind.size();
      pIndices = new double[numIndices];
      for (i=0; i < numIndices; ++i)
      {
        pIndices[i] = static_cast<double>(ind[i]+1);
      }
    }
    UNPROTECT(protectCount);
  }
  else if (IS_LOGICAL(indices))
  {
    unsigned long i, trueCount=0;
    for (i=0; i < static_cast<unsigned long>(GET_LENGTH(indices)); ++i)
    {
      if ( LOGICAL_DATA(indices)[i] == TRUE )
      {
        ++trueCount;
      }
    }
    newIndices=true;
    pIndices = new double[trueCount];
    unsigned long j=0;
    for (i=0; i < static_cast<unsigned long>(GET_LENGTH(indices)); ++i)
    {
      if ( LOGICAL_DATA(indices)[i] == TRUE )
      {
        pIndices[j++] = i+1;
      }
    }
  }
  else if (IS_CHARACTER(indices))
  {
    newIndices=true;
    pIndices = new double[GET_LENGTH(indices)];
    Names names = (isCol ? pMat->column_names() : pMat->row_names());
    long i, index;
    for (i=0; i < static_cast<long>(GET_LENGTH(indices)); ++i)
    {
      index = EqAtIndex( CHAR(STRING_ELT(indices,i)), names );
      if (index >= 0)
        pIndices[i] = index+1;
//      pIndices[i] = ((index == -1) ? NA_REAL : index);
    }
  }
  else
  {
    pIndices=NULL;
  }
}

template<typename CType, typename RType, typename BMAccessorType>
SEXP GetMatrixElements( BigMatrix *pMat, double NA_C, double NA_R, 
  SEXP col, SEXP row, SEXPTYPE sxpType)
{
  NewVec<RType> new_vec;
  VecPtr<RType> vec_ptr; 
  BMAccessorType mat(*pMat);
  bool newRows=false;
  bool zeroRows=false;
  bool newCols=false;
  bool zeroCols=false;
  double *pCols=NULL;
  double *pRows=NULL;
  long numCols = GET_LENGTH(col);
  long numRows = GET_LENGTH(row);
  MakeIndicesNumeric(col, pCols, numCols, pMat, newCols, zeroCols, true);
  if (zeroCols)
  {
		warning("zero indices found\n");
    return new_vec(0);
  }
  if (pCols == NULL)
  {
    return R_NilValue; 
  }
  MakeIndicesNumeric(row, pRows, numRows, pMat, newRows, zeroRows, false);
  if (zeroRows)
  {
    return new_vec(0);
  }
  if (pRows == NULL)
  {
    return R_NilValue; 
  }
  // Can we return big.matrix objects instead?
  if (TooManyRIndices(numCols*numRows))
  {
    return R_NilValue;
  }
  SEXP ret = PROTECT(NEW_LIST(3));
  int protectCount = 1;
  SET_VECTOR_ELT( ret, 1, NULL_USER_OBJECT );
  SET_VECTOR_ELT( ret, 2, NULL_USER_OBJECT );
  SEXP retMat = PROTECT( Rf_allocMatrix(sxpType, numRows, numCols) );
  ++protectCount;
  SET_VECTOR_ELT(ret, 0, retMat);
  //SEXP ret = PROTECT( new_vec(numCols*numRows) );
  RType *pRet = vec_ptr(retMat);
  CType *pColumn = NULL;
  long k=0;
  long i,j;
  bool outOfBounds=false;
  for (i=0; i < numCols; ++i) 
  {
    if (isna(pCols[i]))
    {
      continue;
    }
    else if (pCols[i] > pMat->ncol() || pCols[i] < 1)
    {
      outOfBounds=true;
      break;
    }
    pColumn = mat[static_cast<long>(pCols[i])-1];
    for (j=0; j < numRows; ++j) 
    {
      if (isna(pRows[j]))
      {
        pRet[k] = static_cast<RType>(NA_R);
      }
      else if (pRows[j] > pMat->nrow() || pRows[j] < 1)
      {
        outOfBounds=true;
      }
      else
      {
        pRet[k] = 
          pColumn[static_cast<long>(pRows[j])-1] == static_cast<CType>(NA_C) ? 
            static_cast<RType>(NA_R) : 
            static_cast<RType>(pColumn[static_cast<long>(pRows[j])-1]);
      }
      ++k;
    }
  }
  if (outOfBounds)
  {
    // Note: this exact same thing is done below... maybe create a
    // cleanup function?
    if (newCols)
    {
      delete [] pCols;
    }
    if (newRows)
    {
      delete [] pRows;
    }
    UNPROTECT(protectCount);
    error("Indexes out of range.");
    return R_NilValue;
  }
  Names colNames = pMat->column_names();
  if (!colNames.empty())
  {
    ++protectCount;
    SEXP rCNames = PROTECT(allocVector(STRSXP, numCols));
    for (i=0; i < numCols; ++i)
    {
      if (!isna(pCols[i]))
        SET_STRING_ELT( rCNames, i, 
          mkChar(colNames[static_cast<long>(pCols[i])-1].c_str()) );
    }
    SET_VECTOR_ELT(ret, 2, rCNames);
  }
  Names rowNames = pMat->row_names();
  if (!rowNames.empty())
  {
    ++protectCount;
    SEXP rRNames = PROTECT(allocVector(STRSXP, numRows));
    for (i=0; i < numRows; ++i)
    {
      if (!isna(pRows[i]))
			{
        SET_STRING_ELT( rRNames, i, 
          mkChar(rowNames[static_cast<long>(pRows[i])-1].c_str()) );	
			}
    }
    SET_VECTOR_ELT(ret, 1, rRNames);
  }
  if (newCols)
  {
    delete [] pCols;
  }
  if (newRows)
  {
    delete [] pRows;
  }
  UNPROTECT(protectCount);
  return ret;
}

/*
template<typename Type, typename BMAccessorType>
SEXP GetMatrixElementsSpecialized(BigMatrix *pMat, Type NA, SEXP col, SEXP row)
{
  NewVec<Type> new_vec;
  VecPtr<Type> vec_ptr; 
  // See if the Accessor is slowing down Get
  BMAccessorType mat(*pMat);
  double *pCols = NUMERIC_DATA(col);
  double *pRows = NUMERIC_DATA(row);
  long numCols = GET_LENGTH(col);
  long numRows = GET_LENGTH(row);
  SEXP ret = PROTECT( new_vec(numCols*numRows) );
  Type *pRet = vec_ptr(ret);
  long i,j;
  long k=0;
  Type *pCol = NULL;
  for (i=0; i < numCols; ++i) 
  {
    if (isna(pCols[i]))
    {
      continue;
    }
    pCol = mat[static_cast<long>(pCols[i]-1)];
    for (j=0; j < numRows; ++j) 
    {
      pRet[k] = isna(pRows[j]) ? NA : pCol[static_cast<long>(pRows[j])-1];
      ++k;
    }
  }
  UNPROTECT(1);
  return(ret);
}
*/

template<typename T, typename BMAccessorType>
SEXP ReadMatrix(SEXP fileName, BigMatrix *pMat,
              SEXP firstLine, SEXP numLines, SEXP numCols, SEXP separator,
              SEXP hasRowNames, SEXP useRowNames, double C_NA, double posInf, 
							double negInf, double notANumber)
{
  BMAccessorType mat(*pMat);
  SEXP ret = PROTECT(NEW_LOGICAL(1));
  LOGICAL_DATA(ret)[0] = (Rboolean)0;
  int fl = INTEGER_VALUE(firstLine);
  int nl = INTEGER_VALUE(numLines);
  string sep(CHAR(STRING_ELT(separator,0)));
  long i=0,j;
  bool rowSizeReserved = false;
  //double val;

  ifstream file;
  string lc, element;
  file.open(STRING_VALUE(fileName));
  if (!file.is_open())
  {
    UNPROTECT(1);
    return ret;
  }
  for (i=0; i < fl; ++i)
  {
    std::getline(file, lc);
  }

  for (i=0; i < nl; ++i)
  {
    // getline may be slow
    std::getline(file, lc);
  
    string::size_type first=0, last=0;
    j=0;
    while (first < lc.size() && last < lc.size())
    {
      last = lc.find_first_of(sep, first);
      if (last > lc.size())
        last = lc.size();
      element = lc.substr(first, last-first);
      // Skip the row name.
			if (LOGICAL_VALUE(hasRowNames) && LOGICAL_VALUE(useRowNames) && 0==j)
			{
        Names rn = pMat->row_names();
        if (!rowSizeReserved)
        {
          rowSizeReserved = true;
          rn.reserve(nl);
        }
        rn.push_back(element.substr(1, element.size()-2));
				pMat->row_names( rn );
			}
			else
      {
				long offset= LOGICAL_VALUE(hasRowNames) && LOGICAL_VALUE(useRowNames) ?
					1 : 0;
        if (element == "NA")
        {
          mat[j-offset][i] = static_cast<T>(C_NA);
        }
        else if (element == "inf")
        {
          mat[j-offset][i] = static_cast<T>(posInf);
        }
        else if (element == "-inf")
        {
          mat[j-offset][i] = static_cast<T>(negInf);
        }
        else if (element == "NaN")
        {
          mat[j-offset][i] = static_cast<T>(notANumber);
        }
        else
        {
          mat[j-offset][i] = static_cast<T>(atof(element.c_str()));
        }
      }
      
      first = last+1;
      ++j;
    }
  }
  file.close();
  LOGICAL_DATA(ret)[0] = (Rboolean)1;
  UNPROTECT(1);
  return ret;
}

template<typename T, typename BMAccessorType>
void WriteMatrix( BigMatrix *pMat, SEXP fileName, SEXP rowNames,
  SEXP colNames, SEXP sep, double C_NA )
{
  BMAccessorType mat(*pMat);
  FILE *FP = fopen(STRING_VALUE(fileName), "w");
  long i,j;
  string  s;
  string sepString = string(CHAR(STRING_ELT(sep, 0)));

  Names cn = pMat->column_names();
  Names rn = pMat->row_names();
  if (LOGICAL_VALUE(colNames) == Rboolean(TRUE) && !cn.empty())
  {
//    if ( LOGICAL_VALUE(rowNames) == Rboolean(TRUE) && !rn.empty())
//    {
//      s += string("\"")+"\""+sepString;
//    }
    for (i=0; i < (int) cn.size(); ++i)
      s += "\"" + cn[i] + "\"" + (((int)cn.size()-1 == i) ? "\n" : sepString);
  }
  fprintf(FP, s.c_str());
  s.clear();
  for (i=0; i < pMat->nrow(); ++i) 
  {
    if ( LOGICAL_VALUE(rowNames) == Rboolean(TRUE) && !rn.empty())
    {
      s += "\"" + rn[i] + "\"" + sepString;
    }
    for (j=0; j < pMat->ncol(); ++j) 
    {
      if ( isna(mat[j][i]) )
      {
        s += "NA";
      }
      else
      {
        s += ttos(mat[j][i]);
      }
      if (j < pMat->ncol()-1)
      { 
        s += sepString;
      }
      else 
      {
        s += "\n";
      }
    }
    fprintf(FP, s.c_str());
    s.clear();
  }
  fclose(FP);
}

template<typename T, typename BMAccessorType>
SEXP MatrixHashRanges( BigMatrix *pMat, SEXP selectColumn )
{
  BMAccessorType mat(*pMat);
  long sc = (long)NUMERIC_VALUE(selectColumn)-1;
  if (pMat->nrow()==0) return(R_NilValue);
  int uniqueValCount=1;
  T lastVal = mat[sc][0];
  long i;
  T val;
  for (i=1; i < pMat->nrow(); ++i) {
    val = mat[sc][i];
    if (val != lastVal) {
      lastVal = val;
      uniqueValCount += 1;
    }
  }
  SEXP ret = PROTECT(NEW_INTEGER(uniqueValCount*2));
  int *pRet = INTEGER_DATA(ret);
  int j=0;
  lastVal = mat[sc][0];
  pRet[j++]=1;
  for (i=1; i < pMat->nrow(); ++i) {
    val = mat[sc][i];
    if (val != lastVal) {
      pRet[j++] = i;
      pRet[j++] = i+1;
      lastVal = val;
    }
  }
  pRet[uniqueValCount*2-1] = pMat->nrow();
  UNPROTECT(1);
  return(ret);
}

template<typename T, typename BMAccessorType>
SEXP ColCountNA( BigMatrix *pMat, SEXP column )
{
  BMAccessorType mat(*pMat);
  long col = (long)NUMERIC_VALUE(column);
  long i, counter;
  counter=0;
  for (i=0; i < pMat->nrow(); ++i)
  {
    if (mat[col-1][i] == NA_INTEGER || 
      isna((double)mat[col-1][i]))
    {
      ++counter;
    }
  }
  SEXP ret = PROTECT(NEW_NUMERIC(1));
  NUMERIC_DATA(ret)[0] = (double)counter;
  UNPROTECT(1);
  return(ret);
}

template<typename T1, typename BMAccessorType>
int Ckmeans2(BigMatrix *pMat, SEXP centAddr, SEXP ssAddr,
              SEXP clustAddr, SEXP clustsizesAddr, 
              SEXP nn, SEXP kk, SEXP mm, SEXP mmaxiters)
{
  // BIG x m
//  T1 **x = (T1**) pMat->matrix();
  BMAccessorType x(*pMat);

  // k x m
  BigMatrix *pcent = (BigMatrix*)R_ExternalPtrAddr(centAddr);
  double **cent = (double**) pcent->matrix();

  // k x 1
  BigMatrix *pss = (BigMatrix*)R_ExternalPtrAddr(ssAddr);
  double **ss = (double**) pss->matrix();

  // n x 1
  BigMatrix *pclust = (BigMatrix*)R_ExternalPtrAddr(clustAddr);
  int **clust = (int**) pclust->matrix();

  // k x 1
  BigMatrix *pclustsizes = (BigMatrix*)R_ExternalPtrAddr(clustsizesAddr);
  double **clustsizes = (double**) pclustsizes->matrix();

  long n = (long) NUMERIC_VALUE(nn);        // Very unlikely to need long, but...
  int k = INTEGER_VALUE(kk);                // Number of clusters
  long m = (long) NUMERIC_VALUE(mm);        // columns of data
  int maxiters = INTEGER_VALUE(mmaxiters); // maximum number of iterations

  int oldcluster, newcluster;           // just for ease of coding.
  int cl, bestcl;
  long col, j;
  double temp;
  int done = 0;
  long nchange;
  int iter = 0;

  vector<double> d(k);                        // Vector of distances, internal only.
  vector<double> temp1(k);
  vector<vector<double> > tempcent(m, temp1);   // For copy of global centroids k x m

  //char filename[10];
  //int junk;
  //junk = sprintf(filename, "Cfile%d.txt", INTEGER_VALUE(ii));
  //ofstream outFile;
  //outFile.open(filename, ios::out);
  //outFile << "This is node " << INTEGER_VALUE(ii) << endl;
  //outFile << "Before do: n, i, k, m, p, start:" <<
  //       n << ", " << i << ", " << k << ", " << m << ", " << p <<
  //       ", " << start << endl;


  // Before starting the loop, we only have cent (centers) as passed into the function.
  // Calculate clust and clustsizes, then update cent as centroids.
  
  for (cl=0; cl<k; cl++) clustsizes[0][cl] = 0;
  for (j=0; j<n; j++) {
    bestcl = 0;
    for (cl=0; cl<k; cl++) {
      d[cl] = 0.0;
      for (col=0; col<m; col++) {
        temp = (double)x[col][j] - cent[col][cl];
        d[cl] += temp * temp;
      }
      if (d[cl]<d[bestcl]) bestcl = cl;
    }
    clust[0][j] = bestcl + 1;
    clustsizes[0][bestcl]++;
    for (col=0; col<m; col++)
      tempcent[col][bestcl] += (double)x[col][j];
  }
  for (cl=0; cl<k; cl++)
    for (col=0; col<m; col++)
      cent[col][cl] = tempcent[col][cl] / clustsizes[0][cl];

  do {

    nchange = 0;
    for (j=0; j<n; j++) { // For each of my points, this is offset from hash position

      oldcluster = clust[0][j] - 1;
      bestcl = 0;
      for (cl=0; cl<k; cl++) {         // Consider each of the clusters
        d[cl] = 0.0;                   // We'll get the distance to this cluster.
        for (col=0; col<m; col++) {    // Loop over the dimension of the data
          temp = (double)x[col][j] - cent[col][cl];
          d[cl] += temp * temp;
        }
        if (d[cl]<d[bestcl]) bestcl = cl;
      } // End of looking over the clusters for this j

      if (d[bestcl] < d[oldcluster]) {           // MADE A CHANGE!
        newcluster = bestcl;
        clust[0][j] = newcluster + 1;
        nchange++;
        clustsizes[0][newcluster]++;
        clustsizes[0][oldcluster]--;
        for (col=0; col<m; col++) {
          cent[col][oldcluster] += ( cent[col][oldcluster] - (double)x[col][j] ) / clustsizes[0][oldcluster];
          cent[col][newcluster] += ( (double)x[col][j] - cent[col][newcluster] ) / clustsizes[0][newcluster];
        }
      }

    } // End of this pass over my points.

    iter++;
    if ( (nchange==0) || (iter>=maxiters) ) done = 1;

  } while (done==0);

  // Collect the sums of squares now that we're done.
  for (cl=0; cl<k; cl++) ss[0][cl] = 0.0;
  for (j=0; j<n; j++) {
    for (col=0; col<m; col++) {
      cl = clust[0][j]-1;
      temp = (double)x[col][j] - cent[col][cl];
      ss[0][cl] += temp * temp;
    }
  }

  // At this point, cent is the centers, ss is the within-groups sums of squares,
  // clust is the cluster memberships, clustsizes is the cluster sizes.

  //outFile.close();
  return iter;

}


extern "C"
{

SEXP HasRowColNames(SEXP address)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(address);
  SEXP ret = PROTECT(NEW_LOGICAL(2));
  LOGICAL_DATA(ret)[0] = 
    pMat->row_names().empty() ? Rboolean(0) : Rboolean(1);
  LOGICAL_DATA(ret)[1] = 
    pMat->column_names().empty() ? Rboolean(0) : Rboolean(1);
  UNPROTECT(1);
  return ret;
}

SEXP GetIndexRowNames(SEXP address, SEXP indices)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(address);
  Names rn = pMat->row_names();
  return StringVec2RChar(rn, NUMERIC_DATA(indices), GET_LENGTH(indices));
}

SEXP GetIndexColNames(SEXP address, SEXP indices)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(address);
  Names cn = pMat->column_names();
  return StringVec2RChar(cn, NUMERIC_DATA(indices), GET_LENGTH(indices));
}

SEXP GetColumnNamesBM(SEXP address)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(address);
  Names cn = pMat->column_names();
  return StringVec2RChar(cn);
}

SEXP GetRowNamesBM(SEXP address)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(address);
  Names rn = pMat->row_names();
  return StringVec2RChar(rn);
}

void SetColumnNames(SEXP address, SEXP columnNames)
{
  BigMatrix *pMat = (BigMatrix*) R_ExternalPtrAddr(address);
  Names cn;
  long i;
  for (i=0; i < GET_LENGTH(columnNames); ++i)
    cn.push_back(string(CHAR(STRING_ELT(columnNames, i))));
  pMat->column_names(cn);
}

void SetRowNames(SEXP address, SEXP rowNames)
{
  BigMatrix *pMat = (BigMatrix*) R_ExternalPtrAddr(address);
  Names rn;
  long i;
  for (i=0; i < GET_LENGTH(rowNames); ++i)
    rn.push_back(string(CHAR(STRING_ELT(rowNames, i))));
  pMat->row_names(rn);
}

SEXP CGetNrow(SEXP bigMatAddr)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatAddr);
  SEXP ret = PROTECT(NEW_NUMERIC(1));
  NUMERIC_DATA(ret)[0] = (double)pMat->nrow();
  UNPROTECT(1);
  return(ret);
}

SEXP CGetNcol(SEXP bigMatAddr)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatAddr);
  SEXP ret = PROTECT(NEW_NUMERIC(1));
  NUMERIC_DATA(ret)[0] = (double)pMat->ncol();
  UNPROTECT(1);
  return(ret);
}

SEXP CGetType(SEXP bigMatAddr)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatAddr);
  SEXP ret = PROTECT(NEW_INTEGER(1));
  INTEGER_DATA(ret)[0] = pMat->matrix_type();
  UNPROTECT(1);
  return(ret);
}

SEXP IsShared(SEXP bigMatAddr)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatAddr);
  SEXP ret = PROTECT(NEW_LOGICAL(1));
  // Make this an R logical.
  LOGICAL_DATA(ret)[0] = pMat->shared() ? (Rboolean)1 : (Rboolean)0;
  UNPROTECT(1);
  return(ret);
}

SEXP IsSharedMemoryBigMatrix(SEXP bigMatAddr)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatAddr);
  SEXP ret = PROTECT(NEW_LOGICAL(1));
  LOGICAL_DATA(ret)[0] = 
    dynamic_cast<SharedMemoryBigMatrix*>(pMat) == NULL ? 
      static_cast<Rboolean>(0) :
      static_cast<Rboolean>(1);
  UNPROTECT(1);
  return ret;
}

SEXP IsFileBackedBigMatrix(SEXP bigMatAddr)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatAddr);
  SEXP ret = PROTECT(NEW_LOGICAL(1));
  LOGICAL_DATA(ret)[0] = 
    dynamic_cast<FileBackedBigMatrix*>(pMat) == NULL ? 
      static_cast<Rboolean>(0) :
      static_cast<Rboolean>(1);
  UNPROTECT(1);
  return ret;
}

SEXP IsSeparated(SEXP bigMatAddr)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatAddr);
  SEXP ret = PROTECT(NEW_LOGICAL(1));
  // Make this an R logical.
  LOGICAL_DATA(ret)[0] = pMat->separated_columns() ? (Rboolean)1 : (Rboolean)0;
  UNPROTECT(1);
  return(ret);
}

void CDestroyMatrix(SEXP bigMatrixAddr)
{
  delete (BigMatrix*)R_ExternalPtrAddr(bigMatrixAddr);
  R_ClearExternalPtr(bigMatrixAddr);
}

SEXP CCreateMatrix(SEXP row, SEXP col, SEXP ini, SEXP type, SEXP separated) 
{
  // TODO: Make a function to initialize the values of the matrix
  LocalBigMatrix *pMat = new LocalBigMatrix;
  if (!(pMat->create( static_cast<long>(NUMERIC_VALUE(row)),
    static_cast<long>(NUMERIC_VALUE(col)),
    INTEGER_VALUE(type),
    static_cast<bool>(LOGICAL_VALUE(separated)))) )
  {
    fprintf(stderr, "Memory for big.matrix could no be allocated.\n");
    delete pMat;
    return R_NilValue;
  }
  if (GET_LENGTH(ini) != 0)
  {
    if (pMat->separated_columns())
    {
      switch (pMat->matrix_type())
      {
        case 1:
          SetAllMatrixElements<char, SepBigMatrixAccessor<char> >(
            pMat, ini, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_REAL);
          break;
        case 2:
          SetAllMatrixElements<short, SepBigMatrixAccessor<short> >(
            pMat, ini, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX, NA_REAL);
          break;
        case 4:
          SetAllMatrixElements<char, SepBigMatrixAccessor<int> >(
            pMat, ini, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_REAL);
          break;
        case 8:
          SetAllMatrixElements<double, SepBigMatrixAccessor<double> >(
            pMat, ini, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
      }
    }
    else
    {
      switch (pMat->matrix_type())
      {
        case 1:
          SetAllMatrixElements<char, BigMatrixAccessor<char> >(
            pMat, ini, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_REAL);
          break;
        case 2:
          SetAllMatrixElements<short, BigMatrixAccessor<short> >(
            pMat, ini, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX, NA_REAL);
          break;
        case 4:
          SetAllMatrixElements<char, BigMatrixAccessor<int> >(
            pMat, ini, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_REAL);
          break;
        case 8:
          SetAllMatrixElements<double, BigMatrixAccessor<double> >(
            pMat, ini, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
      }
    }
  }
  SEXP address = R_MakeExternalPtr(pMat, R_NilValue, R_NilValue);
  R_RegisterCFinalizerEx(address, (R_CFinalizer_t) CDestroyMatrix, 
    (Rboolean) TRUE);
  return address;
}

void CAddMatrixCol(SEXP bigMatAddr, SEXP init)
{
  // TODO: FIX THIS
/*
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatAddr);
  pMat->insert_column(pMat->ncol(), NUMERIC_VALUE(init), "");
*/
}

void CEraseMatrixCol(SEXP bigMatAddr, SEXP eraseColumn)
{
  // TODO: FIX THIS
/*
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatAddr);
  pMat->remove_column( (long)NUMERIC_VALUE(eraseColumn)-1 );
*/
}

inline bool Lcomp(double a, double b, int op) {
  return(op==0 ? a<=b : a<b);
}
inline bool Gcomp(double a, double b, int op) {
  return(op==0 ? a>=b : a>b);
}

} // close extern C?

template<typename T, typename MatrixType>
SEXP MWhichMatrix( MatrixType mat, long nrow, SEXP selectColumn, SEXP minVal,
  SEXP maxVal, SEXP chkMin, SEXP chkMax, SEXP opVal, double C_NA )
{
  long numSc = GET_LENGTH(selectColumn);
  double *sc = NUMERIC_DATA(selectColumn);
  double *min = NUMERIC_DATA(minVal);
  double *max = NUMERIC_DATA(maxVal);
  int *chkmin = INTEGER_DATA(chkMin);
  int *chkmax = INTEGER_DATA(chkMax);

  double minV, maxV;
  int ov = INTEGER_VALUE(opVal);
  long count = 0;
  long i,j;
  double val;
  for (i=0; i < nrow; ++i) {
    for (j=0; j < numSc; ++j)  {
      minV = min[j];
      maxV = max[j];
      if (isna(minV)) {
        minV = static_cast<T>(C_NA);
        maxV = static_cast<T>(C_NA);
      }
      val = (double) mat[(long)sc[j]-1][i];
      if (chkmin[j]==-1) { // this is an 'neq'
        if (ov==1) { 
          // OR with 'neq'
          if  ( (minV!=val) ||
                ( (isna(val) && !isna(minV)) ||
                  (!isna(val) && isna(minV)) ) ) {
            ++count;
            break;
          }
        } else {
          // AND with 'neq'   // if they are equal, then break out.
          if ( (minV==val) || (isna(val) && isna(minV)) ) break;
        }
      } else { // not a 'neq'     

        // If it's an OR operation and it's true for one, it's true for the
        // whole row. JJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJ
        if ( ( (Gcomp(val, minV, chkmin[j]) && Lcomp(val, maxV, chkmax[j])) ||
               (isna(val) && isna(minV))) && ov==1 ) { 
          ++count;
          break;
        }
        // If it's an AND operation and it's false for one, it's false for
        // the whole row. JJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJ
        if ( ( (Lcomp(val, minV, 1-chkmin[j]) || Gcomp(val, maxV, 1-chkmax[j])) ||
               (isna(val) && !isna(minV)) || (!isna(val) && isna(minV)) ) &&
             ov == 0 ) break;
      }
    }
    // If it's an AND operation and it's true for each column, it's true
    // for the entire row.
    if (j==numSc && ov == 0) ++count;
  }

  if (count==0) return NEW_INTEGER(0);

  SEXP ret = PROTECT(NEW_NUMERIC(count));
  double *retVals = NUMERIC_DATA(ret);
  long k = 0;
  for (i=0; i < nrow; ++i) {
    for (j=0; j < numSc; ++j) {
      minV = min[j];
      maxV = max[j];
      if (isna(minV)) {
        minV = static_cast<T>(C_NA);
        maxV = static_cast<T>(C_NA);
      }
      val = (double) mat[(long)sc[j]-1][i];

      if (chkmin[j]==-1) { // this is an 'neq'
        if (ov==1) {
          // OR with 'neq'
          if  ( (minV!=val) ||
                ( (isna(val) && !isna(minV)) || 
                  (!isna(val) && isna(minV)) ) ) {
            retVals[k++] = i+1;
            break;
          }
        } else {
          // AND with 'neq'   // if they are equal, then break out.
          if ( (minV==val) || (isna(val) && isna(minV)) ) break;
        }
      } else { // not a 'neq'

        if ( ( (Gcomp(val, minV, chkmin[j]) && Lcomp(val, maxV, chkmax[j])) ||
               (isna(val) && isna(minV))) && ov==1 ) {
          retVals[k++] = i+1;
          break;
        }
        if ( ( (Lcomp(val, minV, 1-chkmin[j]) || Gcomp(val, maxV, 1-chkmax[j])) ||
               (isna(val) && !isna(minV)) || (!isna(val) && isna(minV)) ) &&
             ov == 0 ) break;

      }
    } // end j loop
    if (j==numSc && ov == 0) retVals[k++] = i+1;
  } // end i loop
  UNPROTECT(1);
  return(ret);
}

extern "C"{

SEXP GetTypeString( SEXP bigMatAddr )
{
  BigMatrix *pMat = 
    reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  SEXP ret = PROTECT(allocVector(STRSXP, 1));
  switch (pMat->matrix_type())
  {
    case 1:
      SET_STRING_ELT(ret, 0, mkChar("char"));
      break;
    case 2:
      SET_STRING_ELT(ret, 0, mkChar("short"));
      break;
    case 4:
      SET_STRING_ELT(ret, 0, mkChar("integer"));
      break;
    case 8:
      SET_STRING_ELT(ret, 0, mkChar("double"));
  }
  UNPROTECT(1);
  return ret;
}

SEXP MWhichBigMatrix( SEXP bigMatAddr, SEXP selectColumn, SEXP minVal,
                     SEXP maxVal, SEXP chkMin, SEXP chkMax, SEXP opVal )
{
  BigMatrix *pMat = 
    reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return MWhichMatrix<char>( SepBigMatrixAccessor<char>(*pMat),
          pMat->nrow(), selectColumn, minVal, maxVal, chkMin, chkMax, 
          opVal, NA_CHAR);
      case 2:
        return MWhichMatrix<short>( SepBigMatrixAccessor<short>(*pMat),
          pMat->nrow(), selectColumn, minVal, maxVal, chkMin, chkMax, 
          opVal, NA_SHORT);
      case 4:
        return MWhichMatrix<int>( SepBigMatrixAccessor<int>(*pMat),
          pMat->nrow(), selectColumn, minVal, maxVal, chkMin, chkMax, 
          opVal, NA_INTEGER);
      case 8:
        return MWhichMatrix<double>( SepBigMatrixAccessor<double>(*pMat),
          pMat->nrow(), selectColumn, minVal, maxVal, chkMin, chkMax, 
          opVal, NA_REAL);
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return MWhichMatrix<char>( BigMatrixAccessor<char>(*pMat),
          pMat->nrow(), selectColumn, minVal, maxVal, chkMin, chkMax, 
          opVal, NA_CHAR);
      case 2:
        return MWhichMatrix<short>( BigMatrixAccessor<short>(*pMat),
          pMat->nrow(), selectColumn, minVal, maxVal, chkMin, chkMax, 
          opVal, NA_SHORT);
      case 4:
        return MWhichMatrix<int>( BigMatrixAccessor<int>(*pMat),
          pMat->nrow(), selectColumn, minVal, maxVal, chkMin, chkMax, 
          opVal, NA_INTEGER);
      case 8:
        return MWhichMatrix<double>( BigMatrixAccessor<double>(*pMat),
          pMat->nrow(), selectColumn, minVal, maxVal, chkMin, chkMax, 
          opVal, NA_REAL);
    }
  }
  return R_NilValue;
}

SEXP MWhichRIntMatrix( SEXP matrixVector, SEXP nrow, SEXP selectColumn,
  SEXP minVal, SEXP maxVal, SEXP chkMin, SEXP chkMax, SEXP opVal )
{
  long numRows = static_cast<long>(INTEGER_VALUE(nrow));
  BigMatrixAccessor<int> mat(INTEGER_DATA(matrixVector), numRows);
  return MWhichMatrix<int, BigMatrixAccessor<int> >(mat, numRows, 
    selectColumn, minVal, maxVal, chkMin, chkMax, opVal, NA_INTEGER);
}

SEXP MWhichRNumericMatrix( SEXP matrixVector, SEXP nrow, SEXP selectColumn,
  SEXP minVal, SEXP maxVal, SEXP chkMin, SEXP chkMax, SEXP opVal )
{
  long numRows = static_cast<long>(INTEGER_VALUE(nrow));
  BigMatrixAccessor<double> mat(NUMERIC_DATA(matrixVector), numRows);
  return MWhichMatrix<double, BigMatrixAccessor<double> >(mat, numRows,
    selectColumn, minVal, maxVal, chkMin, chkMax, opVal, NA_REAL);
}

SEXP MatrixHashRanges( SEXP bigMatAddr, SEXP selectColumn )
{
  BigMatrix *pMat = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return MatrixHashRanges<char, SepBigMatrixAccessor<char> >(
          pMat, selectColumn);
      case 2:
        return MatrixHashRanges<short, SepBigMatrixAccessor<short> >(
          pMat, selectColumn);
      case 4:
        return MatrixHashRanges<int, SepBigMatrixAccessor<int> >(
          pMat, selectColumn);
      case 8:
        return MatrixHashRanges<double, SepBigMatrixAccessor<double> >(
          pMat, selectColumn);
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return MatrixHashRanges<char, BigMatrixAccessor<char> >(
          pMat, selectColumn);
      case 2:
        return MatrixHashRanges<short, BigMatrixAccessor<short> >(
          pMat, selectColumn);
      case 4:
        return MatrixHashRanges<int, BigMatrixAccessor<int> >(
          pMat, selectColumn);
      case 8:
        return MatrixHashRanges<double, BigMatrixAccessor<double> >(
          pMat, selectColumn);
    }
  }
  return R_NilValue;
}

SEXP CCountLines(SEXP fileName)
{ 
  FILE *FP;
  double lineCount = 0;
  char readChar;
  FP = fopen(STRING_VALUE(fileName), "r");
  SEXP ret = PROTECT(NEW_NUMERIC(1));
  NUMERIC_DATA(ret)[0] = -1;                   
  if (FP == NULL) return(ret);
  do {
    readChar = fgetc(FP);
    if ('\n' == readChar) ++lineCount;
  } while( readChar != EOF );
  fclose(FP);
  NUMERIC_DATA(ret)[0] = lineCount; 
  UNPROTECT(1);                  
  return(ret);
}

SEXP ColCountNA(SEXP address, SEXP column)
{ 
  BigMatrix *pMat = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(address));
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return ColCountNA<char, SepBigMatrixAccessor<char> >(pMat, column);
      case 2:
        return ColCountNA<short, SepBigMatrixAccessor<short> >(pMat, column);
      case 4:
        return ColCountNA<int, SepBigMatrixAccessor<int> >(pMat, column);
      case 8:
        return ColCountNA<double, SepBigMatrixAccessor<double> >(pMat, column);
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return ColCountNA<char, BigMatrixAccessor<char> >(pMat, column);
      case 2:
        return ColCountNA<short, BigMatrixAccessor<short> >(pMat, column);
      case 4:
        return ColCountNA<int, BigMatrixAccessor<int> >(pMat, column);
      case 8:
        return ColCountNA<double, BigMatrixAccessor<double> >(pMat, column);
    }
  }
  return R_NilValue;
}

SEXP ReadMatrix(SEXP fileName, SEXP bigMatAddr,
              SEXP firstLine, SEXP numLines, SEXP numCols, SEXP separator,
              SEXP hasRowNames, SEXP useRowNames)
{
  BigMatrix *pMat = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return ReadMatrix<char, SepBigMatrixAccessor<char> >(
          fileName, pMat, firstLine, numLines, numCols, 
          separator, hasRowNames, useRowNames, NA_CHAR, NA_CHAR, NA_CHAR, 
					NA_CHAR);
      case 2:
        return ReadMatrix<short, SepBigMatrixAccessor<short> >(
          fileName, pMat, firstLine, numLines, numCols, 
          separator, hasRowNames, useRowNames, NA_SHORT, NA_SHORT, NA_SHORT, 
					NA_SHORT);
      case 4:
        return ReadMatrix<int, SepBigMatrixAccessor<int> >(
          fileName, pMat, firstLine, numLines, numCols, 
          separator, hasRowNames, useRowNames, NA_INTEGER, NA_INTEGER, 
					NA_INTEGER, NA_INTEGER);
      case 8:
        return ReadMatrix<double, SepBigMatrixAccessor<double> >(
          fileName, pMat, firstLine, numLines, numCols, 
          separator, hasRowNames, useRowNames, NA_REAL, R_PosInf, R_NegInf, 
					R_NaN);
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return ReadMatrix<char, BigMatrixAccessor<char> >(
          fileName, pMat, firstLine, numLines, numCols, 
          separator, hasRowNames, useRowNames, NA_CHAR, NA_CHAR, NA_CHAR, 
					NA_CHAR);
      case 2:
        return ReadMatrix<short, BigMatrixAccessor<short> >(
          fileName, pMat, firstLine, numLines, numCols, 
          separator, hasRowNames, useRowNames, NA_SHORT, NA_SHORT, NA_SHORT, 
					NA_SHORT);
      case 4:
        return ReadMatrix<int, BigMatrixAccessor<int> >(
          fileName, pMat, firstLine, numLines, numCols, 
          separator, hasRowNames, useRowNames, NA_INTEGER, NA_INTEGER, 
					NA_INTEGER, NA_INTEGER);
      case 8:
        return ReadMatrix<double, BigMatrixAccessor<double> >(
          fileName, pMat, firstLine, numLines, numCols, 
          separator, hasRowNames, useRowNames, NA_REAL, R_PosInf, R_NegInf, 
					R_NaN);
    }
  }
  return R_NilValue;
}


void WriteMatrix( SEXP bigMatAddr, SEXP fileName, SEXP rowNames,
  SEXP colNames, SEXP sep )
{
  BigMatrix *pMat = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        WriteMatrix<char, SepBigMatrixAccessor<char> >(
          pMat, fileName, rowNames, colNames, sep, NA_CHAR);
        break;
      case 2:
        WriteMatrix<short, SepBigMatrixAccessor<short> >(
          pMat, fileName, rowNames, colNames, sep, NA_SHORT);
        break;
      case 4:
        WriteMatrix<int, SepBigMatrixAccessor<int> >(
          pMat, fileName, rowNames, colNames, sep, NA_INTEGER);
        break;
      case 8:
        WriteMatrix<double, SepBigMatrixAccessor<double> >(
          pMat, fileName, rowNames, colNames, sep, NA_REAL);
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        WriteMatrix<char, BigMatrixAccessor<char> >(
          pMat, fileName, rowNames, colNames, sep, NA_CHAR);
        break;
      case 2:
        WriteMatrix<short, BigMatrixAccessor<short> >(
          pMat, fileName, rowNames, colNames, sep, NA_SHORT);
        break;
      case 4:
        WriteMatrix<int, BigMatrixAccessor<int> >(
          pMat, fileName, rowNames, colNames, sep, NA_INTEGER);
        break;
      case 8:
        WriteMatrix<double, BigMatrixAccessor<double> >(
          pMat, fileName, rowNames, colNames, sep, NA_REAL);
    }
  }
}

SEXP GetMatrixElements(SEXP bigMatAddr, SEXP col, SEXP row)
{
  BigMatrix *pMat = 
    reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  if (pMat->separated_columns())
  {
    switch(pMat->matrix_type())
    {
      case 1:
        return GetMatrixElements<char, int, SepBigMatrixAccessor<char> >
          (pMat, NA_CHAR, NA_INTEGER, col, row, INTSXP);
      case 2:
        return GetMatrixElements<short,int, SepBigMatrixAccessor<short> >
          (pMat, NA_SHORT, NA_INTEGER, col, row, INTSXP);
      case 4:
        return GetMatrixElements<int, int, SepBigMatrixAccessor<int> >
          (pMat, NA_INTEGER, NA_INTEGER, col, row, INTSXP);
      case 8:
        return GetMatrixElements<double, double, SepBigMatrixAccessor<double> >(
          pMat, NA_REAL, NA_REAL, col, row, REALSXP);
    }
  }
  else
  {
    switch(pMat->matrix_type())
    {
      case 1:
        return GetMatrixElements<char, int, BigMatrixAccessor<char> >(
          pMat, NA_CHAR, NA_INTEGER, col, row, INTSXP);
      case 2:
        return GetMatrixElements<short, int, BigMatrixAccessor<short> >(
          pMat, NA_SHORT, NA_INTEGER, col, row, INTSXP);
      case 4:
        return GetMatrixElements<int, int, BigMatrixAccessor<int> >(
          pMat, NA_INTEGER, NA_INTEGER, col, row, INTSXP);
      case 8:
        return GetMatrixElements<double, double, BigMatrixAccessor<double> >
          (pMat, NA_REAL, NA_REAL, col, row, REALSXP);
    }
  }
  return R_NilValue;
}

void SetMatrixElements(SEXP bigMatAddr, SEXP col, SEXP row, SEXP values)
{
  BigMatrix *pMat = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        SetMatrixElements<char, int, SepBigMatrixAccessor<char> >( 
          pMat, col, row, values, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_INTEGER);
        break;
      case 2:
        SetMatrixElements<short, int, SepBigMatrixAccessor<short> >( 
          pMat, col, row, values, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX, 
          NA_INTEGER);
        break;
      case 4:
        SetMatrixElements<int, int, SepBigMatrixAccessor<int> >( 
          pMat, col, row, values, NA_INTEGER, R_INT_MIN, R_INT_MAX, NA_INTEGER);
        break;
      case 8:
        SetMatrixElements<double, double, SepBigMatrixAccessor<double> >( 
          pMat, col, row, values, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        SetMatrixElements<char, int, BigMatrixAccessor<char> >( 
          pMat, col, row, values, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_INTEGER);
        break;
      case 2:
        SetMatrixElements<short, int, BigMatrixAccessor<short> >( 
          pMat, col, row, values, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX, 
          NA_INTEGER);
        break;
      case 4:
        SetMatrixElements<int, int, BigMatrixAccessor<int> >( 
          pMat, col, row, values, NA_INTEGER, R_INT_MIN, R_INT_MAX, NA_INTEGER);
        break;
      case 8:
        SetMatrixElements<double, double, BigMatrixAccessor<double> >( 
          pMat, col, row, values, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
    }
  }
}

// It is assumed that value is a double.
void SetAllMatrixElements(SEXP bigMatAddr, SEXP value)
{
  BigMatrix *pMat = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        SetAllMatrixElements<char, SepBigMatrixAccessor<char> >( 
          pMat, value, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_REAL);
        break;
      case 2:
        SetAllMatrixElements<short, SepBigMatrixAccessor<short> >( 
          pMat, value, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX, NA_REAL);
        break;
      case 4:
        SetAllMatrixElements<int, SepBigMatrixAccessor<int> >( 
          pMat, value, NA_INTEGER, R_INT_MIN, R_INT_MAX, NA_REAL);
        break;
      case 8:
        SetAllMatrixElements<double, SepBigMatrixAccessor<double> >( 
          pMat, value, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        SetAllMatrixElements<char, BigMatrixAccessor<char> >( 
          pMat, value, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_INTEGER);
        break;
      case 2:
        SetAllMatrixElements<short, BigMatrixAccessor<short> >( 
          pMat, value, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX, 
          NA_INTEGER);
        break;
      case 4:
        SetAllMatrixElements<int, BigMatrixAccessor<int> >( 
          pMat, value, NA_INTEGER, R_INT_MIN, R_INT_MAX, NA_INTEGER);
        break;
      case 8:
        SetAllMatrixElements<double, BigMatrixAccessor<double> >( 
          pMat, value, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
    }
  }
}

void CDestroySharedMatrix(SEXP bigMatrixAddr) 
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatrixAddr);
  delete pMat;
  R_ClearExternalPtr(bigMatrixAddr);
}

SEXP CCreateSharedMatrix(SEXP row, SEXP col, SEXP colnames, SEXP rownames,
  SEXP typeLength, SEXP ini, SEXP separated) 
{
  SharedMemoryBigMatrix *pMat = new SharedMemoryBigMatrix();
  if (!pMat->create( static_cast<long>(NUMERIC_VALUE(row)),
    static_cast<long>(NUMERIC_VALUE(col)),
    INTEGER_VALUE(typeLength),
    static_cast<bool>(LOGICAL_VALUE(separated))))
  {
    delete pMat;
    return NULL_USER_OBJECT;
  }
  if (colnames != NULL_USER_OBJECT)
  {
    pMat->column_names(RChar2StringVec(colnames));
  }
  if (rownames != NULL_USER_OBJECT)
  {
    pMat->row_names(RChar2StringVec(rownames));
  }
  if (GET_LENGTH(ini) != 0)
  {
    if (pMat->separated_columns())
    {
      switch (pMat->matrix_type())
      {
        case 1:
          SetAllMatrixElements<char, SepBigMatrixAccessor<char> >(
            pMat, ini, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_REAL);
          break;
        case 2:
          SetAllMatrixElements<short, SepBigMatrixAccessor<short> >(
            pMat, ini, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX, NA_REAL);
          break;
        case 4:
          SetAllMatrixElements<char, SepBigMatrixAccessor<int> >(
            pMat, ini, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_REAL);
          break;
        case 8:
          SetAllMatrixElements<double, SepBigMatrixAccessor<double> >(
            pMat, ini, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
      }
    }
    else
    {
      switch (pMat->matrix_type())
      {
        case 1:
          SetAllMatrixElements<char, BigMatrixAccessor<char> >(
            pMat, ini, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_REAL);
          break;
        case 2:
          SetAllMatrixElements<short, BigMatrixAccessor<short> >(
            pMat, ini, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX, NA_REAL);
          break;
        case 4:
          SetAllMatrixElements<char, BigMatrixAccessor<int> >(
            pMat, ini, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_REAL);
          break;
        case 8:
          SetAllMatrixElements<double, BigMatrixAccessor<double> >(
            pMat, ini, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
      }
    }
  }
  SEXP address = R_MakeExternalPtr( dynamic_cast<BigMatrix*>(pMat),
    R_NilValue, R_NilValue);
  R_RegisterCFinalizerEx(address, (R_CFinalizer_t) CDestroySharedMatrix, 
      (Rboolean) TRUE);
  return address;
}

void* GetDataPtr(SEXP address)
{
  SharedBigMatrix *pMat = 
    reinterpret_cast<SharedBigMatrix*>(R_ExternalPtrAddr(address));
	return pMat->data_ptr();
}

SEXP CCreateFileBackedBigMatrix(SEXP fileName, SEXP filePath, SEXP row, 
  SEXP col, SEXP colnames, SEXP rownames, SEXP typeLength, SEXP ini, 
  SEXP separated, SEXP preserve) 
{
  FileBackedBigMatrix *pMat = new FileBackedBigMatrix();
  string fn;
  string path = ((filePath == NULL_USER_OBJECT) ? "" : RChar2String(filePath));
  if (isNull(fileName))
  {
    fn=pMat->uuid()+".bin";
  }
  else
  {
    fn = RChar2String(fileName);
  }
  if (!pMat->create( fn, RChar2String(filePath),
    static_cast<long>(NUMERIC_VALUE(row)),
    static_cast<long>(NUMERIC_VALUE(col)),
    INTEGER_VALUE(typeLength),
    static_cast<bool>(LOGICAL_VALUE(separated)),
    static_cast<bool>(LOGICAL_VALUE(preserve))))
  {
    delete pMat;
    error("Problem creating filebacked matrix.");
    return NULL_USER_OBJECT;
  }
  if (colnames != NULL_USER_OBJECT)
  {
    pMat->column_names(RChar2StringVec(colnames));
  }
  if (rownames != NULL_USER_OBJECT)
  {
    pMat->row_names(RChar2StringVec(rownames));
  }
  if (GET_LENGTH(ini) != 0)
  {
    if (pMat->separated_columns())
    {
      switch (pMat->matrix_type())
      {
        case 1:
          SetAllMatrixElements<char, SepBigMatrixAccessor<char> >(
            pMat, ini, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_REAL);
          break;
        case 2:
          SetAllMatrixElements<short, SepBigMatrixAccessor<short> >(
            pMat, ini, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX, NA_REAL);
          break;
        case 4:
          SetAllMatrixElements<char, SepBigMatrixAccessor<int> >(
            pMat, ini, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_REAL);
          break;
        case 8:
          SetAllMatrixElements<double, SepBigMatrixAccessor<double> >(
            pMat, ini, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
      }
    }
    else
    {
      switch (pMat->matrix_type())
      {
        case 1:
          SetAllMatrixElements<char, BigMatrixAccessor<char> >(
            pMat, ini, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_REAL);
          break;
        case 2:
          SetAllMatrixElements<short, BigMatrixAccessor<short> >(
            pMat, ini, NA_SHORT, R_SHORT_MIN, R_SHORT_MAX, NA_REAL);
          break;
        case 4:
          SetAllMatrixElements<char, BigMatrixAccessor<int> >(
            pMat, ini, NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_REAL);
          break;
        case 8:
          SetAllMatrixElements<double, BigMatrixAccessor<double> >(
            pMat, ini, NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
      }
    }
  }
  SEXP address = R_MakeExternalPtr( dynamic_cast<BigMatrix*>(pMat),
    R_NilValue, R_NilValue);
  R_RegisterCFinalizerEx(address, (R_CFinalizer_t) CDestroySharedMatrix, 
      (Rboolean) TRUE);
  return address;
}

/*
SEXP GetDescriptorString(SEXP address)
{
  SharedBigMatrix *pMat = 
    reinterpret_cast<SharedBigMatrix*>(R_ExternalPtrAddr(address));
  // JJJJJJJJ Do automatic writing of the description.
  // At this point, write out the description to the file: fn+"descriptor"  ?
  // Example format:
  // structure(list(sharedType = "FileBacked", sharedName = "example.binba0f899b-95d7-478c-a7a5-d0836deccfb9",
  //                fileName = "example.bin", nrow = 3, ncol = 3, rowNames = NULL,
  //                colNames = NULL, type = "integer", separated = FALSE), .Names = c("sharedType",
  //           "sharedName", "fileName", "nrow", "ncol", "rowNames", "colNames", "type", "separated"))
  string fnd;
  fnd = fn + ".descriptor";
  fp.open(fnd.c_str(), ios::out);
  string filetype;
  filetype = "double";
  if (pMat->matrix_type() == 1) filetype = "char";
  if (pMat->matrix_type() == 2) filetype = "short";
  if (pMat->matrix_type() == 4) filetype = "integer";
  string separatedtype;
  separatedtype = "FALSE";
  if (pMat->separated_columns()) separatedtype = "TRUE";
  fp << "structure(list(sharedType = \"FileBacked\", sharedName = \"";
  fp << pMat->shared_name();
  fp << "\"," << endl;
  fp << "filename = \"" << fn << "\", nrow = ";
  fp << (long) NUMERIC_VALUE(row);
  fp << ", ncol = ";
  fp << (long) NUMERIC_VALUE(col);
  fp << ", rowNames = ";
  Names rn = pMat->row_names();
  if (rn.size()==0) fp << "NULL";
  else {
    long i;
    fp << "c(";
    for (i=0; i< static_cast<long>(rn.size()); i++) {
      if (i>0) fp << ", ";
      fp << "\"" << rn[i] << "\"";
    }
    fp << ")";
  }
  fp << "," << endl;
  fp << "colNames = ";
  Names cn = pMat->column_names();
  if (cn.size()==0) fp << "NULL";
  else {
    long i;
    fp << "c(";
    for (i=0; i< static_cast<long>(cn.size()); i++) {
      if (i>0) fp << ", ";
      fp << "\"" << cn[i] << "\"";
    }
    fp << ")";
  }
  fp << "," << endl;
  fp << "type = \"" << filetype << "\", separated = " << separatedtype << ")," << endl;
  fp << ".Names = c(\"sharedType\", \"sharedName\", \"fileName\", \"nrow\", \"ncol\", \"rowNames\",";
  fp << "\"colNames\", \"type\", \"separated\"))" << endl;
  fp.close();

  // END JJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJ
}
*/

SEXP CAttachSharedBigMatrix(SEXP sharedName, SEXP rows, SEXP cols, 
  SEXP rowNames, SEXP colNames, SEXP typeLength, SEXP separated)
{
  SharedMemoryBigMatrix *pMat = new SharedMemoryBigMatrix();
  bool connected = pMat->connect( 
    string(CHAR(STRING_ELT(sharedName,0))),
    static_cast<long>(NUMERIC_VALUE(rows)),
    static_cast<long>(NUMERIC_VALUE(cols)),
    INTEGER_VALUE(typeLength),
    static_cast<bool>(LOGICAL_VALUE(separated)));
  if (!connected)
  {
    delete pMat;
    return NULL_USER_OBJECT;
  }
  if (GET_LENGTH(colNames) > 0)
  {
    pMat->column_names(RChar2StringVec(colNames));
  }
  if (GET_LENGTH(rowNames) > 0)
  {
    pMat->row_names(RChar2StringVec(rowNames));
  }
  SEXP address = R_MakeExternalPtr( dynamic_cast<BigMatrix*>(pMat),
    R_NilValue, R_NilValue);
  R_RegisterCFinalizerEx(address, (R_CFinalizer_t) CDestroySharedMatrix, 
      (Rboolean) TRUE);
  return address;
}

SEXP CAttachFileBackedBigMatrix(SEXP sharedName, SEXP fileName, 
  SEXP filePath, SEXP rows, SEXP cols, SEXP rowNames, SEXP colNames, 
  SEXP typeLength, SEXP separated)
{
  FileBackedBigMatrix *pMat = new FileBackedBigMatrix();
  bool connected = pMat->connect( 
    string(CHAR(STRING_ELT(sharedName,0))),
    string(CHAR(STRING_ELT(fileName,0))),
    string(CHAR(STRING_ELT(filePath,0))),
    static_cast<long>(NUMERIC_VALUE(rows)),
    static_cast<long>(NUMERIC_VALUE(cols)),
    INTEGER_VALUE(typeLength),
    static_cast<bool>(LOGICAL_VALUE(separated)),
    true);
  if (!connected)
  {
    delete pMat;
    return NULL_USER_OBJECT;
  }
  if (GET_LENGTH(colNames) > 0)
  {
    pMat->column_names(RChar2StringVec(colNames));
  }
  if (GET_LENGTH(rowNames) > 0)
  {
    pMat->row_names(RChar2StringVec(rowNames));
  }
  SEXP address = R_MakeExternalPtr( dynamic_cast<BigMatrix*>(pMat),
    R_NilValue, R_NilValue);
  R_RegisterCFinalizerEx(address, (R_CFinalizer_t) CDestroySharedMatrix, 
      (Rboolean) TRUE);
  return address;
}

SEXP SharedName( SEXP address )
{
  SharedBigMatrix *pMat = 
    reinterpret_cast<SharedBigMatrix*>(R_ExternalPtrAddr(address));
  return String2RChar(pMat->shared_name());
}

SEXP FileName( SEXP address )
{
  FileBackedBigMatrix *pMat = 
    reinterpret_cast<FileBackedBigMatrix*>(R_ExternalPtrAddr(address));
  return String2RChar(pMat->file_name());
}

SEXP GetBigSharedMatrixInfo( SEXP address )
{
  // We need to include:
  // 1. the shared memory identifier
  // 2. number of rows
  // 3. number of columns
  // 4. row names
  // 5. column names
  // 6. type length
  // 7. whether or not the matrix has separated columns

// TODO: FIX THIS
/*
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(address);
  int pc=0; // Protect Count
  SEXP ret = PROTECT(NEW_LIST(4));
  ++pc;
  SEXP listNames = PROTECT(allocVector(STRSXP, 4));
  ++pc;

  ColumnMutexInfos &cmi = pMat->column_mutex_infos(); 
  
 eSEXP columnKeys = PROTECT(NEW_INTEGER(cmi.size()));
  
  ++pc;
  unsigned int i;
  for (i=0; i < cmi.size(); ++i)
    INTEGER_DATA(columnKeys)[i] = cmi[i].data_key();
  SET_VECTOR_ELT(ret, 0, columnKeys);

  SEXP columnMutexKeys = PROTECT(NEW_INTEGER(cmi.size()));
  ++pc;
  for (i=0; i < cmi.size(); ++i)
    INTEGER_DATA(columnMutexKeys)[i] = cmi[i].mutex_key();
  SET_VECTOR_ELT(ret, 1, columnMutexKeys);
  
  SEXP sharedCountKey = PROTECT(NEW_INTEGER(1));
  ++pc;
  INTEGER_DATA(sharedCountKey)[0] = pMat->counter_data_key();
  SET_VECTOR_ELT(ret, 2, sharedCountKey);

  SEXP sharedCountMutexKey = PROTECT(NEW_INTEGER(1));
  ++pc;
  INTEGER_DATA(sharedCountMutexKey)[0] = pMat->counter_mutex_key();
  SET_VECTOR_ELT(ret, 3, sharedCountMutexKey);
  
  SET_STRING_ELT(listNames, 0, mkChar("colKeys"));
  SET_STRING_ELT(listNames, 1, mkChar("colMutexKeys"));
  SET_STRING_ELT(listNames, 2, mkChar("shCountKey"));
  SET_STRING_ELT(listNames, 3, mkChar("shCountMutexKey"));
  SET_NAMES(ret, listNames);  
  UNPROTECT(pc);
  return ret;
*/
  return R_NilValue;
}

void BigMatrixRLock( SEXP address, SEXP lockCols )
{
  SharedBigMatrix *pMat = 
    reinterpret_cast<SharedBigMatrix*>(R_ExternalPtrAddr(address));
  vector<unsigned long> indexes( GET_LENGTH(lockCols ) );
  unsigned int i;
  for (i=0; i < indexes.size(); ++i)
  {
    indexes[i] = static_cast<unsigned long>(NUMERIC_DATA(lockCols)[i])-1;
  }
  pMat->read_lock( indexes );
}

void BigMatrixRWLock( SEXP address, SEXP lockCols )
{
  SharedBigMatrix *pMat = 
    reinterpret_cast<SharedBigMatrix*>(R_ExternalPtrAddr(address));
  vector<unsigned long> indexes( GET_LENGTH(lockCols ) );
  unsigned long i;
  for (i=0; i < indexes.size(); ++i)
  {
    indexes[i] = static_cast<unsigned long>(NUMERIC_DATA(lockCols)[i])-1;
  }
  pMat->read_write_lock( indexes );
}   

void BigMatrixRelease( SEXP address, SEXP lockCols )
{
  SharedBigMatrix *pMat = 
    reinterpret_cast<SharedBigMatrix*>(R_ExternalPtrAddr(address));
  vector<unsigned long> indexes( GET_LENGTH(lockCols ) );
  unsigned long i;
  for (i=0; i < indexes.size(); ++i)
  {
    indexes[i] = static_cast<unsigned long>(NUMERIC_DATA(lockCols)[i])-1;
  }
  pMat->unlock( indexes );
}

// TODO: Ckmeans2 is probably failing because I am only making
// a matrix accessor for pMat.  This needs to be done for ALL
// big matrix objects being used.
SEXP Ckmeans2main(SEXP matType, 
                  SEXP bigMatrixAddr, SEXP centAddr, SEXP ssAddr,
                  SEXP clustAddr, SEXP clustsizesAddr,
                  SEXP nn, SEXP kk, SEXP mm, SEXP mmaxiters)
{
  SEXP ret = PROTECT(NEW_NUMERIC(1));
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatrixAddr);
  int iter = 0;
  if (pMat->separated_columns())
  {
    switch (INTEGER_VALUE(matType)) 
    {
      case 1:
        iter = Ckmeans2<char, SepBigMatrixAccessor<char> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
      case 2:
        iter = Ckmeans2<short, SepBigMatrixAccessor<short> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
      case 4:
        iter = Ckmeans2<int, SepBigMatrixAccessor<int> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
      case 8:
        iter = Ckmeans2<double, SepBigMatrixAccessor<double> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
    }
  }
  else
  {
    switch (INTEGER_VALUE(matType)) 
    {
      case 1:
        iter = Ckmeans2<char, BigMatrixAccessor<char> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
      case 2:
        iter = Ckmeans2<short, BigMatrixAccessor<short> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
      case 4:
        iter = Ckmeans2<int, BigMatrixAccessor<int> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
      case 8:
        iter = Ckmeans2<double, BigMatrixAccessor<double> >(
          pMat, centAddr, ssAddr, clustAddr, clustsizesAddr,
          nn, kk, mm, mmaxiters);
        break;
    }
  }
  NUMERIC_DATA(ret)[0] = (double)iter;
  UNPROTECT(1);
  return ret;
}

} // extern "C"

