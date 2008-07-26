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

//#include <malloc.h>
#include <stdio.h>

#include <string>
#include <fstream>
#include <sstream>

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include "ostypedef.h"

#ifndef WIN
#include <stdlib.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#endif //WIN

#include "BigMatrix.h"
#include "util.h"

// structs to handle calling mwhich on R matrices
template <typename T>
struct MatrixWrapper
{
  MatrixWrapper( T *pNewMat, long newNRow) : pMat(pNewMat), nrow(newNRow) {};
  T* operator[](const long col) {return pMat+nrow*col;};
  T *pMat;
  long nrow;
};

template<typename T>
string ttos(T i)
{
  stringstream s;
  s << i;
  return s.str();
}

template<typename CType, typename RType>
void SetMatrixElements( BigMatrix *pMat, SEXP col, SEXP row, SEXP values,
  double NA_C, double C_MIN, double C_MAX, double NA_R)
{
  CType** mat = reinterpret_cast<CType**>(pMat->matrix());
  double *pCol = NUMERIC_DATA(col);
  long nCol = GET_LENGTH(col);
  double *pRow = NUMERIC_DATA(row);
  long nRow = GET_LENGTH(row);
  bool outOfRange=false;
  VecPtr<RType> vec_ptr;
  RType *pVals = vec_ptr(values);
  long i,j,k, lcol, lrow;
  k=0;
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

template<typename CType, typename RType>
SEXP GetMatrixElements( BigMatrix *pMat, double NA_C, double NA_R, 
  SEXP col, SEXP row)
{
  NewVec<RType> new_vec;
  VecPtr<RType> vec_ptr; 
  CType** mat = reinterpret_cast<CType**>(pMat->matrix());
  double *pCols = NUMERIC_DATA(col);
  double *pRows = NUMERIC_DATA(row);
  long numCols = GET_LENGTH(col);
  long numRows = GET_LENGTH(row);
  SEXP ret = PROTECT( new_vec(numCols*numRows) );
  RType *pRet = vec_ptr(ret);
  long i,j;
  long k=0;
  for (i=0; i < numCols; ++i) {
    for (j=0; j < numRows; ++j) {
      if (pCols[i] == NA_REAL || pRows[j] == NA_REAL)
      {
        pRet[k] = static_cast<RType>(NA_R);
      }
      else
      {
        long col = static_cast<long>(pCols[i]) - 1;
        long row = static_cast<long>(pRows[j]) - 1;
        pRet[k] =
          (mat[col][row]) == static_cast<CType>(NA_C) ? 
            static_cast<RType>(NA_R) : 
            static_cast<RType>(mat[col][row]);
      }
      ++k;
    }
  }
  UNPROTECT(1);
  return(ret);
}

template<typename T>
SEXP ReadMatrix(SEXP fileName, BigMatrix *pMat,
	            SEXP firstLine, SEXP numLines, SEXP numCols, SEXP separator,
              SEXP useRowNames, double C_NA, double posInf, double negInf,
              double notANumber)
{
  T** mat = reinterpret_cast<T**>(pMat->matrix());
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
      if (useRowNames == NULL_USER_OBJECT ||
        LOGICAL_VALUE(useRowNames) == FALSE && 0 != j)
      {
        if (element == "NA")
        {
          mat[j][i] = static_cast<T>(C_NA);
        }
        else if (element == "inf")
        {
          mat[j][i] = static_cast<T>(posInf);
        }
        else if (element == "-inf")
        {
          mat[j][i] = static_cast<T>(negInf);
        }
        else if (element == "NaN")
        {
          mat[j][i] = static_cast<T>(notANumber);
        }
        else
        {
          mat[j][i] = static_cast<T>(atof(element.c_str()));
        }
      }
      else
      {
        Names &rn = pMat->row_names();
        if (!rowSizeReserved)
        {
          rowSizeReserved = true;
          rn.reserve(nl);
        }
        if (element != "")
        {
          rn.push_back(element.substr(1, element.size()-2));
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

template<typename T>
void WriteMatrix( BigMatrix *pMat, SEXP fileName, SEXP rowNames,
  SEXP colNames, SEXP sep, double C_NA )
{
  T** mat = reinterpret_cast<T**>(pMat->matrix());
  FILE *FP = fopen(STRING_VALUE(fileName), "w");
  long i,j;
  string  s;
  string sepString = string(CHAR(STRING_ELT(sep, 0)));

  Names &cn = pMat->column_names();
  if (LOGICAL_VALUE(colNames) == Rboolean(TRUE) && !cn.empty())
  {
    s += "\"" + sepString + "\"";
    for (i=0; i < (int) cn.size(); ++i)
      s += "\"" + cn[i] + "\"" + (((int)cn.size()-1 == i) ? "\n" : sepString);
  }
  fprintf(FP, s.c_str());
  s.clear();
  Names &rn = pMat->row_names();
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

template<typename T>
SEXP MatrixHashRanges( BigMatrix *pMat, SEXP selectColumn )
{
  T** mat = reinterpret_cast<T**>(pMat->matrix());
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

template<typename T>
SEXP ColCountNA( BigMatrix *pMat, SEXP column )
{
  T** mat = reinterpret_cast<T**>(pMat->matrix());
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

template<typename T1>
int Ckmeans2(SEXP bigMatrixAddr, SEXP centAddr, SEXP ssAddr,
              SEXP clustAddr, SEXP clustsizesAddr, 
              SEXP nn, SEXP kk, SEXP mm, SEXP mmaxiters)
{
  // BIG x m
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatrixAddr);
  T1 **x = (T1**) pMat->matrix();

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

/*------------------------------------------------------------------------------

  We envision many rows and relatively few columns (many cases and relatively
  few variables).  Consequently, our structure is a vector of pointers to vectors
  of data; this makes it easy to add or delete variables, obviously.
  In addition, all indices should come from R starting with 1, and need to
  be converted into C indices (starting at 0) within C.  In R, row i and column j
  would be x[i,j] but in C would be x[j-1,i-1].

------------------------------------------------------------------------------*/


SEXP HasRowColNames(SEXP address)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(address);
  SEXP ret = PROTECT(NEW_LOGICAL(2));
  LOGICAL_DATA(ret)[0] = 
    pMat->row_names().empty() ? Rboolean(0) : Rboolean(1);
  LOGICAL_DATA(ret)[1] = 
    pMat->row_names().empty() ? Rboolean(0) : Rboolean(1);
  UNPROTECT(1);
  return ret;
}

SEXP GetIndexRowNames(SEXP address, SEXP indices)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(address);
  Names &rn = pMat->row_names();
  if (rn.empty())
    return NULL_USER_OBJECT;
  SEXP ret = PROTECT(allocVector(STRSXP, GET_LENGTH(indices)));
  long i;
  for (i=0; i < GET_LENGTH(indices); ++i)
    SET_STRING_ELT(ret,i,mkChar(rn[(long)NUMERIC_DATA(indices)[i]-1].c_str()));
  UNPROTECT(1);
  return ret;
}

SEXP GetIndexColNames(SEXP address, SEXP indices)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(address);
  Names &cn = pMat->column_names();
  if (cn.empty())
    return NULL_USER_OBJECT;
  SEXP ret = PROTECT(allocVector(STRSXP, GET_LENGTH(indices)));
  long i;
  for (i=0; i < GET_LENGTH(indices); ++i)
  {
    SET_STRING_ELT(ret,i,mkChar(cn[(long)NUMERIC_DATA(indices)[i]-1].c_str()));
  }
  UNPROTECT(1);
  return ret;
}

SEXP GetColumnNamesBM(SEXP address)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(address);
  Names &colNames = pMat->column_names();
  if (colNames.empty())
    return NULL_USER_OBJECT;
  SEXP ret = PROTECT(allocVector(STRSXP, colNames.size()));
  long i;
  for (i=0; i < (int)colNames.size(); ++i)
    SET_STRING_ELT(ret, i, mkChar(colNames[i].c_str()));
  UNPROTECT(1);
  return ret;
}

SEXP GetRowNamesBM(SEXP address)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(address);
  Names &rowNames = pMat->row_names();
  if (rowNames.empty())
    return NULL_USER_OBJECT;
  SEXP ret;
  ret = PROTECT(allocVector(STRSXP, rowNames.size()));
  long i;
  for (i=0; i < (int)rowNames.size(); ++i)
    SET_STRING_ELT(ret, i, mkChar(rowNames[i].c_str()));
  UNPROTECT(1);
  return ret;
}

void SetColumnNames(SEXP address, SEXP columnNames)
{
  BigMatrix *pMat = (BigMatrix*) R_ExternalPtrAddr(address);
  Names &cn = pMat->column_names();
  cn.clear();
  long i;
  for (i=0; i < GET_LENGTH(columnNames); ++i)
    cn.push_back(string(CHAR(STRING_ELT(columnNames, i))));
}

void SetRowNames(SEXP address, SEXP rowNames)
{
  BigMatrix *pMat = (BigMatrix*) R_ExternalPtrAddr(address);
  Names &rn = pMat->row_names();
  rn.clear();
  long i;
  for (i=0; i < GET_LENGTH(rowNames); ++i)
    rn.push_back(string(CHAR(STRING_ELT(rowNames, i))));
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

void CDestroyMatrix(SEXP bigMatrixAddr)
{
  delete (BigMatrix*)R_ExternalPtrAddr(bigMatrixAddr);
  R_ClearExternalPtr(bigMatrixAddr);
}

SEXP CCreateMatrix(SEXP row, SEXP col, SEXP ini, SEXP type) 
{
  BigMatrix *pMat = new BigMatrix;
  if ( pMat->init( (long)NUMERIC_VALUE(col), (long)NUMERIC_VALUE(row), 
    INTEGER_VALUE(type), false, NUMERIC_VALUE(ini)) )
  {
    SEXP address = R_MakeExternalPtr(pMat, R_NilValue, R_NilValue);
    R_RegisterCFinalizerEx(address, (R_CFinalizer_t) CDestroyMatrix, 
		  (Rboolean) TRUE);
    return(address);
  }
  else
  {
    fprintf(stderr, "Memory for big.matrix could no be allocated.\n");
    delete pMat;
    return R_NilValue;
  }
}

void CAddMatrixCol(SEXP bigMatAddr, SEXP init)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatAddr);
  pMat->insert_column(pMat->ncol(), NUMERIC_VALUE(init), "");
}

void CEraseMatrixCol(SEXP bigMatAddr, SEXP eraseColumn)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatAddr);
  pMat->remove_column( (long)NUMERIC_VALUE(eraseColumn)-1 );
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
  switch (pMat->matrix_type())
  {
    case 1:
    {
      char** mat = reinterpret_cast<char**>(pMat->matrix());
      return MWhichMatrix<char, char**>(mat, pMat->nrow(), selectColumn, 
        minVal, maxVal, chkMin, chkMax, opVal, NA_CHAR);
    }
    case 2:
    {
      short** mat = reinterpret_cast<short**>(pMat->matrix());
      return MWhichMatrix<short, short**>(mat, pMat->nrow(), selectColumn, 
        minVal, maxVal, chkMin, chkMax, opVal, NA_SHORT);
    }
    case 4:
    {
      int** mat = reinterpret_cast<int**>(pMat->matrix());
      return MWhichMatrix<int, int**>(mat, pMat->nrow(), selectColumn, minVal, 
        maxVal, chkMin, chkMax, opVal, NA_INTEGER);
    }
    case 8:
    {
      double** mat = reinterpret_cast<double**>(pMat->matrix());
      return MWhichMatrix<double, double **>(mat, pMat->nrow(), selectColumn, 
        minVal, maxVal, chkMin, chkMax, opVal, NA_REAL);
    }
  }
  return R_NilValue;
}

SEXP MWhichRIntMatrix( SEXP matrixVector, SEXP nrow, SEXP selectColumn,
  SEXP minVal, SEXP maxVal, SEXP chkMin, SEXP chkMax, SEXP opVal )
{
  long numRows = static_cast<long>(INTEGER_VALUE(nrow));
  MatrixWrapper<int> mat(INTEGER_DATA(matrixVector), numRows);
  return MWhichMatrix<int, MatrixWrapper<int> >(mat, numRows, 
    selectColumn, minVal, maxVal, chkMin, chkMax, opVal, NA_INTEGER);
}

SEXP MWhichRNumericMatrix( SEXP matrixVector, SEXP nrow, SEXP selectColumn,
  SEXP minVal, SEXP maxVal, SEXP chkMin, SEXP chkMax, SEXP opVal )
{
  long numRows = static_cast<long>(INTEGER_VALUE(nrow));
  MatrixWrapper<double> mat(NUMERIC_DATA(matrixVector), numRows);
  return MWhichMatrix<double, MatrixWrapper<double> >(mat, numRows,
    selectColumn, minVal, maxVal, chkMin, chkMax, opVal, NA_REAL);
}

SEXP MatrixHashRanges( SEXP bigMatAddr, SEXP selectColumn )
{
  BigMatrix *pMat = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  switch (pMat->matrix_type())
  {
    case 1:
      return MatrixHashRanges<char>(pMat, selectColumn);
    case 2:
      return MatrixHashRanges<short>(pMat, selectColumn);
    case 4:
      return MatrixHashRanges<int>(pMat, selectColumn);
    case 8:
      return MatrixHashRanges<double>(pMat, selectColumn);
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
  switch (pMat->matrix_type())
  {
    case 1:
      return ColCountNA<char>(pMat, column);
    case 2:
      return ColCountNA<short>(pMat, column);
    case 4:
      return ColCountNA<short>(pMat, column);
    case 8:
      return ColCountNA<char>(pMat, column);
  }
  return R_NilValue;
}

SEXP ReadMatrix(SEXP fileName, SEXP bigMatAddr,
	            SEXP firstLine, SEXP numLines, SEXP numCols, SEXP separator,
              SEXP useRowNames)
{
  BigMatrix *pMat = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  switch (pMat->matrix_type())
  {
    case 1:
      return ReadMatrix<char>(fileName, pMat, firstLine, numLines, numCols, 
        separator, useRowNames, NA_CHAR, NA_CHAR, NA_CHAR, NA_CHAR);
    case 2:
      return ReadMatrix<short>(fileName, pMat, firstLine, numLines, numCols, 
        separator, useRowNames, NA_SHORT, NA_SHORT, NA_SHORT, NA_SHORT);
    case 4:
      return ReadMatrix<int>(fileName, pMat, firstLine, numLines, numCols, 
        separator, useRowNames, NA_INTEGER, NA_INTEGER, NA_INTEGER, 
        NA_INTEGER);
    case 8:
      return ReadMatrix<double>(fileName, pMat, firstLine, numLines, numCols, 
        separator, useRowNames, NA_REAL, R_PosInf, R_NegInf, R_NaN);
  }
  return R_NilValue;
}


void WriteMatrix( SEXP bigMatAddr, SEXP fileName, SEXP rowNames,
  SEXP colNames, SEXP sep )
{
  BigMatrix *pMat = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  switch (pMat->matrix_type())
  {
    case 1:
      WriteMatrix<char>(pMat, fileName, rowNames, colNames, sep, NA_CHAR);
      break;
    case 2:
      WriteMatrix<short>(pMat, fileName, rowNames, colNames, sep, NA_SHORT);
      break;
    case 4:
      WriteMatrix<int>(pMat, fileName, rowNames, colNames, sep, NA_INTEGER);
      break;
    case 8:
      WriteMatrix<double>(pMat, fileName, rowNames, colNames, sep, NA_REAL);
  }
}

SEXP GetMatrixElements(SEXP bigMatAddr, SEXP col, SEXP row)
{
  BigMatrix *pMat = 
    reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  switch(pMat->matrix_type())
  {
    case 1:
      return GetMatrixElements<char,int>(pMat, NA_CHAR, NA_INTEGER, col, row);
    case 2:
      return GetMatrixElements<short,int>(pMat, NA_SHORT, NA_INTEGER, col, row);
    case 4:
      return GetMatrixElements<int,int>(pMat, NA_INTEGER, NA_INTEGER, col, row);
    case 8:
      return GetMatrixElements<double,double>(pMat, NA_REAL, NA_REAL, col, row);
  }
  return R_NilValue;
}

void SetMatrixElements(SEXP bigMatAddr, SEXP col, SEXP row, SEXP values)
{
  BigMatrix *pMat = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  switch (pMat->matrix_type())
  {
    case 1:
      SetMatrixElements<char,int>( pMat, col, row, values, 
        NA_CHAR, R_CHAR_MIN, R_CHAR_MAX, NA_INTEGER);
      break;
    case 2:
      SetMatrixElements<short,int>( pMat, col, row, values, 
        NA_SHORT, R_SHORT_MIN, R_SHORT_MAX, NA_INTEGER);
      break;
    case 4:
      SetMatrixElements<int,int>( pMat, col, row, values, 
        NA_INTEGER, R_INT_MIN, R_INT_MAX, NA_INTEGER);
      break;
    case 8:
      SetMatrixElements<double,double>( pMat, col, row, values, 
        NA_REAL, R_DOUBLE_MIN, R_DOUBLE_MAX, NA_REAL);
  }
}

/////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////
/// BEGIN CbigmemoryShared !!!!!!!!!!!!!!

#ifndef WIN

//-------------------------------------------------------------------------
// The functions, many of which are type-specific and need to be replicated
// by the proprocessBM.txt script in the main package directory.


void CDestroySharedMatrix(SEXP bigMatrixAddr) 
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatrixAddr);
  delete pMat;
  R_ClearExternalPtr(bigMatrixAddr);
}

SEXP CCreateSharedMatrix(SEXP row, SEXP col, SEXP colnames, SEXP rownames,
  SEXP typeLength, SEXP ini) 
{
  typedef vector<long> Keys;
  
  BigMatrix *pMat = new BigMatrix;
  if ( pMat->init((long)NUMERIC_VALUE(col), (long)NUMERIC_VALUE(row), 
    INTEGER_VALUE(typeLength), true, NUMERIC_VALUE(ini)) )
  {
    pMat->SetColumnNames(colnames);
    pMat->SetRowNames(rownames);
    SEXP address = R_MakeExternalPtr(pMat, R_NilValue, R_NilValue);
    R_RegisterCFinalizerEx(address, (R_CFinalizer_t) CDestroySharedMatrix, 
		  (Rboolean) TRUE);
    return(address);
  }
  else
  {
    fprintf(stderr, "Memory for big.matrix could no be allocated.\n");
    fprintf(stderr, "Hint: You may need to change the settings in /etc/sysctl.conf.\n");
    delete pMat;
    return R_NilValue;
  }
}

SEXP CAttachSharedMatrix(SEXP col, SEXP colNames, SEXP row, SEXP rowNames, 
  SEXP typeLength, SEXP colKeys, SEXP colMutexKeys, SEXP shCountKey, 
  SEXP shCountMutexKey)
{
  BigMatrix *pMat = new BigMatrix;
  bool connected = 
    pMat->connect((long)NUMERIC_VALUE(col), (long)NUMERIC_VALUE(row), 
      INTEGER_VALUE(typeLength), colKeys, colMutexKeys, shCountKey, 
      shCountMutexKey);
  if (!connected)
    return R_NilValue;
  pMat->SetColumnNames(colNames);
  pMat->SetRowNames(rowNames);

  SEXP address;
  address = R_MakeExternalPtr(pMat, R_NilValue, R_NilValue);
  R_RegisterCFinalizerEx(address, (R_CFinalizer_t) CDestroySharedMatrix, 
    (Rboolean) TRUE);
  return(address);
}

SEXP GetBigSharedMatrixInfo( SEXP address )
{
  // We need to include:
  // 1. column keys
  // 2. column mutex keys
  // 3. shared count key
  // 4. shared count mutex key
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(address);
  int pc=0; // Protect Count
  SEXP ret = PROTECT(NEW_LIST(4));
  ++pc;
  SEXP listNames = PROTECT(allocVector(STRSXP, 4));
  ++pc;

  ColumnMutexInfos &cmi = pMat->column_mutex_infos(); 
    //*(pMat->pColumnMutexInfos);
  SEXP columnKeys = PROTECT(NEW_INTEGER(cmi.size()));
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
}

void BigMatrixRLock( SEXP address, SEXP lockCols )
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(address);
  double *pLockCols = NUMERIC_DATA(lockCols);
  long i;

  ColumnMutexInfos &cmi = pMat->column_mutex_infos();
  for (i=0; i < GET_LENGTH(lockCols); ++i)
    cmi[(long)pLockCols[i]-1].rlock();
}

void BigMatrixRWLock( SEXP address, SEXP lockCols )
{     
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(address);
  double *pLockCols = NUMERIC_DATA(lockCols);
  long i;
      
  ColumnMutexInfos &cmi = pMat->column_mutex_infos();
  for (i=0; i < GET_LENGTH(lockCols); ++i)
  {
    cmi[(long)pLockCols[i]-1].rwlock();
  }
}   

void BigMatrixRelease( SEXP address, SEXP lockCols )
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(address);
  double *pLockCols = NUMERIC_DATA(lockCols);
  long i;

  ColumnMutexInfos &cmi = pMat->column_mutex_infos();
  for (i=0; i < GET_LENGTH(lockCols); ++i)
    cmi[(long)pLockCols[i]-1].unlock();
}

#endif //WIN

SEXP Ckmeans2main(SEXP matType, 
                  SEXP bigMatrixAddr, SEXP centAddr, SEXP ssAddr,
                  SEXP clustAddr, SEXP clustsizesAddr,
                  SEXP nn, SEXP kk, SEXP mm, SEXP mmaxiters)
{
  SEXP ret = PROTECT(NEW_NUMERIC(1));
  int iter = 0;
  switch (INTEGER_VALUE(matType)) {
    case 1:
      iter = Ckmeans2<char>(bigMatrixAddr, centAddr, ssAddr, clustAddr, clustsizesAddr,
                     nn, kk, mm, mmaxiters);
      break;
    case 2:
      iter = Ckmeans2<short>(bigMatrixAddr, centAddr, ssAddr, clustAddr, clustsizesAddr,
                     nn, kk, mm, mmaxiters);
      break;
    case 4:
      iter = Ckmeans2<int>(bigMatrixAddr, centAddr, ssAddr, clustAddr, clustsizesAddr,
                     nn, kk, mm, mmaxiters);
      break;
    case 8:
      iter = Ckmeans2<double>(bigMatrixAddr, centAddr, ssAddr, clustAddr, clustsizesAddr,
                     nn, kk, mm, mmaxiters);
      break;
  }
  NUMERIC_DATA(ret)[0] = (double)iter;
  UNPROTECT(1);
  return ret;
}

} // extern "C"

