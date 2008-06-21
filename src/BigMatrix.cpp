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

#include "BigMatrix.h"

template<typename T>
void* CreateBigMatrix(long numRows, long numCols, T init)
{
  T** pMat = new T*[numCols];
  int i,j;
  for (i=0; i < numCols; ++i)
  {
    pMat[i] = new T[numRows];
    for (j=0; j < numRows; ++j)
    {
      pMat[i][j] = init;
    }
  }
  return (void*)pMat;
}

template<typename T>
void DestroyBigMatrix(void *pMatrix, long numCols)
{
  T** pMat = reinterpret_cast<T**>(pMatrix);
  int i;
  for (i=0; i < numCols; ++i)
  {
    delete [] pMat[i];
  }
  delete [] pMat;
}

#ifndef WIN
template<typename T>
void* CreateSharedBigMatrix( ColumnMutexInfos &cmi, long numRows, T init )
{
  T** pMat = new T*[cmi.size()];
  unsigned long i;
  int j;
  for (i=0; i < cmi.size(); ++i)
  {
    pMat[i] = (T*)cmi[i].data();
    for (j=0; j < numRows; ++j)
    {
      pMat[i][j] = init;
    }
  }
  return (void*)pMat;
}

template<typename T>
void* InitSharedMatrix(ColumnMutexInfos &cmi)
{
  T** pMat = new T*[cmi.size()];
  unsigned long i;
  for (i=0; i < cmi.size(); ++i)
  {
    pMat[i] = (T*)cmi[i].data();
  }
  return (void*)pMat;
}

#endif //WIN

BigMatrix::BigMatrix()
{
  _ncol=0;
  _nrow=0;
  _matType = 0;
  _pColNames = new Names;
  _pRowNames = new Names;
  _shared = false;
#ifndef WIN
  _pShCounter = NULL;
  _pColumnMutexInfos = NULL;
#endif //WIN
}

// ncol has to be equal to the number of new column names.
bool BigMatrix::SetColumnNames( SEXP newColNames )
{
  if (GET_LENGTH(newColNames)==_ncol)
  {
    Names &cn = *_pColNames;
    cn.clear();
    int i;
    for (i=0; i < _ncol; ++i)
      cn.push_back(string(CHAR(STRING_ELT(newColNames, i))));
    return true;
  }
  if (GET_LENGTH(newColNames)==0)
  {
    _pColNames->clear();
    return true;
  }
  return false;
}
bool BigMatrix::SetRowNames( SEXP newRowNames )
{
  if (GET_LENGTH(newRowNames)==_nrow)
  {
    Names &rn = *_pRowNames;
    rn.clear();
    int i;
    for (i=0; i < _nrow; ++i)
      rn.push_back(string(CHAR(STRING_ELT(newRowNames, i))));
    return true;
  }
  if (GET_LENGTH(newRowNames)==0)
  {
    _pRowNames->clear();
    return true;
  }
  return false;
}

#ifndef WIN
bool BigMatrix::connect( long numCol, long numRow, int connectMatrixType,
  SEXP colKeys, SEXP colMutexKeys, SEXP shCountKey, SEXP shCountMutexKey)
{
  _ncol = numCol;
  _nrow = numRow;
  _shared = true;
  _matType = connectMatrixType;
  if (!_counterInfo.connect(INTEGER_VALUE(shCountMutexKey), 
    INTEGER_VALUE(shCountKey)))
  {
    return false;
  }
  _pShCounter = (int*)_counterInfo.data();
  ++(*_pShCounter);

  _pColumnMutexInfos = new ColumnMutexInfos(_ncol);
  ColumnMutexInfos &cmi = *_pColumnMutexInfos;
  int i;
  for (i=0; i < _ncol; ++i)
  {
    if (!cmi[i].connect( INTEGER_DATA(colMutexKeys)[i], 
      INTEGER_DATA(colKeys)[i] ) )
    {
      return false;
    }
  }

  switch(_matType)
  {
    case 1:
      _matrix = InitSharedMatrix<char>(cmi);
      break;
    case 2:
      _matrix = InitSharedMatrix<short>(cmi);
      break;
    case 4:
      _matrix = InitSharedMatrix<int>(cmi);
      break;
    case 8:
      _matrix = InitSharedMatrix<double>(cmi);
      break;
  }
  return true;
}
#endif //WIN

template<typename T>
void RemAndCopy(BigMatrix &bigMat, long remCol, long newNumCol)
{
  T** oldMat = reinterpret_cast<T**>(bigMat.matrix());
  T** newMat = new T*[newNumCol];
  delete [] (oldMat[remCol]);
  long i,j;
  for (i=0,j=0; i < newNumCol+1; ++i)
  {
    if (i != remCol)
      newMat[j++] = oldMat[i];
  }
  delete [] oldMat;
  bigMat.matrix() = reinterpret_cast<void*>(newMat);
}

bool BigMatrix::remove_column(long col)
{
  if (!_pColNames->empty())
    _pColNames->erase(_pColNames->begin()+col);
  --_ncol;
  switch (_matType)
  {
    case 1:
      RemAndCopy<char>(*this, col, _ncol);
      break;
    case 2:
      RemAndCopy<short>(*this, col, _ncol);
      break;
    case 4:
      RemAndCopy<int>(*this, col, _ncol);
      break;
    case 8:
      RemAndCopy<double>(*this, col, _ncol);
      break;
  }
  return true;
}

template<typename T>
void AddAndCopy(BigMatrix &bigMat, long pos, long newNumCol, long nrow, 
  double init)
{
  T** oldMat = reinterpret_cast<T**>(bigMat.matrix());
  T* addRow = new T[nrow];
  T** newMat = new T*[newNumCol];
  long i,j;

  for (i=0; i < nrow; ++i)
    addRow[i] = (T)init;

  bool added=false;
  for (i=0,j=0; i < newNumCol-1; ++i)
  {
    if (j==pos)
    {
      newMat[j++] = addRow;
      added=true;
    }
    newMat[j++] = oldMat[i];
  }
  if (!added)
    newMat[j] = addRow;
  delete [] reinterpret_cast<T**>(bigMat.matrix());
  bigMat.matrix() = reinterpret_cast<void*>(newMat);
}

bool BigMatrix::insert_column(long pos, double init, string name)
{
  if (!_pColNames->empty())
    _pColNames->insert( _pColNames->begin()+pos, name);
  ++_ncol;
  switch (_matType)
  {
    case 1:
      AddAndCopy<char>(*this, pos, _ncol, _nrow, init);
      break;
    case 2:
      AddAndCopy<short>(*this, pos, _ncol, _nrow, init);
      break;
    case 4:
      AddAndCopy<int>(*this, pos, _ncol, _nrow, init);
      break;
    case 8:
      AddAndCopy<double>(*this, pos, _ncol, _nrow, init);
      break;
  }
  return true;
}

bool BigMatrix::init( long numCol, long numRow, int newMatrixType, 
  bool isShared, double init )
{
  _ncol = numCol;
  _nrow = numRow;
  _shared = isShared;
  _matType = newMatrixType;
  if (!_shared)
  {
    switch (_matType)
    {
      case 0:
        return false;
      case 1:
        _matrix = CreateBigMatrix(_nrow, _ncol, (char)init);
        break;
      case 2:
        _matrix = CreateBigMatrix(_nrow, _ncol, (short)init);
        break;
      case 4:
        _matrix = CreateBigMatrix(_nrow, _ncol, (int)init);
        break;
      case 8:
        _matrix = CreateBigMatrix(_nrow, _ncol, init);
        break;
    }
  }
#ifndef WIN
  else
  {
    _counterInfo.create( sizeof(int) );
    _pShCounter = (int*)_counterInfo.data();
    *_pShCounter = 1;
    _pColumnMutexInfos = new ColumnMutexInfos(_ncol);
    ColumnMutexInfos &cmi = *_pColumnMutexInfos;
    cmi.resize(_ncol);
    unsigned long i;
    for (i=0; i < cmi.size(); ++i)
    {
      cmi[i].create( _matType*_nrow );
    }
    switch (_matType)
    {
      case 1:
        _matrix = CreateSharedBigMatrix(cmi, _nrow, char(init));
        break;
      case 2:
        _matrix = CreateSharedBigMatrix(cmi, _nrow, short(init));
        break;
      case 4:
        _matrix = CreateSharedBigMatrix(cmi, _nrow, (int)init);
        break;
      case 8:
        _matrix = CreateSharedBigMatrix(cmi, _nrow, init);
        break;
    }
  }
#endif //WIN
  return true;
}


BigMatrix::~BigMatrix()
{
  if (_pColNames)
  {
    delete _pColNames;
  }
  if (_pRowNames)
  {
    delete _pRowNames;
  }
  if (!_shared && _matrix)
  {
    switch (_matType)
    {
      case 1:
        DestroyBigMatrix<char>(_matrix, _ncol);
        break;
      case 2:
        DestroyBigMatrix<short>(_matrix, _ncol);
        break;
      case 4:
        DestroyBigMatrix<int>(_matrix, _ncol);
        break;
      case 8:
        DestroyBigMatrix<double>(_matrix, _ncol);
        break;
    }
  }
#ifndef WIN
  else if (_shared && _matrix)
  {
    _pShCounter = NULL;
    switch (_matType)
    {
      case 1:
        delete [] (char*)_matrix;
        break;
      case 2:
        delete [] (short*)_matrix;
        break;
      case 4:
        delete [] (int*)_matrix;
        break;
      case 8:
        delete [] (double*)_matrix;
        break;
    }
    if (_pColumnMutexInfos)
      delete _pColumnMutexInfos;
  }
#endif //WIN
}
