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

#ifndef BIGMATRIX_H
#define BIGMATRIX_H

#include <Rmath.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <string>
#include <vector>
using namespace std;

#include "ostypedef.h"

typedef vector<string> Names;
typedef vector<int> Keys;
//typedef enum {UNKNOWN=0; CHAR=1; SHORT=2; INT=4; DOUBLE=8} BigMatType;

#ifndef WIN
#include "BMMutex.h"
typedef vector<MutexSharedMemory> ColumnMutexInfos;
#endif

class BigMatrix
{
  // Constructor and Destructor
  public:
    BigMatrix();
    ~BigMatrix();

  // Initialization
    bool init( long numCol, long numRow, int newMatrixType, bool isShared,
      double init);
    bool connect(long numCol, long numRow, int connectMatrixType,
      SEXP colKeys, SEXP colMutexKeys, SEXP shCountKey, SEXP shCountMutexKey );
 
    // Accessors
    long ncol() const {return _ncol;};
    long nrow() const {return _nrow;};
    int matrix_type() const {return _matType;};
    bool shared() const {return _shared;}; 
#ifndef WIN
    int counter_data_key() const {return _counterInfo.data_key();};
    int counter_mutex_key() const {return _counterInfo.mutex_key();};
    ColumnMutexInfos& column_mutex_infos() {return *_pColumnMutexInfos;};
#endif //WIN
 
    void*& matrix() {return _matrix;};
    Names& column_names() {return *_pColNames;};
    Names& row_names() {return *_pRowNames;};

    // Mutators
    bool remove_column(long col);
    bool insert_column(long pos, double init, string name);
    bool SetColumnNames( SEXP newColNames );
    bool SetRowNames( SEXP newRowNames );

  // Shared Memory 
#ifndef WIN
    bool rlock(long col) {return (*_pColumnMutexInfos)[col].rlock();};
    bool rwlock(long col) {return (*_pColumnMutexInfos)[col].rwlock();};
    bool unlock(long col) {return (*_pColumnMutexInfos)[col].unlock();};
#endif //WIN


  // Data Members
  private:
    long _ncol;
    long _nrow;
    int _matType;
    void* _matrix;
    bool _shared;
    Names* _pColNames;
    Names* _pRowNames;
  #ifndef WIN
    MutexSharedMemory _counterInfo;
    int* _pShCounter;
    ColumnMutexInfos* _pColumnMutexInfos;
  #endif //WIN
};

#endif // BIGMATRIX_H
