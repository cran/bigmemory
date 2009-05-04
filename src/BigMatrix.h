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

// proposed new names, InMemorySharedBigMatrix, FileBackedSharedBigMatrix
#ifndef BIGMATRIX_H
#define BIGMATRIX_H

#include <boost/interprocess/mapped_region.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/noncopyable.hpp>
#include <string>
#include <vector>

#include "BigMemoryMutex.h"
#include "SharedCounter.h"

using namespace std;

typedef vector<std::string> Names;
typedef boost::interprocess::mapped_region MappedRegion;
typedef boost::shared_ptr<MappedRegion> MappedRegionPtr;
typedef vector<MappedRegionPtr> MappedRegionPtrs;
typedef boost::shared_ptr<BigMemoryMutex> MutexPtr;
typedef vector<MutexPtr> MutexPtrs;
typedef vector<unsigned long> Columns;
//typedef enum {UNKNOWN=0; CHAR=1; SHORT=2; INT=4; DOUBLE=8} BigMatType;

class BigMatrix : public boost::noncopyable
{
  // Constructor and Destructor
  public:
    BigMatrix():_ncol(0),_nrow(0),_matType(0),_matrix(NULL),_sepCols(false){};
    virtual ~BigMatrix(){};

    // The next function returns the matrix data.  It will generally be passed
    // to an appropriate templated function. 
    void* matrix() {return _matrix;};
    
    // Accessors
    long ncol() const {return _ncol;};
    long nrow() const {return _nrow;};
    int matrix_type() const {return _matType;};
    bool shared() const {return _shared;}; 
    bool separated_columns() const {return _sepCols;};
    Names column_names() {return _colNames;};
    Names row_names() {return _rowNames;};

    // Mutators
    bool column_names( Names newColNames )
    {
      _colNames=newColNames;
      return true;
    };
    bool row_names( Names newRowNames )
    {
      _rowNames=newRowNames;
      return true;
    };
		
		void* data_ptr() {return _matrix;};

  // Data Members


  protected:
    long _ncol;
    long _nrow;
    int _matType;
    void* _matrix;
    bool _shared;
    bool _sepCols;
    Names _colNames;
    Names _rowNames;
};
 
class LocalBigMatrix : public BigMatrix
{
  public:
    LocalBigMatrix() : BigMatrix(){_shared=false;};
    virtual ~LocalBigMatrix(){destroy();};
    bool create(const long numRow, const long numCol, const int matrixType, 
      const bool striped);
  protected:
    void destroy();
// TODO:  IMPLEMENT THESE FUNCTIONS WHEN _sepCols IS true.
//    bool remove_column(long col);
//    bool insert_column(long pos, double init, string name);
};

class SharedBigMatrix : public BigMatrix
{
  public:
//    SharedBigMatrix() : BigMatrix(), _pUsageCount(NULL){_shared=true;};
    SharedBigMatrix() : BigMatrix() {_shared=true;};
    virtual ~SharedBigMatrix() {};
    std::string uuid() const {return _uuid;};
    std::string shared_name() const {return _sharedName;};

    bool read_lock( Columns &cols );
    bool read_write_lock( Columns &cols );
    bool unlock( Columns &cols );

  protected:
    virtual bool destroy()=0;
  protected:
    // According to the documentation, shared memory has kernel or 
    // filesystem presistence (mechanism exists until the system reboots
    // or is deleted (kernel) or until the mechanism is explicitly deleted
    // (filesystem)).  As a result, we are going to need a usage counter
    // so that when the last object is done with the shared resource, it
    // can delete the resource.  The destructor will handle deletion
    // of the shared usage counter.
    bool create_uuid();
    bool uuid(const std::string &uuid) {_uuid=uuid; return true;};
  protected:
    std::string _uuid;
    std::string _sharedName;
    MappedRegionPtrs _dataRegionPtrs;
    MutexPtrs _mutexPtrs;
    BigMemoryMutex _mutexLock;
    SharedCounter _sharedCounter;
};

class SharedMemoryBigMatrix : public SharedBigMatrix
{
  public:
    SharedMemoryBigMatrix():SharedBigMatrix(){};
    virtual ~SharedMemoryBigMatrix(){destroy();};
    virtual bool create( const long numRow, const long numCol, 
      const int matrixType, const bool sepCols);
    virtual bool connect( const std::string &uuid, const long numRow, 
      const long numCol, const int matrixType, const bool sepCols);
  protected:
    virtual bool destroy();
}; 

class FileBackedBigMatrix : public SharedBigMatrix
{
  // _sharedName is filename_uuid
  public:
    FileBackedBigMatrix():SharedBigMatrix(){};
    virtual ~FileBackedBigMatrix(){destroy();};
    virtual bool create( const std::string &fileName, 
      const std::string &filePath,const long numRow, 
      const long numCol, const int matrixType, const bool sepCols, 
      const bool preserve);
    virtual bool connect( const std::string &sharedName, 
      const std::string &fileName, const std::string &filePath,
      const long numRow, const long numCol, const int matrixType, 
      const bool sepCols, const bool preserve);
    std::string file_name() const {return _fileName;};
  protected:
    virtual bool destroy();

  protected:
    bool _preserve;
    std::string _fileName;
};

#endif // BIGMATRIX_H
