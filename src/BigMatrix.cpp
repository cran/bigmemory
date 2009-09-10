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

#include <string>
#include <sstream>
#include <fstream>
#include <fcntl.h> // to remove files

#include <boost/interprocess/shared_memory_object.hpp>
#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>
#include <boost/interprocess/sync/scoped_lock.hpp>
#include <boost/interprocess/sync/named_mutex.hpp>
#include <boost/exception/exception.hpp>
#include <boost/uuid.hpp>

#include "BigMatrix.h"

using namespace std;
using namespace boost;
using namespace boost::interprocess;

template<typename T>
std::string ttos(T i)
{
  stringstream s;
  s << i;
  return s.str();
}

template<typename T>
void* CreateLocalSepColMatrix(index_type nrow, index_type ncol)
{
  T** pMat = new T*[ncol];
  int i;
  for (i=0; i < ncol; ++i) 
  {
    pMat[i] = reinterpret_cast<T*>(new char[sizeof(T)*nrow]);
  }
  return reinterpret_cast<void*>(pMat);
}

template<typename T>
void* CreateSepColMatrix(index_type nrow, index_type ncol)
{
  return reinterpret_cast<void*>(new T[nrow*ncol]);
}

bool LocalBigMatrix::create( const index_type numRow, const index_type numCol, 
  const index_type numEbytes, const int matrixType, const bool sepCols)
{
  _nrow = numRow;
	_matrixRows = _nrow;
  _ncol = numCol;
	_matrixCols = _ncol;
  _matType = matrixType;
  _sepCols = sepCols;
  try
  {
    if (_sepCols)
    {
      switch(_matType)
      {
        case 1:
          _pdata = CreateLocalSepColMatrix<char>(_nrow, _ncol);
          break;
        case 2:
          _pdata = CreateLocalSepColMatrix<short>(_nrow, _ncol);
          break;
        case 4:
          _pdata = CreateLocalSepColMatrix<int>(_nrow, _ncol);
          break;
        case 8:
          _pdata = CreateLocalSepColMatrix<double>(_nrow, _ncol);
      }
    }
    else
    {
			_nebytes = numEbytes;
      switch(_matType)
      {
        case 1:
          _pdata = reinterpret_cast<void*>(
						new char[_nebytes+_nrow*_ncol]);
          break;
        case 2:
          _pdata = reinterpret_cast<void*>(
						new char[_nebytes+sizeof(short)*_nrow*_ncol]);
          break;
        case 4:
          _pdata = reinterpret_cast<void*>(
						new char[_nebytes+sizeof(int)*_nrow*_ncol]);
          break;
        case 8:
          _pdata = reinterpret_cast<void*>(
						new char[_nebytes+sizeof(double)*_nrow*_ncol]);
      }
    }
  }
  catch (std::bad_alloc &ex)
  {
    return false;
  }
  return true;
}

template<typename T>
void DestroyLocalSepColMatrix( T** matrix, const index_type ncol)
{
  index_type i;
  for (i=0; i < ncol; ++i)
  {
    delete [] matrix[i];
  }
  delete matrix;

}

template<typename T>
void DestroyLocalMatrix( T* matrix )
{
  delete [] matrix;
}

void LocalBigMatrix::destroy()
{
  if (_pdata && _matrixCols && _matrixRows)
  {
    if (_sepCols)
    {
      DestroyLocalSepColMatrix(reinterpret_cast<char**>(_pdata), _matrixCols);
    }
    else
    {
      DestroyLocalMatrix(reinterpret_cast<char*>(_pdata));
    }
    _pdata=NULL;
    _nrow=0;
		_matrixRows=0;
    _ncol=0;
		_matrixCols=0;
    _nebytes=0;
  }
}

bool SharedBigMatrix::create_uuid()
{
	try
	{
  	// See http://www.boost.org/doc/libs/1_36_0/libs/random/random_demo.cpp
  	// for documentation about seed problems with random number based uuid.
  	named_mutex mutex(open_or_create, "SharedBigMatrix_create_uuid");
  	mutex.lock();
		boost::uuid id = uuid::create();
  	_uuid = id.to_string();
  	mutex.unlock();
  	named_mutex::remove("SharedBigMatrix_create_uuid");
  	return true;
	}
	catch(...)
	{
  	named_mutex::remove("SharedBigMatrix_create_uuid");
		return false;
	}
}

bool SharedBigMatrix::read_lock( Columns &cols )
{
  _mutexLock.read_write_lock();
  Columns::size_type i;
  for (i=0; i < cols.size(); ++i)
  {
    _mutexPtrs[ cols[i] ]->read_lock();
  }
  _mutexLock.unlock();
  return true;
}

bool SharedBigMatrix::read_write_lock( Columns &cols )
{
  _mutexLock.read_write_lock();
  Columns::size_type i;
  for (i=0; i < cols.size(); ++i)
  {
    _mutexPtrs[ cols[i] ]->read_write_lock();
  }
  _mutexLock.unlock();
  return true;
}

bool SharedBigMatrix::unlock( Columns &cols )
{
//  _mutexLock.read_write_lock();
  Columns::size_type i;
  for (i=0; i < cols.size(); ++i)
  {
    _mutexPtrs[ cols[i] ]->unlock();
  }
//  _mutexLock.unlock();
  return true;
}

template<typename T>
void* CreateSharedSepMatrix( const std::string &sharedName, 
  MappedRegionPtrs &dataRegionPtrs, const index_type nrow, 
  const index_type ncol )
{
  T** pMat = new T*[ncol];
  index_type i;
  dataRegionPtrs.resize(ncol);
  for (i=0; i < ncol; ++i)
  {
    try
    {
//      shared_memory_object::remove( (sharedName+"_column_"+ttos(i)).c_str() );
      shared_memory_object shm(create_only, 
        (sharedName + "_column_" + ttos(i)).c_str(),
        read_write);
      shm.truncate( nrow*sizeof(T) );
      dataRegionPtrs[i] = MappedRegionPtr( new MappedRegion(shm, read_write) );
      pMat[i] = reinterpret_cast<T*>( dataRegionPtrs[i]->get_address());
    }
    catch (interprocess_exception &ex)
    {
      index_type j;
      for (j=0; j < i; ++j)
      {
        shared_memory_object::remove( (sharedName+"_column_"+ttos(j)).c_str());
      }
      delete pMat;
      return false;
    }
  }
  return reinterpret_cast<void*>(pMat);
}

bool CreateMutexes( MutexPtrs &mutexPtrs, const std::string &sharedName,
  const index_type ncol )
{
  index_type i;
  mutexPtrs.resize(ncol);
  for (i=0; i < ncol; ++i)
  {
    mutexPtrs[i] = MutexPtr( new BigMemoryMutex );
    mutexPtrs[i]->init( sharedName+"_column_"+ttos(i)+"mutex" );
  }
  return true;
}

template<typename T>
void* CreateSharedMatrix( const std::string &sharedName, 
  MappedRegionPtrs &dataRegionPtrs, const index_type nrow, const index_type ncol, const index_type nebytes)
{
  try
  {
    shared_memory_object shm(create_only, sharedName.c_str(), read_write);
    shm.truncate( nebytes + nrow*ncol*sizeof(T) );
    dataRegionPtrs.push_back(
      MappedRegionPtr(new MappedRegion(shm, read_write)));
  }
  catch (interprocess_exception &ex)
  {
    shared_memory_object::remove( (sharedName.c_str() ) );
    return NULL;
  }
  return dataRegionPtrs[0]->get_address();
}

bool SharedMemoryBigMatrix::create( const index_type numRow, 
  const index_type numCol, const index_type numEbytes, const int matrixType, 
  const bool sepCols )
{
  if (!create_uuid())
	{
		return false;
	}
	try
	{
  	named_mutex mutex( open_or_create, (_uuid+"_counter_mutex").c_str() );
  	mutex.lock();
  	_nrow = numRow;
		_matrixRows = _nrow;
  	_ncol = numCol;
		_matrixCols = _ncol;
  	_matType = matrixType;
  	_sepCols = sepCols;
  	_sharedName=_uuid;
  	_sharedCounter.init(_sharedName+"_counter");
  	if (_sepCols)
  	{
    	switch(_matType)
    	{
      	case 1:
        	_pdata = CreateSharedSepMatrix<char>(_sharedName, _dataRegionPtrs,
          	_nrow, _ncol);
        	break;
      	case 2:
        	_pdata = CreateSharedSepMatrix<short>(_sharedName, _dataRegionPtrs, 
          	_nrow, _ncol);
        	break;
      	case 4:
        	_pdata = CreateSharedSepMatrix<int>(_sharedName, _dataRegionPtrs, 
          	_nrow, _ncol);
        	break;
      	case 8:
        	_pdata = CreateSharedSepMatrix<double>(_sharedName, _dataRegionPtrs,
          	_nrow, _ncol);
    	}
  	}
  	else
  	{
			_nebytes = numEbytes;
    	switch(_matType)
    	{
      	case 1:
        	_pdata = CreateSharedMatrix<char>(_sharedName, _dataRegionPtrs, 
          	_nrow, _ncol, _nebytes);
        	break;
      	case 2:
        	_pdata = CreateSharedMatrix<short>(_sharedName, _dataRegionPtrs,
          	_nrow, _ncol, _nebytes);
        	break;
      	case 4:
        	_pdata = CreateSharedMatrix<int>(_sharedName, _dataRegionPtrs,
          	_nrow, _ncol, _nebytes);
        	break;
      	case 8:
      	  _pdata = CreateSharedMatrix<double>(_sharedName, _dataRegionPtrs,
      	    _nrow, _ncol, _nebytes);
    	}
  	}
  	if (_pdata == NULL)
  	{
    	_sharedCounter.reset();
    	mutex.unlock();
    	named_mutex::remove((_sharedName+"_counter_mutex").c_str());
    	return false;
  	}
  	CreateMutexes(_mutexPtrs, _sharedName, _ncol);
  	_mutexLock.init( _sharedName+"_mutex_lock" );
  	mutex.unlock();
  	named_mutex::remove((_sharedName+"_counter_mutex").c_str());
  	return true;
	}
	catch(...)
	{
		return false;
	}
}

template<typename T>
void* ConnectSharedSepMatrix( const std::string &uuid, 
  MappedRegionPtrs &dataRegionPtrs, const index_type nrow, 
  const index_type ncol )
{
  T** pMat = new T*[ncol];
  index_type i;
  for (i=0; i < ncol; ++i)
  {
		try
		{
      shared_memory_object shm(open_only,
        (uuid + "_column_" + ttos(i)).c_str(),
        read_write);
      dataRegionPtrs.push_back(
        MappedRegionPtr(new MappedRegion(shm, read_write)));
      pMat[i] = reinterpret_cast<T*>(dataRegionPtrs[i]->get_address());
    }
    catch (...)
    {
      dataRegionPtrs.resize(0);
      delete pMat;
      return NULL;
    }
  }
  return reinterpret_cast<void*>(pMat);
}

template<typename T>
void* ConnectSharedMatrix( const std::string &sharedName, 
  MappedRegionPtrs &dataRegionPtrs, const index_type nrow, 
  const index_type ncol)
{
	try 
	{
  	shared_memory_object shm(open_only, sharedName.c_str(), read_write);
  	dataRegionPtrs.push_back(
      MappedRegionPtr(new MappedRegion(shm, read_write)));
	}
	catch(...)
	{
		dataRegionPtrs.resize(0);
		return NULL;
	}
  return reinterpret_cast<void*>(dataRegionPtrs[0]->get_address());
}

bool SharedMemoryBigMatrix::connect( const std::string &uuid, 
	const index_type numRow, const index_type numCol, const index_type numEbytes, 
	const int matrixType, const bool sepCols )
{
	try
	{
  	named_mutex mutex( open_or_create, (uuid+"_counter_mutex").c_str() );
  	mutex.lock();
  	_uuid=uuid;
  	_sharedName = _uuid;
  	_nrow = numRow;
		_matrixRows=_nrow;
  	_ncol = numCol;
		_matrixCols=_ncol;
  	_matType = matrixType;
  	_sepCols = sepCols;
  	_sharedCounter.init(_sharedName+"_counter");
  	if (_sepCols)
  	{
    	switch(_matType)
    	{
// JJJ I don't think nrow is needed below.
      	case 1:
        	_pdata = ConnectSharedSepMatrix<char>(_sharedName, _dataRegionPtrs,
          	_nrow, _ncol);
        	break;
      	case 2:
        	_pdata = ConnectSharedSepMatrix<short>(_sharedName, _dataRegionPtrs,
          	_nrow, _ncol);
        	break;
      	case 4:
        	_pdata = ConnectSharedSepMatrix<int>(_sharedName, _dataRegionPtrs,
          	_nrow, _ncol);
        	break;
      	case 8:
        	_pdata = ConnectSharedSepMatrix<double>(_sharedName, _dataRegionPtrs,
          	_nrow, _ncol);
    	}
  	}
  	else
  	{
      _nebytes = numEbytes;
    	switch(_matType)
    	{
// JJJ I don't think nrow and ncol are needed below.
      	case 1:
        	_pdata = ConnectSharedMatrix<char>(_sharedName, _dataRegionPtrs,
          	_nrow, _ncol);
        	break;
      	case 2:
        	_pdata = ConnectSharedMatrix<short>(_sharedName, _dataRegionPtrs,
          	_nrow, _ncol);
        	break;
      	case 4:
        	_pdata = ConnectSharedMatrix<int>(_sharedName, _dataRegionPtrs,
          	_nrow, _ncol);
        	break;
      	case 8:
        	_pdata = ConnectSharedMatrix<double>(_sharedName, _dataRegionPtrs,
          	_nrow, _ncol);
    	}
  	}
		if (!_pdata)
		{
    	mutex.unlock();
    	named_mutex::remove((_sharedName+"_counter_mutex").c_str());
    	return false;
		}
  	CreateMutexes(_mutexPtrs, _sharedName, _ncol);
  	_mutexLock.init( _sharedName+"_mutex_lock" );
  	mutex.unlock();
  	named_mutex::remove((_sharedName+"_counter_mutex").c_str());
  	return true;
	}
	catch(...)
	{
		return false;
	}
}

void DestroySharedSepMatrix( const std::string &uuid, const index_type ncol )
{
  index_type i;
  for (i=0; i < ncol; ++i)
  {
		try
		{
    	shared_memory_object::remove((uuid+ "_column_" + ttos(i)).c_str());
		}
		catch(...)
		{
		}
  }
}

bool SharedMemoryBigMatrix::destroy()
{
	try
	{
  	named_mutex mutex( open_or_create, (_sharedName+"_counter_mutex").c_str() );
  	mutex.lock();
  	_dataRegionPtrs.resize(0);
  	if (_sepCols)
  	{
    	if (_sharedCounter.get() == 1)
    	{
      	DestroySharedSepMatrix(_uuid, _matrixCols);
    	}
    	if (_pdata)
    	{
          delete [] reinterpret_cast<char**>(_pdata);
    	}
  	}
  	else
  	{
    	if ( _sharedCounter.get() == 1 )
    	{
      	shared_memory_object::remove(_uuid.c_str());
    	}
  	}
  	if (_sharedCounter.get() == 1)
  	{
    	index_type i;
    	for (i=0; i < static_cast<index_type>(_mutexPtrs.size()); ++i)
    	{
      	_mutexPtrs[i]->destroy();
    	}
    	_mutexLock.destroy();
  	}
  	_sharedCounter.reset();
  	mutex.unlock();
  	named_mutex::remove((_sharedName+"_counter_mutex").c_str());
  	return true;
	}
	catch(...)
	{
		return false;
	}
}

template<typename T>
void* ConnectFileBackedSepMatrix( const std::string &sharedName,
  const std::string &filePath, MappedRegionPtrs &dataRegionPtrs, 
  const index_type nrow, const index_type ncol)
{
  T** pMat = new T*[ncol];
  index_type i;
  dataRegionPtrs.resize(ncol);
  for (i=0; i < ncol; ++i)
  {
    std::string columnName = filePath + sharedName + "_column_" + ttos(i);
    // Map the file to this process.
    try
    {
      file_mapping mFile(columnName.c_str(), read_write);
      dataRegionPtrs[i] = MappedRegionPtr(new MappedRegion(mFile, read_write));
      pMat[i] = reinterpret_cast<T*>(dataRegionPtrs[i]->get_address());
    }
    catch (...)
    {
      dataRegionPtrs.resize(0);
      delete pMat;
      return NULL;
    }
  }
  return reinterpret_cast<void*>(pMat);
}

template<typename T>
void* CreateFileBackedSepMatrix( const std::string &fileName, 
  const std::string &filePath, MappedRegionPtrs &dataRegionPtrs, 
  const index_type nrow, const index_type ncol )
{
  index_type i;
  for (i=0; i < ncol; ++i)
  {
    std::string columnName = filePath + fileName + "_column_" + ttos(i);
    // Create the files.
    std::filebuf fbuf;
    if (!fbuf.open( columnName.c_str(), std::ios_base::in | std::ios_base::out |
      std::ios_base::trunc | std::ios_base::binary ))
		{
			return NULL;
		}
    fbuf.pubseekoff( nrow*sizeof(T), std::ios_base::beg);
    // I'm not sure if I need this next line
    fbuf.sputc(0);
    fbuf.close();
  }
  return ConnectFileBackedSepMatrix<T>(fileName, filePath, dataRegionPtrs, 
    nrow, ncol);
}

template<typename T>
void* ConnectFileBackedMatrix( const std::string &fileName, 
  const std::string &filePath, MappedRegionPtrs &dataRegionPtrs, 
  const index_type nrow, const index_type ncol)
{
  try
  {
    file_mapping mFile((filePath+fileName).c_str(), read_write);
    dataRegionPtrs.push_back(
      MappedRegionPtr(new MappedRegion(mFile, read_write)));
	}
	catch (...)
  {
		dataRegionPtrs.resize(0);
    return NULL;
  }
  return reinterpret_cast<void*>(dataRegionPtrs[0]->get_address());
}

template<typename T>
void* CreateFileBackedMatrix( const std::string &fileName, 
  const std::string &filePath, MappedRegionPtrs &dataRegionPtrs, 
  const index_type nrow, const index_type ncol, const index_type nebytes)
{
  // Create the file.
  std::filebuf fbuf;
  if (!fbuf.open( (filePath+fileName).c_str(), 
      std::ios_base::in | std::ios_base::out |
      std::ios_base::trunc | std::ios_base::binary ))
	{
		return NULL;
	}
  fbuf.pubseekoff( nebytes+nrow*ncol*sizeof(T), std::ios_base::beg);
  // I'm not sure if I need this next line
  fbuf.sputc(0);
  fbuf.close();
  return ConnectFileBackedMatrix<T>(fileName, filePath,
    dataRegionPtrs, nrow, ncol);
}

bool FileBackedBigMatrix::create( const std::string &fileName, 
  const std::string &filePath, const index_type numRow, const index_type numCol, 
  const index_type numEbytes, const int matrixType, const bool sepCols, bool preserve )
{
  if (!create_uuid())
	{
		return false;
	}
	try
	{
  	named_mutex 
			mutex(open_or_create,(fileName+uuid()+"_counter_mutex").c_str());
  	mutex.lock();
  	_fileName = fileName;
  	_sharedName=fileName+uuid();
  	_sharedCounter.init(_sharedName+"_counter");
  	_nrow = numRow;
		_matrixRows = _nrow;
  	_ncol = numCol;
		_matrixCols = _ncol;
  	_matType = matrixType;
  	_sepCols = sepCols;
  	_preserve = preserve;
  	if (_sepCols)
  	{
    	switch(_matType)
    	{
      	case 1:
        	_pdata = CreateFileBackedSepMatrix<char>(_fileName, filePath,
          	_dataRegionPtrs, _nrow, _ncol);
        	break;
      	case 2:
        	_pdata = CreateFileBackedSepMatrix<short>(_fileName, filePath,
          	_dataRegionPtrs, _nrow, _ncol);
        	break;
      	case 4:
        	_pdata = CreateFileBackedSepMatrix<int>(_fileName, filePath,
          	_dataRegionPtrs, _nrow, _ncol);
        	break;
      	case 8:
        	_pdata = CreateFileBackedSepMatrix<double>(_fileName, filePath,
          	_dataRegionPtrs, _nrow, _ncol);
    	}
  	}
  	else
  	{
			_nebytes = numEbytes;
    	switch(_matType)
    	{
      	case 1:
        	_pdata = CreateFileBackedMatrix<char>(_fileName, filePath,
          	_dataRegionPtrs, _nrow, _ncol, numEbytes);
        	break;
      	case 2:
        	_pdata = CreateFileBackedMatrix<short>(_fileName, filePath,
          	_dataRegionPtrs, _nrow, _ncol, numEbytes);
        	break;
      	case 4:
        	_pdata = CreateFileBackedMatrix<int>(_fileName, filePath,
          	_dataRegionPtrs, _nrow, _ncol, numEbytes);
        	break;
      	case 8:
        	_pdata = CreateFileBackedMatrix<double>(_fileName, filePath,
          	_dataRegionPtrs, _nrow, _ncol, numEbytes);
    	}
  	}
		if (!_pdata)
		{
			return false;
		}
 	 	CreateMutexes(_mutexPtrs, _sharedName, _ncol);
 	 	_mutexLock.init( _sharedName+"_mutex_lock" );
 	 	mutex.unlock();
  	named_mutex::remove((_sharedName+"_counter_mutex").c_str());
  	return true;
	}
	catch(...)
	{
		return false;
	}
}

bool FileBackedBigMatrix::connect( const std::string &sharedName, 
  const std::string &fileName, const std::string &filePath, const index_type numRow, 
  const index_type numCol, const index_type numEbytes, const int matrixType, 
  const bool sepCols, const bool preserve )
{
	try
	{
  	named_mutex mutex( open_or_create, (sharedName+"_counter_mutex").c_str() );
  	mutex.lock();
	  _sharedName=sharedName;
	  _fileName=fileName;
	  _nrow = numRow;
		_matrixRows = _nrow;
	  _ncol = numCol;
		_matrixCols = _ncol;
	  _matType = matrixType;
	  _sepCols = sepCols;
	  _preserve = preserve;
	  _sharedCounter.init(_sharedName+"_counter");
    _filePath = filePath;
	  if (_sepCols)
	  {
	    switch(_matType)
	    {
	      case 1:
	        _pdata = ConnectFileBackedSepMatrix<char>(_fileName, filePath,
	          _dataRegionPtrs, _nrow, _ncol);
	        break;
	      case 2:
	        _pdata = ConnectFileBackedSepMatrix<short>(_fileName, filePath,
          _dataRegionPtrs, _nrow, _ncol);
	        break;
	      case 4:
	        _pdata = ConnectFileBackedSepMatrix<int>(_fileName, filePath,
	          _dataRegionPtrs, _nrow, _ncol);
	        break;
	      case 8:
	        _pdata = ConnectFileBackedSepMatrix<double>(_fileName, filePath,
	          _dataRegionPtrs, _nrow, _ncol);
	    }
	  }
	  else
	  {
			_nebytes = numEbytes;
	    switch(_matType)
	    {
	      case 1:
	        _pdata = ConnectFileBackedMatrix<char>(_fileName, filePath,
	          _dataRegionPtrs, _nrow, _ncol);
	        break;
	      case 2:
	        _pdata = ConnectFileBackedMatrix<short>(_fileName, filePath,
	          _dataRegionPtrs, _nrow, _ncol);
	        break;
	      case 4:
	        _pdata = ConnectFileBackedMatrix<int>(_fileName, filePath,
	          _dataRegionPtrs, _nrow, _ncol);
	        break;
	      case 8:
	        _pdata = ConnectFileBackedMatrix<double>(_fileName, filePath,
	          _dataRegionPtrs, _nrow, _ncol);
	    }
	  }
		if (!_pdata)
		{
	  	mutex.unlock();
	  	named_mutex::remove((_sharedName+"_counter_mutex").c_str());
			return false;
		}
	  CreateMutexes(_mutexPtrs, _sharedName, _ncol);
	  _mutexLock.init( _sharedName+"_mutex_lock" );
	  mutex.unlock();
	  named_mutex::remove((_sharedName+"_counter_mutex").c_str());
	  return true;
	}
	catch(...)
	{
		return false;
	}
}

void DestroyFileBackedSepMatrix( const std::string &sharedName, 
  const index_type ncol, const std::string &fileName, const bool preserve )
{
  index_type i;
  for (i=0; i < ncol; ++i)
  {
		try
		{
    	shared_memory_object::remove((sharedName + "_column_" + ttos(i)).c_str());
		}
		catch(...)
		{
		}
    if (!preserve)
    {
      std::string removeFileName(fileName + "_column_" + ttos(i));
      unlink( removeFileName.c_str() );
    }
  }
}

bool FileBackedBigMatrix::destroy()
{
	try
	{
  	named_mutex mutex( open_or_create, (_sharedName+"_counter_mutex").c_str() );
  	mutex.lock();
  	_dataRegionPtrs.resize(0);
  	if (_sepCols) 
    {
      if (_sharedCounter.get() == 1) 
      {
        DestroyFileBackedSepMatrix(_sharedName, _matrixCols, 
          _fileName, _preserve);
    	}
    	if (_pdata) 
      {
        switch(_matType)
      	{
          case 1:
            delete [] reinterpret_cast<char**>(_pdata);
          	break;
        	case 2:
          	delete [] reinterpret_cast<short**>(_pdata);
          	break;
        	case 4:
          	delete [] reinterpret_cast<int**>(_pdata);
          	break;
        	case 8:
          	delete [] reinterpret_cast<double**>(_pdata);
      	    }
    	  }
  	} 
    else 
    {             // not _sepCols
      if (_sharedCounter.get() == 1) 
      {
     	  shared_memory_object::remove(_sharedName.c_str());
        if (!_preserve)
        {
     	    unlink( _fileName.c_str() );
        }
      }
  	}

        // In all cases, do the following:
  	if (_sharedCounter.get() == 1)
  	{
      index_type i;
      for (i=0; i < static_cast<index_type>(_mutexPtrs.size()); ++i)
      {
     	  _mutexPtrs[i]->destroy();
      }
      _mutexLock.destroy();
  	}
  	mutex.unlock();
  	named_mutex::remove((_sharedName+"_counter_mutex").c_str());
    _ncol=0;
    _nrow=0;
    _matrixRows=0;
    _matrixCols=0;
    _pdata=0;
    _colNames.clear();
    _rowNames.clear();
  	return true;

	} // end of the try
	catch(...)
	{
		return false;
	}
}
