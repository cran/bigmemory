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

#include "BigMemoryMutex.h"
#include "util.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

extern "C"
{

void DestroyRWUserMutex(SEXP address)
{
  BigMemoryMutex *pMutex = 
    reinterpret_cast<BigMemoryMutex*>(R_ExternalPtrAddr(address));
  delete(pMutex);
  R_ClearExternalPtr(address);
}
SEXP CreateUserRWMutex()
{
  BigMemoryMutex *pMutex = new BigMemoryMutex;
  pMutex->init();
  SEXP address = R_MakeExternalPtr(pMutex, R_NilValue, R_NilValue);
  R_RegisterCFinalizerEx(address, (R_CFinalizer_t)DestroyRWUserMutex,
    (Rboolean)TRUE);
  return(address);
}

SEXP ConnectUserRWMutex(SEXP mutexId)
{
  BigMemoryMutex *pMutex = new BigMemoryMutex;
  if (!pMutex->init( RChar2String(mutexId) ) )
  {
    printf("Failed to connect to mutex!\n");
    delete pMutex;
    return R_NilValue;
  }
  SEXP address = R_MakeExternalPtr(pMutex, R_NilValue, R_NilValue);
  R_RegisterCFinalizerEx(address, (R_CFinalizer_t)DestroyRWUserMutex,
    (Rboolean)TRUE);
  return(address);
}

SEXP GetUserRWMutexInfo(SEXP address)
{
  BigMemoryMutex *pMutex = 
    reinterpret_cast<BigMemoryMutex*>(R_ExternalPtrAddr(address));
  return(String2RChar(pMutex->resource_name()));
}

// These functions will return booleans if the mutexes are timed
void RLockUserRWMutex(SEXP address)
{
  BigMemoryMutex *pMutex = 
    reinterpret_cast<BigMemoryMutex*>(R_ExternalPtrAddr(address));
  pMutex->read_lock();
}

void RWLockUserRWMutex(SEXP address)
{
  BigMemoryMutex *pMutex = 
    reinterpret_cast<BigMemoryMutex*>(R_ExternalPtrAddr(address));
  pMutex->read_write_lock();
}

void UnlockUserRWMutex(SEXP address)
{
  BigMemoryMutex *pMutex = 
    reinterpret_cast<BigMemoryMutex*>(R_ExternalPtrAddr(address));
  pMutex->unlock();
}

} // extern "C"
