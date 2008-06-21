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

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#include "BMMutex.h"

extern "C"
{

void DestroyRWUserMutex(SEXP address)
{
//  UserRWMutex *pMutex = (UserRWMutex*)R_ExternalPtrAddr(address);
  BMMutex *pMutex = reinterpret_cast<BMMutex*>(R_ExternalPtrAddr(address));
  delete(pMutex);
  R_ClearExternalPtr(address);
}
SEXP CreateUserRWMutex()
{
  BMMutex *pMutex = new BMMutex();
  pMutex->create();
  SEXP address = R_MakeExternalPtr(pMutex, R_NilValue, R_NilValue);
  R_RegisterCFinalizerEx(address, (R_CFinalizer_t)DestroyRWUserMutex,
    (Rboolean)TRUE);
  return(address);
}

SEXP ConnectUserRWMutex(SEXP mutexId)
{
  BMMutex *pMutex = new BMMutex();
  if (!pMutex->connect(INTEGER_VALUE(mutexId)))
  {
    printf("Failed to connect to mutex!\n");
  }
  SEXP address = R_MakeExternalPtr(pMutex, R_NilValue, R_NilValue);
  R_RegisterCFinalizerEx(address, (R_CFinalizer_t)DestroyRWUserMutex,
    (Rboolean)TRUE);
  return(address);
}

SEXP GetUserRWMutexInfo(SEXP address)
{
  BMMutex *pMutex = reinterpret_cast<BMMutex*>(R_ExternalPtrAddr(address));
  SEXP ret = NEW_INTEGER(1);
  INTEGER_DATA(ret)[0] = pMutex->memid();
  return(ret);
}

void RLockUserRWMutex(SEXP address)
{
  BMMutex *pMutex = reinterpret_cast<BMMutex*>(R_ExternalPtrAddr(address));
  pMutex->rlock();
}

void RWLockUserRWMutex(SEXP address)
{
  BMMutex *pMutex = reinterpret_cast<BMMutex*>(R_ExternalPtrAddr(address));
  pMutex->rwlock();
}

void UnlockUserRWMutex(SEXP address)
{
  BMMutex *pMutex = reinterpret_cast<BMMutex*>(R_ExternalPtrAddr(address));
  pMutex->unlock();
}

} // extern "C"
