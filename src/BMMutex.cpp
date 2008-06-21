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

#include "BMMutex.h"
#include "DestroyShmIfLast.h"

bool is_last(const int memid)
{
  shmid_ds shmData;
  memset(&shmData, 0, sizeof(shmid_ds));
  if (0 != shmctl(memid, IPC_STAT, &shmData))
  {
    printf("Error Message1: %d %s\n", memid, strerror(errno));
    return false;
  }
  return (1 == shmData.shm_nattch);
}

bool destroy_shared(const int memid)
{
//  if (!is_last(memid))
//    return false;
  if (0 != shmctl(memid, IPC_RMID, NULL))
    return false;
  return true;
}

BMMutex::~BMMutex()
{
  if (pLock)
  {
    if (!is_last())
      detach();
    else
      destroy();
  }
} 

bool BMMutex::detach() 
{ 
  if (-1 != shmdt(pLock))
    return false;
  pLock = NULL;
  return true;
}

bool BMMutex::create()
{
  // May need to come up with a real key (not IPC_PRIVATE).
  sharedMemId = shmget(IPC_PRIVATE, sizeof(pthread_rwlock_t), 
    0664 | IPC_CREAT);// | IPC_CREAT | IPC_EXCL);
  if (reinterpret_cast<pthread_rwlock_t*>(-1) ==
    (pLock = (pthread_rwlock_t*)shmat(sharedMemId, NULL, 0)) )
  {
    return false;
  }
  pthread_rwlockattr_t attr;
  if (0 != pthread_rwlockattr_init(&attr))
    return false;
  if (0 != pthread_rwlockattr_setpshared(&attr, PTHREAD_PROCESS_SHARED))
    return false;
  if (0 != pthread_rwlock_init(pLock, &attr))
    return false;
  return true;
}
    
bool BMMutex::connect(const int shmid)
{
  sharedMemId = shmid;
  if (reinterpret_cast<pthread_rwlock_t*>(-1) 
    == (pLock = (pthread_rwlock_t*)shmat(sharedMemId, NULL, 0)))
  {
    return false;
  }
  return true;
}

bool BMMutex::destroy()
{
  detach();
  if (0!=pthread_rwlock_destroy(pLock))
    return false;
  destroy_shared(memid());
  pLock = NULL;
  sharedMemId=-1;
  return true;
}

bool BMSharedMemory::create(long dataSize)
{
  dataKey = shmget(IPC_PRIVATE, dataSize, 0644 | IPC_CREAT);
  if (-1 == dataKey)
    return false;
  data = shmat(dataKey, NULL, 0);
  return true;
};

bool BMSharedMemory::connect(int newDataKey)
{
  dataKey = newDataKey;
  data = shmat(dataKey, NULL, 0);
  if ( -1 != (long)(data) )
  {
    return true;
  }
  return false;
}

BMSharedMemory::~BMSharedMemory()
{
  if (dataKey != -1)
  {
    if (is_last())
    {
      detach();
      destroy();
    }
    else
      detach();
  }
}

bool BMSharedMemory::destroy()
{
  detach();
  if (!destroy_shared(dataKey))
    return false;
  data = NULL;
  return true;
}

bool BMSharedMemory::detach()
{
  if (-1 != shmdt(data))
    return false;
  data = NULL;
  return true;
}

bool MutexSharedMemory::create(long dataSize)
{
  if ( mem.create(dataSize) && mutex.create() )
    return true;
  return false;
}

bool MutexSharedMemory::connect(int newMutexKey, int newDataKey)
{
  if (mutex.connect(newMutexKey) && mem.connect(newDataKey))
    return true;
  return false;
}

bool MutexSharedMemory::detach()
{
  rwlock();
  if (mem.is_last())
  {
    if( !mem.detach() && !mem.destroy() )
      return false;
    unlock();
    if( mutex.detach() && mutex.destroy())
      return true;
    return false;
  }
  if( !mutex.detach() || !mem.detach() )
    return false;
  unlock();
  return true;
}
