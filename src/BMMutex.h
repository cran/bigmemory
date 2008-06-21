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

#ifndef BMMUTEX
#define BMMUTEX

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <pthread.h>
#include <errno.h>

#include "DestroyShmIfLast.h"

bool is_last(const int memid);

bool destroy_shared(const int memid);

class BMMutex
{
  public:
    BMMutex():pLock(NULL),sharedMemId(-1){};
    BMMutex(const BMMutex &orig)
    {
      if (orig.pLock)
        connect(orig.memid());
    };

    ~BMMutex();

  private:
    bool is_last() { return ::is_last(memid()); }

  public:
 
    bool detach();

    bool create();
    
    bool connect(const int shmid);
    
    bool rlock() 
    { return 0 == pthread_rwlock_rdlock(pLock) ? true : false; };

    bool rwlock() 
    { return 0 == pthread_rwlock_wrlock(pLock) ? true : false; };

    bool unlock() 
    { return 0 == pthread_rwlock_unlock(pLock) ? true : false; };
    
    bool destroy();

    int memid() const {return sharedMemId;};

  private:
    pthread_rwlock_t *pLock;
    int sharedMemId;
};

// TODO: Should an instance of BMSharedMemory know how much memory it is 
// referencing?
struct BMSharedMemory
{
  BMSharedMemory():dataKey(-1),data(NULL){};
  BMSharedMemory(const BMSharedMemory &orig)
  {
    if (orig.dataKey != 1)
      connect(orig.dataKey);
  };
  ~BMSharedMemory();
  bool create(long dataSize);
  bool connect(int newDataKey);
  bool is_last(){return ::is_last(dataKey);};
  bool destroy();
  bool detach();

// Data members
  int dataKey;
  void *data;
};

class MutexSharedMemory
{
  public:
    MutexSharedMemory(){}
    MutexSharedMemory(const MutexSharedMemory &orig):
      mem(orig.mem),mutex(orig.mutex){};
    ~MutexSharedMemory() {};

  public:
    bool create(long dataSize);
    bool connect(int newMutexKey, int newDataKey);
    bool detach();
    void* data() { return mem.data; };
    int data_key() const { return mem.dataKey; };
    int mutex_key() const { return mutex.memid(); };
    bool rwlock() { return mutex.rwlock(); };
    bool rlock() { return mutex.rlock(); };
    bool unlock() { return mutex.unlock(); };

  private: 
    BMSharedMemory mem;
    BMMutex mutex;
};

#endif // BMMUTEX
