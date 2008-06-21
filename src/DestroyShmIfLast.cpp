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

#include "DestroyShmIfLast.h"

void DestroyShmIfLast( const int memid )
{
  struct shmid_ds shmData;
  if (0 != shmctl(memid, IPC_STAT, &shmData))
  {
    printf("Error Message1: %d %s\n", memid, strerror(errno));
  }
  else
  {
    if (shmData.shm_nattch == 0)
    {
      if (0 != shmctl(memid, IPC_RMID, NULL))
        printf("Error Message2: %s\n", strerror(errno));
    }
  }
}
