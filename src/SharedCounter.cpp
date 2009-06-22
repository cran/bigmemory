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

#include <boost/interprocess/shared_memory_object.hpp>
#include "SharedCounter.h"

bool SharedCounter::reset()
{
  if (_pVal)
  {
    --(*_pVal);
    if (get() == 0)
    {
      boost::interprocess::shared_memory_object::remove(_resourceName.c_str());
      _resourceName="";
    }
    delete _pRegion;
  }
  _pVal = NULL;
  _resourceName = "";
  return true;
}

bool SharedCounter::init( const std::string resourceName )
{
  _resourceName = resourceName;
  // See if we are connecting for the first time.
  try
  {  
    boost::interprocess::shared_memory_object shm(
      boost::interprocess::create_only,
      _resourceName.c_str(), 
      boost::interprocess::read_write);
    // It's a new counter.
    shm.truncate( sizeof(long) );
    _pRegion = new boost::interprocess::mapped_region(shm, 
      boost::interprocess::read_write);
    _pVal = reinterpret_cast<long*>(_pRegion->get_address());
    *_pVal = 1;
  }
  catch(std::exception &ex)
  {
    // We are connecting to an existing counter.
    boost::interprocess::shared_memory_object shm(
      boost::interprocess::open_only,
      _resourceName.c_str(), 
      boost::interprocess::read_write);
    _pRegion = new boost::interprocess::mapped_region(shm, 
      boost::interprocess::read_write);
    _pVal = reinterpret_cast<long*>(_pRegion->get_address());
    ++(*_pVal);
  }
  return true;
}

long SharedCounter::get() const
{
  return _pVal == NULL ? 0 : *_pVal;
}
