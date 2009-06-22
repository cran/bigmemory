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

#ifndef _SHARED_COUNTER_H
#define _SHARED_COUNTER_H

#include <string>
#include <boost/interprocess/mapped_region.hpp>

// Note: Shared Counters are not mutex protected.
class SharedCounter
{
  public:
    SharedCounter(): _pVal(NULL),_pRegion(NULL){};
    ~SharedCounter(){reset();};

    bool init( const std::string resourceName);
  public:
    long get() const;
    bool reset();
  private:
    long *_pVal;  
    boost::interprocess::mapped_region *_pRegion;
    std::string _resourceName;
};
#endif //_SHARED_COUNTER_H
