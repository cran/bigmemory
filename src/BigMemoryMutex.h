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

#ifndef BIGMEMORYMUTEX
#define BIGMEMORYMUTEX
#include <string>
#include <boost/noncopyable.hpp>
#include <boost/interprocess/sync/named_upgradable_mutex.hpp>

#include "SharedCounter.h"

class BigMemoryMutex : public boost::noncopyable
{
  public:
    BigMemoryMutex():_pNamedMutex(NULL){};
    virtual ~BigMemoryMutex(){_name=""; if (_pNamedMutex) delete _pNamedMutex;};
    bool init();
    bool init( const std::string &name );
    std::string resource_name() const {return _name;};
    bool read_lock();
    bool read_write_lock();
    bool unlock();
    bool destroy();
  protected:
    std::string _name;
    boost::interprocess::named_upgradable_mutex *_pNamedMutex;
    bool _sharable;
};

class UserMutex : public BigMemoryMutex
{
  public:
    UserMutex(): BigMemoryMutex(){};
    virtual ~UserMutex(){destroy();};
    bool init();
    bool init( const std::string &name );
    bool destroy();
  protected:
    SharedCounter _sharedCounter;
};
#endif //BIGMEMORYMUTEX

