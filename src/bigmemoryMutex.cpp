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

#include <cstdio>
#include <cstring>
#include <boost/uuid.hpp>
#include "bigmemoryMutex.h"
#include <boost/interprocess/sync/named_mutex.hpp>

using namespace boost::interprocess;

bool BigMemoryMutex::init()
{
  if (_pNamedMutex)
  {
    return false;
  }
  named_mutex mutex(open_or_create, "BigMemoryMutex_create_uuid");
  mutex.lock();
  _name = boost::uuid::create().to_string();
  mutex.unlock();
  named_mutex::remove("BigMemoryMutex_create_uuid");
  _pNamedMutex = new named_upgradable_mutex(open_or_create,
    _name.c_str());
  return true;
}

bool UserMutex::init()
{
  if (_pNamedMutex)
  {
    return false;
  }
	try
	{
  	named_mutex mutex(open_or_create, "BigMemoryMutex_create_uuid");
  	mutex.lock();
  	_name = boost::uuid::create().to_string();
  	named_mutex mutex2( open_or_create, (_name+"_counter_mutex").c_str() );
  	mutex2.lock();
  	_sharedCounter.init(_name+"_counter");
  	mutex.unlock();
  	named_mutex::remove("BigMemoryMutex_create_uuid");
  	_pNamedMutex = new named_upgradable_mutex(open_or_create,
    	_name.c_str());
  	mutex2.unlock();
  	named_mutex::remove( (_name+"_counter_mutex").c_str() );
  	return true;
	}
	catch(...)
	{
		named_mutex::remove( (_name+"_counter_mutex").c_str() );
		named_mutex::remove( "BigMemoryMutex_create_uuid" );
		_name = "";
		_sharedCounter.reset();
		if (_pNamedMutex) delete _pNamedMutex;
		return false;
	}
}

bool BigMemoryMutex::init( const std::string &name )
{
	try
	{
  	_name=name;
  	_pNamedMutex = new named_upgradable_mutex(open_or_create,
    	_name.c_str());
  	return true;
	}
	catch(...)
	{
		_name = "";
		if (_pNamedMutex) delete _pNamedMutex;
		return false;
	}
}


bool UserMutex::init( const std::string &name )
{
	try
	{
  	named_mutex mutex( open_or_create, (name+"_counter_mutex").c_str() );
  	mutex.lock();
  	_name=name;
  	_sharedCounter.init(_name+"_counter");
  	_pNamedMutex = new named_upgradable_mutex(open_or_create,
    	_name.c_str());
  	mutex.unlock();
  	named_mutex::remove( (_name+"_counter_mutex").c_str() );
  	return true;
	}
	catch (...)
	{
		named_mutex::remove( (name+"_counter_mutex").c_str() );
		if (_pNamedMutex) delete _pNamedMutex;
		_name = "";
		_sharedCounter.reset();
		return false;
	}
}

bool BigMemoryMutex::read_lock()
{
	try
	{
  	_sharable=true;
  	_pNamedMutex->lock_sharable();
  	return true;
	}
	catch (...)
	{
		unlock();
		return false;
	}
}

bool BigMemoryMutex::read_write_lock()
{
	try
	{
  	_sharable=false;
  	_pNamedMutex->lock();
  	return true;
	}
	catch (...)
	{
		unlock();
		return false;
	}
}

bool BigMemoryMutex::unlock()
{
	try
	{
  	_sharable ? _pNamedMutex->unlock_sharable() : _pNamedMutex->unlock();
  	return true;
	}
	catch (...)
	{
		return false;
	}
}

bool BigMemoryMutex::destroy()
{
  if (_pNamedMutex)
  {
    delete _pNamedMutex;
    _pNamedMutex = NULL;
    boost::interprocess::named_upgradable_mutex::remove(_name.c_str());
    _name = "";
    return true;
  }
  return false;
}

bool UserMutex::destroy()
{
	try
	{
  	named_mutex mutex( open_or_create, (_name+"_counter_mutex").c_str() );
  	mutex.lock();
  	if (_pNamedMutex)
  	{
    	delete _pNamedMutex;
    	_pNamedMutex = NULL;
    	if (_sharedCounter.get() == 1)
    	{
      	boost::interprocess::named_upgradable_mutex::remove(_name.c_str());
      	_sharedCounter.reset();
    	}
    	_name = "";
    	mutex.unlock();
    	named_mutex::remove( (_name+"_counter_mutex").c_str() );
    	return true;
  	}
  	mutex.unlock();
  	named_mutex::remove( (_name+"_counter_mutex").c_str() );
  	return false;
	}
	catch (...)
	{
		named_mutex::remove( (_name+"_counter_mutex").c_str() );
		return false;
	}
}
