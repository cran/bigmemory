#include "BigMemoryMutex.h"
#include <boost/interprocess/sync/named_mutex.hpp>
#include <boost/interprocess/sync/named_upgradable_mutex.hpp>
#include <boost/uuid.hpp>

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
