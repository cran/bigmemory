#include "BigMemoryMutex.h"
#include <boost/interprocess/sync/named_mutex.hpp>
#include <boost/interprocess/sync/named_upgradable_mutex.hpp>
#include <boost/uuid.hpp>
#include "MSCexceptions.h"

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

bool BigMemoryMutex::init( const std::string &name )
{
  _name=name;
  _pNamedMutex = new named_upgradable_mutex(open_or_create,
    _name.c_str());
  return true;
}


bool UserMutex::init( const std::string &name )
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

bool BigMemoryMutex::read_lock()
{
  _pNamedMutex->lock_sharable();
  _sharable=true;
  return true;
}

bool BigMemoryMutex::read_write_lock()
{
  _pNamedMutex->lock();
  _sharable=false;
  return true;
}

bool BigMemoryMutex::unlock()
{
  _sharable ? _pNamedMutex->unlock_sharable() : _pNamedMutex->unlock();
  return true;
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
