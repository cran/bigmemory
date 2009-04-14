#ifndef BIGMEMORYMUTEX
#define BIGMEMORYMUTEX
#include <string>
#include <boost/noncopyable.hpp>
#include <boost/interprocess/sync/named_upgradable_mutex.hpp>
#include "MSCexceptions.h"

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

