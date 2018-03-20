#ifndef __BUFFER_SERVER_HPP__
#define __BUFFER_SERVER_HPP__

#include "xmlioserver_spl.hpp"
#include "buffer.hpp"
#include "mpi.hpp"

namespace xios
{

  class CServerBuffer
  {
    
    public:
    
    CServerBuffer(void) ;
    ~CServerBuffer() ;
    char* buffer ;
    
    bool isBufferFree(size_t count) ;
    void* getBuffer(size_t count) ;
    void freeBuffer(size_t count) ;
  
    size_t first ;   // first occupied element
    size_t current ; // first free element
    size_t end ;
    size_t size ;
    size_t bufferSizeByClient ;
  } ;
  

}

#endif
