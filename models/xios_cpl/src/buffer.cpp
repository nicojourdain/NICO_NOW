#include "xmlioserver_spl.hpp"
#include "buffer.hpp"


namespace xios
{
    CBuffer::CBuffer(void* buffer_,size_t size_)
    {
      own=false ;
      realloc(buffer_,size_) ;
    }
    
    CBuffer::CBuffer(size_t size_)
    {
      own=false ;
      realloc(size_) ;
    }

    void CBuffer::realloc(size_t size_)
    {
      realloc(new char[size_],size_) ;
      own=true ;
    }

    void CBuffer::realloc(void* buffer_,size_t size_)
    {
      if (own) delete [] buffer ;
      buffer=(char*)buffer_ ;
      size=size_ ;
      count=0 ;
      read=buffer ;
      write=buffer ;
      own=false ;
    }

    size_t CBuffer::remain(void)
    {
      return size-count ;
    }    
    
    CBuffer::~CBuffer()
    {
      if (own) delete [] buffer ;
    }
    
}
    
      
    
