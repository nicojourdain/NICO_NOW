#include "xmlioserver_spl.hpp"
#include "buffer_out.hpp"


namespace xios
{
    CBufferOut::CBufferOut(void* buffer,size_t size)
    {
      own=false ;
      realloc(buffer,size) ;
    }
    
    CBufferOut::CBufferOut(void)
    {
      own=false ;
      realloc(0,0) ;      
    }

    CBufferOut::CBufferOut(size_t size)
    {
      own=false ;
      realloc(size) ;
    }

    void CBufferOut::realloc(size_t size)
    {
      realloc(new char[size],size) ;
      own=true ;
    }

    void CBufferOut::realloc(void* buffer,size_t size)
    {
      if (own) delete [] begin ;
      begin=(char*)buffer ;
      size_=size ;
      end=begin+size_ ;
      count_=0 ;
      current=begin ;
      own=false ;
    }

    bool CBufferOut::advance(size_t n) { return advance<char>(n); }

    void* CBufferOut::ptr(void)
    {
      return current ;
    }
    
    size_t CBufferOut::remain(void)
    {
      return size_-count_ ;
    }    

    size_t CBufferOut::count(void)
    {
      return count_ ;
    }

    size_t CBufferOut::size(void)
    {
      return size_ ;
    }
        
    CBufferOut::~CBufferOut()
    {
      if (own) delete [] begin ;
    }
 
}
    
      
    
