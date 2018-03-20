#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "buffer_server.hpp"
#include "cxios.hpp"

namespace xios
{

  CServerBuffer::CServerBuffer(void)
  {
    bufferSizeByClient=CXios::bufferSize*CXios::bufferServerFactorSize ;
    size=bufferSizeByClient ;
    first=0 ;
    current=1 ;
    end=size ;
    buffer=new char[size] ;  // change later for MPI_ALLOC_MEM later
  }

  CServerBuffer::~CServerBuffer()
  {
    delete [] buffer ;
  }
  

  bool CServerBuffer::isBufferFree(size_t count)
  {
    bool ret ;
    
    if (count==0) return true ;
    
    if (current>first)
    {
      if (current+count<size)
      {
        ret=true ;
      }
      else if (current+count==size)
      {
        if (first>0)
        {
          ret=true ;
        }
        else
        {
          ret=false ;
        }
      }
      else
      {
        if (count<first)
        {
          ret=true ;
        }
        else
        {
          ret=false ;
        }
      }
    }
    else
    {
      if (current+count<first)
      {
        ret=true ;
      }
      else
      {
         ret=false ;
      }
    }

    return ret ;
  }
  

  void* CServerBuffer::getBuffer(size_t count)
  {
    char* ret ;
    
    if (count==0) return buffer+current ;
    
    if (current>first)
    {
      if (current+count<size)
      {
        ret=buffer+current ;
        current+=count ;
      }
      else if (current+count==size)
      {
        if (first>0)
        {
          ret=buffer+current ;
          current=0 ;
        }
        else
        {
          ERROR("void* CServerBuffer::getBuffer(size_t count)",
                 <<"cannot allocate required size in buffer") ;
        }
      }
      else
      {
        end=current ;
        if (count<first)
        {
          ret=buffer ;
          current=count ;
        }
        else
        {
          ERROR("void* CServerBuffer::getBuffer(size_t count)",
                 <<"cannot allocate required size in buffer") ;
        }
      }
    }
    else
    {
      if (current+count<first)
      {
        ret=buffer+current ;
        current+=count ;
      }
      else
      {
          ERROR("void* CServerBuffer::getBuffer(size_t count)",
                 <<"cannot allocate required size in buffer") ;
      }
    }

    return ret ;
  }
  
  void CServerBuffer::freeBuffer(size_t count)
  {
    if (count==0) return ;

    if (first==end-1) 
    {
      first=0 ;
      count-- ;
      end=size ;
    }

    if (first<=current)
    {
      if (first+count <current) 
      {
        first+=count ;
      }
      else
      {
          ERROR("void CServerBuffer::freeBuffer(size_t count)",
                 <<"cannot free required size in buffer") ;
      }
    
    }
    else
    {
      if (first+count<end)
      {
        first+=count ;
      }
      else
      {
          ERROR("void CServerBuffer::freeBuffer(size_t count)",
                 <<"cannot free required size in buffer") ;
      }
    }
  }      
       
}
