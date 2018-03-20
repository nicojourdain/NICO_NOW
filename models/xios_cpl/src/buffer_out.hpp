#ifndef __BUFFER_OUT_HPP__
#define __BUFFER_OUT_HPP__


#include "xmlioserver_spl.hpp"

namespace xios
{
  
    class CBufferOut
    {
    
      public:
      
      CBufferOut(size_t size) ;
      CBufferOut(void) ;
      CBufferOut(void* buffer,size_t size) ;

      void realloc(size_t size) ;
      void realloc(void* buffer,size_t size) ;
      
      template<class T>
      bool put(const T& data) ;

      template<class T>
      bool put(const T* data, size_t n) ;

    
      template<class T>
      bool advance(size_t n) ;

      bool advance(size_t n) ;
      
       
 
      template<class T>
      bool put_template(const T& data) ;

      template<class T>
      bool put_template(const T* data, size_t n) ;

      template<class T>
      bool advance_template(size_t n) ;
      
      void* ptr(void) ;
            
      size_t remain(void) ;
      size_t count(void) ;
      size_t size(void) ;
                            
      ~CBufferOut() ;
       char* begin ;
       char* current ;
       char* end;
       size_t count_ ;
       size_t size_ ;
       bool own ; 
    } ;
    

}

//#include "buffer_out_impl.hpp" 


#endif
