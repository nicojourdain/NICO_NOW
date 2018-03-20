#ifndef __BUFFER_IN_HPP__
#define __BUFFER_IN_HPP__


#include "xmlioserver_spl.hpp"

namespace xios
{
  
    class CBufferIn
    {
    
      public:
      
      CBufferIn(size_t size) ;
      CBufferIn(void) ;
      CBufferIn(void* buffer,size_t size) ;

      void realloc(size_t size) ;
      void realloc(void* buffer,size_t size) ;
      

      template<class T>
      bool advance(size_t n) ;

      bool advance(size_t n) ;
      
      template<class T>
      bool get(T& data) ;

      template<class T>
      bool get(T* data, size_t n) ;

      template<class T>
      bool advance_template(size_t n) ;
      
      template<class T>
      bool get_template(T& data) ;

      template<class T>
      bool get_template(T* data, size_t n) ;

      void* ptr(void) ;

      
      size_t remain(void) ;
      size_t count(void) ;                          
      size_t size(void) ;                         
      ~CBufferIn() ;

       char* begin ;
       char* end ;
       char* current ;
       size_t count_ ;
       size_t size_ ;   
       bool own ; 
    } ;
  
}

//#include "buffer_in_impl.hpp" 


#endif
