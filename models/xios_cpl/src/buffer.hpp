#ifndef __BUFFER_HPP__
#define __BUFFER_HPP__


#include "xmlioserver_spl.hpp"

namespace xios
{
  
    class CBuffer
    {
    
      public:
      
      CBuffer(size_t size) ;
      CBuffer(void* buffer,size_t size) ;

      void realloc(size_t size) ;
      void realloc(void* buffer,size_t size) ;
      
      template<class T>
      bool put(const T& data) ;

      template<class T>
      bool put(const T* data, size_t n) ;

      template<class T>
      bool put_ptr(const T*& data_ptr, size_t n) ;
      
      template<class T>
      bool get(T& data) ;

      template<class T>
      bool get(T* data, size_t n) ;

      template<class T>
      bool get_ptr(T*& data, size_t n) ;

      
 
      template<class T>
      bool put_template(const T& data) ;

      template<class T>
      bool put_template(const T* data, size_t n) ;

      template<class T>
      bool put_ptr_template(const T*& data, size_t n) ;
      
      template<class T>
      bool get_template(T& data) ;

      template<class T>
      bool get_template(T* data, size_t n) ;

      template<class T>
      bool get_ptr_template(T*& data, size_t n) ;
      
      size_t remain(void) ;
                            
      ~CBuffer() ;
       char* buffer ;
       char* read ;
       char* write ;
       size_t count ;
       bool own ; 
       size_t size ;   
    } ;
    
    
  
 
}

//#include "buffer_impl.hpp" 


#endif
