#ifndef __XIOS_TYPE_IMPL__
#define __XIOS_TYPE_IMPL__

#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"
#include "message.hpp"




namespace xios
{
 
  using namespace std;
  
  template <typename T>
  CType<T>::CType(void)
  {
    empty=true ;
  }
    
  template <typename T>
  CType<T>::CType(const T& val)
  {
    empty=true ;
    set(val) ;
  }
  
  template <typename T>
  CType<T>::CType(const CType<T>& type)
  {
    empty=true ;
    set(type) ;
  }

  template <typename T>
  CType<T>::CType(const CType_ref<T>& type)
  {
    empty=true ;
    set(type) ;
  }  
  
  template <typename T>
  void CType<T>::set(const T& val)
  {
    if (empty) 
    { 
      ptrValue = new T(val) ;
      empty=false ;
    }
    else *ptrValue = val ;
  }

  template <typename T>
  void CType<T>::set(const CType<T>& type)
  {
    if (type.isEmpty()) reset() ;
    else
    {
      if (empty)
      { 
        ptrValue = new T(*type.ptrValue) ;
        empty=false ;
      }
      else *ptrValue = *type.ptrValue ;
    }
  }

  template <typename T>
  void CType<T>::set(const CType_ref<T>& type)
  {
    if (type.isEmpty()) reset() ;
    else
    {
      if (empty)
      { 
        ptrValue = new T(*type.ptrValue) ;
        empty=false ;
      }
      else *ptrValue = *type.ptrValue ;
    }
  }

  template <typename T>
  T& CType<T>::get(void)
  {
    checkEmpty();
   return *ptrValue ;
  }

  template <typename T>
  const T& CType<T>::get(void) const
  {
    checkEmpty();
    return *ptrValue ;
  }
  
  template <typename T>
  CType<T>& CType<T>::operator = (const T& val)
  {
    set(val) ;
    return *this ;
  }
  
  template <typename T>
  CType<T>& CType<T>::operator = (const CType<T>& type)
  {
    set(type) ;
    return *this ;
  }
  
  template <typename T>
  CType<T>& CType<T>::operator = (const CType_ref<T>& type)
  {
    set(type) ;
    return *this ;
  }
  
   template <typename T>
   CType<T>::operator T&()
   {
    checkEmpty();
    return *ptrValue ;
   }

   template <typename T>
   CType<T>* CType<T>::_clone(void) const
   {
     checkEmpty();
     return new CType(*this) ;
   }

  
  template <typename T>
  void CType<T>::_fromString(const string& str)
  {
    istringstream iss(str);
    allocate() ;
    iss>>*ptrValue ;
  }

  template <typename T>
  size_t CType<T>::_size(void) const
  {
    return sizeof(T) ;
  }
  
  template <typename T>
  bool CType<T>::_isEmpty(void) const
  {
    return empty ;
  }
  
  template <typename T>
  string CType<T>::_toString(void) const
  {
    ostringstream oss;
    checkEmpty();
    oss<<*ptrValue ;
    return oss.str() ;
  }
  
  template <typename T>
  bool CType<T>::_toBuffer(CBufferOut& buffer) const
  {
    checkEmpty();
    return buffer.put(*ptrValue) ;
  }
  
  template <typename T>
  bool CType<T>::_fromBuffer(CBufferIn& buffer)
  {
    allocate() ;
    return buffer.get(*ptrValue) ;
  }
 

  template <typename T>
  void CType<T>::allocate(void)
  {
    if (empty) 
    {
      ptrValue = new T ;
      empty=false ;
    }
  }
  
  template <typename T>
  void CType<T>::_reset(void)
  {
    if (!empty) 
    {
      delete ptrValue ;
      empty=true ;
    }
  }
  
  template <typename T>
  void CType<T>::checkEmpty(void) const
  {
    if (empty) ERROR("template <typename T> void CTypef<T>::checkEmpty(void) const", <<"Type is not initialized") ;
  }  

  
  template <typename T>
  CBufferOut& operator<<(CBufferOut& buffer, const CType<T>& type)
  {
    if (!type.toBuffer(buffer)) ERROR("CBuffer& operator<<(CBuffer& buffer, CType<T>& type)",
                                           <<"Buffer remain size is to low for size type") ;
    return buffer ;
  }


  template <typename T>
  CBufferOut& operator<<(CBufferOut& buffer, const T& type)
  {
    if (!CType<T>(type).toBuffer(buffer)) ERROR("operator<<(CBuffer& buffer, const T& type)",
                                           <<"Buffer remain size is to low for size type") ;      
    return buffer ;
  }
  
  template <typename T>
  CBufferIn& operator>>(CBufferIn& buffer, CType<T>& type)
  {
    if (! type.fromBuffer(buffer)) ERROR("CBuffer& operator<<(CBuffer& buffer, CType<T>& type)",
                                           <<"Buffer remain size is to low for size type") ;
    return buffer ;
  }
  
/*  
  template <typename T>
  CMessage& operator<<(CMessage& msg, const CType<T>& type)
  {
    msg.push(*type.clone()) ;
    return msg ;
  }
*/
  template <typename T>
  CMessage& operator<<(CMessage& msg, const T& type)
  {
    msg.push(CType<T>(type)) ;
    return msg ;
  }

}

#endif
