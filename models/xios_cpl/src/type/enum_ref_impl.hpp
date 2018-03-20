#ifndef __XIOS_ENUM_REF_IMPL__
#define __XIOS_ENUM_REF_IMPL__

#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"
#include "message.hpp"
#include <string>
#include <boost/algorithm/string.hpp>

namespace xios
{
 
    
  using namespace std;
  
  template <typename T>
  CEnum_ref<T>::CEnum_ref(void)
  {
    empty=true ;
  }
    
  template <typename T>
  CEnum_ref<T>::CEnum_ref(T_enum& val)
  {
    empty=true ;
    set_ref(val) ;
  }
  
  template <typename T>
  CEnum_ref<T>::CEnum_ref(CEnum<T>& type)
  {
    empty=true ;
    set_ref(type) ;
  }

  template <typename T>
  CEnum_ref<T>::CEnum_ref(const CEnum_ref<T>& type)
  {
    empty=true ;
    set_ref(type) ;
  }  

  
  template <typename T>
  void CEnum_ref<T>::set_ref(T_enum& val)
  {
    ptrValue=&val ;
    empty=false ;
  }

  template <typename T>
  void CEnum_ref<T>::set_ref(CEnum<T>& type)
  {
    ptrValue=&type.get() ;
    empty=false ;
  }

  template <typename T>
  void CEnum_ref<T>::set_ref(const CEnum_ref<T>& type)
  {
    ptrValue=type.ptrValue ;
    empty=type.empty ;
  }

  template <typename T>
  void CEnum_ref<T>::set(const T_enum& val) const
  {
    checkEmpty() ;
    *ptrValue=val ;
  }

  template <typename T>
  void CEnum_ref<T>::set(const CEnum<T>& type) const
  {
    checkEmpty() ;
    *ptrValue=type.get() ;
  }

  template <typename T>
  void CEnum_ref<T>::set(const CEnum_ref<T>& type) const
  {
    checkEmpty() ;
    *ptrValue=type.get() ;
  }
  
  template <typename T>
  typename T::t_enum & CEnum_ref<T>::get(void) const
  {
    checkEmpty() ;
    return *ptrValue ;
  }
  
  template <typename T>
  const CEnum_ref<T>& CEnum_ref<T>::operator = (T_enum& val) const
  {
    set(val) ;
    return *this ;
  }

  template <typename T>
  const CEnum_ref<T>& CEnum_ref<T>::operator = (CEnum<T>& type) const
  {
    set(type) ;
    return *this ;
  }
   
  template <typename T>
  const CEnum_ref<T>& CEnum_ref<T>::operator = (const CEnum_ref<T>& type) const
  {
    set(type) ;
    return *this ;
  }
  
  template <typename T>
  CEnum_ref<T>::operator T_enum&() const
  {
    checkEmpty() ;
    return *ptrValue ;
  }

  template <typename T>
  CEnum_ref<T>* CEnum_ref<T>::_clone(void) const
  {
    checkEmpty() ;
    return new CEnum_ref<T>(*this) ;
  }
   
  template <typename T>
  void CEnum_ref<T>::_fromString(const string& str) const
  {
    istringstream iss(str);
    checkEmpty() ;
    iss>>*ptrValue ;
  }
 
  template <typename T>
  void CEnum_ref<T>::_fromString(const string& str)
  {
    checkEmpty() ;
    string tmpStr=boost::to_lower_copy(boost::trim_copy(str)) ;
    
    bool found=false ;
    for(int i=0;i<T::getSize();i++)
    {
      if (boost::to_lower_copy(string(T::getStr()[i]))==tmpStr)
      {
        *ptrValue=(T_enum) i ;
        return ;
      }
    }
    
    ostringstream strList ;
    for(int i=0;i<T::getSize();i++) 
    {
      if (i>0) strList<<", " ;
      strList<<boost::to_lower_copy(string(T::getStr()[i])) ;
    }
    
    ERROR("template <typename T> void CEnum_ref<T>::_fromString(const string& str)",
    << tmpStr << " cannot be converted in a valid enumeration, possibilities are : "<<strList.str() ) ;
  }
  
  template <typename T>
  string CEnum_ref<T>::_toString(void) const
  {
    if (empty) return string("empty"); 
    return string((T::getStr())[(int)(*ptrValue)]) ;
  }

    
  template <typename T>
  bool CEnum_ref<T>::_toBuffer(CBufferOut& buffer) const
  {
    checkEmpty() ;
    if (sizeof(*ptrValue)==sizeof(short int)) return buffer.put((short int) *ptrValue) ;
    else if (sizeof(*ptrValue)==sizeof(int)) return buffer.put((int) *ptrValue) ;
    else if (sizeof(*ptrValue)==sizeof(long int)) return buffer.put((long int) *ptrValue) ;
    else ERROR("template <typename T>  bool CEnum_ref<T>::_toBuffer(CBufferOut& buffer) const",
               <<"incompatibility between enumeration and standard integer type") ;
    return false ;
  }
  
  template <typename T>
  bool CEnum_ref<T>::_fromBuffer(CBufferIn& buffer)
  {
    checkEmpty() ;
    bool ret ;
    if (sizeof(*ptrValue)==sizeof(short int)) 
    {
      short int val ;
      ret=buffer.get(val) ;
      if (ret) *ptrValue = (T_enum) val ;
    }
    else if (sizeof(*ptrValue)==sizeof(int)) 
    {
      int val ;
      ret=buffer.get(val) ;
      if (ret) *ptrValue = (T_enum) val ;
    }
    else if (sizeof(*ptrValue)==sizeof(long int)) 
    {
      long int val ;
      ret=buffer.get(val) ;
      if (ret) *ptrValue = (T_enum) val ;
    }
    else ERROR("template <typename T>  bool CEnum_ref<T>::_fromBuffer(CBufferIn& buffer)",
               <<"incompatibility between enumeration and standard integer type") ;
    return ret ;
  }
 
  template <typename T>
  bool CEnum_ref<T>::_fromBuffer(CBufferIn& buffer) const
  {
    checkEmpty() ;
    bool ret ;
    if (sizeof(*ptrValue)==sizeof(short int)) 
    {
      short int val ;
      ret=buffer.get(val) ;
      if (ret) *ptrValue = (T_enum) val ;
    }
    else if (sizeof(*ptrValue)==sizeof(int)) 
    {
      int val ;
      ret=buffer.get(val) ;
      if (ret) *ptrValue = (T_enum) val ;
    }
    else if (sizeof(*ptrValue)==sizeof(long int)) 
    {
      long int val ;
      ret=buffer.get(val) ;
      if (ret) *ptrValue = (T_enum) val ;
    }
    else ERROR("template <typename T>  bool CEnum_ref<T>::_fromBuffer(CBufferIn& buffer)",
               <<"incompatibility between enumeration and standard integer type") ;
  }
 
  template <typename T>
  size_t CEnum_ref<T>::_size(void) const
  {
    return sizeof(T_enum) ;
  }
  
  template <typename T>
  bool CEnum_ref<T>::_isEmpty(void) const
  {
    return empty ;
  }
   
  template <typename T>
  void CEnum_ref<T>::_reset(void)
  {
      empty=true ;
  }
  
  template <typename T>
  void CEnum_ref<T>::checkEmpty(void) const
  {
    if (empty) ERROR("template <typename T> void CEnum_ref<T>::checkEmpty(void)",
                     <<"Type_ref reference is not assigned") ;
  }
                     

  
  template <typename T>
  CBufferOut& operator<<(CBufferOut& buffer, const CEnum_ref<T>& type)
  {
    if (!type.toBuffer(buffer)) ERROR("template <typename T> CBufferOut& operator<<(CBufferOut& buffer, const CEnum_ref<T>& type)",
                                           <<"Buffer remain size is to low for size type") ;
    return buffer ;
  }

  template <typename T>
  CBufferOut& operator<<(CBufferOut& buffer, typename T::t_enum& type)
  {
    if (!CEnum_ref<T>(type).toBuffer(buffer)) ERROR("template <typename T> CBufferOut& operator<<(CBufferOut& buffer, const typename T::t_enum& type)",
                                             <<"Buffer remain size is to low for size type") ;      
    return buffer ;
  }

  template <typename T>
  CBufferIn& operator>>(CBufferIn& buffer, typename T::t_enum& type)
  {
    if (! CEnum_ref<T>(type).fromBuffer(buffer)) ERROR("template <typename T>  CBufferIn& operator>>(CBufferIn& buffer, typename T::t_enum& type)",
                                                       <<"Buffer remain size is to low for size type") ;
    return buffer ;
  }

  template <typename T>
  CBufferIn& operator>>(CBufferIn& buffer, const CEnum_ref<T>& type)
  {
    if (! type.fromBuffer(buffer) ) ERROR("  template <typename T> CBufferIn& operator>>(CBufferIn& buffer, const CEnum_ref<T>& type) ",
                                           <<"Buffer remain size is to low for size type") ;
    return buffer ;
  }


/*
  template <typename T>
  CMessage& operator<<(CMessage& msg, const CEnum_ref<T>& type)
  {
    msg.push(*type.clone()) ;
    return msg ;
  }
*/

  template <typename T>
  CMessage& operator<<(CMessage& msg, typename T::t_enum & type)
  {
    msg.push(*CEnum_ref<T>(type).clone()) ;
    return msg ;
  }
  
}

#endif

