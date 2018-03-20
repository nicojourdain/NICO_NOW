#ifndef __XIOS_ENUM_IMPL__
#define __XIOS_ENUM_IMPL__

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
  CEnum<T>::CEnum(void)
  {
    empty=true ;
  }
    
  template <typename T>
  CEnum<T>::CEnum(const T_enum& val)
  {
    empty=true ;
    set(val) ;
  }
  
  template <typename T>
  CEnum<T>::CEnum(const CEnum<T>& type)
  {
    empty=true ;
    set(type) ;
  }

  template <typename T>
  CEnum<T>::CEnum(const CEnum_ref<T>& type)
  {
    empty=true ;
    set(type) ;
  }  

  template <typename T>
  void CEnum<T>::set(const T_enum& val)
  {
    if (empty) 
    { 
      ptrValue = new T_enum(val) ;
      empty=false ;
    }
    else *ptrValue = val ;
  }

  template <typename T>
  void CEnum<T>::set(const CEnum<T>& type)
  {
    if (type.isEmpty()) reset() ;
    else
    {
      if (empty)
      { 
        ptrValue = new T_enum(*type.ptrValue) ;
        empty=false ;
      }
      else *ptrValue = *type.ptrValue ;
    }
  }

  template <typename T>
  void CEnum<T>::set(const CEnum_ref<T>& type)
  {
    if (type.isEmpty()) reset() ;
    else
    {
      if (empty)
      { 
        ptrValue = new T_enum(*type.ptrValue) ;
        empty=false ;
      }
      else *ptrValue = *type.ptrValue ;
    }
  }


  template <typename T>
  typename T::t_enum & CEnum<T>::get(void)
  {
    checkEmpty();
    return *ptrValue ;
  }

  template <typename T>
  const typename T::t_enum& CEnum<T>::get(void) const
  {
    checkEmpty();
    return *ptrValue ;
  }
  
  template <typename T>
  CEnum<T>& CEnum<T>::operator = (const T_enum& val)
  {
    set(val) ;
    return *this ;
  }
  
  template <typename T>
  CEnum<T>& CEnum<T>::operator = (const CEnum<T>& type)
  {
    set(type) ;
    return *this ;
  }

  template <typename T>
  CEnum<T>& CEnum<T>::operator = (const CEnum_ref<T>& type)
  {
    set(type) ;
    return *this ;
  }


  template <typename T>
  CEnum<T>::operator T_enum&()
  {
    checkEmpty();
    return *ptrValue ;
   }

   template <typename T>
   CEnum<T>* CEnum<T>::_clone(void) const
   {
     checkEmpty();
     return new CEnum(*this) ;
   }

  
  template <class T>
  void CEnum<T>::_fromString(const string& str)
  {
    string tmpStr=boost::to_lower_copy(boost::trim_copy(str)) ;
    
    bool found=false ;
    for(int i=0;i<T::getSize();i++)
    {
      if (boost::to_lower_copy(string(T::getStr()[i]))==tmpStr)
      {
        allocate() ;
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
    
    ERROR("template <typename T> void CEnum<T>::_fromString(const string& str)",
    << tmpStr << " cannot be converted in a valid enumeration, possibilities are : "<<strList.str() ) ;

  }

  template <typename T>
  size_t CEnum<T>::_size(void) const
  {
    return sizeof(T_enum) ;
  }
  
  template <typename T>
  bool CEnum<T>::_isEmpty(void) const
  {
    return empty ;
  }
  
  template <typename T>
  string CEnum<T>::_toString(void) const
  {
    if (empty) return string("empty") ;
    return string((T::getStr())[(int)(*ptrValue)]) ;
  }
  
  template <typename T>
  bool CEnum<T>::_toBuffer(CBufferOut& buffer) const
  {
    checkEmpty();
    if (sizeof(*ptrValue)==sizeof(short int)) return buffer.put((short int) *ptrValue) ;
    else if (sizeof(*ptrValue)==sizeof(int)) return buffer.put((int) *ptrValue) ;
    else if (sizeof(*ptrValue)==sizeof(long int)) return buffer.put((long int) *ptrValue) ;
    else ERROR("template <typename T>  bool CEnum<T>::_toBuffer(CBufferOut& buffer) const",
               <<"incompatibility between enumeration and standard integer type") ;
    return false ;
  }
  
  template <typename T>
  bool CEnum<T>::_fromBuffer(CBufferIn& buffer)
  {
    bool ret ;
    allocate() ;
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
    else ERROR("template <typename T>  bool CEnum<T>::_fromBuffer(CBufferIn& buffer)",
               <<"incompatibility between enumeration and standard integer type") ;
    return ret ;
  }
 

  template <typename T>
  void CEnum<T>::allocate(void)
  {
    if (empty) 
    {
      ptrValue = new T_enum ;
      empty=false ;
    }
  }
  
  template <typename T>
  void CEnum<T>::_reset(void)
  {
    if (!empty) 
    {
      delete ptrValue ;
      empty=true ;
    }
  }
  
  template <typename T>
  void CEnum<T>::checkEmpty(void) const
  {
    if (empty) ERROR("template <typename T> void CEnum<T>::checkEmpty(void) const", <<"Type is not initialized") ;
  }  

  
  template <typename T>
  CBufferOut& operator<<(CBufferOut& buffer, const CEnum<T>& type)
  {
    if (!type.toBuffer(buffer)) ERROR("template <typename T> CBufferOut& operator<<(CBufferOut& buffer, const CEnum<T>& type)",
                                           <<"Buffer remain size is to low for size type") ;
    return buffer ;
  }

  template <typename T>
  CBufferOut& operator<<(CBufferOut& buffer, const typename T::t_enum & type)
  {
    if (!CEnum<T>(type).toBuffer(buffer)) ERROR("template <typename T> CBufferOut& operator<<(CBufferOut& buffer, const typename T::t_enum & type)",
                                           <<"Buffer remain size is to low for size type") ;      
    return buffer ;
  }
  
  template <typename T>
  CBufferIn& operator>>(CBufferIn& buffer, CEnum<T>& type)
  {
    if (! type.fromBuffer(buffer)) ERROR("template <typename T> CBufferIn& operator>>(CBufferIn& buffer, CEnum<T>& type)",
                                           <<"Buffer remain size is to low for size type") ;
    return buffer ;
  }
  
/* 
  template <typename T>
  CMessage& operator<<(CMessage& msg, const CEnum<T>& type)
  {
    msg.push(*type.clone()) ;
    return msg ;
  }
*/

  template <typename T>
  CMessage& operator<<(CMessage& msg, const typename T::t_enum & type)
  {
    msg.push(*CEnum<T>(type).clone()) ;
    return msg ;
  }
  
}

#endif

