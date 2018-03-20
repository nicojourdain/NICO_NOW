#ifndef __MESSAGE_HPP__
#define __MESSAGE_HPP__

#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "base_type.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"


namespace xios
{

  class CMessage
  {
    public:
    
    CMessage(void) ;
    list<CBaseType*> typeList ;
    virtual bool fromBuffer(CBufferIn& buffer) const;
    virtual bool toBuffer(CBufferOut& buffer) const;
    virtual size_t size(void) const;
  
    CMessage& push(const CBaseType& type) ; 
//    CMessage& push(CBaseType& type) ;
    void clear(void) ;  
    ~CMessage() ;    
  } ;
  
  CBufferOut& operator<<(CBufferOut& buffer, CMessage& msg) ;
  CBufferIn& operator>>(CBufferIn& buffer, CMessage& msg) ;

//  CMessage& operator<<(CMessage& msg,CBaseType& type) ;
  CMessage& operator<<(CMessage& msg,const CBaseType& type) ;
 
}

#endif

