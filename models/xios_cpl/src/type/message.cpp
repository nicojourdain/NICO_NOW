#include "xmlioserver_spl.hpp"
#include "message.hpp"
#include "base_type.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"

namespace xios
{

   CMessage::CMessage(void) {}
   
   CMessage& CMessage::push(const CBaseType& type)
   {
     typeList.push_back(type.clone());
     return *this ;
   }
   
//    CMessage& CMessage::push(CBaseType& type)
//   {
//     typeList.push_back(&type);
//     return *this ;
//   }
   
   size_t CMessage::size(void) const
   {
     list<CBaseType*>::const_iterator it; 
     size_t retSize=0 ;
     
     for(it=typeList.begin();it!=typeList.end();it++) retSize+=(*it)->size() ;
     return retSize ;
   }
   
    bool CMessage::fromBuffer(CBufferIn& buffer) const
    {
      list<CBaseType*>::const_iterator it; 
      
      if (buffer.remain()>=size())
      {
        for(it=typeList.begin();it!=typeList.end();it++) (*it)->fromBuffer(buffer) ;
        return true ;
      }
      else
      {
        return false ;
      }
    }
    

    bool CMessage::toBuffer(CBufferOut& buffer) const
    {
      list<CBaseType*>::const_iterator it; 
      if (buffer.remain()>=size())
      {
        for(it=typeList.begin();it!=typeList.end();it++) (*it)->toBuffer(buffer) ;
        return true ;
      }
      else
      {
        return false ;
      }

    }

    
    void CMessage::clear()
    {
      list<CBaseType*>::iterator it; 
      for(it=typeList.begin();it!=typeList.end();it++) delete *it ;
      typeList.clear() ;
    }

    CMessage::~CMessage()
    {
      clear() ;
    }
    

    CBufferOut& operator<<(CBufferOut& buffer, CMessage& msg)
    {
      if (!msg.toBuffer(buffer)) ERROR("CBufferOut& operator<<(CBufferOut& buffer, CMessage& msg)",
                                             <<"Buffer remain size is to low for size type") ;
      return buffer ;
    }

  
    CBufferIn& operator>>(CBufferIn& buffer, CMessage& msg)
    {
      if (!msg.fromBuffer(buffer)) ERROR("CBufferIn& operator>>(CBufferIn& buffer, CMessage& msg)",
                                           <<"Buffer remain size is to low for size type") ;
      return buffer ;
    }

  CMessage& operator<<(CMessage& msg,const CBaseType& type)
 {
    msg.push(type) ;
    return msg ;
  }

//  CMessage& operator<<(CMessage& msg, const CBaseType&  type)
//  {
//    msg.push(*type.clone()) ;
//    return msg ;
//  } 
  
}
