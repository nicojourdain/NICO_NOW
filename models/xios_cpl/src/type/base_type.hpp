#ifndef __BASE_TYPE_HPP__
#define __BASE_TYPE_HPP__

#include "xmlioserver_spl.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"

namespace xios
{

  class CBaseType
  {
    public:
    
    CBaseType(void) {}
    virtual ~CBaseType() {}
    virtual void fromString(const string& str) =0 ;
    virtual string toString(void) const =0;
    
    virtual bool fromBuffer(CBufferIn& buffer) =0;
    virtual bool toBuffer(CBufferOut& buffer) const =0;
    virtual CBaseType* clone(void) const =0;
    virtual size_t size(void) const =0;
    virtual bool isEmpty(void) const =0;
    virtual void reset(void) =0;
  } ;

}

#endif
