#include "object.hpp"

namespace xios
{
   /// ////////////////////// Définitions ////////////////////// ///

   CObject::CObject(void)
      : id(), IdDefined(false)
   { /* Ne rien faire de plus */ }

   CObject::CObject(const StdString & id)
      : id(id), IdDefined(true)
   { /* Ne rien faire de plus */ }

   CObject::CObject(const CObject & object)
      : id(object.id), IdDefined(object.IdDefined)
   { /* Ne rien faire de plus */ }

   CObject::~CObject(void)
   { /* Ne rien faire de plus */ }

   const StdString & CObject::getId(void) const 
   { 
      return (this->id);
   }

   bool CObject::hasId(void) const  
   { 
      return (this->IdDefined);
   }

   void CObject::resetId(void) 
   { 
      this->IdDefined = false ;
   }

   void CObject::setId(const StdString & id)
   { 
      this->id = id ;
      this->IdDefined = true ;
   }
/*
   bool CObject::operator==(const CObject & other) const
   {
      if(!this->hasId() || !other.hasId())
         return (false);
      return (this->id.compare(other.id) == 0);
   }

   bool CObject::operator!=(const CObject & other) const
   { 
      return (!(*this == other));
   }
*/
   StdOStream & operator << (StdOStream & os, const CObject & object)
   { 
      os << object.toString();
      return (os);
   }

} // namespace xios
