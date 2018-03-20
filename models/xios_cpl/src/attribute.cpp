#include "attribute.hpp"
#include "base_type.hpp"
#include "generate_interface.hpp"


namespace xios
{
      /// ////////////////////// Définitions ////////////////////// ///
      CAttribute::CAttribute(const StdString & id)
         : CObject(id), CBaseType()
//         , value()
      { /* Ne rien faire de plus */ }
/*
      CAttribute::CAttribute(const CAttribute & attribut)
         : CObject(attribut.getId()),CBaseType()
      { 
 //        this->value = attribut.getAnyValue(); 
      }
*/      
      CAttribute::~CAttribute(void)
      { /* Ne rien faire de plus */ }
      
      ///--------------------------------------------------------------
/*
      const boost::any & CAttribute::getAnyValue(void) const
      { 
         return (this->value); 
      }


      void CAttribute::setAnyValue(const boost::any & value)
      { 
         this->value = value; 
      }
      
      void CAttribute::clear(void)
      {
         this->value = boost::any(); 
      }

      //---------------------------------------------------------------

      bool CAttribute::isEmpty(void) const
      { 
         return (this->value.empty()); 
      }
*/
      const StdString & CAttribute::getName(void) const
      { 
         return (this->getId()); 
      }
   
      void CAttribute::generateCInterfaceIsDefined(ostream& oss, const string& className)
      {
        CInterface::AttributeIsDefinedCInterface(oss, className, this->getName()) ;
      }
      
      void CAttribute::generateFortran2003InterfaceIsDefined(ostream& oss, const string& className)
      {
        CInterface::AttributeIsDefinedFortran2003Interface(oss, className, this->getName()) ;
      }
      
      
      void CAttribute::generateFortranInterfaceIsDefinedDeclaration_(ostream& oss,const string& className)
      {
        CInterface::AttributeFortranInterfaceIsDefinedDeclaration(oss, className, this->getName()+"_") ;
      }

      void CAttribute::generateFortranInterfaceIsDefinedDeclaration(ostream& oss,const string& className)
      {
        CInterface::AttributeFortranInterfaceIsDefinedDeclaration(oss, className, this->getName()) ;
      }

      void CAttribute::generateFortranInterfaceIsDefinedBody_(ostream& oss,const string& className)
      {
        CInterface::AttributeFortranInterfaceIsDefinedBody(oss, className, this->getName()) ;
      }

      ///--------------------------------------------------------------


      CMessage& operator<<(CMessage& msg,CAttribute& type)
      {
        msg.push(type) ;
        return msg ;
      }

     CMessage& operator<<(CMessage& msg, const CAttribute&  type)
     {
//       msg.push(*type.clone()) ;
       return msg ;
     }
 
      CBufferOut& operator<<(CBufferOut& buffer, CAttribute&  type)
     {
    
       if (!type.toBuffer(buffer)) ERROR("CBufferOut& operator<<(CBufferOut& buffer, CAttribute&  type)",
                                           <<"Buffer remain size is to low for size type") ;
      return buffer ;
     }
     
     CBufferIn& operator>>(CBufferIn& buffer, CAttribute&  type)
     {
    
       if (!type.fromBuffer(buffer)) ERROR("CBufferInt& operator>>(CBufferIn& buffer, CAttribute&  type)",
                                           <<"Buffer remain size is to low for size type") ;
       return buffer ;
     }

 
} // namespace xios
