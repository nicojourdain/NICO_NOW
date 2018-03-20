#ifndef __XIOS_ATTRIBUTE_ENUM_IMPL_HPP__
#define __XIOS_ATTRIBUTE_ENUM_IMPL_HPP__

#include "enum.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"
#include "generate_interface.hpp"
#include "attribute_enum.hpp"

  
namespace xios
{

      /// ////////////////////// Définitions ////////////////////// ///
      template <class T>
         CAttributeEnum<T>::CAttributeEnum(const StdString & id)
         : CAttribute(id)
      { /* Ne rien faire de plus */ }

      template <class T>
         CAttributeEnum<T>::CAttributeEnum(const StdString & id, const T_enum & value)
         : CAttribute(id)
      {
         this->setValue(value);
      }

      template <class T>
      CAttributeEnum<T>::CAttributeEnum(const StdString & id,
                              xios_map<StdString, CAttribute*> & umap)
         : CAttribute(id)
      {
         umap.insert(umap.end(), std::make_pair(id, this));
      }

      template <class T>
         CAttributeEnum<T>::CAttributeEnum
            (const StdString & id, const T_enum & value,
             xios_map<StdString, CAttribute*> & umap)
         : CAttribute(id)
      {
         this->setValue(value);
         umap.insert(umap.end(), std::make_pair(id, this));
      }

      ///--------------------------------------------------------------

      template <class T>
      typename T::t_enum CAttributeEnum<T>::getValue(void) const
      {
         return CEnum<T>::get() ;
      }

      template <class T>
         string CAttributeEnum<T>::getStringValue(void) const
      {
         return CEnum<T>::toString() ;
      }

      template <class T>
         void CAttributeEnum<T>::setValue(const typename T::t_enum & value)
      {
         CEnum<T>::set(value) ;
      }

    template <class T>
    void CAttributeEnum<T>::set(const CAttribute& attr)
    {
      this->set(dynamic_cast<const CAttributeEnum<T>& >(attr)) ;
    } 

   template <class T>
    void CAttributeEnum<T>::set(const CAttributeEnum& attr)
    {
      CEnum<T>::set(attr) ;
    } 

    
    template <class T>
    void CAttributeEnum<T>::setInheritedValue(const CAttribute& attr)
    {
      this->setInheritedValue(dynamic_cast<const CAttributeEnum<T>& >(attr)) ;
    } 

    template <class T>
    void CAttributeEnum<T>::setInheritedValue(const CAttributeEnum& attr)
    {
      if (this->isEmpty() && attr.hasInheritedValue()) inheritedValue.set(attr.getInheritedValue()) ;
    } 

    template <class T>
    typename T::t_enum CAttributeEnum<T>::getInheritedValue(void) const
    {
      if (this->isEmpty()) return inheritedValue.get() ;
      else return getValue() ;
    } 
    
    template <class T>
    string CAttributeEnum<T>::getInheritedStringValue(void) const
    {
       return CEnum<T>::toString() ;
       if (this->isEmpty()) return inheritedValue.toString() ;
       else return CEnum<T>::toString() ; ;
    }
    
    template <class T>
    bool CAttributeEnum<T>::hasInheritedValue(void) const
    {
      return !this->isEmpty() || !inheritedValue.isEmpty() ;
    } 
    
      //---------------------------------------------------------------

      template <class T>
      CAttributeEnum<T>& CAttributeEnum<T>::operator=(const T_enum & value)
      {
         this->setValue(value);
         return *this;
      }

      //---------------------------------------------------------------

      template <class T>
         StdString CAttributeEnum<T>::_toString(void) const
      {
         StdOStringStream oss;
         if (!CEnum<T>::isEmpty() && this->hasId())
            oss << this->getName() << "=\"" << CEnum<T>::toString() << "\"";
         return (oss.str());
      }

      template <class T>
         void CAttributeEnum<T>::_fromString(const StdString & str)
      {
        CEnum<T>::fromString(str) ;
      }

      template <class T>
      bool CAttributeEnum<T>::_toBuffer (CBufferOut& buffer) const
      {
         return CEnum<T>::toBuffer(buffer) ;
      }

      template <class T>
      bool CAttributeEnum<T>::_fromBuffer(CBufferIn& buffer)
      {
        return CEnum<T>::fromBuffer(buffer) ;
      }

      template <typename T>
      void CAttributeEnum<T>::generateCInterface(ostream& oss,const string& className)
      {
        CInterface::AttributeCInterface<CEnumBase>(oss, className, this->getName()) ;
      }
      
      template <typename T>
      void CAttributeEnum<T>::generateFortran2003Interface(ostream& oss,const string& className)
      {
        CInterface::AttributeFortran2003Interface<string>(oss, className, this->getName()) ;
      }
      
      template <typename T>
      void CAttributeEnum<T>::generateFortranInterfaceDeclaration_(ostream& oss,const string& className)
      {
        CInterface::AttributeFortranInterfaceDeclaration<string>(oss, className, this->getName()+"_") ;
      }
 
      template <typename T>
      void CAttributeEnum<T>::generateFortranInterfaceBody_(ostream& oss,const string& className)
      {
        CInterface::AttributeFortranInterfaceBody<string>(oss, className, this->getName()) ;
      }

      template <typename T>
      void CAttributeEnum<T>::generateFortranInterfaceDeclaration(ostream& oss,const string& className)
      {
        CInterface::AttributeFortranInterfaceDeclaration<string>(oss, className, this->getName()) ;
      }
      
      template <typename T>
      void CAttributeEnum<T>::generateFortranInterfaceGetDeclaration_(ostream& oss,const string& className)
      {
        CInterface::AttributeFortranInterfaceGetDeclaration<string>(oss, className, this->getName()+"_") ;
      }
 
      template <typename T>
      void CAttributeEnum<T>::generateFortranInterfaceGetBody_(ostream& oss,const string& className)
      {
        CInterface::AttributeFortranInterfaceGetBody<string>(oss, className, this->getName()) ;
      }

      template <typename T>
      void CAttributeEnum<T>::generateFortranInterfaceGetDeclaration(ostream& oss,const string& className)
      {
        CInterface::AttributeFortranInterfaceGetDeclaration<string>(oss, className, this->getName()) ;
      }
 
} // namespace xios

#endif // __XIOS_ATTRIBUTE_ENUM_IMPL_HPP__
