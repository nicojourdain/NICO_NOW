#ifndef __XIOS_ATTRIBUTE_ARRAY_IMPL_HPP__
#define __XIOS_ATTRIBUTE_ARRAY_IMPL_HPP__

#include "buffer_in.hpp"
#include "buffer_out.hpp"
#include "generate_interface.hpp"
#include "attribute_array.hpp"

  
namespace xios
{
      /// ////////////////////// Définitions ////////////////////// ///
      template <typename T_numtype, int N_rank>
      CAttributeArray<T_numtype, N_rank>::CAttributeArray(const StdString & id)
         : CAttribute(id)
      { /* Ne rien faire de plus */ }

      template <typename T_numtype, int N_rank>
      CAttributeArray<T_numtype,N_rank>::CAttributeArray(const StdString & id, const CArray<T_numtype,N_rank>& value)
         : CAttribute(id)
      {
         this->setValue(value);
      }

      template <typename T_numtype, int N_rank>
      CAttributeArray<T_numtype, N_rank>::CAttributeArray(const StdString & id, xios_map<StdString, CAttribute*> & umap)
         : CAttribute(id)
      {
         umap.insert(umap.end(), std::make_pair(id, this));
      }

      template <typename T_numtype, int N_rank>
      CAttributeArray<T_numtype, N_rank>::CAttributeArray (const StdString & id, const CArray<T_numtype,N_rank>& value,
                                                           xios_map<StdString, CAttribute*> & umap)
         : CAttribute(id)
      {
         this->setValue(value);
         umap.insert(umap.end(), std::make_pair(id, this));
      }

      ///--------------------------------------------------------------

      template <typename T_numtype, int N_rank>
      CArray<T_numtype,N_rank> CAttributeArray<T_numtype, N_rank>::getValue(void) const
      {
        return this->copy() ;
      }

      template <typename T_numtype, int N_rank>
      void CAttributeArray<T_numtype,N_rank>::setValue(const CArray<T_numtype,N_rank>& value)
      {
        this->resize(value.shape()) ;
        *this=value ;
      }

    template <typename T_numtype, int N_rank>
    void CAttributeArray<T_numtype,N_rank>::set(const CAttribute& attr)
    {
      this->set(dynamic_cast<const CAttributeArray<T_numtype,N_rank>& >(attr)) ;
    } 

    template <typename T_numtype, int N_rank>
    void CAttributeArray<T_numtype,N_rank>::set(const CAttributeArray& attr)
    {
      this->setValue(attr) ;
    } 
    
    
    template <typename T_numtype, int N_rank>
    void CAttributeArray<T_numtype,N_rank>::setInheritedValue(const CAttribute& attr)
    {
      this->setInheritedValue(dynamic_cast<const CAttributeArray<T_numtype,N_rank>& >(attr)) ;
    } 

    template <typename T_numtype, int N_rank>
    void CAttributeArray<T_numtype,N_rank>::setInheritedValue(const CAttributeArray& attr)
    {
      if (this->isEmpty() && attr.hasInheritedValue()) 
      {
        inheritedValue.resize(attr.shape()) ;
        inheritedValue=attr ;
      }
    } 

    template <typename T_numtype, int N_rank>
    CArray<T_numtype,N_rank> CAttributeArray<T_numtype,N_rank>::getInheritedValue(void) const
    {
      if (this->isEmpty()) return inheritedValue.copy() ;
      else return getValue() ;
    } 
    
    template <typename T_numtype, int N_rank>
    bool CAttributeArray<T_numtype,N_rank>::hasInheritedValue(void) const
    {
      return !this->isEmpty() || !inheritedValue.isEmpty() ;
    } 
    

    template <typename T_numtype, int N_rank>
    StdString CAttributeArray<T_numtype,N_rank>::_toString(void) const
    {
      StdOStringStream oss;
      if (! isEmpty() && this->hasId()) oss << this->getName() << "=\"" << CArray<T_numtype, N_rank>::toString() << "\"";
      return (oss.str());
    }

      template <typename T_numtype, int N_rank>
         void CAttributeArray<T_numtype, N_rank>::_fromString(const StdString & str)
      {
        CArray<T_numtype, N_rank>::fromString(str) ;
      }

      template <typename T_numtype, int N_rank>
      bool CAttributeArray<T_numtype, N_rank>::_toBuffer (CBufferOut& buffer) const
      {
         return CArray<T_numtype, N_rank>::toBuffer(buffer) ;
      }

      template <typename T_numtype, int N_rank>
      bool CAttributeArray<T_numtype, N_rank>::_fromBuffer(CBufferIn& buffer)
      {
        return CArray<T_numtype, N_rank>::fromBuffer(buffer) ;
      }

      template <typename T_numtype, int N_rank>
      void CAttributeArray<T_numtype, N_rank>::generateCInterface(ostream& oss,const string& className)
      {
        CInterface::AttributeCInterface<CArray<T_numtype, N_rank> >(oss, className, this->getName()) ;
      }
      
      template <typename T_numtype, int N_rank>
      void CAttributeArray<T_numtype, N_rank>::generateFortran2003Interface(ostream& oss,const string& className)
      {
        CInterface::AttributeFortran2003Interface<CArray<T_numtype, N_rank> >(oss, className, this->getName()) ;
      }
      
      template <typename T_numtype, int N_rank>
      void CAttributeArray<T_numtype, N_rank>::generateFortranInterfaceDeclaration_(ostream& oss,const string& className)
      {
        CInterface::AttributeFortranInterfaceDeclaration<CArray<T_numtype, N_rank> >(oss, className, this->getName()+"_") ;
      }
 
      template <typename T_numtype, int N_rank>
      void CAttributeArray<T_numtype, N_rank>::generateFortranInterfaceBody_(ostream& oss,const string& className)
      {
        CInterface::AttributeFortranInterfaceBody<CArray<T_numtype, N_rank> >(oss, className, this->getName()) ;
      }

      template <typename T_numtype, int N_rank>
      void CAttributeArray<T_numtype, N_rank>::generateFortranInterfaceDeclaration(ostream& oss,const string& className)
      {
        CInterface::AttributeFortranInterfaceDeclaration<CArray<T_numtype, N_rank> >(oss, className, this->getName()) ;
      }
      
      template <typename T_numtype, int N_rank>
      void CAttributeArray<T_numtype, N_rank>::generateFortranInterfaceGetDeclaration_(ostream& oss,const string& className)
      {
        CInterface::AttributeFortranInterfaceGetDeclaration<CArray<T_numtype, N_rank> >(oss, className, this->getName()+"_") ;
      }
 
      template <typename T_numtype, int N_rank>
      void CAttributeArray<T_numtype, N_rank>::generateFortranInterfaceGetBody_(ostream& oss,const string& className)
      {
        CInterface::AttributeFortranInterfaceGetBody<CArray<T_numtype, N_rank> >(oss, className, this->getName()) ;
      }

      template <typename T_numtype, int N_rank>
      void CAttributeArray<T_numtype, N_rank>::generateFortranInterfaceGetDeclaration(ostream& oss,const string& className)
      {
        CInterface::AttributeFortranInterfaceGetDeclaration<CArray<T_numtype, N_rank> >(oss, className, this->getName()) ;
      }
} // namespace xios

#endif // __XIOS_ATTRIBUTE_ENUM_IMPL_HPP__
