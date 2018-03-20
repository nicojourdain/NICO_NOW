#ifndef __XMLIO_CAttribute__
#define __XMLIO_CAttribute__

/// boost headers ///
#include <boost/any.hpp>

/// xios headers ///
#include "xmlioserver_spl.hpp"
#include "object.hpp"
#include "base_type.hpp"
#include "message.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"

namespace xios
{
      /// ////////////////////// Déclarations ////////////////////// ///
      class CAttribute : public CObject, public virtual CBaseType
      {
            typedef CObject SuperClass;

         public :

            /// Constructeurs ///
            explicit CAttribute(const StdString & id);
//            CAttribute(const CAttribute & attribut);
//            CAttribute(const CAttribute * const attribut); // Not implemented.

            /// Accesseurs ///
            const StdString & getName(void) const;
//            const boost::any & getAnyValue(void) const;
//            template <typename T> inline T getValue(void) const;
//            template <typename T> inline T* getRef(void);

//            /// Mutateurs ///
//            template <typename T> inline void setValue(const T & value);
//            void setAnyValue(const boost::any & value);
//            void clear(void);

            /// Test ///
//            bool isEmpty(void) const;
//            template <typename T> inline bool isType(void) const;
            virtual void set(const CAttribute& ) =0 ;
            /// Destructeur ///
            virtual ~CAttribute(void);

            /// Autres ///
            virtual StdString toString(void) const = 0;
            virtual void fromString(const StdString & str) = 0;

//            virtual void toBinary  (StdOStream & os) const = 0;
//            virtual void fromBinary(StdIStream & is) = 0;

            virtual void generateCInterface(ostream& oss, const string& className) = 0 ;
            virtual void generateCInterfaceIsDefined(ostream& oss, const string& className) ;
            virtual void generateFortran2003Interface(ostream& oss, const string& className) = 0 ;
            virtual void generateFortran2003InterfaceIsDefined(ostream& oss, const string& className) ;
            virtual void generateFortranInterfaceDeclaration_(ostream& oss,const string& className) = 0 ;
            virtual void generateFortranInterfaceDeclaration(ostream& oss,const string& className) = 0 ;
            virtual void generateFortranInterfaceBody_(ostream& oss,const string& className) = 0 ;
            virtual void generateFortranInterfaceGetDeclaration_(ostream& oss,const string& className) = 0 ;
            virtual void generateFortranInterfaceGetDeclaration(ostream& oss,const string& className) = 0 ;
            virtual void generateFortranInterfaceGetBody_(ostream& oss,const string& className) = 0 ;
            virtual void generateFortranInterfaceIsDefinedDeclaration_(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceIsDefinedDeclaration(ostream& oss,const string& className)  ;
            virtual void generateFortranInterfaceIsDefinedBody_(ostream& oss,const string& className) ;

            virtual void setInheritedValue(const CAttribute& ) = 0 ;
            virtual bool hasInheritedValue(void) const = 0;
            
         protected :

            /// Constructeurs ///
//            CAttribute(void);  // Not implemented.

            /// Propriété ///
//            boost::any value;

      }; // class CAttribute

      /// ////////////////////// Définitions ////////////////////// ///
/*
      template <typename T>
         T CAttribute::getValue(void) const
      { 
         return (boost::any_cast<T>(this->value)); 
      }

      template <typename T>
         T* CAttribute::getRef(void)
      { 
         return (boost::any_cast<T>(&value)); 
      }

      template <typename T>
         void CAttribute::setValue(const T & value)
      { 
         this->value = value; 
      }

      template<typename T>
         bool CAttribute::isType(void) const
      { 
         return (this->value.type() == typeid(T)); 
      }
*/
 
   CMessage& operator<<(CMessage& msg,CAttribute& type) ;
   CMessage& operator<<(CMessage& msg, const CAttribute&  type) ;
 
   CBufferOut& operator<<(CBufferOut& buffer,CAttribute& type) ;
   CBufferIn& operator>>(CBufferIn& buffer, CAttribute&  type) ;

}
  // namespace xios

#endif // __XMLIO_CAttribute__
