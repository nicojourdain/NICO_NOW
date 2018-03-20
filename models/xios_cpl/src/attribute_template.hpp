#ifndef __XMLIO_CAttributeTemplate__
#define __XMLIO_CAttributeTemplate__

/// boost headers ///
#include <boost/lexical_cast.hpp>

/// xios headers ///
#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "attribute.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"
#include "type.hpp"


namespace xios
{
      /// ////////////////////// Déclarations ////////////////////// ///
      template <class T>
         class CAttributeTemplate : public CAttribute, public CType<T>
      {
            typedef CAttribute SuperClass;

         public :

            /// Typedef ///
            typedef T ValueType;

            /// Constructeurs ///
            explicit CAttributeTemplate(const StdString & id);
            CAttributeTemplate(const StdString & id,
                               xios_map<StdString, CAttribute*> & umap);
            CAttributeTemplate(const StdString & id, const ValueType & value);
            CAttributeTemplate(const StdString & id, const ValueType & value,
                               xios_map<StdString, CAttribute*> & umap);
//            CAttributeTemplate(const CAttribute & attribut) throw (CException);
//            CAttributeTemplate(const CAttribute * const attribut); // Not implemented.

          public :

            /// Accesseur ///
            ValueType getValue(void) const;
//            ValueType* getRef(void) ;

            /// Mutateurs ///
            void setValue(const ValueType & value);
            
            void set(const CAttribute& attr) ;
            void set(const CAttributeTemplate& attr) ;

            void setInheritedValue(const CAttributeTemplate& attr );
            void setInheritedValue(const CAttribute& attr );
            T getInheritedValue(void) const ;
            bool hasInheritedValue(void) const;
            
            /// Destructeur ///
            virtual ~CAttributeTemplate(void) { }

            /// Operateur ///
            CAttributeTemplate& operator=(const ValueType & value);

            /// Autre ///
            virtual StdString toString(void) const { return _toString();}
            virtual void fromString(const StdString & str) { _fromString(str);}
//            virtual CAttributeTemplate* clone() const {}
//            virtual void toBinary  (StdOStream & os) const;
//            virtual void fromBinary(StdIStream & is);            

            virtual bool toBuffer  (CBufferOut& buffer) const { return _toBuffer(buffer);} 
            virtual bool fromBuffer(CBufferIn& buffer) { return _fromBuffer(buffer); } 
//            virtual size_t size(void) const;
            virtual void generateCInterface(ostream& oss,const string& className) ;
            virtual void generateFortran2003Interface(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceDeclaration_(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceBody_(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceDeclaration(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceGetDeclaration_(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceGetBody_(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceGetDeclaration(ostream& oss,const string& className) ;
//            virtual void generateFortranInterfaceIsDefinedDeclaration_(ostream& oss,const string& className) ;
//            virtual void generateFortranInterfaceIsDefinedBody_(ostream& oss,const string& className) ;
//            virtual void generateFortranInterfaceIsDefinedDeclaration(ostream& oss,const string& className) ;

      
         protected :

            /// Constructeurs ///
//            CAttributeTemplate(void); // Not implemented.
         private :
          StdString _toString(void) const;
          void _fromString(const StdString & str);
          bool _toBuffer  (CBufferOut& buffer) const;
          bool _fromBuffer(CBufferIn& buffer) ;
          
          CType<T> inheritedValue ;
      }; // class CAttribute    
      
   
   template <class T>  void FromBinary(StdIStream & is, T & obj);
   
} // namespace xios

#endif // __XMLIO_CAttributeTemplate__
