#ifndef __XIOS_ATTRIBUTE_ENUM__
#define __XIOS_ATTRIBUTE_ENUM__

/// xios headers ///
#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "attribute.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"
#include "type.hpp"
#include "enum.hpp"


namespace xios
{
      /// ////////////////////// Déclarations ////////////////////// ///
      template <class T>
         class CAttributeEnum : public CAttribute, public CEnum<T>
      {
        typedef typename T::t_enum T_enum ;
        public :

            /// Constructeurs ///
            explicit CAttributeEnum(const StdString & id);
            CAttributeEnum(const StdString & id,
                               xios_map<StdString, CAttribute*> & umap);
            CAttributeEnum(const StdString & id, const T_enum & value);
            CAttributeEnum(const StdString & id, const T_enum & value,
                               xios_map<StdString, CAttribute*> & umap);

            /// Accesseur ///
            T_enum getValue(void) const;
            string getStringValue(void) const;


            /// Mutateurs ///
            void setValue(const T_enum & value);
            
            void set(const CAttribute& attr) ;
            void set(const CAttributeEnum& attr) ;
            
            void setInheritedValue(const CAttributeEnum& attr );
            void setInheritedValue(const CAttribute& attr );
            T_enum getInheritedValue(void)  const;
            string getInheritedStringValue(void) const;
            bool hasInheritedValue(void) const;          
          
            /// Destructeur ///
            virtual ~CAttributeEnum(void) { }

            /// Operateur ///
            CAttributeEnum& operator=(const T_enum & value);

            /// Autre ///
            virtual StdString toString(void) const { return _toString();}
            virtual void fromString(const StdString & str) { _fromString(str);}

            virtual bool toBuffer  (CBufferOut& buffer) const { return _toBuffer(buffer);} 
            virtual bool fromBuffer(CBufferIn& buffer) { return _fromBuffer(buffer); } 
            
            virtual void generateCInterface(ostream& oss,const string& className) ;
            virtual void generateFortran2003Interface(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceDeclaration_(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceBody_(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceDeclaration(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceGetDeclaration_(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceGetBody_(ostream& oss,const string& className) ;
            virtual void generateFortranInterfaceGetDeclaration(ostream& oss,const string& className) ;

      
         protected :

            /// Constructeurs ///
//            CAttributeTemplate(void); // Not implemented.
         private :
          StdString _toString(void) const;
          void _fromString(const StdString & str);
          bool _toBuffer  (CBufferOut& buffer) const;
          bool _fromBuffer(CBufferIn& buffer) ;
          CEnum<T> inheritedValue ;
      }; // class CAttributeEnum    
   
} // namespace xios

#endif // __XIOS_ATTRIBUTE_ENUM__
