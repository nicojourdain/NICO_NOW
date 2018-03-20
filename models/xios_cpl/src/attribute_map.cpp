#include "attribute_map.hpp"
#include "indent.hpp"

namespace xios
{
      /// ////////////////////// Définitions ////////////////////// ///
      CAttributeMap * CAttributeMap::Current = NULL;

      CAttributeMap::CAttributeMap(void)
         : xios_map<StdString, CAttribute*>()
      { CAttributeMap::Current = this; }

      CAttributeMap::~CAttributeMap(void)
      { /* Ne rien faire de plus */ }
      
      ///--------------------------------------------------------------

      void CAttributeMap::clearAllAttributes(void)
      {
         typedef std::pair<StdString, CAttribute*> StdStrAttPair;
         SuperClassMap::const_iterator it = SuperClassMap::begin(), end = SuperClassMap::end();
         for (; it != end; it++)
         {
            const StdStrAttPair & att = *it;
            att.second->reset();
         }
      }

      //---------------------------------------------------------------

      bool CAttributeMap::hasAttribute(const StdString & key) const
      { 
         return (this->find(key) != this->end()); 
      }
      
      //---------------------------------------------------------------
      
      void CAttributeMap::setAttribute(const StdString & key, CAttribute * const attr)
      {
         if (!this->hasAttribute(key))
            ERROR("CAttributeMap::setAttribute(key, attr)",
                   << "[ key = " << key << "] key not found !");
         if (attr == NULL)
            ERROR("CAttributeMap::setAttribute(key, attr)",
                   << "[ key = " << key << "] attr is null !");
         this->find(key)->second->set(*attr) ;
//         this->find(key)->second->setAnyValue(attr->getAnyValue());
      }
      
      //---------------------------------------------------------------
      
      CAttribute * CAttributeMap::operator[](const StdString & key)
      {
         if (!this->hasAttribute(key))
            ERROR("CAttributeMap::operator[](const StdString & key)",
                  << "[ key = " << key << "] key not found !");
         return (SuperClassMap::operator[](key));
      }
      
      //---------------------------------------------------------------
      
      StdString CAttributeMap::toString(void) const
      {
         typedef std::pair<StdString, CAttribute*> StdStrAttPair;
         StdOStringStream oss;
         
         SuperClassMap::const_iterator it = SuperClassMap::begin(), end = SuperClassMap::end();
         for (; it != end; it++)
         {
            const StdStrAttPair & att = *it;
            if (!att.second->isEmpty())
               oss << *att.second << " ";
         }
         return (oss.str());
      }
      
      //---------------------------------------------------------------
      
      void CAttributeMap::fromString(const StdString & str)
      { 
         ERROR("CAttributeMap::fromString(const StdString & str)",
               << "[ str = " << str << "] Not implemented yet !"); 
      }
      
      //---------------------------------------------------------------

      //StdOStream & operator << (StdOStream & os, const CAttributeMap & attributmap)
      //{ os << attributmap.toString(); return (os); }
      
      //---------------------------------------------------------------
      
      void CAttributeMap::setAttributes(const xml::THashAttributes & attributes)
      {
         for (xml::THashAttributes::const_iterator it  = attributes.begin();
                                                   it != attributes.end();
                                                   it ++)
         {
            if ((*it).first.compare(StdString("id")) != 0 &&
                (*it).first.compare(StdString("src"))!= 0)
            {
               //if (CAttributeMap::operator[]((*it).first)->isEmpty())
               CAttributeMap::operator[]((*it).first)->fromString((*it).second);
            }
         }
      }
      
      //---------------------------------------------------------------
      
      void CAttributeMap::setAttributes(const CAttributeMap * const _parent, bool apply)
      {
         typedef std::pair<StdString, CAttribute*> StdStrAttPair;
         
         SuperClassMap::const_iterator it = _parent->begin(), end = _parent->end();
         for (; it != end; it++)
         {
            const StdStrAttPair & el = *it;
            if (this->hasAttribute(el.first))
            {
               CAttribute * currentAtt = CAttributeMap::operator[](el.first);
               CAttribute * parentAtt = el.second ;
               if (apply)
               {
                 if (currentAtt->isEmpty() && !el.second->isEmpty())
                 {
                    this->setAttribute(el.first, el.second);
                 }
               }
               else currentAtt->setInheritedValue(*parentAtt) ;
            }
         }
      }
      
      //---------------------------------------------------------------
/*      
      void CAttributeMap::toBinary(StdOStream & os) const
      {
         typedef std::pair<StdString, CAttribute*> StdStrAttPair;
         SuperClassMap::const_iterator it = this->begin(), end = this->end();
         
         const StdSize nbatt = SuperClassMap::size();
         os.write (reinterpret_cast<const char*>(&nbatt) , sizeof(StdSize));
         
         for (; it != end; it++)
         {
            const StdString & key   = it->first;
            const CAttribute* value = it->second;            
            const StdSize size = key.size();
            
            os.write (reinterpret_cast<const char*>(&size) , sizeof(StdSize));
            os.write (key.data(), size * sizeof(char));
            
            if (!value->isEmpty())
            {
               bool b = true;
               os.write (reinterpret_cast<const char*>(&b) , sizeof(bool));
               value->toBinary(os);
            }
            else 
            {
               bool b = false;
               os.write (reinterpret_cast<const char*>(&b) , sizeof(bool));
            }
         }
      }
      
      //---------------------------------------------------------------
      
      void CAttributeMap::fromBinary(StdIStream & is)
      {
         StdSize nbatt = 0;
         is.read (reinterpret_cast<char*>(&nbatt), sizeof(StdSize));
         
         for (StdSize i = 0; i < nbatt; i++)
         {
            bool hasValue = false;
            StdSize size  = 0;
            is.read (reinterpret_cast<char*>(&size), sizeof(StdSize));
            StdString key(size, ' ');
            is.read (const_cast<char *>(key.data()), size * sizeof(char));
            
            if (!this->hasAttribute(key))
               ERROR("CAttributeMap::fromBinary(StdIStream & is)",
                     << "[ key = " << key << "] key not found !");
                                        
            is.read (reinterpret_cast<char*>(&hasValue), sizeof(bool));
            
            if (hasValue)          
               this->operator[](key)->fromBinary(is);
         }
      }
 */     
      void CAttributeMap::generateCInterface(ostream& oss, const string& className)
      {
         SuperClassMap::const_iterator it = SuperClassMap::begin(), end = SuperClassMap::end();
         for (; it != end; it++)
         {
           it->second->generateCInterface(oss,className) ;
           it->second->generateCInterfaceIsDefined(oss,className) ;
           oss<<iendl<<iendl ;
         }
      }

      void CAttributeMap::generateFortran2003Interface(ostream& oss, const string& className)
      {
         SuperClassMap::const_iterator it = SuperClassMap::begin(), end = SuperClassMap::end();
         for (; it != end; it++)
         {
           it->second->generateFortran2003Interface(oss,className) ;
           it->second->generateFortran2003InterfaceIsDefined(oss,className) ;
          
           oss<<iendl<<iendl ;
         }
      }      
 
      ///--------------------------------------------------------------

      void CAttributeMap::generateFortranInterface_hdl_(ostream& oss, const string& className)
      {
         oss<<"SUBROUTINE xios(set_"<<className<<"_attr_hdl_)   &"<<iendl++ ;
         ostringstream* oss2 ;
         SuperClassMap::const_iterator it ;
         SuperClassMap::const_iterator begin = SuperClassMap::begin(), end = SuperClassMap::end();
         
         oss2=new ostringstream ;
         
         *oss2<<"( "<<className<<"_hdl"  ;
         
         for ( it=begin ; it != end; it++) 
         {
           *oss2<<", "<<it->second->getName()<<"_" ;
           if (oss2->str().size()>90) 
           {
             oss<<oss2->str()<<"  &"<<iendl ;
             delete oss2 ;
             oss2=new ostringstream ;
           }
         }
         *oss2<<" )" ;
         oss<<oss2->str()<<iendl ;
         oss<<iendl ;
         delete oss2 ; 
         
         oss<<"IMPLICIT NONE"<<iendl++ ;
         oss<<"TYPE(txios("<<className<<")) , INTENT(IN) :: "<<className<<"_hdl"<<iendl ;
         
         for (it=begin; it != end; it++)
         {
           it->second->generateFortranInterfaceDeclaration_(oss,className) ;
         }
         
         oss<<iendl ;
         
         for (it=begin; it != end; it++)
         {
           it->second->generateFortranInterfaceBody_(oss,className) ;
           oss<<iendl ;
         }
         
         oss<<iendl--<<iendl-- ;
         oss<<"END SUBROUTINE xios(set_"<<className<<"_attr_hdl_)"<<iendl ;
         
      }      

      void CAttributeMap::generateFortranInterfaceGet_hdl_(ostream& oss, const string& className)
      {
         oss<<"SUBROUTINE xios(get_"<<className<<"_attr_hdl_)   &"<<iendl++ ;
         ostringstream* oss2 ;
         SuperClassMap::const_iterator it ;
         SuperClassMap::const_iterator begin = SuperClassMap::begin(), end = SuperClassMap::end();
         
         oss2=new ostringstream ;
         
         *oss2<<"( "<<className<<"_hdl"  ;
         
         for ( it=begin ; it != end; it++) 
         {
           *oss2<<", "<<it->second->getName()<<"_" ;
           if (oss2->str().size()>90) 
           {
             oss<<oss2->str()<<"  &"<<iendl ;
             delete oss2 ;
             oss2=new ostringstream ;
           }
         }
         *oss2<<" )" ;
         oss<<oss2->str()<<iendl ;
         oss<<iendl ;
         delete oss2 ; 
         
         oss<<"IMPLICIT NONE"<<iendl++ ;
         oss<<"TYPE(txios("<<className<<")) , INTENT(IN) :: "<<className<<"_hdl"<<iendl ;
         
         for (it=begin; it != end; it++)
         {
           it->second->generateFortranInterfaceGetDeclaration_(oss,className) ;
         }
         
         oss<<iendl ;
         
         for (it=begin; it != end; it++)
         {
           it->second->generateFortranInterfaceGetBody_(oss,className) ;
           oss<<iendl ;
         }
         
         oss<<iendl--<<iendl-- ;
         oss<<"END SUBROUTINE xios(get_"<<className<<"_attr_hdl_)"<<iendl ;
         
      }     
      

      void CAttributeMap::generateFortranInterfaceIsDefined_hdl_(ostream& oss, const string& className)
      {
         oss<<"SUBROUTINE xios(is_defined_"<<className<<"_attr_hdl_)   &"<<iendl++ ;
         ostringstream* oss2 ;
         SuperClassMap::const_iterator it ;
         SuperClassMap::const_iterator begin = SuperClassMap::begin(), end = SuperClassMap::end();
         
         oss2=new ostringstream ;
         
         *oss2<<"( "<<className<<"_hdl"  ;
         
         for ( it=begin ; it != end; it++) 
         {
           *oss2<<", "<<it->second->getName()<<"_" ;
           if (oss2->str().size()>90) 
           {
             oss<<oss2->str()<<"  &"<<iendl ;
             delete oss2 ;
             oss2=new ostringstream ;
           }
         }
         *oss2<<" )" ;
         oss<<oss2->str()<<iendl ;
         oss<<iendl ;
         delete oss2 ; 
         
         oss<<"IMPLICIT NONE"<<iendl++ ;
         oss<<"TYPE(txios("<<className<<")) , INTENT(IN) :: "<<className<<"_hdl"<<iendl ;
         
         for (it=begin; it != end; it++)
         {
           it->second->generateFortranInterfaceIsDefinedDeclaration_(oss,className) ;
         }
         
         oss<<iendl ;
         
         for (it=begin; it != end; it++)
         {
           it->second->generateFortranInterfaceIsDefinedBody_(oss,className) ;
           oss<<iendl ;
         }
         
         oss<<iendl--<<iendl-- ;
         oss<<"END SUBROUTINE xios(is_defined_"<<className<<"_attr_hdl_)"<<iendl ;
         
      }      
       

      void CAttributeMap::generateFortranInterface_hdl(ostream& oss, const string& className)
      {
         oss<<"SUBROUTINE xios(set_"<<className<<"_attr_hdl)  &"<<iendl++ ;
         ostringstream* oss2 ;
         SuperClassMap::const_iterator it ;
         SuperClassMap::const_iterator begin = SuperClassMap::begin(), end = SuperClassMap::end();
         
         oss2=new ostringstream ;
         *oss2<<"( "<<className<<"_hdl"  ;
         for ( it=begin ; it != end; it++) 
         {
           *oss2<<", "<<it->second->getName() ;
           if (oss2->str().size()>90) 
           {
             oss<<oss2->str()<<"  &"<<iendl ;
             delete oss2 ;
             oss2=new ostringstream ;
           }
         }
         *oss2<<" )" ;
         oss<<oss2->str()<<iendl ;
         oss<<iendl ;
         delete oss2 ; 
         oss2=new ostringstream ;
         
         oss<<"IMPLICIT NONE"<<iendl++ ;
         oss<<"TYPE(txios("<<className<<")) , INTENT(IN) :: "<<className<<"_hdl"<<iendl ;
         
         for (it=begin; it != end; it++)
         {
           it->second->generateFortranInterfaceDeclaration(oss,className) ;
         }
         
         oss<<iendl ;
         
         oss<<"CALL xios(set_"<<className<<"_attr_hdl_)  &"<<iendl ;
         
         *oss2<<"( "<<className<<"_hdl"  ;
         for ( it=begin ; it != end; it++) 
         {
           *oss2<<", "<<it->second->getName() ;
           if (oss2->str().size()>90) 
           {
             oss<<oss2->str()<<"  &"<<iendl ;
             delete oss2 ;
             oss2=new ostringstream ;
           }
         }
         *oss2<<" )" ;
         oss<<oss2->str() ;
         delete oss2 ; 
         
         oss<<iendl--<<iendl-- ;
         oss<<"END SUBROUTINE xios(set_"<<className<<"_attr_hdl)"<<iendl ;
      }      
      
 
      void CAttributeMap::generateFortranInterfaceGet_hdl(ostream& oss, const string& className)
      {
         oss<<"SUBROUTINE xios(get_"<<className<<"_attr_hdl)  &"<<iendl++ ;
         ostringstream* oss2 ;
         SuperClassMap::const_iterator it ;
         SuperClassMap::const_iterator begin = SuperClassMap::begin(), end = SuperClassMap::end();
         
         oss2=new ostringstream ;
         *oss2<<"( "<<className<<"_hdl"  ;
         for ( it=begin ; it != end; it++) 
         {
           *oss2<<", "<<it->second->getName() ;
           if (oss2->str().size()>90) 
           {
             oss<<oss2->str()<<"  &"<<iendl ;
             delete oss2 ;
             oss2=new ostringstream ;
           }
         }
         *oss2<<" )" ;
         oss<<oss2->str()<<iendl ;
         oss<<iendl ;
         delete oss2 ; 
         oss2=new ostringstream ;
         
         oss<<"IMPLICIT NONE"<<iendl++ ;
         oss<<"TYPE(txios("<<className<<")) , INTENT(IN) :: "<<className<<"_hdl"<<iendl ;
         
         for (it=begin; it != end; it++)
         {
           it->second->generateFortranInterfaceGetDeclaration(oss,className) ;
         }
         
         oss<<iendl ;
         
         oss<<"CALL xios(get_"<<className<<"_attr_hdl_)  &"<<iendl ;
         
         *oss2<<"( "<<className<<"_hdl"  ;
         for ( it=begin ; it != end; it++) 
         {
           *oss2<<", "<<it->second->getName() ;
           if (oss2->str().size()>90) 
           {
             oss<<oss2->str()<<"  &"<<iendl ;
             delete oss2 ;
             oss2=new ostringstream ;
           }
         }
         *oss2<<" )" ;
         oss<<oss2->str() ;
         delete oss2 ; 
         
         oss<<iendl--<<iendl-- ;
         oss<<"END SUBROUTINE xios(get_"<<className<<"_attr_hdl)"<<iendl ;
      }      


      void CAttributeMap::generateFortranInterfaceIsDefined_hdl(ostream& oss, const string& className)
      {
         oss<<"SUBROUTINE xios(is_defined_"<<className<<"_attr_hdl)  &"<<iendl++ ;
         ostringstream* oss2 ;
         SuperClassMap::const_iterator it ;
         SuperClassMap::const_iterator begin = SuperClassMap::begin(), end = SuperClassMap::end();
         
         oss2=new ostringstream ;
         *oss2<<"( "<<className<<"_hdl"  ;
         for ( it=begin ; it != end; it++) 
         {
           *oss2<<", "<<it->second->getName() ;
           if (oss2->str().size()>90) 
           {
             oss<<oss2->str()<<"  &"<<iendl ;
             delete oss2 ;
             oss2=new ostringstream ;
           }
         }
         *oss2<<" )" ;
         oss<<oss2->str()<<iendl ;
         oss<<iendl ;
         delete oss2 ; 
         oss2=new ostringstream ;
         
         oss<<"IMPLICIT NONE"<<iendl++ ;
         oss<<"TYPE(txios("<<className<<")) , INTENT(IN) :: "<<className<<"_hdl"<<iendl ;
         
         for (it=begin; it != end; it++)
         {
           it->second->generateFortranInterfaceIsDefinedDeclaration(oss,className) ;
         }
         
         oss<<iendl ;
         
         oss<<"CALL xios(is_defined_"<<className<<"_attr_hdl_)  &"<<iendl ;
         
         *oss2<<"( "<<className<<"_hdl"  ;
         for ( it=begin ; it != end; it++) 
         {
           *oss2<<", "<<it->second->getName() ;
           if (oss2->str().size()>90) 
           {
             oss<<oss2->str()<<"  &"<<iendl ;
             delete oss2 ;
             oss2=new ostringstream ;
           }
         }
         *oss2<<" )" ;
         oss<<oss2->str() ;
         delete oss2 ; 
         
         oss<<iendl--<<iendl-- ;
         oss<<"END SUBROUTINE xios(is_defined_"<<className<<"_attr_hdl)"<<iendl ;
      }      

      
      void CAttributeMap::generateFortranInterface_id(ostream& oss, const string& className)
      {
         oss<<"SUBROUTINE xios(set_"<<className<<"_attr)  &"<<iendl++ ;
         ostringstream* oss2 ;
         SuperClassMap::const_iterator it ;
         SuperClassMap::const_iterator begin = SuperClassMap::begin(), end = SuperClassMap::end();
         
         oss2=new ostringstream ;
         *oss2<<"( "<<className<<"_id"  ;
         for ( it=begin ; it != end; it++) 
         {
           *oss2<<", "<<it->second->getName() ;
           if (oss2->str().size()>90) 
           {
             oss<<oss2->str()<<"  &"<<iendl ;
             delete oss2 ;
             oss2=new ostringstream ;
           }
         }
         *oss2<<" )" ;
         oss<<oss2->str()<<iendl ;
         oss<<iendl ;
         delete oss2 ; 
         oss2=new ostringstream ;
         
         oss<<"IMPLICIT NONE"<<iendl++ ;

         oss<<"TYPE(txios("<<className<<"))  :: "<<className<<"_hdl"<<iendl ;
         oss<<"CHARACTER(LEN=*), INTENT(IN) ::"<<className<<"_id"<<iendl ;
         
         for (it=begin; it != end; it++)
         {
           it->second->generateFortranInterfaceDeclaration(oss,className) ;
         }
         
         oss<<iendl ;
         oss<<"CALL xios(get_"<<className<<"_handle)("<<className<<"_id,"<<className<<"_hdl)"<<iendl ; 
         oss<<"CALL xios(set_"<<className<<"_attr_hdl_)   &"<<iendl ;
         *oss2<<"( "<<className<<"_hdl"  ;
         for ( it=begin ; it != end; it++) 
         {
           *oss2<<", "<<it->second->getName() ;
           if (oss2->str().size()>90) 
           {
             oss<<oss2->str()<<"  &"<<iendl ;
             delete oss2 ;
             oss2=new ostringstream ;
           }
         }
         *oss2<<" )" ;
         oss<<oss2->str() ;
         delete oss2 ; 
         
         oss<<iendl--<<iendl-- ;
         oss<<"END SUBROUTINE xios(set_"<<className<<"_attr)"<<iendl ;
         
      }      
      
      void CAttributeMap::generateFortranInterfaceGet_id(ostream& oss, const string& className)
      {
         oss<<"SUBROUTINE xios(get_"<<className<<"_attr)  &"<<iendl++ ;
         ostringstream* oss2 ;
         SuperClassMap::const_iterator it ;
         SuperClassMap::const_iterator begin = SuperClassMap::begin(), end = SuperClassMap::end();
         
         oss2=new ostringstream ;
         *oss2<<"( "<<className<<"_id"  ;
         for ( it=begin ; it != end; it++) 
         {
           *oss2<<", "<<it->second->getName() ;
           if (oss2->str().size()>90) 
           {
             oss<<oss2->str()<<"  &"<<iendl ;
             delete oss2 ;
             oss2=new ostringstream ;
           }
         }
         *oss2<<" )" ;
         oss<<oss2->str()<<iendl ;
         oss<<iendl ;
         delete oss2 ; 
         oss2=new ostringstream ;
         
         oss<<"IMPLICIT NONE"<<iendl++ ;

         oss<<"TYPE(txios("<<className<<"))  :: "<<className<<"_hdl"<<iendl ;
         oss<<"CHARACTER(LEN=*), INTENT(IN) ::"<<className<<"_id"<<iendl ;
         
         for (it=begin; it != end; it++)
         {
           it->second->generateFortranInterfaceGetDeclaration(oss,className) ;
         }
         
         oss<<iendl ;
         oss<<"CALL xios(get_"<<className<<"_handle)("<<className<<"_id,"<<className<<"_hdl)"<<iendl ; 
         oss<<"CALL xios(get_"<<className<<"_attr_hdl_)   &"<<iendl ;
         *oss2<<"( "<<className<<"_hdl"  ;
         for ( it=begin ; it != end; it++) 
         {
           *oss2<<", "<<it->second->getName() ;
           if (oss2->str().size()>90) 
           {
             oss<<oss2->str()<<"  &"<<iendl ;
             delete oss2 ;
             oss2=new ostringstream ;
           }
         }
         *oss2<<" )" ;
         oss<<oss2->str() ;
         delete oss2 ; 
         
         oss<<iendl--<<iendl-- ;
         oss<<"END SUBROUTINE xios(get_"<<className<<"_attr)"<<iendl ;
         
      }      
      
      void CAttributeMap::generateFortranInterfaceIsDefined_id(ostream& oss, const string& className)
      {
         oss<<"SUBROUTINE xios(is_defined_"<<className<<"_attr)  &"<<iendl++ ;
         ostringstream* oss2 ;
         SuperClassMap::const_iterator it ;
         SuperClassMap::const_iterator begin = SuperClassMap::begin(), end = SuperClassMap::end();
         
         oss2=new ostringstream ;
         *oss2<<"( "<<className<<"_id"  ;
         for ( it=begin ; it != end; it++) 
         {
           *oss2<<", "<<it->second->getName() ;
           if (oss2->str().size()>90) 
           {
             oss<<oss2->str()<<"  &"<<iendl ;
             delete oss2 ;
             oss2=new ostringstream ;
           }
         }
         *oss2<<" )" ;
         oss<<oss2->str()<<iendl ;
         oss<<iendl ;
         delete oss2 ; 
         oss2=new ostringstream ;
         
         oss<<"IMPLICIT NONE"<<iendl++ ;

         oss<<"TYPE(txios("<<className<<"))  :: "<<className<<"_hdl"<<iendl ;
         oss<<"CHARACTER(LEN=*), INTENT(IN) ::"<<className<<"_id"<<iendl ;
         
         for (it=begin; it != end; it++)
         {
           it->second->generateFortranInterfaceIsDefinedDeclaration(oss,className) ;
         }
         
         oss<<iendl ;
         oss<<"CALL xios(get_"<<className<<"_handle)("<<className<<"_id,"<<className<<"_hdl)"<<iendl ; 
         oss<<"CALL xios(is_defined_"<<className<<"_attr_hdl_)   &"<<iendl ;
         *oss2<<"( "<<className<<"_hdl"  ;
         for ( it=begin ; it != end; it++) 
         {
           *oss2<<", "<<it->second->getName() ;
           if (oss2->str().size()>90) 
           {
             oss<<oss2->str()<<"  &"<<iendl ;
             delete oss2 ;
             oss2=new ostringstream ;
           }
         }
         *oss2<<" )" ;
         oss<<oss2->str() ;
         delete oss2 ; 
         
         oss<<iendl--<<iendl-- ;
         oss<<"END SUBROUTINE xios(is_defined_"<<className<<"_attr)"<<iendl ;
         
      }      
      ///--------------------------------------------------------------
  

} // namespace xmlioser
