#ifndef  __XIOS_GENERATE_INTERFACE_HPP__
#define  __XIOS_GENERATE_INTERFACE_HPP__

#include "xmlioserver_spl.hpp"

namespace xios
{
  class CInterface
  {
    public:
    
    template <class T>
    static void AttributeCInterface(ostream& oss,const string& className,const string& name) ;
    static void AttributeIsDefinedCInterface(ostream& oss, const string& className,const string& name);
  
   template <class T>
   static void AttributeFortran2003Interface(ostream& oss,const string& className,const string& name) ;
   static void AttributeIsDefinedFortran2003Interface(ostream& oss,const string& className,const string& name);
   
   template <class T>
   static void AttributeFortranInterfaceDeclaration(ostream& oss,const string& className,const string& name) ;
   
   template <class T>
   static void AttributeFortranInterfaceGetDeclaration(ostream& oss,const string& className,const string& name) ;

   static void AttributeFortranInterfaceIsDefinedDeclaration(ostream& oss,const string& className,const string& name) ;

   template <class T>
   static void AttributeFortranInterfaceBody(ostream& oss,const string& className,const string& name) ;
  
   template <class T>
   static void AttributeFortranInterfaceGetBody(ostream& oss,const string& className,const string& name) ;
 
   static void AttributeFortranInterfaceIsDefinedBody(ostream& oss,const string& className,const string& name) ;
 
   template <class T>
   static string getStrFortranType(void) ;

   template <class T>
   static string getStrFortranKind(void) ;
    
   template <class T>
   static string getStrFortranKindC(void) ;
   
   template <class T>
   static bool matchingTypeCFortran(void) ;
   

  };
/*  
  template<> string CInterface::getStrFortranType<int>(void) {return string("INTEGER") ;}
  template<> string CInterface::getStrFortranType<bool>(void) {return string("LOGICAL") ;}
  template<> string CInterface::getStrFortranType<double>(void) {return string("REAL") ;}
  template<> string CInterface::getStrFortranType<float>(void) {return string("REAL") ;}
  
  template<> string CInterface::getStrFortranKind<int>(void) {return string("") ;}
  template<> string CInterface::getStrFortranKind<bool>(void) {return string("") ;}
  template<> string CInterface::getStrFortranKind<double>(void) {return string("(KIND=8)") ;}
  template<> string CInterface::getStrFortranKind<float>(void) {return string("(KIND=4)") ;}
  
  template<> string CInterface::getStrFortranKindC<int>(void) {return string("(KIND=C_INT)") ;}
  template<> string CInterface::getStrFortranKindC<bool>(void) {return string("(KIND=C_BOOL)") ;}
  template<> string CInterface::getStrFortranKindC<double>(void) {return string("(KIND=C_DOUBLE)") ;}
  template<> string CInterface::getStrFortranKindC<float>(void) {return string("(KIND=C_FLOAT)") ;}
  
  template<> bool CInterface::matchingTypeCFortran<int>(void) { return true ; } 
  template<> bool CInterface::matchingTypeCFortran<bool>(void) { return false ;} 
  template<> bool CInterface::matchingTypeCFortran<double>(void) { return true; }
  template<> bool CInterface::matchingTypeCFortran<float>(void) { return true; }
*/
}
#endif
