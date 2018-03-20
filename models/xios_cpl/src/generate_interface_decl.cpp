#include "generate_interface_impl.hpp"

namespace xios
{

#define macro(T) \
  template void CInterface::AttributeCInterface<T>(ostream& oss,const string& className,const string& name) ; \
  template void CInterface::AttributeFortran2003Interface<T>(ostream& oss,const string& className,const string& name) ; \
  template void CInterface::AttributeFortranInterfaceDeclaration<T>(ostream& oss,const string& className,const string& name) ; \
  template void CInterface::AttributeFortranInterfaceGetDeclaration<T>(ostream& oss,const string& className,const string& name) ; \
  template void CInterface::AttributeFortranInterfaceBody<T>(ostream& oss,const string& className,const string& name) ; \
  template void CInterface::AttributeFortranInterfaceGetBody<T>(ostream& oss,const string& className,const string& name) ; \
  template string CInterface::getStrFortranType<T>(void) ; \
  template string CInterface::getStrFortranKind<T>(void) ; \
  template string CInterface::getStrFortranKindC<T>(void) ; \
  template bool CInterface::matchingTypeCFortran<T>(void) ; 
  
  macro(bool)
  macro(int)
  macro(double)
}
