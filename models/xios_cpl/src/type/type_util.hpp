#ifndef __TYPE_UTIL_HPP__
#define __TYPE_UTIL_HPP__

#include <string>
namespace xios
{
    class CDomain ;
    class CDomainGroup;
    class CField;
    class CFieldGroup;
    class CGrid;
    class CGridGroup;
    class CAxis;
    class CAxisGroup;
    class CFile;
    class CFileGroup;
    class CContext;
    class CContextGroup;
    class CVariable ;
    class CVariableGroup ;
        
  template <typename T> inline string getStrType(void) ;
    
#define macro(T) template <> inline string getStrType<T>(void) { return std::string(#T) ; }

  macro(short)
  macro(unsigned short)
  macro(int)
  macro(unsigned int)
  macro(long)
  macro(unsigned long)
  macro(float)
  macro(double)
  macro(long double)
  macro(char)
  macro(unsigned char)
  macro(wchar_t)
  macro(bool)
#undef macro
  
#define macro(T) template <> inline string getStrType<T>(void) { return std::string(#T) ; }
  macro(CDomain)
  macro(CDomainGroup)
  macro(CField)
  macro(CFieldGroup)
  macro(CGrid)
  macro(CGridGroup)
  macro(CAxis)
  macro(CAxisGroup)
  macro(CFile)
  macro(CFileGroup)
  macro(CContext)
  macro(CContextGroup)
  macro(CVariable)
  macro(CVariableGroup)
  
 
#undef macro

}


#endif
