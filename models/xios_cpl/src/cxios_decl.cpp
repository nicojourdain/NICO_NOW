#include "cxios_impl.hpp"
#include "xmlioserver_spl.hpp"
#include <string>

namespace xios
{
# define macro(T) \
  template T CXios::getin<T>(const string& id) ; \
  template T CXios::getin<T>(const string& id, const T& defaultValue) ;
  
  macro(int)
  macro(double)
  macro(bool)
  macro(StdSize)
  macro(string)
}
