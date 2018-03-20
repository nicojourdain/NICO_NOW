#ifndef __XIOS_IMPL_HPP__
#define __XIOS_IMPL_HPP__

#include "xmlioserver_spl.hpp"
#include "variable.hpp"
#include "object_template.hpp"
#include "cxios.hpp"

namespace xios
{
  template <typename T>
  T CXios::getin(const string& id)
  {
    return CVariable::get("xios",id)->getData<T>() ;
  }

  template <typename T>
  T CXios::getin(const string& id, const T& defaultValue)
  {
    if (CVariable::has("xios",id)) return CVariable::get("xios",id)->getData<T>() ;
    else return defaultValue ;
  }


}
#endif
