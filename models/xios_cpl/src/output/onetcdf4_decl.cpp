#include "onetcdf4_impl.hpp"

namespace xios
{
# define  macro(type,size) \
  template void CONetCDF4::writeData<type,size>(const CArray<type, size>& data, const StdString & name, \
                                               bool collective, StdSize record, \
                                               const std::vector<StdSize> * start, \
                                               const std::vector<StdSize> * count) ;
 
  macro(double,1)
  macro(double,2)
  macro(double,3)                                              
 
  template void CONetCDF4::setDefaultValue<double>(const StdString & varname, const double* value) ;
  template void CONetCDF4::setDefaultValue<float>(const StdString & varname, const float* value) ;
}
