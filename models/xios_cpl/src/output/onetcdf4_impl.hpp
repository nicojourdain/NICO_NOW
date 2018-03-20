#ifndef __ONETCDF4_IMPL_HPP__
#define __ONETCDF4_IMPL_HPP__

#include "onetcdf4.hpp"

namespace xios
{
  template <class T, int ndim>
  void CONetCDF4::writeData(const CArray<T, ndim>& data, const StdString & name,
                            bool collective, StdSize record,
                            const std::vector<StdSize> * start,
                            const std::vector<StdSize> * count)
  {
    int grpid = this->getCurrentGroup();
    int varid = this->getVariable(name);
    StdSize array_size = 1;
    std::vector<StdSize> sstart, scount;

    if (this->wmpi && collective)
    CheckError(nc_var_par_access(grpid, varid, NC_COLLECTIVE));
    if (this->wmpi && !collective)
    CheckError(nc_var_par_access(grpid, varid, NC_INDEPENDENT));

    this->getWriteDataInfos
    (name, record, array_size,  sstart, scount, start, count);
    if (data.numElements() != array_size)
    {
      ERROR("CONetCDF4::writeData(...)",
      << "[ input array size = "  << data.numElements()
      << ", intern array size = " << array_size
      << " ] Invalid input data !" );
    }
         
    this->writeData_(grpid, varid, sstart, scount, data.dataFirst());
  }
      
//----------------------------------------------------------------
           
  template <class T>
  void CONetCDF4::setDefaultValue(const StdString & varname, const T * value)
  {
    int grpid = this->getCurrentGroup();
    int varid = this->getVariable(varname);
         
    if (value != NULL)
    {
      CheckError(nc_def_var_fill(grpid, varid, 0, (void*)value));
      this->addAttribute(StdString("missing_value"), *value, &varname);
    }
    else CheckError(nc_def_var_fill(grpid, varid, 1, NULL));         
  }
     
  ///---------------------------------------------------------------

}



#endif
