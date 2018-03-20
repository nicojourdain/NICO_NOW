#ifndef __XIOS_NETCDF_HPP__
#define __XIOS_NETCDF_HPP__
#include "mpi.hpp"
#define MPI_INCLUDED
#include <netcdf.h>

#if defined(USING_NETCDF_INTERNAL)
  const bool using_netcdf_internal=true ;
#else
  const bool using_netcdf_internal=false ;
#endif

#include "netcdf_version.hpp"

#if NETCDF_VERSION >= 4110

#  if defined(USING_NETCDF_PAR)
extern "C"
{
#  include <netcdf_par.h>
}
#  endif
#endif

namespace xios
{
  inline int nc_create_par(const char *path, int cmode, MPI_Comm comm, MPI_Info info,int *ncidp)
  {
#if defined(USING_NETCDF_PAR)
    return ::nc_create_par(path, cmode, comm, info, ncidp) ;
#else
    ERROR("int nc_create_par(const char *path, int cmode, MPI_Comm comm, MPI_Info info,int *ncidp)",
           << "must not be use with netcdf sequential version") ;
    return -1 ;
#endif
  }
  
  inline int nc_open_par(const char *path, int mode, MPI_Comm comm, MPI_Info info,int *ncidp)
  { 
#if defined(USING_NETCDF_PAR)
    return ::nc_open_par(path, mode, comm, info, ncidp) ;
#else
    ERROR("int nc_open_par(const char *path, int mode, MPI_Comm comm, MPI_Info info,int *ncidp)",
           << "must not be use with netcdf sequential version") ;
    return -1 ;
#endif
  }
  
  inline int nc_var_par_access(int ncid, int varid, int par_access)
  {
#if defined(USING_NETCDF_PAR)
    return ::nc_var_par_access(ncid, varid, par_access) ;
#else
    ERROR("int nc_var_par_access(int ncid, int varid, int par_access)",
           << "must not be use with netcdf sequential version") ;
    return -1 ;
#endif
  }
}


#endif
