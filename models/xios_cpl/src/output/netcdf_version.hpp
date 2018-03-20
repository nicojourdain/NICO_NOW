#ifndef __XIOS_NETCDF_VERSION_HPP__
#define __XIOS_NETCDF_VERSION_HPP__

#if defined(NC_DISKLESS) && defined (NC_MMAP)

#  define NETCDF_VERSION 4211

#elif defined(NC_ETRANSLATION)

# define NETCDF_VERSION 4130

#elif defined(NC_EURL) && defined(NC_ECONSTRAINT)

#  define NETCDF_VERSION 4120

#elif defined (NC_PNETCDF)

#  define NETCDF_VERSION 4110

#elif defined(NC_CHUNKED) && defined(NC_CONTIGUOUS)

# define NETCDF_VERSION 4010

#elif defined(NC_NETCDF4)

# define NETCDF_VERSION 4000

#else

#  error "netcdf version prior 4.0 incompatible with XIOS"
 
#endif

#endif
