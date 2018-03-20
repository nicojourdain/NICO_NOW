#ifndef __XMLIO_CArray_mac__
#define __XMLIO_CArray_mac__

/// ////////////////////// Macros ////////////////////// ///

// Type Fortran
#define ARRAY(valuetype, numdims) boost::shared_ptr<CArray<valuetype, numdims> >

#define ARRAY_ASSIGN(value, valuetype, numdims, extent)\
   value.reset(new CArray<valuetype, numdims>(boost::extents extent)) 

#define ARRAY_CREATE(value, valuetype, numdims, extent)\
   ARRAY(valuetype, numdims) value =                   \
   ARRAY(valuetype, numdims)(new CArray<valuetype, numdims>(boost::extents extent))

// Type C
#define ARRAY_C_ASSIGN(value, valuetype, numdims, extent)\
   value = ARRAY(valuetype, numdims)                     \
   (new CArray<valuetype, numdims>(boost::extents extent, c_storage_order()))

#define ARRAY_C_CREATE(value, valuetype, numdims, extent)\
   ARRAY_C_ASSIGN(ARRAY(valuetype, numdims) value, valuetype, numdims, extent)

///---------------------------------------------------------------

#endif // __XMLIO_CArray_mac__
