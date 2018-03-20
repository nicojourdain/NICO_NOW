#ifndef __XMLIO_Configure__
#define __XMLIO_Configure__


/// /////////// Macros /////////// ///
#define DECLARE_PROPERTY(type, name, value) \
   extern type name; // = value

namespace xios
{
#include "properties.conf"
} // namespace xios

#endif // __XMLIO_Configure__
