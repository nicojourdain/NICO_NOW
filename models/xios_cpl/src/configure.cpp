#define __XMLIO_Configure__ // < Ne pas supprimer

/// xios headers ///
#include "xmlioserver_spl.hpp"

/// /////////// Macros /////////// ///
#undef  DECLARE_PROPERTY
#define DECLARE_PROPERTY(type, name, value) \
   type name = value;

namespace xios
{
#include "properties.conf"
} // namespace xios
