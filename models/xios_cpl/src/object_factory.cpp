#include "object_factory.hpp"

namespace xios
{
   /// ////////////////////// Définitions ////////////////////// ///

   StdString CObjectFactory::CurrContext("");

   void CObjectFactory::SetCurrentContextId(const StdString & context)
   { CObjectFactory::CurrContext = context; }

   StdString & CObjectFactory::GetCurrentContextId(void)
   { return (CObjectFactory::CurrContext); }

} // namespace xios
