#include "group_factory.hpp"

namespace xios
{
   /// ////////////////////// Définitions ////////////////////// ///
   StdString CGroupFactory::CurrContext("");

   void CGroupFactory::SetCurrentContextId(const StdString & context)
   { 
      CGroupFactory::CurrContext = context;
   }

   StdString & CGroupFactory::GetCurrentContextId(void)
   { 
      return (CGroupFactory::CurrContext);
   }

} // namespace xios
