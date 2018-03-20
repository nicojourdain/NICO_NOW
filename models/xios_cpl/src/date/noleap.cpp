#include "noleap.hpp"
#include "calendar.hpp"

namespace xios
{
      /// ////////////////////// Définitions ////////////////////// ///

      CNoLeapCalendar::CNoLeapCalendar(const StdString & dateStr)
         : CCalendar("NoLeap", dateStr)
      { initializeDate(dateStr); }

      CNoLeapCalendar::CNoLeapCalendar(const StdString & dateStr,const StdString & timeOriginStr)
         : CCalendar("NoLeap", dateStr, timeOriginStr)
      { initializeDate(dateStr, timeOriginStr); }

      CNoLeapCalendar::CNoLeapCalendar(int yr, int mth, int d,
                                       int hr, int min, int sec)
         : CCalendar("NoLeap")
      { initializeDate(yr, mth, d, hr, min, sec) ; }


      CNoLeapCalendar::~CNoLeapCalendar(void)
      { /* Ne rien faire de plus */ }

      ///--------------------------------------------------------------

      StdString CNoLeapCalendar::getType(void) const
      { return (StdString("noleap")); }

      ///--------------------------------------------------------------
} // namespace xmlioserver

