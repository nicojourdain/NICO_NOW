#include "allleap.hpp"

namespace xios
{
      /// ////////////////////// Définitions ////////////////////// ///

      CAllLeapCalendar::CAllLeapCalendar(const StdString & dateStr)
         : CCalendar("AllLeap")
      { initializeDate(dateStr); }

      CAllLeapCalendar::CAllLeapCalendar(const StdString & dateStr,const StdString & timeOriginStr)
         : CCalendar("AllLeap")
      { initializeDate(dateStr, timeOriginStr); }

      CAllLeapCalendar::CAllLeapCalendar(int yr, int mth, int d,
                                         int hr, int min, int sec)
         : CCalendar("AllLeap")
      { initializeDate(yr, mth, d, hr, min, sec) ; }

      CAllLeapCalendar::~CAllLeapCalendar(void)
      { /* Ne rien faire de plus */ }

      ///--------------------------------------------------------------

      int CAllLeapCalendar::getYearTotalLength(const CDate & date) const
      { return (366 * 86400); }

      int CAllLeapCalendar::getMonthLength(const CDate & date) const
      {
         if (date.getMonth() == 2) return (29);
         return (CCalendar::getMonthLength(date));
      }

      StdString CAllLeapCalendar::getType(void) const
      { return (StdString("all_leap")); }

      ///--------------------------------------------------------------
} // namespace xmlioserver

