#include "julian.hpp"

namespace xios
{
      /// ////////////////////// Définitions ////////////////////// ///

      CJulianCalendar::CJulianCalendar(const StdString & dateStr)
         : CCalendar("Julian")
      { initializeDate(dateStr); }

      CJulianCalendar::CJulianCalendar(const StdString & dateStr,const StdString & timeOriginStr)
         : CCalendar("Julian")
      { initializeDate(dateStr, timeOriginStr); }

      CJulianCalendar::CJulianCalendar(int yr, int mth, int d,
                                       int hr, int min, int sec)
         : CCalendar("Julian")
      { initializeDate(yr, mth, d, hr, min, sec) ; }

      CJulianCalendar::~CJulianCalendar(void)
      { /* Ne rien faire de plus */ }

      ///--------------------------------------------------------------

      int CJulianCalendar::getYearTotalLength(const CDate & date) const
      { // Retourne la durée d'une année en seconde.
         if (date.getYear() % 4 == 0) return (366 * 86400);
         return (365 * 86400);
      }

      int CJulianCalendar::getMonthLength(const CDate & date) const
      { // Retourne la durée du mois en jour.
         if (date.getMonth() == 2)
         {
            if (date.getYear()%4 == 0) return 29;
               return 28;
         }
         return (CCalendar::getMonthLength(date));
      }

      StdString CJulianCalendar::getType(void) const
      { return (StdString("julian")); }

      ///--------------------------------------------------------------
} // namespace xmlioserver
