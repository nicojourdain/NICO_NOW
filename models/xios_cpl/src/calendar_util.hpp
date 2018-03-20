#ifndef __XMLIO_CCalendar_util__
#define __XMLIO_CCalendar_util__

/// xios headers ///
#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "calendar.hpp"

namespace xios
{
      /// ////////////////////// Déclarations ////////////////////// ///

      CDuration operator*(const double    & scal, const CDuration & ddr);
      CDuration operator-(const CDuration & ddr , const CDuration & dr);
      CDuration operator+(const CDuration & ddr , const CDuration & dr);
      CDuration operator*(const CDuration & ddr , const double    & scal);
      CDuration operator-(const CDuration & ddr);

      CDate operator+(const CDate & dt, const CDuration & dr); // Non testée.
      CDate operator-(const CDate & dt, const CDuration & dr);

      CDuration operator-(const CDate & dt0, const CDate & dt1);

      /// Les opérateurs de comparaison. (Non testés pour le moment)
      bool operator==(const CDate& dt0, const CDate& dt1);
      bool operator< (const CDate& dt0, const CDate& dt1);

      bool operator!=(const CDate & dt0, const CDate & dt1);
      bool operator> (const CDate & dt0, const CDate & dt1);
      bool operator>=(const CDate & dt0, const CDate & dt1);
      bool operator<=(const CDate & dt0, const CDate & dt1);

      ///---------------------------------------------------------------

} // namespace xios

#endif //__XMLIO_CCalendar_util__
