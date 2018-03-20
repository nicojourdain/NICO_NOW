#ifndef __XMLIO_CDuration__
#define __XMLIO_CDuration__

/// xios headers ///
#include "xmlioserver_spl.hpp"
#include "exception.hpp"

namespace xios
{
      /// ////////////////////// Déclarations ////////////////////// ///
      typedef long long int Time;
      class CCalendar;

      ///---------------------------------------------------------------
      typedef struct _duration
      {
         public :

            /// Opérateurs ///
            struct _duration & operator=(const struct _duration& duration);

            friend StdOStream & operator<<(StdOStream & out, const struct _duration& duration);
            friend StdIStream & operator>>(StdIStream & in , struct _duration& duration);

            /// Test ///
            bool isNone(void) const;

            /// Traitement ///
            struct _duration & resolve(const CCalendar & calendar);
            struct _duration & solveTimeStep(const CCalendar & c) ;
            /// Autres ///
            StdString toString(void) const;

         public: /* static */

            static struct _duration FromString(const StdString & str);

            /// Propriétés publiques ///
            double year, month, day, hour, minute, second, timestep;

      } CDuration;

      ///---------------------------------------------------------------

      const extern CDuration Year, Month , Week  , Day    ,
                             Hour, Minute, Second, NoneDu, TimeStep ;
      ///---------------------------------------------------------------

} // namespace xios

#endif // __XMLIO_CDuration__
