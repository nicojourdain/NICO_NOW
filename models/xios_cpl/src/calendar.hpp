#ifndef __XMLIO_CCalendar__
#define __XMLIO_CCalendar__

/// xios headers ///
#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "date.hpp"

namespace xios
{
      /// ////////////////////// Déclarations ////////////////////// ///

      typedef enum _monthEnum
      {  JAN = 1, FEB = 2, MAR = 3, APR = 4 , MAY = 5 , JUN = 6 ,
         JUL = 7, AUG = 8, SEP = 9, OCT = 10, NOV = 11, DEC = 12  } MonthEnum;

      ///---------------------------------------------------------------

      class CDate;

      class CCalendar : public CObject
      {
            /// Typedef ///
            typedef CObject SuperClass;

         public :

            /// Destructeur ///
            virtual ~CCalendar(void);

         protected :

            /// Constructeurs ///
            CCalendar(void);
            CCalendar(const StdString & id) ;
            CCalendar(const StdString & id,
                      int yr, int mth, int d,
                      int hr = 0, int min = 0, int sec = 0);
            CCalendar(const StdString & id, const StdString & dateStr);
            CCalendar(const StdString & id, const StdString & dateStr, const StdString & timeOrigin);

            CCalendar(const CCalendar & calendar);       // Not implemented yet.
            CCalendar(const CCalendar * const calendar); // Not implemented yet.

         public :

            //------------------------------------------------------------

            /// Autres ///
            virtual StdString toString(void) const;
            virtual void fromString(const StdString & str);

            /// Mutateur ///
            void setTimeStep(const CDuration & duration);

            /// Traitemants ///
            CDate & update(int step);

            /// Accesseurs ///
            const CDuration & getTimeStep(void) const;
            const CDate & getInitDate(void) const;
            const CDate & getTimeOrigin(void) const;
             CDate & getCurrentDate(void);
            
         public :
         
            //------------------------------------------------------------
            virtual int getMonthLength(const CDate & date) const ;

            virtual StdString getType(void) const;

            virtual int getYearTotalLength(const CDate & date) const ; // Retourne la durée d'une année en seconde.

            virtual int getYearLength  (void) const; // Retourne la durée d'une année en mois.
            virtual int getDayLength   (void) const; // Retourne la durée d'un jour en heures.
            virtual int getHourLength  (void) const; // Retourne la durée d'une heure en minute.
            virtual int getMinuteLength(void) const; // Retourne la durée d'une minute en secondes.

            virtual int getNbSecond(const CDate & date) const;
            virtual StdString getMonthName(int month_id) const;

            virtual const StdString getMonthShortName(int month_id) const;
            void initializeDate(int yr, int mth, int d, int hr = 0, int min = 0, int sec = 0) ;
            void initializeDate(const StdString & dateStr);
            void initializeDate(const StdString & dateStr, const StdString & timeOrigin);

            //------------------------------------------------------------

         private :

            /// Propriétés privées ///
            CDate initDate;
            CDate timeOrigin;
            CDate currentDate;
            CDuration timestep;

      }; // class CCalendar

} // namespace xios

#endif // __XMLIO_CCalendar__
