#include "calendar_util.hpp"

namespace xios
{
      /// ////////////////////// Définitions ////////////////////// ///

      CDuration operator*(const double & scal, const CDuration & ddr)
      { return (ddr * scal); }

      CDuration operator-(const CDuration & ddr , const CDuration & dr)
      {
         CDuration dur(ddr);
         dur.year -= dr.year;  dur.month  -= dr.month ; dur.day    -= dr.day;
         dur.hour -= dr.hour;  dur.minute -= dr.minute; dur.second -= dr.second; dur.timestep -= dr.timestep;
         return (dur);
      }

      CDuration operator+(const CDuration & ddr , const CDuration & dr)
      {
         CDuration dur(ddr);
         dur.year += dr.year;  dur.month  += dr.month ; dur.day    += dr.day;
         dur.hour += dr.hour;  dur.minute += dr.minute; dur.second += dr.second; dur.timestep += dr.timestep;
         return (dur);
      }

      CDuration operator*(const CDuration & ddr , const double & scal)
      {
         CDuration dur(ddr);
         dur.year *= scal;  dur.month  *= scal; dur.day    *= scal;
         dur.hour *= scal;  dur.minute *= scal; dur.second *= scal; dur.timestep *= scal;
         return (dur);
      }

      CDuration operator-(const CDuration & ddr)
      {
         CDuration dur(ddr);
         dur.year   = -dur.year;
         dur.month  = -dur.month;
         dur.day    = -dur.day;
         dur.hour   = -dur.hour;
         dur.minute = -dur.minute;
         dur.second = -dur.second;
         dur.timestep = -dur.timestep;
         return (dur);
      }

      //-----------------------------------------------------------------

      CDate operator+(const CDate & dt, const CDuration & dr)
      {
         CDuration drr (dr);
         int year = 0, month = 0, day = 0, hour = 0, minute = 0, second = 0;
         const CCalendar & c = dt.getRelCalendar();
         
         drr.timestep=0 ;       
         drr=drr+dr.timestep*dt.getRelCalendar().getTimeStep() ;
         
         drr.resolve(dt.getRelCalendar());

         // Ajustement des minutes par rapport aux secondes.
         second += dt.getSecond() + drr.second;
         if (second <  0) { minute --; second += c.getMinuteLength(); }
         if (second >= c.getMinuteLength()) { minute ++; second -= c.getMinuteLength(); }

         // Ajustement des heures en fonction des minutes.
         minute += dt.getMinute() + drr.minute;
         if (minute < 0) { hour --; minute += c.getHourLength(); }
         if (minute >= c.getHourLength()) { hour ++; minute -= c.getHourLength(); }

         // Ajustement des jours en fonction des heures.
         hour += dt.getHour() + drr.hour;
         if (hour <  0) { drr.day --; hour += c.getDayLength(); }
         if (hour >= c.getDayLength()) { drr.day ++; hour -= c.getDayLength(); }

         // Ajustement des mois en fonction des jours.
         CDate dtt(dt); 
         drr.day+=dtt.getDay()-1 ;
         dtt.setDay(1) ;         

         if ( drr.day >= 0 )
         {
           for(; c.getMonthLength(dtt) <= drr.day; dtt.addMonth (1))
           { drr.day -=  c.getMonthLength(dtt); drr.month += 1 ; }

           day = drr.day+1 ;
         }
         else 
         {
           dtt.addMonth(-1) ;
           drr.month-=1 ;
           for(; c.getMonthLength(dtt) < -drr.day; dtt.addMonth (-1))
           { drr.day+=c.getMonthLength(dtt) ; drr.month-=1 ; }
           day=c.getMonthLength(dtt)+drr.day+1 ;
         }
            
/*
         if (day <  0) { drr.month --; day += c.getMonthLength(dtt); }
         if (day > c.getMonthLength(dtt)) { drr.month ++; day -= c.getMonthLength(dtt); } // << Problème ici
         if (day == 0){ day = c.getMonthLength(dtt); drr.month --; }
*/         
         drr.resolve(dt.getRelCalendar());

         // Ajustement des années en fonction des mois.
         month += dt.getMonth() + drr.month;
         if (month <  0) { drr.year --; month += c.getYearLength(); }
         if (month >  c.getYearLength()) { drr.year ++; month -= c.getYearLength(); }
         if (month == 0){ month = c.getYearLength(); drr.year--; }

         year += dt.getYear() + drr.year;

         return (CDate(dt.getRelCalendar(), year, month, day, hour, minute, second));
      }

      CDate operator-(const CDate & dt, const CDuration & dr) { return (dt + (-dr)); }

      //-----------------------------------------------------------------

      CDuration operator-(const CDate & dt0, const CDate & dt1)
      {
         // TODO :: Vérifier que les deux dates (dt0 et dt1) ont une référence vers le même calendrier.
         CDuration dur =
         { dt0.getYear() - dt1.getYear(), dt0.getMonth()  - dt1.getMonth() , dt0.getDay()    - dt1.getDay(),
           dt0.getHour() - dt1.getHour(), dt0.getMinute() - dt1.getMinute(), dt0.getSecond() - dt1.getSecond() };
         return (dur.resolve(dt0.getRelCalendar()));
      }

      //-----------------------------------------------------------------

      /// Les opérateurs de comparaison. (Non testés pour le moment)
      bool operator==(const CDate& dt0, const CDate& dt1)
      {
         // TODO :: Vérifier que les deux dates (dt0 et dt1) ont une référence vers le même calendrier.
         return ((dt0.getYear() == dt1.getYear()) && (dt0.getMonth()  == dt1.getMonth())  && (dt1.getDay()    == dt0.getDay()) &&
                 (dt0.getHour() == dt1.getHour()) && (dt0.getMinute() == dt1.getMinute()) && (dt1.getSecond() == dt0.getSecond()));
      }

      bool operator< (const CDate& dt0, const CDate& dt1)
      {
         // TODO :: Vérifier que les deux dates (dt0 et dt1) ont une référence vers le même calendrier.
         if   (dt0.getYear()  < dt1.getYear())
         { 
            return true;
         }
         else if (dt0.getYear() == dt1.getYear())
         { 
            if   (dt0.getMonth()  < dt1.getMonth())
            {
               return true;
            }
            else if (dt0.getMonth() == dt1.getMonth())
            {
               if   (dt0.getDay()  < dt1.getDay())
               {
                   return true;
               }
               else if (dt0.getDay() == dt1.getDay())
               {
                  if    (dt0.getHour()  < dt1.getHour())
                  {
                     return true;
                  }
                  else if (dt0.getHour() == dt1.getHour())
                  { 
                     if   (dt0.getMinute()  < dt1.getMinute())
                     {
                        return true;
                     }
                     else if (dt0.getMinute() == dt1.getMinute())
                     {
                        if (dt0.getSecond() < dt1.getSecond())
                           return true;
                     }
                  }
               }
            }
         }
         return false;
      }

      //-----------------------------------------------------------------

      bool operator!=(const CDate & dt0, const CDate & dt1){ return !(dt1 == dt0); }
      bool operator> (const CDate & dt0, const CDate & dt1){ return (dt1 < dt0); }
      bool operator>=(const CDate & dt0, const CDate & dt1){ return ((dt0 > dt1) || (dt1 == dt0)); }
      bool operator<=(const CDate & dt0, const CDate & dt1){ return ((dt0 < dt1) || (dt1 == dt0)); }

      ///----------------------------------------------------------------

} // namespace xios






