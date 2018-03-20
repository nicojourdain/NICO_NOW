#include "calendar.hpp"
#include "duration.hpp"
#include "date.hpp"
#include "calendar_util.hpp"

namespace xios
{
      /// ////////////////////// Définitions ////////////////////// ///
      CCalendar::CCalendar(void)
         : CObject()
         , initDate(*this)
         , timeOrigin(*this)
         , currentDate(*this)
      {   }

      CCalendar::CCalendar(const StdString & id)
               : CObject(id)
               , initDate(*this)
               , timeOrigin(*this)
               , currentDate(*this)
      { }
      
      CCalendar::CCalendar(const StdString & id,
                           int yr, int mth, int d  ,
                           int hr, int min, int sec)
               : CObject(id)
               , initDate(*this)
               , timeOrigin(*this)
               , currentDate(*this)
      { 
        initializeDate(yr, mth, d, hr, min, sec) ;
      }

      CCalendar::CCalendar(const StdString & id, const StdString & dateStr)
               : CObject(id)
               , initDate(CDate::FromString(dateStr, *this))
               , timeOrigin(initDate)
               , currentDate(initDate)
      { 
        initializeDate(dateStr) ;
      }

      CCalendar::CCalendar(const StdString & id, const StdString & dateStr, const StdString & timeOriginStr)
               : CObject(id)
               , initDate(*this)
               , timeOrigin(*this)
               , currentDate(*this)
      { 
        initializeDate(dateStr, timeOriginStr) ;
      }


      void CCalendar::initializeDate( int yr, int mth, int d  ,
                                 int hr, int min, int sec)
      { 
        initDate=CDate(*this,yr, mth, d, hr, min, sec) ;
        timeOrigin=initDate;
        currentDate=initDate ;
      }

      void CCalendar::initializeDate(const StdString & dateStr)
      { 
        initDate=CDate::FromString(dateStr, *this) ;
        timeOrigin=initDate ;
        currentDate=initDate ;
      }

      void CCalendar::initializeDate(const StdString & dateStr, const StdString & timeOriginStr)
      { 
        initDate=CDate::FromString(dateStr, *this) ;
        timeOrigin=CDate::FromString(timeOriginStr, *this) ;
        currentDate=initDate ;
      }
      

      CCalendar::~CCalendar(void)
      { /* Ne rien faire de plus */ }

      ///---------------------------------------------------------------

      StdString CCalendar::toString(void) const
      {
         StdOStringStream oss;
         oss <<   "[type: "   << this->getId()
             << ", start: "   << this->initDate
             << ", current: " << this->currentDate << "]";
         return (oss.str());
      }

      void CCalendar::fromString(const StdString & str)
      { ERROR("CCalendar::fromString(str)",
               << "[ str = " << str << "] Not implemented yet !"); }

      //-----------------------------------------------------------------

      void CCalendar::setTimeStep(const CDuration & duration)
      { this->timestep = duration; }

      CDate & CCalendar::update(int step)
      { 
         info(20) << "update step : " << step <<" timestep "<<this->timestep << std::endl;
         return (this->getCurrentDate() = this->getInitDate() + step * this->timestep);
      }

      //-----------------------------------------------------------------

      const CDuration & CCalendar::getTimeStep(void) const { return (this->timestep); }
      const CDate & CCalendar::getInitDate(void) const     { return (this->initDate); }
      const CDate & CCalendar::getTimeOrigin(void) const     { return (this->timeOrigin); }
      CDate & CCalendar::getCurrentDate(void)              { return (this->currentDate); }

      //-----------------------------------------------------------------

      int CCalendar::getMonthLength(const CDate & date) const
      { // Retourne la durée du mois en jour.
         static const int NoLeapMonthLength[] =
            {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
         return (NoLeapMonthLength[date.getMonth()-1]);
      }

      StdString CCalendar::getType(void) const { return (StdString(this->getId())); }

      int CCalendar::getYearTotalLength(const CDate & date) const { return (365 * 86400); }

      int CCalendar::getYearLength  (void) const { return (12); }
      int CCalendar::getDayLength   (void) const { return (24); }
      int CCalendar::getHourLength  (void) const { return (60); }
      int CCalendar::getMinuteLength(void) const { return (60); }

      int CCalendar::getNbSecond(const CDate & date) const
      { // Retourne le nombre de secondes écoulées depuis le début de l'année.
         CDate _d0(date); int  nbday = 0;

         for(_d0.setMonth(1); _d0.getMonth() < date.getMonth(); _d0.setMonth(_d0.getMonth()+1))
            nbday += getMonthLength(_d0);
         return ((((nbday + date.getDay()) * getDayLength() + date.getHour()) * getHourLength()
                     + date.getMinute()) * getMinuteLength() + date.getSecond());
      }

      StdString CCalendar::getMonthName(int month_id) const
      {
         static const StdString Monthname_str[] =
            { "january", "february", "march"    , "april"  , "may"     , "june"    ,
              "july"   , "august"  , "september", "october", "november", "december" };
         return(Monthname_str[month_id-1]);
      }

      const StdString CCalendar::getMonthShortName(int month_id) const
      { StdString value = this->getMonthName(month_id); value.resize(3); return (value); }

      ///----------------------------------------------------------------

} // namespace xios
