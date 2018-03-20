#include "date.hpp"
#include "calendar.hpp"
#include "calendar_type.hpp"
#include <boost/date_time/gregorian/gregorian.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

using namespace boost::posix_time ;
using namespace boost::gregorian ;

namespace xios
{
      /// ////////////////////// Définitions ////////////////////// ///
      CDate::CDate(const CCalendar& calendar)
         : relCalendar(calendar)
         , year(0), month(1) , day(1)
         , hour(0), minute(0), second(0)
      {   }
      
      CDate::CDate(const CCalendar& calendar,
                   int yr, int mth, int d,
                   int hr, int min, int sec)
         : relCalendar(calendar)
         , year(yr), month(mth) , day(d)
         , hour(hr), minute(min), second(sec)
      {
         if(!this->checkDate())
         {
            DEBUG(<< "La date initialisée a été modifiée "
                  << "car elle était incorrecte par rapport au calendrier souhaité.");
         }
      }

      CDate::CDate(const CDate & date)
            : relCalendar(date.getRelCalendar()),
              year(date.year), month(date.month)  , day(date.day),
              hour(date.hour), minute(date.minute), second(date.second)
      {
         if(!this->checkDate())
         {
            DEBUG(<< "La date initialisée a été modifiée "
                  << "car elle était incorrecte par rapport au calendrier souhaité.");
         }
      }

      CDate::~CDate(void)
      { /* Ne rien faire de plus */ }

      ///---------------------------------------------------------------

      CDate & CDate::operator=(const CDate & date)
      {
         // relCalendar = d.getRelCalendar(); << inutile si fonction bien utilisée
         year = date.year; month  = date.month ; day    = date.day;
         hour = date.hour; minute = date.minute; second = date.second;
         return (*this);
      }

      StdOStream & operator<<(StdOStream & out, const CDate & date)
      {
         std::streamsize s ; 
         char c ;
         
         s = out.width (4);  c = out.fill ('0') ; out << date.year << '-';
         s = out.width (2);  c = out.fill ('0') ; out << date.month << '-';
         s = out.width (2);  c = out.fill ('0') ; out << date.day << ' ';
         s = out.width (2);  c = out.fill ('0') ; out << date.hour << ':';
         s = out.width (2);  c = out.fill ('0') ; out << date.minute << ':';
         s = out.width (2);  c = out.fill ('0') ; out << date.second ;

         return (out);
      }

      StdIStream & operator>>(StdIStream & in, CDate & date) // Non testée.
      {
        char sep = '-'; // Le caractère c est utilisé pour "recueillir" les séparateurs "/" et ":".
        char c ;
         
        in >> date.year >> c ;
        if (c==sep)
        {
          in >> date.month >> c ;
          if (c==sep)
          {
            in >> date.day  ;
            c=in.get();
            sep=' ' ;
            if (c==sep)
            {
              in >> date.hour >> c ;
              sep=':' ;
              if (c==sep) 
              {
                in>>date.minute >> c;
                if (c==sep)
                {
                  in>>date.second ;
                  if(!date.checkDate())
                    ERROR("StdIStream & operator >> (StdIStream & in, CDate & date)",<<"Bad date format or not conform to calendar" );
                    return (in);
                }
              }
            }
          }
        }
        ERROR("StdIStream & operator >> (StdIStream & in, CDate & date)",<<"Bad date format or not conform to calendar" );
        return (in);
      }

      CDate::operator Time(void) const // Non vérifiée, pas optimisée ...
      {
         // Todo : Tester si la date courante est supérieure à la date initiale.
         Time retvalue = - relCalendar.getNbSecond(relCalendar.getInitDate())
                         + relCalendar.getNbSecond(*this);

         if ((relCalendar.getId().compare("D360")    == 0) ||
             (relCalendar.getId().compare("AllLeap") == 0) ||
             (relCalendar.getId().compare("NoLeap")  == 0))
         return (retvalue + (getYear() - relCalendar.getTimeOrigin().getYear())
                                       * relCalendar.getYearTotalLength(*this));

         for(CDate _d(relCalendar.getTimeOrigin());
            _d.getYear() < getYear(); _d.setYear(_d.getYear()+1))
            retvalue += relCalendar.getYearTotalLength(_d);
         return (retvalue);
      }

      //----------------------------------------------------------------

      bool CDate::checkDate(void)
      {
         bool retValue = true;

         // Vérificatio de la valeur du mois.
         if (month  < 1) { retValue = false; month  = 1; }
         if (month  > relCalendar.getYearLength())
         { retValue = false; month = relCalendar.getYearLength(); }

         // Vérification de la valeur du jour.
         if (day    < 1) { retValue = false; month  = 1; }
         if (day    > (&relCalendar)->getMonthLength(*this))
         { retValue = false; day = (&relCalendar)->getMonthLength(*this); }

         // Vérification de la valeur de l'heure.
         if (hour   < 0) { retValue = false; hour  = 0; }
         if (hour   >= relCalendar.getDayLength())
         { retValue = false; hour = relCalendar.getDayLength()-1; }

         // Vérification de la valeur des minutes.
         if (minute < 0) { retValue = false; minute = 0; }
         if (minute >= relCalendar.getHourLength())
         { retValue = false; minute = relCalendar.getHourLength()-1; }

         // Vérification de la valeur des secondes.
         if (second < 0) { retValue = false; month  = 0; }
         if (second >= relCalendar.getMinuteLength())
         { retValue = false; second = relCalendar.getMinuteLength()-1; }

         return retValue;
      }

      //----------------------------------------------------------------

      int CDate::getYear  (void) const { return (this->year  ); }
      int CDate::getMonth (void) const { return (this->month ); }
      int CDate::getDay   (void) const { return (this->day   ); }
      int CDate::getHour  (void) const { return (this->hour  ); }
      int CDate::getMinute(void) const { return (this->minute); }
      int CDate::getSecond(void) const { return (this->second); }

      //----------------------------------------------------------------

      const CCalendar & CDate::getRelCalendar(void) const
      { return (this->relCalendar); }

      //----------------------------------------------------------------

      void CDate::setYear  (int newyear)  { this->year  = newyear; }
      void CDate::setMonth (int newmonth) { this->month = newmonth; }
      void CDate::setDay (int newday) { this->day = newday; }

      //----------------------------------------------------------------

      void CDate::addMonth (int value)
      {// Value doit être égale à 1 ou -1.
         this->month += value;
         if (this->month == 13) { year++; this->month = 1 ; }
         if (this->month == 0 ) { year--; this->month = 12; }
      }

      //----------------------------------------------------------------

      CDate CDate::FromString(const StdString & str, const CCalendar & calendar)
      {
         CDate dt(calendar);
         StdIStringStream iss(str);
         iss >> dt;
         return dt;
      }
      
      //----------------------------------------------------------------
      
      StdString CDate::getStryyyymmdd(void) const
      { 
         std::streamsize s ; 
         char c ;

         ostringstream oss ;

         s = oss.width (4);  c = oss.fill ('0') ; oss << year ;
         s = oss.width (2);  c = oss.fill ('0') ; oss << month;
         s = oss.width (2);  c = oss.fill ('0') ; oss << day ;

         return oss.str();
      }
      

      string CDate::getStr(const string& str) const
      {
        ostringstream oss ;
        int level;
        
        level=0 ;
        for(string::const_iterator it=str.begin();it!=str.end();++it)
        {
          if (level==0)
          {
            if (*it=='%') level++ ;
            else oss<<*it ;
          }
          else if (level==1)
          {
            switch (*it)
            {
              case 'y' :
                oss.width (4);  oss.fill ('0') ; oss << year ;
                level=0 ;
                break ;
              case 'm' : // month or minute
                level++ ;
                break ;
              case 'd' :
                oss.width (2);  oss.fill ('0') ; oss << day ;
                level=0;
                break ;
              case 'h' :
                oss.width (2);  oss.fill ('0') ; oss << hour ;
                level=0;
                break ;
              case 's' :
                oss.width (2);  oss.fill ('0') ; oss << second ;
                level=0 ;
                break;
              default :
                oss<<'%'<<*it ;
                level=0 ;
            }
          }
          else if (level==2)
          {
            switch (*it)
            {
              case 'o' : // month
                oss.width (2);  oss.fill ('0') ; oss << month ;
                level=0 ;
                break ;
              case 'i' : //minute
                oss.width (2);  oss.fill ('0') ; oss << minute ;
                level=0 ;
                break ;
              default :
                oss<<"%m"<<*it ;
                level=0 ;
            }
          }
        }
        return oss.str();
      }
      
      StdString CDate::toString(void) const
      { 
         StdOStringStream oss;
         oss << (*this);
         return (oss.str()); 
      }

      ///---------------------------------------------------------------

} // namespace xios
