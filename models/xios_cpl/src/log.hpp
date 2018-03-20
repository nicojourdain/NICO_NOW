#ifndef __XIOS_LOG_HPP__
#define __XIOS_LOG_HPP__

#include <string>
#include <iostream>
#include <string>

namespace xios
{
  using namespace std ;

  class CLog : public ostream
  {
    public :
    CLog(const string& name_) : ostream(cout.rdbuf()),level(0),name(name_) {}
    CLog& operator()(int l) 
    {  
      if (l<=level) 
      {
        rdbuf(cout.rdbuf()) ;
        *this<<"-> "<<name<<" : " ;
      }
      else rdbuf(NULL) ;
      return *this;
    }
    void setLevel(int l) {level=l; } 
    int getLevel() {return level ;}
    bool isActive(void) { if (rdbuf()==NULL) return true ; else return false ;}
    bool isActive(int l) {if (l<=level) return true ; else return false ; }

    private :
    int level ;
    string name ;
  };

  extern CLog info;
  extern CLog report;
}
#endif
