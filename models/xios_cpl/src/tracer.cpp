#include "tracer.hpp"
#ifdef VTRACE
#include <vt_user.h>
#endif
#include <string>

namespace xios
{
  using namespace std ;
  
  void traceOn(void)
  {
#ifdef VTRACE
    VT_ON() ;
#endif
  }
  
  void traceOff(void) 
  {
#ifdef VTRACE
    VT_OFF() ;
#endif
  }
  
  void traceBegin(const string& name)
  {
#ifdef VTRACE
    VT_USER_START(name.c_str()) ;
#endif
  }
  
  void traceEnd(const string& name)
  {
#ifdef VTRACE
    VT_USER_END(name.c_str()) ;
#endif
  }
  
//  void marker(const string& name,const string& text) ;
  
  
}
