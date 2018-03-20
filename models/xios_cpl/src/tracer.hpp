#ifndef __TRACER_HPP__
#define __TRACER_HPP__

#include <string>

namespace xios
{
  using namespace std ;
  
  void traceOn(void) ;
  void traceOff(void) ;
  
  void traceBegin(const string& name) ;
  void traceEnd(const string& name) ;
  
  void marker(const string& name,const string& text) ;

}



#endif
