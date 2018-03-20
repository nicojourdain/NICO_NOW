#include "timer.hpp"
#include "mpi.hpp"
#include <string>
#include <map>
#include "tracer.hpp"

namespace xios
{
  using namespace std;
 
  map<string,CTimer*> CTimer::allTimer ;
  
  CTimer::CTimer(const string& name_) : name(name_) 
  { 
    reset() ;
  }

  double CTimer::getTime(void)
  {
    return MPI_Wtime();
  }
  
  void CTimer::suspend(void)
  {
    if (!suspended) 
    {
      traceEnd(name) ;
      cumulatedTime+=getTime()-lastTime ;
    }
    suspended=true ;
  }
  
  void CTimer::resume(void)
  {
    if (suspended) 
    {
      lastTime=getTime() ;
      traceBegin(name) ;
    }
    suspended=false ;
  }
  
  void CTimer::reset(void)
  {
    cumulatedTime=0. ;
    suspended=true ;
  }
  
  double CTimer::getCumulatedTime(void)
  {
    return cumulatedTime ;
  }
  
  CTimer& CTimer::get(const string name)
  {
    map<string,CTimer*>::iterator it ;
    it=allTimer.find(name) ;
    if (it==allTimer.end()) it=allTimer.insert(pair<string,CTimer*>(name,new CTimer(name))).first ;
    return *(it->second) ;
  }
}
