#include "indent.hpp"
#include <ostream>
#include <iostream>

using namespace std ;

namespace xios
{
  Cindent iendl ;
  Cindent ireset(0,true) ;
  int Cindent::defaultIncSize=2 ;
  int Cindent::index=ios::xalloc() ;
  
  Cindent::Cindent(int i,bool r) : offset(i), reset(r), incSize(defaultIncSize) 
  { }
  
  Cindent Cindent::operator++()
  {
    return Cindent(incSize) ;
  }
  
  Cindent Cindent::operator--()
  {
    return Cindent(-incSize) ;
  }

  Cindent Cindent::operator++(int)
  {
    return Cindent(incSize) ;
  }
  
  Cindent Cindent::operator--(int)
  {
    return Cindent(-incSize) ;
  }

  Cindent Cindent::operator+=(int i)
  {
    return Cindent(incSize*i) ;
  }  

  Cindent Cindent::operator-=(int i)
  {
    return Cindent(-incSize*i) ;
  }  

  ostream& Cindent::iendl(ostream& o) const
  {
    if (reset)
    {
      o.iword(index)=0 ;
      return o ;
    }
    else
    {
      o.iword(index)+=offset ;
      if (o.iword(index)<0) o.iword(index)=0 ;
      o<<"\n" ;
      int mem=o.width(o.iword(index)) ;
      o<<"";
      o.width(mem) ;
      return o ;
    }
  }
  
  ostream& operator <<(ostream& o, const Cindent& indent) 
  {
    return indent.iendl(o) ; 
  }
  
}
