#ifndef __XIOS_INDENT_HPP__
#define __XIOS_INDENT_HPP__

#include <ostream>

namespace xios
{
  class Cindent  
  {
    public:
    static int defaultIncSize;
    static int index ;
    int incSize ;
    int offset ;
    bool reset ;
    public :

    Cindent(int i=0, bool r=false) ;
    Cindent operator++(int) ;
    Cindent operator--(int) ;
    Cindent operator++() ;
    Cindent operator--() ;
    Cindent operator+=(int n) ;
    Cindent operator-=(int n) ;
    std::ostream& iendl(std::ostream& o) const ;
  };
  
  std::ostream& operator <<(std::ostream& o, const Cindent& indent) ;

  extern Cindent iendl;
  extern Cindent ireset;

}  
#endif
