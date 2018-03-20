#include <iostream>
#include <sstream>
#include <string>

#include <boost/date_time/gregorian/gregorian.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include "calendar_type.hpp"
#include "date.hpp"
#include "calendar_util.hpp"
//#include "test_enum.hpp"
#include "type.hpp"
//#include "axis.hpp"
//#include "field.hpp"
#include "declare_attribute.hpp"
#include "attribute_map.hpp"
#include "array_new.hpp"
#include "attribute_array.hpp"
#include "attribute_array_impl.hpp"



using namespace std ;
using namespace boost::posix_time ;
using namespace boost::gregorian ;
using namespace xios;
using namespace blitz;

  class CEnum_color
  {
    public:
    enum t_enum { rouge=0, vert, bleu} ;
    static const char** getStr(void) { static const char * enumStr[] = { "rouge", "vert", "bleu" } ; return enumStr ; }   
    int getSize(void) { return 3 ; }   
  } ;

#include "enum.hpp"
#include "enum_impl.hpp"
#include "enum_ref_impl.hpp"
#include "attribute_enum.hpp"
#include "attribute_enum_impl.hpp"

template class CEnum<CEnum_color> ;
template class CAttributeEnum<CEnum_color> ;


int main(void)
{
//      ptime t(time_from_string("2012-02-30 15:24")) ;
//      std::cout << to_simple_string(t) << std::endl;
  CGregorianCalendar MyCalendar("2011-03-01 00:00") ;
  cout<<MyCalendar.getInitDate()<<endl;
  cout<<MyCalendar.getCurrentDate()<<endl ;
  cout<<MyCalendar.getCurrentDate()-1*Day<<endl ;
      
  //CEnum<CEnum_color> MyEnum ;
  //MyEnum.val=CEnum<CEnum_color>::rouge ;
  
  CEnum_color::t_enum y;
  
  CType<int> a(10) ;
  CType<int> b ;
  
  a=5 ;
  b=10.5 ;
  a=a+b ;
  cout<<a<<endl ;
  if (a==5) cout<<"a que coucou"<<endl ;
  else if (a!=5) cout<<"a que pas coucou"<<endl ;
  
  CType_ref<int> c(a);
  cout<<c<<endl ;
  a=3 ;
  cout<<c<<endl ;
//  c=b ;
  cout<<c<<endl ;
  b=4 ;
  cout<<c<<endl ;
  c.set_ref(b) ;
  cout<<c<<endl ;
  int x=c+c+a ;
  cout<<x<<endl ;
 
//  test::CAttributeTemplate<int> xx("toto") ;
//  test::totoatt toto ;
//  test::titiatt titi ;
//  CAxis A ;
//  CTest A ;
//  CAxis B ;
//    CField A ;
//    cout<<xx.size()<<endl ;

  CEnum<CEnum_color> color,color1 ;
  color=CEnum<CEnum_color>::rouge ;
  color1=color ;
  
  if (color1==CEnum<CEnum_color>::rouge) cout<<"Rouge !!"<<endl ;
  if (color1==color) cout<<"Rouge !!"<<endl ;
  color=CEnum<CEnum_color>::bleu ;
 
  if (color1==color) cout<<"Rouge !!"<<endl ;
  
  cout<<color1.toString()<<endl ;
  color1.fromString("vert ") ;
  cout<<color1.toString()<<endl ;
 
//  color1.fromString("jaune") ;
  cout<<color1.toString()<<endl ;
  cout<<sizeof(CEnum_color::t_enum)<<endl ; 
  
  CEnum_color::t_enum tt ;
  tt=(CEnum_color::t_enum)1 ;
  int ii=tt ;
  cout<<ii<<endl ;
  
  CAttributeEnum<CEnum_color> toto("toto",CEnum_color::rouge) ;
  CAttributeEnum<CEnum_color> titi("titi",CEnum_color::vert) ;
  cout<<toto.toString()<<endl ;
  toto.fromString(" vert  ") ;
  cout<<toto.toString()<<endl ;

  tt=toto ;
  cout<<tt<<endl ;
  cout<<titi.toString()<<endl ;
  
  if (titi==toto) cout<<"titi==toto"<<endl ;
  else cout<<"titi!=toto"<<endl ;
  titi=CEnum_color::rouge ;
  if (titi==toto) cout<<"titi==toto"<<endl ;
  else cout<<"titi!=toto"<<endl ;

  CArray<int,2> A(2,2) ;
  CArray<int,2> B(2,2) ;
  A=1,2,3,4 ;
  B = 1 ;
  B+=A ;
  cout<<B.toString()<<endl ;
  cout<<A.toString()<<endl ;
  string str="(0,1) x (0,1) [4 3 2 1]" ;
  CArray<int,2> C ;
  CBufferOut out(B.size()) ;
  
  B.toBuffer(out) ;
  
  CBufferIn in(out.begin,B.size()) ;
  C.fromBuffer(in) ;
  
  cout<<C<<endl ; 
  
  CAttributeArray<int,2> attr("toto") ;
  attr.resize(2,2) ;
  attr=C ;
  cout<<"attr "<<attr<<endl ;
  cout<<attr.toString()<<endl ;
  return 1 ;  
}
