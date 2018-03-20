

namespace xios
{
  
    
  class CEnum_color
  {
    public:
    enum t_enum { rouge=0, vert, bleu} ;
    t_enum val ;
    
    const char** getStr(void) { static const char * enumStr[] = { "rouge", "vert", "bleu" } ; return enumStr ; }   
    int getSize(void) { return 3 ; }   
  } ;
  
  template <typename CEnumType>
  class CEnum : public CEnumType
  {
    public :
//    typedef CEnum<CEnumType>::t_enum  myEnum; 
  } ;
}
     
