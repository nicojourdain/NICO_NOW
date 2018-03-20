#include "xml_parser_impl.hpp"
#include "group_template.hpp"
#include "context.hpp"
#include "axis.hpp"
#include "domain.hpp"
#include "field.hpp"
#include "file.hpp"
#include "variable.hpp"


namespace xios
{
  namespace xml
  {
    template void CXMLParser::ParseInclude<CContext>(StdIStream & stream, CContext& object) ;
 
 #   define macro(T) \
    template void CXMLParser::ParseInclude< CGroupTemplate<C##T, C##T##Group, C##T##Attributes> >(StdIStream & stream, CGroupTemplate<C##T, C##T##Group, C##T##Attributes>& object) ;
    
    macro( Context ) 
    macro( Axis ) 
    macro( Domain ) 
    macro( Grid ) 
    macro( Field ) 
    macro( File ) 
    macro( Variable ) 
  }
}
