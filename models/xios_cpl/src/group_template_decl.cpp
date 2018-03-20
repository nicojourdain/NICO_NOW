#include "group_template_impl.hpp"
#include "node_type.hpp"

namespace xios
{
#  define macro(T) \
  template class CGroupTemplate<C##T, C##T##Group, C##T##Attributes> ;
  
  macro(Context) 
  macro(Field) 
  macro(File) 
  macro(Domain) 
  macro(Grid) 
  macro(Axis) 
  macro(Variable) 
  

}
