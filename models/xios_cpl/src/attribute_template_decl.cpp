#include "attribute_template_impl.hpp"
#include "attribute_template_specialisation.hpp"
#include <string>

namespace xios
{
  template class CAttributeTemplate<int> ;
  template class CAttributeTemplate<double> ;
  template class CAttributeTemplate<bool> ;
  template class CAttributeTemplate<string> ;
}
