#include "object_template_impl.hpp"
#include "xmlioserver_spl.hpp"
#include "field.hpp"
#include "context.hpp"
#include "file.hpp"
#include "domain.hpp"
#include "grid.hpp"
#include "axis.hpp"
#include "variable.hpp"


namespace xios
{
  template class CObjectTemplate<CContext> ;
  template class CObjectTemplate<CField> ;
  template class CObjectTemplate<CFile> ;
  template class CObjectTemplate<CDomain> ;
  template class CObjectTemplate<CGrid> ;
  template class CObjectTemplate<CAxis> ;
  template class CObjectTemplate<CVariable> ;
  
  template class CObjectTemplate<CContextGroup> ;
  template class CObjectTemplate<CFieldGroup> ;
  template class CObjectTemplate<CFileGroup> ;
  template class CObjectTemplate<CDomainGroup> ;
  template class CObjectTemplate<CGridGroup> ;
  template class CObjectTemplate<CAxisGroup> ;
  template class CObjectTemplate<CVariableGroup> ;
  }
