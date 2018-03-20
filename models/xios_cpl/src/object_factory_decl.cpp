#include "object_factory_impl.hpp"
#include "node_type.hpp"

namespace xios
{
#define macro(U) \
  template shared_ptr<U> CObjectFactory::GetObject<U>(const StdString & id);  \
  template shared_ptr<U> CObjectFactory::GetObject<U>(const StdString& context,const StdString & id); \
  template shared_ptr<U> CObjectFactory::GetObject<U>(const U * const object); \
  template int CObjectFactory::GetObjectNum<U>(void); \
  template int CObjectFactory::GetObjectIdNum<U>(void); \
  template const std::vector<shared_ptr<U> >& CObjectFactory::GetObjectVector<U>(const StdString & context ); \
  template bool CObjectFactory::HasObject<U>(const StdString & id); \
  template bool CObjectFactory::HasObject<U>(const StdString& context,const StdString & id); \
  template boost::shared_ptr<U> CObjectFactory::CreateObject<U>(const StdString & id ); \
  template  StdString CObjectFactory::GenUId<U>(void) ; 
  
  macro(CField)
  macro(CFile)
  macro(CGrid)
  macro(CAxis)
  macro(CDomain)
  macro(CContext)
  macro(CVariable)

  macro(CFieldGroup)
  macro(CFileGroup)
  macro(CGridGroup)
  macro(CAxisGroup)
  macro(CDomainGroup)
  macro(CContextGroup)
  macro(CVariableGroup)
}
