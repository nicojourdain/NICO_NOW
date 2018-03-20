#include "group_factory_impl.hpp"
#include "node_type.hpp"

namespace xios
{
# define  macro(U) \
  template void CGroupFactory::AddGroup<U>(shared_ptr<U> pgroup,shared_ptr<U> cgroup); \
  template void CGroupFactory::AddChild<U>(shared_ptr<U> group, shared_ptr<U::RelChild> child); \
  template shared_ptr<U>  CGroupFactory::GetGroup<U>(shared_ptr<U> group, const StdString & id); \
  template shared_ptr<U::RelChild> CGroupFactory::GetChild<U>(shared_ptr<U> group, const StdString & id); \
  template int CGroupFactory::GetGroupNum<U>(shared_ptr<U> group); \
  template int CGroupFactory::GetGroupIdNum<U>(shared_ptr<U> group); \
  template int CGroupFactory::GetChildNum<U>(shared_ptr<U> group); \
  template int CGroupFactory::GetChildIdNum<U>(boost::shared_ptr<U> group); \
  template bool CGroupFactory::HasGroup<U>(shared_ptr<U> group, const StdString & id); \
  template bool CGroupFactory::HasChild<U>(boost::shared_ptr<U> group, const StdString & id); \
  template shared_ptr<U> CGroupFactory::CreateGroup<U>(shared_ptr<U> group, const StdString & id ); \
  template shared_ptr<U::RelChild>  CGroupFactory::CreateChild<U>(shared_ptr<U> group, const StdString & id);

  macro(CFieldGroup)
  macro(CFileGroup)
  macro(CGridGroup)
  macro(CAxisGroup)
  macro(CDomainGroup)
  macro(CContextGroup)
  macro(CVariableGroup)
}
