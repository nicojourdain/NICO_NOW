/* ************************************************************************** *
 *               Interface auto generated - do not modify                   *
 * ************************************************************************** */

#include <boost/multi_array.hpp>
#include <boost/shared_ptr.hpp>
#include "xmlioserver.hpp"
#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "icutil.hpp"
#include "timer.hpp"
#include "node_type.hpp"

extern "C"
{
  typedef xios::CDomainGroup*  domaingroup_Ptr;
  
  void cxios_set_domaingroup_data_dim(domaingroup_Ptr domaingroup_hdl, int data_dim)
  {
     CTimer::get("XIOS").resume();
    domaingroup_hdl->data_dim.setValue(data_dim);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_dim);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_data_dim(domaingroup_Ptr domaingroup_hdl, int* data_dim)
  {
    *data_dim = domaingroup_hdl->data_dim.getInheritedValue();
  }
  
  bool cxios_is_defined_domaingroup_data_dim(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->data_dim.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_data_i_index(domaingroup_Ptr domaingroup_hdl, int* data_i_index, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(data_i_index,shape(extent1),neverDeleteData) ;
    domaingroup_hdl->data_i_index.reference(tmp.copy());
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_i_index);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_data_i_index(domaingroup_Ptr domaingroup_hdl, int* data_i_index, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(data_i_index,shape(extent1),neverDeleteData) ;
    tmp=domaingroup_hdl->data_i_index.getInheritedValue() ;
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_domaingroup_data_i_index(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->data_i_index.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_data_ibegin(domaingroup_Ptr domaingroup_hdl, int data_ibegin)
  {
     CTimer::get("XIOS").resume();
    domaingroup_hdl->data_ibegin.setValue(data_ibegin);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_ibegin);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_data_ibegin(domaingroup_Ptr domaingroup_hdl, int* data_ibegin)
  {
    *data_ibegin = domaingroup_hdl->data_ibegin.getInheritedValue();
  }
  
  bool cxios_is_defined_domaingroup_data_ibegin(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->data_ibegin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_data_j_index(domaingroup_Ptr domaingroup_hdl, int* data_j_index, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(data_j_index,shape(extent1),neverDeleteData) ;
    domaingroup_hdl->data_j_index.reference(tmp.copy());
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_j_index);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_data_j_index(domaingroup_Ptr domaingroup_hdl, int* data_j_index, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<int,1> tmp(data_j_index,shape(extent1),neverDeleteData) ;
    tmp=domaingroup_hdl->data_j_index.getInheritedValue() ;
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_domaingroup_data_j_index(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->data_j_index.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_data_jbegin(domaingroup_Ptr domaingroup_hdl, int data_jbegin)
  {
     CTimer::get("XIOS").resume();
    domaingroup_hdl->data_jbegin.setValue(data_jbegin);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_jbegin);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_data_jbegin(domaingroup_Ptr domaingroup_hdl, int* data_jbegin)
  {
    *data_jbegin = domaingroup_hdl->data_jbegin.getInheritedValue();
  }
  
  bool cxios_is_defined_domaingroup_data_jbegin(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->data_jbegin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_data_n_index(domaingroup_Ptr domaingroup_hdl, int data_n_index)
  {
     CTimer::get("XIOS").resume();
    domaingroup_hdl->data_n_index.setValue(data_n_index);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_n_index);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_data_n_index(domaingroup_Ptr domaingroup_hdl, int* data_n_index)
  {
    *data_n_index = domaingroup_hdl->data_n_index.getInheritedValue();
  }
  
  bool cxios_is_defined_domaingroup_data_n_index(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->data_n_index.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_data_ni(domaingroup_Ptr domaingroup_hdl, int data_ni)
  {
     CTimer::get("XIOS").resume();
    domaingroup_hdl->data_ni.setValue(data_ni);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_ni);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_data_ni(domaingroup_Ptr domaingroup_hdl, int* data_ni)
  {
    *data_ni = domaingroup_hdl->data_ni.getInheritedValue();
  }
  
  bool cxios_is_defined_domaingroup_data_ni(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->data_ni.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_data_nj(domaingroup_Ptr domaingroup_hdl, int data_nj)
  {
     CTimer::get("XIOS").resume();
    domaingroup_hdl->data_nj.setValue(data_nj);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->data_nj);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_data_nj(domaingroup_Ptr domaingroup_hdl, int* data_nj)
  {
    *data_nj = domaingroup_hdl->data_nj.getInheritedValue();
  }
  
  bool cxios_is_defined_domaingroup_data_nj(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->data_nj.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_domain_group_ref(domaingroup_Ptr domaingroup_hdl, const char * domain_group_ref, int domain_group_ref_size)
  {
    std::string domain_group_ref_str;
    if(!cstr2string(domain_group_ref, domain_group_ref_size, domain_group_ref_str)) return;
     CTimer::get("XIOS").resume();
    domaingroup_hdl->domain_group_ref.setValue(domain_group_ref_str);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->domain_group_ref);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_domain_group_ref(domaingroup_Ptr domaingroup_hdl, char * domain_group_ref, int domain_group_ref_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(domaingroup_hdl->domain_group_ref.getInheritedValue(),domain_group_ref , domain_group_ref_size))
      ERROR("void cxios_get_domaingroup_domain_group_ref(domaingroup_Ptr domaingroup_hdl, char * domain_group_ref, int domain_group_ref_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_domaingroup_domain_group_ref(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->domain_group_ref.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_group_ref(domaingroup_Ptr domaingroup_hdl, const char * group_ref, int group_ref_size)
  {
    std::string group_ref_str;
    if(!cstr2string(group_ref, group_ref_size, group_ref_str)) return;
     CTimer::get("XIOS").resume();
    domaingroup_hdl->group_ref.setValue(group_ref_str);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->group_ref);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_group_ref(domaingroup_Ptr domaingroup_hdl, char * group_ref, int group_ref_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(domaingroup_hdl->group_ref.getInheritedValue(),group_ref , group_ref_size))
      ERROR("void cxios_get_domaingroup_group_ref(domaingroup_Ptr domaingroup_hdl, char * group_ref, int group_ref_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_domaingroup_group_ref(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->group_ref.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_ibegin(domaingroup_Ptr domaingroup_hdl, int ibegin)
  {
     CTimer::get("XIOS").resume();
    domaingroup_hdl->ibegin.setValue(ibegin);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->ibegin);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_ibegin(domaingroup_Ptr domaingroup_hdl, int* ibegin)
  {
    *ibegin = domaingroup_hdl->ibegin.getInheritedValue();
  }
  
  bool cxios_is_defined_domaingroup_ibegin(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->ibegin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_iend(domaingroup_Ptr domaingroup_hdl, int iend)
  {
     CTimer::get("XIOS").resume();
    domaingroup_hdl->iend.setValue(iend);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->iend);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_iend(domaingroup_Ptr domaingroup_hdl, int* iend)
  {
    *iend = domaingroup_hdl->iend.getInheritedValue();
  }
  
  bool cxios_is_defined_domaingroup_iend(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->iend.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_jbegin(domaingroup_Ptr domaingroup_hdl, int jbegin)
  {
     CTimer::get("XIOS").resume();
    domaingroup_hdl->jbegin.setValue(jbegin);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->jbegin);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_jbegin(domaingroup_Ptr domaingroup_hdl, int* jbegin)
  {
    *jbegin = domaingroup_hdl->jbegin.getInheritedValue();
  }
  
  bool cxios_is_defined_domaingroup_jbegin(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->jbegin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_jend(domaingroup_Ptr domaingroup_hdl, int jend)
  {
     CTimer::get("XIOS").resume();
    domaingroup_hdl->jend.setValue(jend);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->jend);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_jend(domaingroup_Ptr domaingroup_hdl, int* jend)
  {
    *jend = domaingroup_hdl->jend.getInheritedValue();
  }
  
  bool cxios_is_defined_domaingroup_jend(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->jend.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_latvalue(domaingroup_Ptr domaingroup_hdl, double* latvalue, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<double,1> tmp(latvalue,shape(extent1),neverDeleteData) ;
    domaingroup_hdl->latvalue.reference(tmp.copy());
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->latvalue);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_latvalue(domaingroup_Ptr domaingroup_hdl, double* latvalue, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<double,1> tmp(latvalue,shape(extent1),neverDeleteData) ;
    tmp=domaingroup_hdl->latvalue.getInheritedValue() ;
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_domaingroup_latvalue(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->latvalue.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_long_name(domaingroup_Ptr domaingroup_hdl, const char * long_name, int long_name_size)
  {
    std::string long_name_str;
    if(!cstr2string(long_name, long_name_size, long_name_str)) return;
     CTimer::get("XIOS").resume();
    domaingroup_hdl->long_name.setValue(long_name_str);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->long_name);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_long_name(domaingroup_Ptr domaingroup_hdl, char * long_name, int long_name_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(domaingroup_hdl->long_name.getInheritedValue(),long_name , long_name_size))
      ERROR("void cxios_get_domaingroup_long_name(domaingroup_Ptr domaingroup_hdl, char * long_name, int long_name_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_domaingroup_long_name(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->long_name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_lonvalue(domaingroup_Ptr domaingroup_hdl, double* lonvalue, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<double,1> tmp(lonvalue,shape(extent1),neverDeleteData) ;
    domaingroup_hdl->lonvalue.reference(tmp.copy());
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->lonvalue);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_lonvalue(domaingroup_Ptr domaingroup_hdl, double* lonvalue, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<double,1> tmp(lonvalue,shape(extent1),neverDeleteData) ;
    tmp=domaingroup_hdl->lonvalue.getInheritedValue() ;
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_domaingroup_lonvalue(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->lonvalue.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_mask(domaingroup_Ptr domaingroup_hdl, bool* mask, int extent1, int extent2)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,2> tmp(mask,shape(extent1,extent2),neverDeleteData) ;
    domaingroup_hdl->mask.reference(tmp.copy());
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->mask);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_mask(domaingroup_Ptr domaingroup_hdl, bool* mask, int extent1, int extent2)
  {
    CTimer::get("XIOS").resume();
    CArray<bool,2> tmp(mask,shape(extent1,extent2),neverDeleteData) ;
    tmp=domaingroup_hdl->mask.getInheritedValue() ;
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_domaingroup_mask(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->mask.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_name(domaingroup_Ptr domaingroup_hdl, const char * name, int name_size)
  {
    std::string name_str;
    if(!cstr2string(name, name_size, name_str)) return;
     CTimer::get("XIOS").resume();
    domaingroup_hdl->name.setValue(name_str);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->name);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_name(domaingroup_Ptr domaingroup_hdl, char * name, int name_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(domaingroup_hdl->name.getInheritedValue(),name , name_size))
      ERROR("void cxios_get_domaingroup_name(domaingroup_Ptr domaingroup_hdl, char * name, int name_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_domaingroup_name(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_ni(domaingroup_Ptr domaingroup_hdl, int ni)
  {
     CTimer::get("XIOS").resume();
    domaingroup_hdl->ni.setValue(ni);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->ni);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_ni(domaingroup_Ptr domaingroup_hdl, int* ni)
  {
    *ni = domaingroup_hdl->ni.getInheritedValue();
  }
  
  bool cxios_is_defined_domaingroup_ni(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->ni.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_ni_glo(domaingroup_Ptr domaingroup_hdl, int ni_glo)
  {
     CTimer::get("XIOS").resume();
    domaingroup_hdl->ni_glo.setValue(ni_glo);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->ni_glo);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_ni_glo(domaingroup_Ptr domaingroup_hdl, int* ni_glo)
  {
    *ni_glo = domaingroup_hdl->ni_glo.getInheritedValue();
  }
  
  bool cxios_is_defined_domaingroup_ni_glo(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->ni_glo.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_nj(domaingroup_Ptr domaingroup_hdl, int nj)
  {
     CTimer::get("XIOS").resume();
    domaingroup_hdl->nj.setValue(nj);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->nj);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_nj(domaingroup_Ptr domaingroup_hdl, int* nj)
  {
    *nj = domaingroup_hdl->nj.getInheritedValue();
  }
  
  bool cxios_is_defined_domaingroup_nj(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->nj.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_nj_glo(domaingroup_Ptr domaingroup_hdl, int nj_glo)
  {
     CTimer::get("XIOS").resume();
    domaingroup_hdl->nj_glo.setValue(nj_glo);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->nj_glo);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_nj_glo(domaingroup_Ptr domaingroup_hdl, int* nj_glo)
  {
    *nj_glo = domaingroup_hdl->nj_glo.getInheritedValue();
  }
  
  bool cxios_is_defined_domaingroup_nj_glo(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->nj_glo.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_standard_name(domaingroup_Ptr domaingroup_hdl, const char * standard_name, int standard_name_size)
  {
    std::string standard_name_str;
    if(!cstr2string(standard_name, standard_name_size, standard_name_str)) return;
     CTimer::get("XIOS").resume();
    domaingroup_hdl->standard_name.setValue(standard_name_str);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->standard_name);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_standard_name(domaingroup_Ptr domaingroup_hdl, char * standard_name, int standard_name_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(domaingroup_hdl->standard_name.getInheritedValue(),standard_name , standard_name_size))
      ERROR("void cxios_get_domaingroup_standard_name(domaingroup_Ptr domaingroup_hdl, char * standard_name, int standard_name_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_domaingroup_standard_name(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->standard_name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_zoom_ibegin(domaingroup_Ptr domaingroup_hdl, int zoom_ibegin)
  {
     CTimer::get("XIOS").resume();
    domaingroup_hdl->zoom_ibegin.setValue(zoom_ibegin);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->zoom_ibegin);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_zoom_ibegin(domaingroup_Ptr domaingroup_hdl, int* zoom_ibegin)
  {
    *zoom_ibegin = domaingroup_hdl->zoom_ibegin.getInheritedValue();
  }
  
  bool cxios_is_defined_domaingroup_zoom_ibegin(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->zoom_ibegin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_zoom_ibegin_loc(domaingroup_Ptr domaingroup_hdl, int zoom_ibegin_loc)
  {
     CTimer::get("XIOS").resume();
    domaingroup_hdl->zoom_ibegin_loc.setValue(zoom_ibegin_loc);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->zoom_ibegin_loc);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_zoom_ibegin_loc(domaingroup_Ptr domaingroup_hdl, int* zoom_ibegin_loc)
  {
    *zoom_ibegin_loc = domaingroup_hdl->zoom_ibegin_loc.getInheritedValue();
  }
  
  bool cxios_is_defined_domaingroup_zoom_ibegin_loc(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->zoom_ibegin_loc.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_zoom_jbegin(domaingroup_Ptr domaingroup_hdl, int zoom_jbegin)
  {
     CTimer::get("XIOS").resume();
    domaingroup_hdl->zoom_jbegin.setValue(zoom_jbegin);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->zoom_jbegin);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_zoom_jbegin(domaingroup_Ptr domaingroup_hdl, int* zoom_jbegin)
  {
    *zoom_jbegin = domaingroup_hdl->zoom_jbegin.getInheritedValue();
  }
  
  bool cxios_is_defined_domaingroup_zoom_jbegin(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->zoom_jbegin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_zoom_jbegin_loc(domaingroup_Ptr domaingroup_hdl, int zoom_jbegin_loc)
  {
     CTimer::get("XIOS").resume();
    domaingroup_hdl->zoom_jbegin_loc.setValue(zoom_jbegin_loc);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->zoom_jbegin_loc);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_zoom_jbegin_loc(domaingroup_Ptr domaingroup_hdl, int* zoom_jbegin_loc)
  {
    *zoom_jbegin_loc = domaingroup_hdl->zoom_jbegin_loc.getInheritedValue();
  }
  
  bool cxios_is_defined_domaingroup_zoom_jbegin_loc(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->zoom_jbegin_loc.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_zoom_ni(domaingroup_Ptr domaingroup_hdl, int zoom_ni)
  {
     CTimer::get("XIOS").resume();
    domaingroup_hdl->zoom_ni.setValue(zoom_ni);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->zoom_ni);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_zoom_ni(domaingroup_Ptr domaingroup_hdl, int* zoom_ni)
  {
    *zoom_ni = domaingroup_hdl->zoom_ni.getInheritedValue();
  }
  
  bool cxios_is_defined_domaingroup_zoom_ni(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->zoom_ni.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_zoom_ni_loc(domaingroup_Ptr domaingroup_hdl, int zoom_ni_loc)
  {
     CTimer::get("XIOS").resume();
    domaingroup_hdl->zoom_ni_loc.setValue(zoom_ni_loc);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->zoom_ni_loc);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_zoom_ni_loc(domaingroup_Ptr domaingroup_hdl, int* zoom_ni_loc)
  {
    *zoom_ni_loc = domaingroup_hdl->zoom_ni_loc.getInheritedValue();
  }
  
  bool cxios_is_defined_domaingroup_zoom_ni_loc(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->zoom_ni_loc.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_zoom_nj(domaingroup_Ptr domaingroup_hdl, int zoom_nj)
  {
     CTimer::get("XIOS").resume();
    domaingroup_hdl->zoom_nj.setValue(zoom_nj);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->zoom_nj);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_zoom_nj(domaingroup_Ptr domaingroup_hdl, int* zoom_nj)
  {
    *zoom_nj = domaingroup_hdl->zoom_nj.getInheritedValue();
  }
  
  bool cxios_is_defined_domaingroup_zoom_nj(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->zoom_nj.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_domaingroup_zoom_nj_loc(domaingroup_Ptr domaingroup_hdl, int zoom_nj_loc)
  {
     CTimer::get("XIOS").resume();
    domaingroup_hdl->zoom_nj_loc.setValue(zoom_nj_loc);
    domaingroup_hdl->sendAttributToServer(domaingroup_hdl->zoom_nj_loc);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_domaingroup_zoom_nj_loc(domaingroup_Ptr domaingroup_hdl, int* zoom_nj_loc)
  {
    *zoom_nj_loc = domaingroup_hdl->zoom_nj_loc.getInheritedValue();
  }
  
  bool cxios_is_defined_domaingroup_zoom_nj_loc(domaingroup_Ptr domaingroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return domaingroup_hdl->zoom_nj_loc.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  
}
