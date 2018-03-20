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
  typedef xios::CAxisGroup*  axisgroup_Ptr;
  
  void cxios_set_axisgroup_group_ref(axisgroup_Ptr axisgroup_hdl, const char * group_ref, int group_ref_size)
  {
    std::string group_ref_str;
    if(!cstr2string(group_ref, group_ref_size, group_ref_str)) return;
     CTimer::get("XIOS").resume();
    axisgroup_hdl->group_ref.setValue(group_ref_str);
    axisgroup_hdl->sendAttributToServer(axisgroup_hdl->group_ref);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_axisgroup_group_ref(axisgroup_Ptr axisgroup_hdl, char * group_ref, int group_ref_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(axisgroup_hdl->group_ref.getInheritedValue(),group_ref , group_ref_size))
      ERROR("void cxios_get_axisgroup_group_ref(axisgroup_Ptr axisgroup_hdl, char * group_ref, int group_ref_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_axisgroup_group_ref(axisgroup_Ptr axisgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return axisgroup_hdl->group_ref.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_axisgroup_long_name(axisgroup_Ptr axisgroup_hdl, const char * long_name, int long_name_size)
  {
    std::string long_name_str;
    if(!cstr2string(long_name, long_name_size, long_name_str)) return;
     CTimer::get("XIOS").resume();
    axisgroup_hdl->long_name.setValue(long_name_str);
    axisgroup_hdl->sendAttributToServer(axisgroup_hdl->long_name);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_axisgroup_long_name(axisgroup_Ptr axisgroup_hdl, char * long_name, int long_name_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(axisgroup_hdl->long_name.getInheritedValue(),long_name , long_name_size))
      ERROR("void cxios_get_axisgroup_long_name(axisgroup_Ptr axisgroup_hdl, char * long_name, int long_name_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_axisgroup_long_name(axisgroup_Ptr axisgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return axisgroup_hdl->long_name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_axisgroup_name(axisgroup_Ptr axisgroup_hdl, const char * name, int name_size)
  {
    std::string name_str;
    if(!cstr2string(name, name_size, name_str)) return;
     CTimer::get("XIOS").resume();
    axisgroup_hdl->name.setValue(name_str);
    axisgroup_hdl->sendAttributToServer(axisgroup_hdl->name);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_axisgroup_name(axisgroup_Ptr axisgroup_hdl, char * name, int name_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(axisgroup_hdl->name.getInheritedValue(),name , name_size))
      ERROR("void cxios_get_axisgroup_name(axisgroup_Ptr axisgroup_hdl, char * name, int name_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_axisgroup_name(axisgroup_Ptr axisgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return axisgroup_hdl->name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_axisgroup_positive(axisgroup_Ptr axisgroup_hdl, const char * positive, int positive_size)
  {
    std::string positive_str;
    if(!cstr2string(positive, positive_size, positive_str)) return;
     CTimer::get("XIOS").resume();
    axisgroup_hdl->positive.fromString(positive_str);
    axisgroup_hdl->sendAttributToServer(axisgroup_hdl->positive);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_axisgroup_positive(axisgroup_Ptr axisgroup_hdl, char * positive, int positive_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(axisgroup_hdl->positive.getInheritedStringValue(),positive , positive_size))
      ERROR("void cxios_get_axisgroup_positive(axisgroup_Ptr axisgroup_hdl, char * positive, int positive_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_axisgroup_positive(axisgroup_Ptr axisgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return axisgroup_hdl->positive.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_axisgroup_size(axisgroup_Ptr axisgroup_hdl, int size)
  {
     CTimer::get("XIOS").resume();
    axisgroup_hdl->size.setValue(size);
    axisgroup_hdl->sendAttributToServer(axisgroup_hdl->size);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_axisgroup_size(axisgroup_Ptr axisgroup_hdl, int* size)
  {
    *size = axisgroup_hdl->size.getInheritedValue();
  }
  
  bool cxios_is_defined_axisgroup_size(axisgroup_Ptr axisgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return axisgroup_hdl->size.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_axisgroup_standard_name(axisgroup_Ptr axisgroup_hdl, const char * standard_name, int standard_name_size)
  {
    std::string standard_name_str;
    if(!cstr2string(standard_name, standard_name_size, standard_name_str)) return;
     CTimer::get("XIOS").resume();
    axisgroup_hdl->standard_name.setValue(standard_name_str);
    axisgroup_hdl->sendAttributToServer(axisgroup_hdl->standard_name);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_axisgroup_standard_name(axisgroup_Ptr axisgroup_hdl, char * standard_name, int standard_name_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(axisgroup_hdl->standard_name.getInheritedValue(),standard_name , standard_name_size))
      ERROR("void cxios_get_axisgroup_standard_name(axisgroup_Ptr axisgroup_hdl, char * standard_name, int standard_name_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_axisgroup_standard_name(axisgroup_Ptr axisgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return axisgroup_hdl->standard_name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_axisgroup_unit(axisgroup_Ptr axisgroup_hdl, const char * unit, int unit_size)
  {
    std::string unit_str;
    if(!cstr2string(unit, unit_size, unit_str)) return;
     CTimer::get("XIOS").resume();
    axisgroup_hdl->unit.setValue(unit_str);
    axisgroup_hdl->sendAttributToServer(axisgroup_hdl->unit);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_axisgroup_unit(axisgroup_Ptr axisgroup_hdl, char * unit, int unit_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(axisgroup_hdl->unit.getInheritedValue(),unit , unit_size))
      ERROR("void cxios_get_axisgroup_unit(axisgroup_Ptr axisgroup_hdl, char * unit, int unit_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_axisgroup_unit(axisgroup_Ptr axisgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return axisgroup_hdl->unit.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_axisgroup_value(axisgroup_Ptr axisgroup_hdl, double* value, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<double,1> tmp(value,shape(extent1),neverDeleteData) ;
    axisgroup_hdl->value.reference(tmp.copy());
    axisgroup_hdl->sendAttributToServer(axisgroup_hdl->value);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_axisgroup_value(axisgroup_Ptr axisgroup_hdl, double* value, int extent1)
  {
    CTimer::get("XIOS").resume();
    CArray<double,1> tmp(value,shape(extent1),neverDeleteData) ;
    tmp=axisgroup_hdl->value.getInheritedValue() ;
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_axisgroup_value(axisgroup_Ptr axisgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return axisgroup_hdl->value.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_axisgroup_zoom_begin(axisgroup_Ptr axisgroup_hdl, int zoom_begin)
  {
     CTimer::get("XIOS").resume();
    axisgroup_hdl->zoom_begin.setValue(zoom_begin);
    axisgroup_hdl->sendAttributToServer(axisgroup_hdl->zoom_begin);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_axisgroup_zoom_begin(axisgroup_Ptr axisgroup_hdl, int* zoom_begin)
  {
    *zoom_begin = axisgroup_hdl->zoom_begin.getInheritedValue();
  }
  
  bool cxios_is_defined_axisgroup_zoom_begin(axisgroup_Ptr axisgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return axisgroup_hdl->zoom_begin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_axisgroup_zoom_end(axisgroup_Ptr axisgroup_hdl, int zoom_end)
  {
     CTimer::get("XIOS").resume();
    axisgroup_hdl->zoom_end.setValue(zoom_end);
    axisgroup_hdl->sendAttributToServer(axisgroup_hdl->zoom_end);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_axisgroup_zoom_end(axisgroup_Ptr axisgroup_hdl, int* zoom_end)
  {
    *zoom_end = axisgroup_hdl->zoom_end.getInheritedValue();
  }
  
  bool cxios_is_defined_axisgroup_zoom_end(axisgroup_Ptr axisgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return axisgroup_hdl->zoom_end.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_axisgroup_zoom_size(axisgroup_Ptr axisgroup_hdl, int zoom_size)
  {
     CTimer::get("XIOS").resume();
    axisgroup_hdl->zoom_size.setValue(zoom_size);
    axisgroup_hdl->sendAttributToServer(axisgroup_hdl->zoom_size);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_axisgroup_zoom_size(axisgroup_Ptr axisgroup_hdl, int* zoom_size)
  {
    *zoom_size = axisgroup_hdl->zoom_size.getInheritedValue();
  }
  
  bool cxios_is_defined_axisgroup_zoom_size(axisgroup_Ptr axisgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return axisgroup_hdl->zoom_size.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  
}
