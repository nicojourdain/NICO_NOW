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
  typedef xios::CFieldGroup*  fieldgroup_Ptr;
  
  void cxios_set_fieldgroup_axis_ref(fieldgroup_Ptr fieldgroup_hdl, const char * axis_ref, int axis_ref_size)
  {
    std::string axis_ref_str;
    if(!cstr2string(axis_ref, axis_ref_size, axis_ref_str)) return;
     CTimer::get("XIOS").resume();
    fieldgroup_hdl->axis_ref.setValue(axis_ref_str);
    fieldgroup_hdl->sendAttributToServer(fieldgroup_hdl->axis_ref);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_fieldgroup_axis_ref(fieldgroup_Ptr fieldgroup_hdl, char * axis_ref, int axis_ref_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(fieldgroup_hdl->axis_ref.getInheritedValue(),axis_ref , axis_ref_size))
      ERROR("void cxios_get_fieldgroup_axis_ref(fieldgroup_Ptr fieldgroup_hdl, char * axis_ref, int axis_ref_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_fieldgroup_axis_ref(fieldgroup_Ptr fieldgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return fieldgroup_hdl->axis_ref.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_fieldgroup_default_value(fieldgroup_Ptr fieldgroup_hdl, double default_value)
  {
     CTimer::get("XIOS").resume();
    fieldgroup_hdl->default_value.setValue(default_value);
    fieldgroup_hdl->sendAttributToServer(fieldgroup_hdl->default_value);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_fieldgroup_default_value(fieldgroup_Ptr fieldgroup_hdl, double* default_value)
  {
    *default_value = fieldgroup_hdl->default_value.getInheritedValue();
  }
  
  bool cxios_is_defined_fieldgroup_default_value(fieldgroup_Ptr fieldgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return fieldgroup_hdl->default_value.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_fieldgroup_domain_ref(fieldgroup_Ptr fieldgroup_hdl, const char * domain_ref, int domain_ref_size)
  {
    std::string domain_ref_str;
    if(!cstr2string(domain_ref, domain_ref_size, domain_ref_str)) return;
     CTimer::get("XIOS").resume();
    fieldgroup_hdl->domain_ref.setValue(domain_ref_str);
    fieldgroup_hdl->sendAttributToServer(fieldgroup_hdl->domain_ref);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_fieldgroup_domain_ref(fieldgroup_Ptr fieldgroup_hdl, char * domain_ref, int domain_ref_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(fieldgroup_hdl->domain_ref.getInheritedValue(),domain_ref , domain_ref_size))
      ERROR("void cxios_get_fieldgroup_domain_ref(fieldgroup_Ptr fieldgroup_hdl, char * domain_ref, int domain_ref_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_fieldgroup_domain_ref(fieldgroup_Ptr fieldgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return fieldgroup_hdl->domain_ref.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_fieldgroup_enabled(fieldgroup_Ptr fieldgroup_hdl, bool enabled)
  {
     CTimer::get("XIOS").resume();
    fieldgroup_hdl->enabled.setValue(enabled);
    fieldgroup_hdl->sendAttributToServer(fieldgroup_hdl->enabled);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_fieldgroup_enabled(fieldgroup_Ptr fieldgroup_hdl, bool* enabled)
  {
    *enabled = fieldgroup_hdl->enabled.getInheritedValue();
  }
  
  bool cxios_is_defined_fieldgroup_enabled(fieldgroup_Ptr fieldgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return fieldgroup_hdl->enabled.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_fieldgroup_field_ref(fieldgroup_Ptr fieldgroup_hdl, const char * field_ref, int field_ref_size)
  {
    std::string field_ref_str;
    if(!cstr2string(field_ref, field_ref_size, field_ref_str)) return;
     CTimer::get("XIOS").resume();
    fieldgroup_hdl->field_ref.setValue(field_ref_str);
    fieldgroup_hdl->sendAttributToServer(fieldgroup_hdl->field_ref);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_fieldgroup_field_ref(fieldgroup_Ptr fieldgroup_hdl, char * field_ref, int field_ref_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(fieldgroup_hdl->field_ref.getInheritedValue(),field_ref , field_ref_size))
      ERROR("void cxios_get_fieldgroup_field_ref(fieldgroup_Ptr fieldgroup_hdl, char * field_ref, int field_ref_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_fieldgroup_field_ref(fieldgroup_Ptr fieldgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return fieldgroup_hdl->field_ref.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_fieldgroup_freq_offset(fieldgroup_Ptr fieldgroup_hdl, const char * freq_offset, int freq_offset_size)
  {
    std::string freq_offset_str;
    if(!cstr2string(freq_offset, freq_offset_size, freq_offset_str)) return;
     CTimer::get("XIOS").resume();
    fieldgroup_hdl->freq_offset.setValue(freq_offset_str);
    fieldgroup_hdl->sendAttributToServer(fieldgroup_hdl->freq_offset);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_fieldgroup_freq_offset(fieldgroup_Ptr fieldgroup_hdl, char * freq_offset, int freq_offset_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(fieldgroup_hdl->freq_offset.getInheritedValue(),freq_offset , freq_offset_size))
      ERROR("void cxios_get_fieldgroup_freq_offset(fieldgroup_Ptr fieldgroup_hdl, char * freq_offset, int freq_offset_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_fieldgroup_freq_offset(fieldgroup_Ptr fieldgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return fieldgroup_hdl->freq_offset.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_fieldgroup_freq_op(fieldgroup_Ptr fieldgroup_hdl, const char * freq_op, int freq_op_size)
  {
    std::string freq_op_str;
    if(!cstr2string(freq_op, freq_op_size, freq_op_str)) return;
     CTimer::get("XIOS").resume();
    fieldgroup_hdl->freq_op.setValue(freq_op_str);
    fieldgroup_hdl->sendAttributToServer(fieldgroup_hdl->freq_op);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_fieldgroup_freq_op(fieldgroup_Ptr fieldgroup_hdl, char * freq_op, int freq_op_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(fieldgroup_hdl->freq_op.getInheritedValue(),freq_op , freq_op_size))
      ERROR("void cxios_get_fieldgroup_freq_op(fieldgroup_Ptr fieldgroup_hdl, char * freq_op, int freq_op_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_fieldgroup_freq_op(fieldgroup_Ptr fieldgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return fieldgroup_hdl->freq_op.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_fieldgroup_grid_ref(fieldgroup_Ptr fieldgroup_hdl, const char * grid_ref, int grid_ref_size)
  {
    std::string grid_ref_str;
    if(!cstr2string(grid_ref, grid_ref_size, grid_ref_str)) return;
     CTimer::get("XIOS").resume();
    fieldgroup_hdl->grid_ref.setValue(grid_ref_str);
    fieldgroup_hdl->sendAttributToServer(fieldgroup_hdl->grid_ref);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_fieldgroup_grid_ref(fieldgroup_Ptr fieldgroup_hdl, char * grid_ref, int grid_ref_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(fieldgroup_hdl->grid_ref.getInheritedValue(),grid_ref , grid_ref_size))
      ERROR("void cxios_get_fieldgroup_grid_ref(fieldgroup_Ptr fieldgroup_hdl, char * grid_ref, int grid_ref_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_fieldgroup_grid_ref(fieldgroup_Ptr fieldgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return fieldgroup_hdl->grid_ref.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_fieldgroup_group_ref(fieldgroup_Ptr fieldgroup_hdl, const char * group_ref, int group_ref_size)
  {
    std::string group_ref_str;
    if(!cstr2string(group_ref, group_ref_size, group_ref_str)) return;
     CTimer::get("XIOS").resume();
    fieldgroup_hdl->group_ref.setValue(group_ref_str);
    fieldgroup_hdl->sendAttributToServer(fieldgroup_hdl->group_ref);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_fieldgroup_group_ref(fieldgroup_Ptr fieldgroup_hdl, char * group_ref, int group_ref_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(fieldgroup_hdl->group_ref.getInheritedValue(),group_ref , group_ref_size))
      ERROR("void cxios_get_fieldgroup_group_ref(fieldgroup_Ptr fieldgroup_hdl, char * group_ref, int group_ref_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_fieldgroup_group_ref(fieldgroup_Ptr fieldgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return fieldgroup_hdl->group_ref.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_fieldgroup_level(fieldgroup_Ptr fieldgroup_hdl, int level)
  {
     CTimer::get("XIOS").resume();
    fieldgroup_hdl->level.setValue(level);
    fieldgroup_hdl->sendAttributToServer(fieldgroup_hdl->level);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_fieldgroup_level(fieldgroup_Ptr fieldgroup_hdl, int* level)
  {
    *level = fieldgroup_hdl->level.getInheritedValue();
  }
  
  bool cxios_is_defined_fieldgroup_level(fieldgroup_Ptr fieldgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return fieldgroup_hdl->level.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_fieldgroup_long_name(fieldgroup_Ptr fieldgroup_hdl, const char * long_name, int long_name_size)
  {
    std::string long_name_str;
    if(!cstr2string(long_name, long_name_size, long_name_str)) return;
     CTimer::get("XIOS").resume();
    fieldgroup_hdl->long_name.setValue(long_name_str);
    fieldgroup_hdl->sendAttributToServer(fieldgroup_hdl->long_name);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_fieldgroup_long_name(fieldgroup_Ptr fieldgroup_hdl, char * long_name, int long_name_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(fieldgroup_hdl->long_name.getInheritedValue(),long_name , long_name_size))
      ERROR("void cxios_get_fieldgroup_long_name(fieldgroup_Ptr fieldgroup_hdl, char * long_name, int long_name_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_fieldgroup_long_name(fieldgroup_Ptr fieldgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return fieldgroup_hdl->long_name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_fieldgroup_name(fieldgroup_Ptr fieldgroup_hdl, const char * name, int name_size)
  {
    std::string name_str;
    if(!cstr2string(name, name_size, name_str)) return;
     CTimer::get("XIOS").resume();
    fieldgroup_hdl->name.setValue(name_str);
    fieldgroup_hdl->sendAttributToServer(fieldgroup_hdl->name);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_fieldgroup_name(fieldgroup_Ptr fieldgroup_hdl, char * name, int name_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(fieldgroup_hdl->name.getInheritedValue(),name , name_size))
      ERROR("void cxios_get_fieldgroup_name(fieldgroup_Ptr fieldgroup_hdl, char * name, int name_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_fieldgroup_name(fieldgroup_Ptr fieldgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return fieldgroup_hdl->name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_fieldgroup_operation(fieldgroup_Ptr fieldgroup_hdl, const char * operation, int operation_size)
  {
    std::string operation_str;
    if(!cstr2string(operation, operation_size, operation_str)) return;
     CTimer::get("XIOS").resume();
    fieldgroup_hdl->operation.setValue(operation_str);
    fieldgroup_hdl->sendAttributToServer(fieldgroup_hdl->operation);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_fieldgroup_operation(fieldgroup_Ptr fieldgroup_hdl, char * operation, int operation_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(fieldgroup_hdl->operation.getInheritedValue(),operation , operation_size))
      ERROR("void cxios_get_fieldgroup_operation(fieldgroup_Ptr fieldgroup_hdl, char * operation, int operation_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_fieldgroup_operation(fieldgroup_Ptr fieldgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return fieldgroup_hdl->operation.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_fieldgroup_prec(fieldgroup_Ptr fieldgroup_hdl, int prec)
  {
     CTimer::get("XIOS").resume();
    fieldgroup_hdl->prec.setValue(prec);
    fieldgroup_hdl->sendAttributToServer(fieldgroup_hdl->prec);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_fieldgroup_prec(fieldgroup_Ptr fieldgroup_hdl, int* prec)
  {
    *prec = fieldgroup_hdl->prec.getInheritedValue();
  }
  
  bool cxios_is_defined_fieldgroup_prec(fieldgroup_Ptr fieldgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return fieldgroup_hdl->prec.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_fieldgroup_standard_name(fieldgroup_Ptr fieldgroup_hdl, const char * standard_name, int standard_name_size)
  {
    std::string standard_name_str;
    if(!cstr2string(standard_name, standard_name_size, standard_name_str)) return;
     CTimer::get("XIOS").resume();
    fieldgroup_hdl->standard_name.setValue(standard_name_str);
    fieldgroup_hdl->sendAttributToServer(fieldgroup_hdl->standard_name);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_fieldgroup_standard_name(fieldgroup_Ptr fieldgroup_hdl, char * standard_name, int standard_name_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(fieldgroup_hdl->standard_name.getInheritedValue(),standard_name , standard_name_size))
      ERROR("void cxios_get_fieldgroup_standard_name(fieldgroup_Ptr fieldgroup_hdl, char * standard_name, int standard_name_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_fieldgroup_standard_name(fieldgroup_Ptr fieldgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return fieldgroup_hdl->standard_name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_fieldgroup_unit(fieldgroup_Ptr fieldgroup_hdl, const char * unit, int unit_size)
  {
    std::string unit_str;
    if(!cstr2string(unit, unit_size, unit_str)) return;
     CTimer::get("XIOS").resume();
    fieldgroup_hdl->unit.setValue(unit_str);
    fieldgroup_hdl->sendAttributToServer(fieldgroup_hdl->unit);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_fieldgroup_unit(fieldgroup_Ptr fieldgroup_hdl, char * unit, int unit_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(fieldgroup_hdl->unit.getInheritedValue(),unit , unit_size))
      ERROR("void cxios_get_fieldgroup_unit(fieldgroup_Ptr fieldgroup_hdl, char * unit, int unit_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_fieldgroup_unit(fieldgroup_Ptr fieldgroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return fieldgroup_hdl->unit.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  
}
