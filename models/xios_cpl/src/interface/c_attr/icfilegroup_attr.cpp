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
  typedef xios::CFileGroup*  filegroup_Ptr;
  
  void cxios_set_filegroup_description(filegroup_Ptr filegroup_hdl, const char * description, int description_size)
  {
    std::string description_str;
    if(!cstr2string(description, description_size, description_str)) return;
     CTimer::get("XIOS").resume();
    filegroup_hdl->description.setValue(description_str);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->description);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_filegroup_description(filegroup_Ptr filegroup_hdl, char * description, int description_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(filegroup_hdl->description.getInheritedValue(),description , description_size))
      ERROR("void cxios_get_filegroup_description(filegroup_Ptr filegroup_hdl, char * description, int description_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_filegroup_description(filegroup_Ptr filegroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return filegroup_hdl->description.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_filegroup_enabled(filegroup_Ptr filegroup_hdl, bool enabled)
  {
     CTimer::get("XIOS").resume();
    filegroup_hdl->enabled.setValue(enabled);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->enabled);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_filegroup_enabled(filegroup_Ptr filegroup_hdl, bool* enabled)
  {
    *enabled = filegroup_hdl->enabled.getInheritedValue();
  }
  
  bool cxios_is_defined_filegroup_enabled(filegroup_Ptr filegroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return filegroup_hdl->enabled.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_filegroup_group_ref(filegroup_Ptr filegroup_hdl, const char * group_ref, int group_ref_size)
  {
    std::string group_ref_str;
    if(!cstr2string(group_ref, group_ref_size, group_ref_str)) return;
     CTimer::get("XIOS").resume();
    filegroup_hdl->group_ref.setValue(group_ref_str);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->group_ref);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_filegroup_group_ref(filegroup_Ptr filegroup_hdl, char * group_ref, int group_ref_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(filegroup_hdl->group_ref.getInheritedValue(),group_ref , group_ref_size))
      ERROR("void cxios_get_filegroup_group_ref(filegroup_Ptr filegroup_hdl, char * group_ref, int group_ref_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_filegroup_group_ref(filegroup_Ptr filegroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return filegroup_hdl->group_ref.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_filegroup_min_digits(filegroup_Ptr filegroup_hdl, int min_digits)
  {
     CTimer::get("XIOS").resume();
    filegroup_hdl->min_digits.setValue(min_digits);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->min_digits);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_filegroup_min_digits(filegroup_Ptr filegroup_hdl, int* min_digits)
  {
    *min_digits = filegroup_hdl->min_digits.getInheritedValue();
  }
  
  bool cxios_is_defined_filegroup_min_digits(filegroup_Ptr filegroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return filegroup_hdl->min_digits.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_filegroup_name(filegroup_Ptr filegroup_hdl, const char * name, int name_size)
  {
    std::string name_str;
    if(!cstr2string(name, name_size, name_str)) return;
     CTimer::get("XIOS").resume();
    filegroup_hdl->name.setValue(name_str);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->name);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_filegroup_name(filegroup_Ptr filegroup_hdl, char * name, int name_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(filegroup_hdl->name.getInheritedValue(),name , name_size))
      ERROR("void cxios_get_filegroup_name(filegroup_Ptr filegroup_hdl, char * name, int name_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_filegroup_name(filegroup_Ptr filegroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return filegroup_hdl->name.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_filegroup_name_suffix(filegroup_Ptr filegroup_hdl, const char * name_suffix, int name_suffix_size)
  {
    std::string name_suffix_str;
    if(!cstr2string(name_suffix, name_suffix_size, name_suffix_str)) return;
     CTimer::get("XIOS").resume();
    filegroup_hdl->name_suffix.setValue(name_suffix_str);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->name_suffix);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_filegroup_name_suffix(filegroup_Ptr filegroup_hdl, char * name_suffix, int name_suffix_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(filegroup_hdl->name_suffix.getInheritedValue(),name_suffix , name_suffix_size))
      ERROR("void cxios_get_filegroup_name_suffix(filegroup_Ptr filegroup_hdl, char * name_suffix, int name_suffix_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_filegroup_name_suffix(filegroup_Ptr filegroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return filegroup_hdl->name_suffix.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_filegroup_output_freq(filegroup_Ptr filegroup_hdl, const char * output_freq, int output_freq_size)
  {
    std::string output_freq_str;
    if(!cstr2string(output_freq, output_freq_size, output_freq_str)) return;
     CTimer::get("XIOS").resume();
    filegroup_hdl->output_freq.setValue(output_freq_str);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->output_freq);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_filegroup_output_freq(filegroup_Ptr filegroup_hdl, char * output_freq, int output_freq_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(filegroup_hdl->output_freq.getInheritedValue(),output_freq , output_freq_size))
      ERROR("void cxios_get_filegroup_output_freq(filegroup_Ptr filegroup_hdl, char * output_freq, int output_freq_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_filegroup_output_freq(filegroup_Ptr filegroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return filegroup_hdl->output_freq.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_filegroup_output_level(filegroup_Ptr filegroup_hdl, int output_level)
  {
     CTimer::get("XIOS").resume();
    filegroup_hdl->output_level.setValue(output_level);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->output_level);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_filegroup_output_level(filegroup_Ptr filegroup_hdl, int* output_level)
  {
    *output_level = filegroup_hdl->output_level.getInheritedValue();
  }
  
  bool cxios_is_defined_filegroup_output_level(filegroup_Ptr filegroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return filegroup_hdl->output_level.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_filegroup_par_access(filegroup_Ptr filegroup_hdl, const char * par_access, int par_access_size)
  {
    std::string par_access_str;
    if(!cstr2string(par_access, par_access_size, par_access_str)) return;
     CTimer::get("XIOS").resume();
    filegroup_hdl->par_access.setValue(par_access_str);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->par_access);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_filegroup_par_access(filegroup_Ptr filegroup_hdl, char * par_access, int par_access_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(filegroup_hdl->par_access.getInheritedValue(),par_access , par_access_size))
      ERROR("void cxios_get_filegroup_par_access(filegroup_Ptr filegroup_hdl, char * par_access, int par_access_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_filegroup_par_access(filegroup_Ptr filegroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return filegroup_hdl->par_access.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_filegroup_split_freq(filegroup_Ptr filegroup_hdl, const char * split_freq, int split_freq_size)
  {
    std::string split_freq_str;
    if(!cstr2string(split_freq, split_freq_size, split_freq_str)) return;
     CTimer::get("XIOS").resume();
    filegroup_hdl->split_freq.setValue(split_freq_str);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->split_freq);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_filegroup_split_freq(filegroup_Ptr filegroup_hdl, char * split_freq, int split_freq_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(filegroup_hdl->split_freq.getInheritedValue(),split_freq , split_freq_size))
      ERROR("void cxios_get_filegroup_split_freq(filegroup_Ptr filegroup_hdl, char * split_freq, int split_freq_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_filegroup_split_freq(filegroup_Ptr filegroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return filegroup_hdl->split_freq.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_filegroup_split_freq_format(filegroup_Ptr filegroup_hdl, const char * split_freq_format, int split_freq_format_size)
  {
    std::string split_freq_format_str;
    if(!cstr2string(split_freq_format, split_freq_format_size, split_freq_format_str)) return;
     CTimer::get("XIOS").resume();
    filegroup_hdl->split_freq_format.setValue(split_freq_format_str);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->split_freq_format);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_filegroup_split_freq_format(filegroup_Ptr filegroup_hdl, char * split_freq_format, int split_freq_format_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(filegroup_hdl->split_freq_format.getInheritedValue(),split_freq_format , split_freq_format_size))
      ERROR("void cxios_get_filegroup_split_freq_format(filegroup_Ptr filegroup_hdl, char * split_freq_format, int split_freq_format_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_filegroup_split_freq_format(filegroup_Ptr filegroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return filegroup_hdl->split_freq_format.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_filegroup_sync_freq(filegroup_Ptr filegroup_hdl, const char * sync_freq, int sync_freq_size)
  {
    std::string sync_freq_str;
    if(!cstr2string(sync_freq, sync_freq_size, sync_freq_str)) return;
     CTimer::get("XIOS").resume();
    filegroup_hdl->sync_freq.setValue(sync_freq_str);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->sync_freq);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_filegroup_sync_freq(filegroup_Ptr filegroup_hdl, char * sync_freq, int sync_freq_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(filegroup_hdl->sync_freq.getInheritedValue(),sync_freq , sync_freq_size))
      ERROR("void cxios_get_filegroup_sync_freq(filegroup_Ptr filegroup_hdl, char * sync_freq, int sync_freq_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_filegroup_sync_freq(filegroup_Ptr filegroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return filegroup_hdl->sync_freq.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_filegroup_type(filegroup_Ptr filegroup_hdl, const char * type, int type_size)
  {
    std::string type_str;
    if(!cstr2string(type, type_size, type_str)) return;
     CTimer::get("XIOS").resume();
    filegroup_hdl->type.fromString(type_str);
    filegroup_hdl->sendAttributToServer(filegroup_hdl->type);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_filegroup_type(filegroup_Ptr filegroup_hdl, char * type, int type_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(filegroup_hdl->type.getInheritedStringValue(),type , type_size))
      ERROR("void cxios_get_filegroup_type(filegroup_Ptr filegroup_hdl, char * type, int type_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_filegroup_type(filegroup_Ptr filegroup_hdl )
  {
     CTimer::get("XIOS").resume();
    return filegroup_hdl->type.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  
}
