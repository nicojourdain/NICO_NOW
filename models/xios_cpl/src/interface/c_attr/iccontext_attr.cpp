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
  typedef xios::CContext*  context_Ptr;
  
  void cxios_set_context_calendar_type(context_Ptr context_hdl, const char * calendar_type, int calendar_type_size)
  {
    std::string calendar_type_str;
    if(!cstr2string(calendar_type, calendar_type_size, calendar_type_str)) return;
     CTimer::get("XIOS").resume();
    context_hdl->calendar_type.setValue(calendar_type_str);
    context_hdl->sendAttributToServer(context_hdl->calendar_type);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_context_calendar_type(context_Ptr context_hdl, char * calendar_type, int calendar_type_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(context_hdl->calendar_type.getInheritedValue(),calendar_type , calendar_type_size))
      ERROR("void cxios_get_context_calendar_type(context_Ptr context_hdl, char * calendar_type, int calendar_type_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_context_calendar_type(context_Ptr context_hdl )
  {
     CTimer::get("XIOS").resume();
    return context_hdl->calendar_type.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_context_output_dir(context_Ptr context_hdl, const char * output_dir, int output_dir_size)
  {
    std::string output_dir_str;
    if(!cstr2string(output_dir, output_dir_size, output_dir_str)) return;
     CTimer::get("XIOS").resume();
    context_hdl->output_dir.setValue(output_dir_str);
    context_hdl->sendAttributToServer(context_hdl->output_dir);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_context_output_dir(context_Ptr context_hdl, char * output_dir, int output_dir_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(context_hdl->output_dir.getInheritedValue(),output_dir , output_dir_size))
      ERROR("void cxios_get_context_output_dir(context_Ptr context_hdl, char * output_dir, int output_dir_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_context_output_dir(context_Ptr context_hdl )
  {
     CTimer::get("XIOS").resume();
    return context_hdl->output_dir.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_context_start_date(context_Ptr context_hdl, const char * start_date, int start_date_size)
  {
    std::string start_date_str;
    if(!cstr2string(start_date, start_date_size, start_date_str)) return;
     CTimer::get("XIOS").resume();
    context_hdl->start_date.setValue(start_date_str);
    context_hdl->sendAttributToServer(context_hdl->start_date);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_context_start_date(context_Ptr context_hdl, char * start_date, int start_date_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(context_hdl->start_date.getInheritedValue(),start_date , start_date_size))
      ERROR("void cxios_get_context_start_date(context_Ptr context_hdl, char * start_date, int start_date_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_context_start_date(context_Ptr context_hdl )
  {
     CTimer::get("XIOS").resume();
    return context_hdl->start_date.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_context_time_origin(context_Ptr context_hdl, const char * time_origin, int time_origin_size)
  {
    std::string time_origin_str;
    if(!cstr2string(time_origin, time_origin_size, time_origin_str)) return;
     CTimer::get("XIOS").resume();
    context_hdl->time_origin.setValue(time_origin_str);
    context_hdl->sendAttributToServer(context_hdl->time_origin);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_context_time_origin(context_Ptr context_hdl, char * time_origin, int time_origin_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(context_hdl->time_origin.getInheritedValue(),time_origin , time_origin_size))
      ERROR("void cxios_get_context_time_origin(context_Ptr context_hdl, char * time_origin, int time_origin_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_context_time_origin(context_Ptr context_hdl )
  {
     CTimer::get("XIOS").resume();
    return context_hdl->time_origin.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  void cxios_set_context_timestep(context_Ptr context_hdl, const char * timestep, int timestep_size)
  {
    std::string timestep_str;
    if(!cstr2string(timestep, timestep_size, timestep_str)) return;
     CTimer::get("XIOS").resume();
    context_hdl->timestep.setValue(timestep_str);
    context_hdl->sendAttributToServer(context_hdl->timestep);
     CTimer::get("XIOS").suspend();
  }
  
  void cxios_get_context_timestep(context_Ptr context_hdl, char * timestep, int timestep_size)
  {
     CTimer::get("XIOS").resume();
    if(!string_copy(context_hdl->timestep.getInheritedValue(),timestep , timestep_size))
      ERROR("void cxios_get_context_timestep(context_Ptr context_hdl, char * timestep, int timestep_size)", <<"Input string is to short");
     CTimer::get("XIOS").suspend();
  }
  
  bool cxios_is_defined_context_timestep(context_Ptr context_hdl )
  {
     CTimer::get("XIOS").resume();
    return context_hdl->timestep.hasInheritedValue();
     CTimer::get("XIOS").suspend();
  }
  
  
  
  
}
