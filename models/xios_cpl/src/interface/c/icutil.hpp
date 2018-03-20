/* ************************************************************************** *
 *      Copyright © IPSL/LSCE, xios, Avril 2010 - Octobre 2011         *
 * ************************************************************************** */


#ifndef __ICUTIL_HPP__
#define __ICUTIL_HPP__

#include <string>
#include <string.h>
// ///////////////////////// Définitions/Déclarations /////////////////////// //

inline bool cstr2string(const char * cstr, int cstr_size, std::string & str)
{
  std::string valtemp;
  std::size_t d, f = 0;
  if (cstr_size != -1) 
  { 
     valtemp.append (cstr, cstr_size);
     d = valtemp.find_first_not_of(' ');
     f = valtemp.find_last_not_of (' ');
     str = valtemp.substr(d, f-d+1); 
     return (true);
  }
  else
  {
     return (false);
  }  
}

inline bool string_copy(const string& str, char* cstr,int cstr_size)
{
  
  if (str.size()>cstr_size) return false ;
  else
  {
    std::memset (cstr,' ',cstr_size);
    str.copy(cstr,cstr_size) ;
    return true ;
  }
}
/*
  template<class T>
  inline  bool array_copy(ARRAY(T,1) array_in, T* array_out, size_t extent1)
  {
    if (array_in->num_elements() != extent1) return false ;
    std::copy(array_in->data(), array_in->data() + array_in->num_elements(), array_out);
    return true ;
  }

  template<class T>
  inline  bool array_copy(ARRAY(T,2) array_in, T* array_out, size_t extent1, size_t extent2)
  {
    if (array_in->num_elements() != extent1*extent2) return false ;
    std::copy(array_in->data(), array_in->data() + array_in->num_elements(), array_out);
    return true ;
  }

  template<class T>
  inline  bool array_copy(ARRAY(T,3) array_in, T* array_out, size_t extent1, size_t extent2, size_t extent3)
  {
    if (array_in->num_elements() != extent1*extent2*extent3) return false ;
    std::copy(array_in->data(), array_in->data() + array_in->num_elements(), array_out);
    return true ;
  }        
*/

#endif // __ICUTIL_HPP__
