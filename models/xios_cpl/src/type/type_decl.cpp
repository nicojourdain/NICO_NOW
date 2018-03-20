#include "type.hpp"
#include "type_impl.hpp"
#include "type_ref_impl.hpp"
#include "type_specialisation.hpp"
#include <string> ;
#include "xmlioserver_spl.hpp"

namespace xios
{
#define macro(decl_type)         \
  template class CType<decl_type> ; \
  template class CType_ref<decl_type> ; \
  template CBufferOut& operator<< <decl_type> (CBufferOut& buffer, const CType<decl_type>& type) ; \
  template CBufferOut& operator<< <decl_type> (CBufferOut& buffer, const CType_ref<decl_type>& type) ; \
  template CBufferOut& operator<< <decl_type> (CBufferOut& buffer, decl_type& type) ; \
  template CBufferOut& operator<< <decl_type> (CBufferOut& buffer, const decl_type& type) ; \
  template CBufferIn& operator>> <decl_type> (CBufferIn& buffer, CType<decl_type>& type) ; \
  template CBufferIn& operator>> <decl_type> (CBufferIn& buffer, const CType_ref<decl_type>& type) ; \
  template CBufferIn& operator>> <decl_type> (CBufferIn& buffer, decl_type& type) ; \
/*  template CMessage& operator<< <decl_type> (CMessage& msg, const CType<decl_type>& type) ;*/ \
/*  template CMessage& operator<< <decl_type> (CMessage& msg, const CType_ref<decl_type>& type) ;*/ \
  template CMessage& operator<< <decl_type> (CMessage& msg, const decl_type& type) ; \
  template CMessage& operator<< <decl_type> (CMessage& msg, decl_type& type) ;
  
  macro(string) 
  macro(int) 
  macro(double)
  macro(bool)  
  macro(StdSize) 

}
