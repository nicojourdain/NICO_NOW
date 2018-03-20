#include "buffer.hpp"
#include "buffer_impl.hpp"

namespace xios
{
#  define macro(T) \
    template bool CBuffer::put_template<T>(const T& data) ; \
    template bool CBuffer::put_template<T>(const T* data, size_t n) ; \
    template bool CBuffer::put_ptr_template<T>(const T*& data, size_t n) ; \
    template bool CBuffer::get_template<T>(T& data) ; \
    template bool CBuffer::get_template<T>(T* data, size_t n) ; \
    template bool CBuffer::get_ptr_template<T>(T*& data, size_t n) ;
  
  
  macro(char)
  macro(int)
  macro(short)
  macro(long)
  macro(uint)
  macro(ushort)
  macro(ulong)
  macro(float)
  macro(double)
  macro(long double)
}
