#include "buffer_in.hpp"
#include "buffer_in_impl.hpp"

namespace xios
{
# define macro(T) \
  template bool CBufferIn::get_template<T>(T& data); \
  template bool CBufferIn::get_template<T>(T* data, size_t n); \
  template bool CBufferIn::advance_template<T>(size_t n);
 

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

