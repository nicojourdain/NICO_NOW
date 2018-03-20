#include "buffer_out.hpp"
#include "buffer_out_impl.hpp"


namespace xios
{
# define macro(T) \
  template bool CBufferOut::put_template<T>(const T& data) ; \
  template bool CBufferOut::put_template<T>(const T* data, size_t n) ; \
  template bool CBufferOut::advance_template<T>(size_t n) ; \
 

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
