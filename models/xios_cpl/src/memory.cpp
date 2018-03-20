#include "memory.hpp"
#include "exception.hpp"

namespace xios
{
 
  void noMemory(void)
  {
    ERROR("void noMemory(void)",<<"Out of memory") ;
  }
}
