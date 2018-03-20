#include "functor.hpp"
#include "array_new.hpp"

namespace xios
{
   namespace func
   {
      /// ////////////////////// Définitions ////////////////////// ///

      CFunctor::CFunctor(const StdString & id, CArray<double, 1>& doutput)
         : SuperClass(id), doutput(doutput), nbcall(0)
      { /* Ne rien faire de plus */  }

      CFunctor::~CFunctor(void)
      { /* Ne rien faire de plus */ }

      //---------------------------------------------------------------

      CArray<double,1> CFunctor::getDataOutput(void) const
      { 
         return (this->doutput);
      }

      //---------------------------------------------------------------

      StdString CFunctor::toString(void) const
      {
         ERROR("CFunctor::toString()", << "Not implemented yet !");
         return (SuperClass::getId());
      }

      void CFunctor::fromString(const StdString & str)
      {
         ERROR("CFunctor::fromString(str)",
                << "[ str = " << str << "] Not implemented yet !");
      }

      //---------------------------------------------------------------

      CArray<double,1> CFunctor::operator ()(const CArray<double,1>& dinput)
      {
         this->nbcall++;
         if (dinput.numElements() != this->doutput.numElements())
            ERROR("CFunctor::operator ()(dinput)",
                   << "[ input size = "  << dinput.numElements()
                   << ", output size = " << this->doutput.numElements() << " ]"
                   << " size of input array !=  size of output array !");
         this->apply(dinput, this->doutput);
         return (this->doutput);
      }

      void CFunctor::final(void) 
      {
         this->nbcall = 0;
      } 

      //---------------------------------------------------------------

   } // namespace func
} // namespace xios
