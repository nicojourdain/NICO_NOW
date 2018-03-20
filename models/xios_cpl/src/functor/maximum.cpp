#include "maximum.hpp"
#include "array_new.hpp"



namespace xios
{
   namespace func
   {
      /// ////////////////////// Définitions ////////////////////// ///

      CMaximum::CMaximum(CArray<double,1>& doutput)
         : SuperClass(StdString("maximum"), doutput)
      { /* Ne rien faire de plus */ }

      CMaximum::~CMaximum(void)
      { /* Ne rien faire de plus */ }

      //---------------------------------------------------------------

      void CMaximum::apply(const CArray<double,1>& _dinput,
                                 CArray<double,1>& _doutput)
      {
        const double * it1  = _dinput.dataFirst(),
       	             * end1 = _dinput.dataFirst() + _dinput.numElements();
              double * it   = _doutput.dataFirst();
         if (this->nbcall == 1) for (; it1 != end1; it1++, it++) *it = *it1;
         else for (; it1 != end1; it1++, it++) *it = std::max(*it1, *it);


      }

      //---------------------------------------------------------------

   } // namespace func
} // namespace xmlioserver
