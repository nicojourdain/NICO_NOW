#include "minimum.hpp"
#include "array_new.hpp"
#include <algorithm>

namespace xios
{
   namespace func
   {
      /// ////////////////////// Définitions ////////////////////// ///

      CMinimum::CMinimum(CArray<double,1>& doutput)
         : SuperClass(StdString("minimum"), doutput)
      { /* Ne rien faire de plus */ }

      CMinimum::~CMinimum(void)
      { /* Ne rien faire de plus */ }

      //---------------------------------------------------------------

      void CMinimum::apply(const CArray<double,1>& _dinput,
                                 CArray<double,1>& _doutput)
      {
        const double * it1  = _dinput.dataFirst(),
                      * end1 = _dinput.dataFirst() + _dinput.numElements();
        double * it   = _doutput.dataFirst();
        
        if (this->nbcall == 1)  for (; it1 != end1; it1++, it++) *it = *it1;
        else for (; it1 != end1; it1++, it++) *it = std::min(*it1, *it);

      }

      //---------------------------------------------------------------

   } // namespace func
} // namespace xmlioserver
