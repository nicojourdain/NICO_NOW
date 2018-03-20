#include "once.hpp"
#include "array_new.hpp"

namespace xios
{
   namespace func
   {
      /// ////////////////////// Définitions ////////////////////// ///

      COnce::COnce(CArray<double,1>& doutput)
         : SuperClass(StdString("once"), doutput)
      { /* Ne rien faire de plus */ }

      COnce::~COnce(void)
      { /* Ne rien faire de plus */ }

      //---------------------------------------------------------------

      void COnce::apply(const CArray<double,1>& _dinput,
                              CArray<double,1>& _doutput)
      {
/*       	 const double * it1  = _dinput->data(),
       	              * end1 = _dinput->data() + _dinput->num_elements();
       	       double * it   = _doutput->data();
        for (; it1 != end1; it1++, it++) *it  = *it1;*/
        _doutput=_dinput ;
      }

      //---------------------------------------------------------------

   } // namespace func
} // namespace xmlioserver

