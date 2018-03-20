#include "accumulate.hpp"
#include "array_new.hpp"

namespace xios
{
   namespace func
   {
      /// ////////////////////// Définitions ////////////////////// ///

      CAccumulate::CAccumulate(CArray<double,1>& doutput)
         : SuperClass(StdString("average"), doutput)
      { /* Ne rien faire de plus */ }

      CAccumulate::~CAccumulate(void)
      { /* Ne rien faire de plus */ }

      //---------------------------------------------------------------

      void CAccumulate::apply(const CArray<double,1>& _dinput,
                                 CArray<double,1>& _doutput)
      {
/*
         const double * it1  = _dinput->data(),
                      * end1 = _dinput->data() + _dinput->num_elements(); 
               double * it   = _doutput->data();
         if (this->nbcall == 1)
              for (; it1 != end1; it1++, it++) *it  = *it1;
         else for (; it1 != end1; it1++, it++) *it += *it1;               
*/
        if (this->nbcall == 1) _doutput=_dinput ;
        else _doutput+=_dinput ;
      
      }
      
      void CAccumulate::final(void)
      {
/*          double * it1  = this->getDataOutput()->data(),
                 * end1 = this->getDataOutput()->data() + this->getDataOutput()->num_elements();
          for (; it1 != end1; it1++) *it1 /= this->nbcall;
          this->nbcall = 0;                                                   
*/
//        doutput/=this->nbcall; 
        this->nbcall = 0; 
      }
   } // namespace func
} // namespace xmlioserver
