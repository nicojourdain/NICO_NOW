#ifndef __XMLIO_CMinimum__
#define __XMLIO_CMinimum__

/// xmlioserver headers ///
#include "functor.hpp"
#include "array_new.hpp"

namespace xios
{
   namespace func
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CMinimum : public CFunctor
      {
         /// Définition de type ///
         typedef CFunctor SuperClass;

         public :

            /// Constructeurs ///
            //CMinimum(void);                             // Not implemented.
            //CMinimum(const CFunData & data);
            CMinimum(CArray<double,1>& doutput);
            //CMinimum(const CMinimum & Minimum);         // Not implemented.
            //CMinimum(const CMinimum * const Minimum);   // Not implemented.

            /// Traitement ///
            virtual void apply(const CArray<double,1>& dinput, CArray<double,1>& doutput);

            /// Destructeur ///
            virtual ~CMinimum(void);

      }; // class CMinimum

   } // namespace func
} // namespace xmlioserver

#endif //__XMLIO_CMinimum__
