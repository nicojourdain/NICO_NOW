#ifndef __XMLIO_COnce__
#define __XMLIO_COnce__

/// xios headers ///
#include "functor.hpp"
#include "array_new.hpp"

namespace xios
{
   namespace func
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class COnce : public CFunctor
      {
         /// Définition de type ///
         typedef CFunctor SuperClass;

         public :

            /// Constructeurs ///
            //COnce(void);                       // Not implemented.
            COnce(CArray<double,1>& doutput);
            //COnce(const COnce & once);         // Not implemented.
            //COnce(const COnce * const once);   // Not implemented.

            /// Traitement ///
            virtual void apply(const CArray<double,1>& dinput, CArray<double,1>& doutput);

            /// Destructeur ///
            virtual ~COnce(void);

      }; // class COnce

   } // namespace func
} // namespace xios

#endif //__XMLIO_COnce__

