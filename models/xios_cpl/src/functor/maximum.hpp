#ifndef __XMLIO_CMaximum__
#define __XMLIO_CMaximum__

/// xios headers ///
#include "functor.hpp"
#include "array_new.hpp"

namespace xios
{
   namespace func
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CMaximum : public CFunctor
      {
         /// Définition de type ///
         typedef CFunctor SuperClass;

         public :

            /// Constructeurs ///
            //CMaximum(void);                             // Not implemented.
            //CMaximum(const CFunData & data);
            CMaximum(CArray<double,1>& doutput);
            //CMaximum(const CMaximum & Maximum);         // Not implemented.
            //CMaximum(const CMaximum * const Maximum);   // Not implemented.

            /// Traitement ///
            virtual void apply(const CArray<double,1>& dinput, CArray<double,1>& doutput);

            /// Destructeur ///
            virtual ~CMaximum(void);

      }; // class CMaximum

   } // namespace func
} // namespace xmlioserver

#endif //__XMLIO_CMaximum__
