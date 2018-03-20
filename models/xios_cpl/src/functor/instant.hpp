#ifndef __XMLIO_CInstant__
#define __XMLIO_CInstant__

/// xios headers ///
#include "functor.hpp"
#include "array_new.hpp"

namespace xios
{
   namespace func
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CInstant : public CFunctor
      {
         /// Définition de type ///
         typedef CFunctor SuperClass;

         public :

            /// Constructeurs ///
            //CInstant(void);                             // Not implemented.
            //CInstant(const CFunData & data);
            CInstant(CArray<double,1>& doutput);
            //CInstant(const CInstant & instant);         // Not implemented.
            //CInstant(const CInstant * const instant);   // Not implemented.

            /// Traitement ///
            virtual void apply(const CArray<double,1>& dinput, CArray<double,1>& doutput);

            /// Destructeur ///
            virtual ~CInstant(void);

      }; // class CInstant

   } // namespace func
} // namespace xmlioserver

#endif //__XMLIO_CInstant__
