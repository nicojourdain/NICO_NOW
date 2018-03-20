#ifndef __XMLIO_CAccumulate__
#define __XMLIO_CAccumulate__

/// xmlioserver headers ///
#include "functor.hpp"
#include "array_new.hpp"

namespace xios
{
   namespace func
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CAccumulate : public CFunctor
      {
         /// Définition de type ///
         typedef CFunctor SuperClass;

         public :

            /// Constructeurs ///
            //CAccumulate(void);                             // Not implemented.
            //CAccumulate(const CFunData & data);
            CAccumulate(CArray<double,1>& doutput);
            //CAccumulate(const CAccumulate & accumulate);         // Not implemented.
            //CAccumulate(const CAccumulate * const accumulate);   // Not implemented.

            /// Traitement ///
            virtual void apply(const CArray<double,1>& dinput, CArray<double,1>& doutput);
            virtual void final(void) ;
            
            /// Destructeur ///
            virtual ~CAccumulate(void);

      }; // class CAccumulate

   } // namespace func
} // namespace xmlioserver

#endif //__XMLIO_CAccumulate__
