#ifndef __XMLIO_CAverage__
#define __XMLIO_CAverage__

/// xmlioserver headers ///
#include "functor.hpp"
#include "array_new.hpp"

namespace xios
{
   namespace func
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CAverage : public CFunctor
      {
         /// Définition de type ///
         typedef CFunctor SuperClass;

         public :

            /// Constructeurs ///
            //CAverage(void);                             // Not implemented.
            //CAverage(const CFunData & data);
            CAverage(CArray<double,1>& doutput);
            //CAverage(const CAverage & average);         // Not implemented.
            //CAverage(const CAverage * const average);   // Not implemented.

            /// Traitement ///
            virtual void apply(const CArray<double,1>& dinput, CArray<double,1>& doutput);
            virtual void final(void) ;
            
            /// Destructeur ///
            virtual ~CAverage(void);

      }; // class CAverage

   } // namespace func
} // namespace xmlioserver

#endif //__XMLIO_CAverage__
