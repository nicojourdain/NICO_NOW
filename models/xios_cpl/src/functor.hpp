#ifndef __XMLIO_CFunctor__
#define __XMLIO_CFunctor__

/// xios headers ///
#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "array_new.hpp"

namespace xios
{
   namespace func
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CFunctor : public CObject
      {
         /// Définition de type ///
         typedef CObject SuperClass;

         public :

            /// Accesseurs ///
            CArray<double,1> getDataOutput(void) const;
            /// Opérateur ///
            CArray<double,1> operator ()(const CArray<double,1>& dinput);

            /// Destructeur ///
            virtual ~CFunctor(void);

            //Traitement ///
            virtual void final(void);

         protected :

            /// Traitement ///
            virtual void apply(const CArray<double,1>& dinput, CArray<double,1>& doutput) = 0;

            /// Autres ///
            virtual StdString toString(void) const;
            virtual void fromString(const StdString & str);

            /// Constructeurs ///
            CFunctor(void);                             // Not implemented.
            CFunctor(const StdString & id, CArray<double,1>& doutput);
            CFunctor(const CFunctor & functor);         // Not implemented.
            CFunctor(const CFunctor * const functor);   // Not implemented.

         protected :
            /// Propriétés privées ///
            CArray<double,1>& doutput;
            /// Propriétés protégées ///   
            int nbcall;            
      }; // class CFunctor
   } // namespace func
} // namespace xios

//#include "functor_type.hpp"

#endif // __XMLIO_CFunctor__
