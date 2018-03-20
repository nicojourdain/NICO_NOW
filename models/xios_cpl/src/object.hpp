#ifndef __XMLIO_CObject__
#define __XMLIO_CObject__

/// xios headers ///
#include "xmlioserver_spl.hpp"

namespace xios
{
   /// ////////////////////// Déclarations ////////////////////// ///

   class CObject
   {
      public :

         /// Destructeur ///
         virtual ~CObject(void);

         /// Accesseurs ///
         const StdString & getId(void) const;

         /// Mutateurs ///
         void resetId(void);
         void setId(const StdString & id);

         /// Tests ///
         bool hasId(void) const;

         /// Opérateurs ///
//         bool operator==(const CObject & other) const;
//         bool operator!=(const CObject & other) const;

         /// Flux ///
         friend StdOStream & operator << (StdOStream & os, const CObject & object);

         /// Autres ///
         virtual StdString toString(void) const = 0;
         virtual void fromString(const StdString & str) = 0;

      protected :

         /// Constructeurs ///
         CObject(void);
         explicit CObject(const StdString & id);
         CObject(const CObject & object);
         CObject(const CObject * const object); // Not implemented.

      private :

         /// Propriétés ///
         StdString id ;    // identifiant de l'Object
         bool IdDefined ;  // true si l'object est identifié, false sinon.

   }; // class CObject

} // namespace xios

#endif // __XMLIO_CObject__

