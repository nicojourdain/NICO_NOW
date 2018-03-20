#ifndef __XMLIO_CAxis__
#define __XMLIO_CAxis__

/// xios headers ///
#include "xmlioserver_spl.hpp"
#include "group_factory.hpp"

#include "declare_group.hpp"
#include "attribute_array.hpp"
#include "attribute_enum.hpp"
#include "attribute_enum_impl.hpp"

namespace xios {
   
   /// ////////////////////// Déclarations ////////////////////// ///

   class CAxisGroup;
   class CAxisAttributes;
   class CAxis;

   ///--------------------------------------------------------------

   // Declare/Define CAxisAttribute
   BEGIN_DECLARE_ATTRIBUTE_MAP(CAxis)
#  include "axis_attribute.conf"
   END_DECLARE_ATTRIBUTE_MAP(CAxis)

   ///--------------------------------------------------------------

   class CAxis
      : public CObjectTemplate<CAxis>
      , public CAxisAttributes
   {
         /// typedef ///
         typedef CObjectTemplate<CAxis>   SuperClass;
         typedef CAxisAttributes SuperClassAttribute;

      public :

         typedef CAxisAttributes RelAttributes;
         typedef CAxisGroup      RelGroup;

         /// Constructeurs ///
         CAxis(void);
         explicit CAxis(const StdString & id);
         CAxis(const CAxis & axis);       // Not implemented yet.
         CAxis(const CAxis * const axis); // Not implemented yet.

         /// Accesseurs ///
         const std::set<StdString> & getRelFiles(void) const;

         /// Test ///
         bool IsWritten(const StdString & filename) const;

         /// Mutateur ///
         void addRelFile(const StdString & filename);

         /// Vérifications ///
         void checkAttributes(void);

         /// Destructeur ///
         virtual ~CAxis(void);

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);
         
         static ENodeType GetType(void);

      private :

         bool isChecked;
         std::set<StdString> relFiles;


   }; // class CAxis

   ///--------------------------------------------------------------

   // Declare/Define CAxisGroup and CAxisDefinition
   DECLARE_GROUP(CAxis);

   ///--------------------------------------------------------------

} // namespace xios

#endif // __XMLIO_CAxis__
