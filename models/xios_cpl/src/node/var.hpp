#ifndef __XMLIO_CVar__
#define __XMLIO_CVar__

namespace xios
{
      /// ////////////////////// Déclarations ////////////////////// ///

      class CVarGroup;
      class CVarAttributes;
      class CBaseVar;
      class CVar;

      ///--------------------------------------------------------------

      // Declare/Define CVarAttribute
      BEGIN_DECLARE_ATTRIBUTE_MAP(CVar)
#include "var_attribute.conf"
      END_DECLARE_ATTRIBUTE_MAP(CVar)

      ///--------------------------------------------------------------

      class CBaseVar
      { // Declare CVar functionalities
         public :

            /// Constructeur et Destructeur ///
            CBaseVar(void);
            ~CBaseVar(void);

      }; // class CBaseVar

      ///--------------------------------------------------------------

      class CVar
         : public CObjectTemplate<CVar>
         , public CVarAttributes
         , public CBaseVar
      {
            /// typedef ///
            typedef CObjectTemplate<CVar>   SuperClass;
            typedef CVarAttributes SuperClassAttribute;
            typedef CBaseVar       SuperClassBase;

         public :

            typedef CVarAttributes RelAttributes;
            typedef CVarGroup      RelGroup;

            /// Constructeurs ///
            CVar(void);
            explicit CVar(const StdString & id);
            CVar(const CVar & var);       // Not implemented yet.
            CVar(const CVar * const var); // Not implemented yet.

            /// Destructeur ///
            virtual ~CVar(void);

            /// Accesseurs statiques ///
            static inline StdString GetName(void);
            static inline StdString GetDefName(void);

      }; // class CVar

      ///--------------------------------------------------------------

      // Declare/Define CVarGroup and CVarDefinition
      DECLARE_GROUP(CVar);

      /// ////////////////////// Définitions ////////////////////// ///

      CBaseVar::CBaseVar(void)
      { /* Ne rien faire de plus */ }

      CBaseVar::~CBaseVar(void)
      { /* Ne rien faire de plus */ }

      ///--------------------------------------------------------------

      CVar::CVar(void)
         : CObjectTemplate<CVar>()
         , CVarAttributes()
         , CBaseVar()
      { /* Ne rien faire de plus */ }

      CVar::CVar(const StdString & id)
         : CObjectTemplate<CVar>(id)
         , CVarAttributes()
         , CBaseVar()
      { /* Ne rien faire de plus */ }

      CVar::~CVar(void)
      { /* Ne rien faire de plus */ }

      StdString CVar::GetName(void)   { return (StdString("var")); }
      StdString CVar::GetDefName(void){ return (CVar::GetName()); }

} // namespace xios

#endif // __XMLIO_CVar__
