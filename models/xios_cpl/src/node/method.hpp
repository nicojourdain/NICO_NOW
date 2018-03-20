#ifndef __XMLIO_CMethod__
#define __XMLIO_CMethod__

namespace xios
{
      /// ////////////////////// Déclarations ////////////////////// ///

      class CMethodGroup;
      class CMethodAttributes;
      class CBaseMethod;
      class CMethod;

      ///--------------------------------------------------------------

      // Declare/Define CMethodAttribute
      BEGIN_DECLARE_ATTRIBUTE_MAP(CMethod)
#include "method_attribute.conf"
      END_DECLARE_ATTRIBUTE_MAP(CMethod)

      ///--------------------------------------------------------------

      class CBaseMethod
      { // Declare CMethod functionalities
         public :

            /// Constructeur et Destructeur ///
            CBaseMethod(void);
            ~CBaseMethod(void);

      }; // class CBaseMethod

      ///--------------------------------------------------------------

      class CMethod
         : public CObjectTemplate<CMethod>
         , public CMethodAttributes
         , public CBaseMethod
      {
            /// typedef ///
            typedef CObjectTemplate<CMethod>   SuperClass;
            typedef CMethodAttributes SuperClassAttribute;
            typedef CBaseMethod       SuperClassBase;

         public :

            typedef CMethodAttributes RelAttributes;
            typedef CMethodGroup      RelGroup;

            /// Constructeurs ///
            CMethod(void);
            explicit CMethod(const StdString & id);
            CMethod(const CMethod & method);       // Not implemented yet.
            CMethod(const CMethod * const method); // Not implemented yet.

            /// Destructeur ///
            virtual ~CMethod(void);

            /// Accesseurs statiques ///
            static inline StdString GetName(void);
            static inline StdString GetDefName(void);

      }; // class CMethod

      ///--------------------------------------------------------------

      // Declare/Define CMethodGroup and CMethodDefinition
      DECLARE_GROUP(CMethod);

      /// ////////////////////// Définitions ////////////////////// ///

      CBaseMethod::CBaseMethod(void)
      { /* Ne rien faire de plus */ }

      CBaseMethod::~CBaseMethod(void)
      { /* Ne rien faire de plus */ }

      ///--------------------------------------------------------------

      CMethod::CMethod(void)
         : CObjectTemplate<CMethod>()
         , CMethodAttributes()
         , CBaseMethod()
      { /* Ne rien faire de plus */ }

      CMethod::CMethod(const StdString & id)
         : CObjectTemplate<CMethod>(id)
         , CMethodAttributes()
         , CBaseMethod()
      { /* Ne rien faire de plus */ }

      CMethod::~CMethod(void)
      { /* Ne rien faire de plus */ }

      StdString CMethod::GetName(void)   { return (StdString("method")); }
      StdString CMethod::GetDefName(void){ return (CMethod::GetName()); }

} // namespace xios

#endif // __XMLIO_CMethod__
