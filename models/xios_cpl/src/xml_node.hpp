#ifndef __XMLIO_CXMLNode__
#define __XMLIO_CXMLNode__

/// rapidXML headers ///
#include <rapidxml.hpp>

/// xios headers ///
#include "xmlioserver_spl.hpp"

namespace xios
{
   namespace xml
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      typedef xios_map<StdString, StdString> THashAttributes;

      class CXMLNode
      {
         public :

            /// Constructeurs ///
            CXMLNode(rapidxml::xml_node<char> * const root);

            /// Destructeur ///
            ~CXMLNode(void);

            /// Accesseurs ///
            StdString getElementName(void) const;
            THashAttributes getAttributes(void) const;

            /// Mutateurs ///
            bool goToNextElement(void);
            bool goToChildElement(void);
            bool goToParentElement(void);
            bool getContent(StdString & content);

            /// Accesseurs statiques ///
            static const StdString & GetRootName(void);

         private :

            /// Constructeurs ///
            CXMLNode(void);                        // Not implemented yet.
            CXMLNode(const CXMLNode & node);       // Not implemented yet.
            CXMLNode(const CXMLNode * const node); // Not implemented yet.

            rapidxml::xml_node<char> * node;
            int level;

            static StdString RootName;

      }; //class CXMLParser

   }// namespace xml
} // namespace xios

#endif // __XMLIO_CXMLNode__
