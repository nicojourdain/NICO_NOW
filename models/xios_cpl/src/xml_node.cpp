#include "xml_node.hpp"

namespace xios
{
   namespace xml
   {
      /// ////////////////////// Définitions ////////////////////// ///

      StdString CXMLNode::RootName("simulation");

      CXMLNode::CXMLNode(rapidxml::xml_node<char> * const root)
         : node(root)
         , level(0)
      { /* Ne rien faire de plus */ }

      CXMLNode::~CXMLNode(void)
      { /* Ne rien faire de plus */ }

      StdString CXMLNode::getElementName(void) const
      { 
         return (this->node->name()); 
      }

      bool CXMLNode::goToNextElement(void)
      {
         bool retvalue = false;
         for(rapidxml::xml_node<char> * nextElement = this->node->next_sibling();
                                      ; nextElement = this->node->next_sibling())
         {
            if (nextElement == NULL) break;
            else if (nextElement->type() == rapidxml::node_element)
            { 
               node = nextElement;
               return (!retvalue);
            }
         }
         return (retvalue);
      }

      bool CXMLNode::goToChildElement(void)
      {
         bool retvalue = false;
         rapidxml::xml_node<char> * nextElement = this->node->first_node();
         if (nextElement != NULL)
         {
            for(;;nextElement = this->node->next_sibling())
            {
               if (nextElement == NULL) break;
               else if (nextElement->type() == rapidxml::node_element)
               { 
                  node = nextElement;
                  level++;
                  return (!retvalue); 
               }
            }
         }
         return (retvalue);
      }

      bool CXMLNode::goToParentElement(void)
      {
         bool retvalue = false;
         if (!(this->getElementName().compare(CXMLNode::RootName)) || (level == 0))
            return (retvalue);
         node = node->parent();
         level--;
         return (!retvalue);
      }

      bool CXMLNode::getContent(StdString & content)
      {
         if (this->node->value_size() == 0) return (false);
         content.assign(this->node->value(), this->node->value_size());
         return (true);
      }

      const StdString & CXMLNode::GetRootName(void)
      { 
         return (CXMLNode::RootName); 
      }

      THashAttributes CXMLNode::getAttributes(void) const
      {
         THashAttributes attributes;
         rapidxml::xml_attribute<char> *currentAttr = NULL;

         if ((currentAttr = this->node->first_attribute()) != NULL)
         {
            do 
            {
               attributes.insert(std::pair<StdString, StdString>
                                (StdString(currentAttr->name()),
                                 StdString(currentAttr->value())));
            } while ((currentAttr = currentAttr->next_attribute()) != NULL);
         }

         return (attributes) ;
      }

   }// namespace xml
} // namespace xmlioserve
