#include "xml_parser.hpp"

#include "context.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"

namespace xios
{
   namespace xml
   {
      /// ////////////////////// Définitions ////////////////////// ///

      void CXMLParser::ParseFile(const StdString & filename)
      {
         StdIFStream ifs ( filename.c_str() , StdIFStream::in );
         CXMLParser::ParseStream(ifs);
      }

      void CXMLParser::ParseString(const StdString & xmlContent)
      {
         StdIStringStream iss ( xmlContent /*, StdIStringStream::in*/ );
         CXMLParser::ParseStream(iss);
      }

      void CXMLParser::ParseStream(StdIStream & stream)
      {
         if (!stream.good())
            ERROR("CXMLParser::ParseStream(const StdIStream & stream)",
                  << "Bad xml stream !");
         StdOStringStream oss;
         while(!stream.eof() && !stream.fail ())
            oss.put(stream.get());
         try
         {
            //const StdString xmlcontent( oss.str(), 0, oss.str().size()-2); //<POURQUOI ?
            const StdString xmlcontent( oss.str(), 0, oss.str().size()-1 );
            rapidxml::xml_document<char> doc;
            doc.parse<0>(const_cast<char*>(xmlcontent.c_str()));

            CXMLNode node(doc.first_node());
            THashAttributes attributes;

            if (node.getElementName().compare(CXMLNode::GetRootName()) != 0)
               ERROR("CXMLParser::ParseStream(StdIStream & stream)",
                     << "Root element should be named simulation (actual = \'"
                     << node.getElementName() << "\')!");

            if (node.goToChildElement())
            {
               do
               {
                  CContextGroup* group_context = CContext::getRoot() ;
 
                  attributes = node.getAttributes();

                  if (attributes.end() == attributes.find("id"))
                  {  
                     DEBUG("Le context ne sera pas traité car il n'est pas identifié !");
                     continue; 
                  }

                  CContext::setCurrent(attributes["id"]) ;

                  bool hasctxt = CContext::has(attributes["id"]);

                  if(hasctxt)
                  {  
                     DEBUG("Le context ne sera pas traité car "
                           << "il existe déjà un autre context possédant le même nom !");
                     continue; 
                  }

                  CContext* context = CContext::create(attributes["id"]);
//                  if (!hasctxt)  group_context->addChild(context);
                  context->parse(node);

                  attributes.clear();

               } while (node.goToNextElement());
            }
         }
         catch (rapidxml::parse_error & exc)
         {
            ERROR("CXMLParser::ParseStream(StdIStream & stream)",
                  << "RapidXML error : " << exc.what() << " !");
         }
      }

   }// namespace xml
} // namespace xios
