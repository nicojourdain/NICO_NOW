#ifndef __XMLIO_CXML_PARSER_IMPL__
#define __XMLIO_CXML_PARSER_IMPL__

/// xios headers ///
#include "xml_parser.hpp"

namespace xios
{
   namespace xml
   {
     template <class T> void CXMLParser::ParseInclude(StdIStream & stream, T& object)
      {
         StdOStringStream oss;
         while(!stream.eof() && !stream.fail ())
            oss.put(stream.get());
         try
         {
            const StdString xmlcontent( oss.str(), 0, oss.str().size()-1 );
            rapidxml::xml_document<char> doc;
            doc.parse<0>(const_cast<char*>(xmlcontent.c_str()));
            CXMLNode node(doc.first_node());
            object.parse(node);
         }
         catch (rapidxml::parse_error & exc)
         {
            ERROR("CXMLParser::ParseStream(StdIStream & stream)",
                  << "RapidXML error : " << exc.what() << " !");
         }
      }

   } // namespace xml
} // namespace xios

#endif // __XMLIO_CXML_PARSER_IMPL__

