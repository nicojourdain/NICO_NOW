#ifndef __XMLIO_CXMLParser__
#define __XMLIO_CXMLParser__

/// xios headers ///
#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "xml_node.hpp"


namespace xios
{
   namespace xml
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CXMLParser
      {
         public :

            static void ParseFile(const StdString & filename);
            static void ParseString(const StdString & xmlContent);
            static void ParseStream(StdIStream & stream);
            template <class T>
               static void ParseInclude(StdIStream & stream, T & object);

      }; //class CXMLParser
/*
      template <class T>
         void CXMLParser::ParseInclude(StdIStream & stream, T& object)
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
*/
   }// namespace xml
} // namespace xios

#endif // __XMLIO_CXMLParser__
