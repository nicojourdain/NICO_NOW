#include "indent_xml.hpp"

/// boost headers ///
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/split.hpp>

namespace xios
{
   /// ////////////////////// Définitions ////////////////////// ///
   unsigned int CIndent::Indent   = 0;
   StdString    CIndent::Increm   = StdString("   ");
   bool         CIndent::WithLine = false;

   StdOStream & CIndent::NIndent(StdOStream& out)
   {
      static unsigned int LineNB = 1;
      if (CIndent::WithLine) out << LineNB++ << ". ";
      for(unsigned int i = 0; i < CIndent::Indent; out << CIndent::Increm , i++){}
      return (out);
   }

   StdOStream & CIndent::IncIndent(StdOStream& out)
   { CIndent::Indent++; return (CIndent::NIndent(out)); }

   StdOStream & CIndent::DecEndl  (StdOStream& out)
   { CIndent::Indent--; return (out); }

   ///----------------------------------------

   StdString CIndentedXml::Indented(const StdString & content)
   {
      StdOStringStream retvalue;
      std::vector<StdString> str;
      boost::split(str, content, boost::is_any_of("\n"));
      
      std::vector<StdString>::iterator it = str.begin(), end = str.end();
      
      for (; it != end; it++)
      {
         StdString & line = *it;
         if (line.find("<? ") != StdString::npos ||
             line.find(xml::CXMLNode::GetRootName()) != StdString::npos)
            retvalue << CIndent::NIndent << line <<  std::endl;
         else if (line.find("</") != StdString::npos)
            retvalue << CIndent::NIndent   << line << CIndent::DecEndl << std::endl;
         else if (line.find(" />") != StdString::npos)
            retvalue << CIndent::IncIndent << line << CIndent::DecEndl << std::endl;
         else
            retvalue << CIndent::IncIndent << line <<  std::endl;
      }
      return (retvalue.str());
   }
} // namespace xios
