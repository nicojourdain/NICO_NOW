#include "attribute_template.hpp"
#include "attribute_template_impl.hpp"

#include <cfloat>

namespace xios
{
/*
      /// ////////////////////// Définitions ////////////////////// ///

      /// Spécialisations des templates pour la fonction [toString] ///
      
      template <>
         StdString CAttributeTemplate<bool>::toString(void) const
      {
         StdOStringStream oss;
         if (!this->isEmpty() && this->hasId())
         {
            if (this->getValue())
               oss << this->getName() << "=\".TRUE.\"";
            else
               oss << this->getName() << "=\".FALSE.\"";
         }
         return (oss.str());
      }

      //---------------------------------------------------------------

      /// Spécialisations des templates pour la fonction [fromString] ///

      template <> // Chaîne de caractères.
         void CAttributeTemplate<StdString>::fromString(const StdString & str)
      { 
         this->setValue(str); 
      }

      template <> // Entier
         void CAttributeTemplate<int>::fromString(const StdString & str)
      {
         try
         {
            this->setValue(boost::lexical_cast<int>(str));
         }
         catch(boost::bad_lexical_cast &)
         {
            ERROR("void CAttributeTemplate<int>::fromString(const StdString & str)",
                  << "[ str = " << str << " ] Bad cast !");
         }
      }

      template <> // Double
         void CAttributeTemplate<double>::fromString(const StdString & str)
      {
         if (str.find("max") != StdString::npos)
         {
            this->setValue(DBL_MAX);
            return;
         }
         if (str.find("min") != StdString::npos)
         {
            this->setValue(DBL_MIN);
            return;
         }
         
         try
         {
            this->setValue(boost::lexical_cast<double>(str));
         }
         catch(boost::bad_lexical_cast &)
         {
            ERROR("void CAttributeTemplate<double>::fromString(const StdString & str)",
                  << "[ str = " << str << " ] Bad cast !");
         }
      }

      template <> // Booléen
         void CAttributeTemplate<bool>::fromString(const StdString & str)
      {
         if (str.find(".TRUE.") != StdString::npos)
            this->setValue(true);
         else
            this->setValue(false);
      }

      //---------------------------------------------------------------

      template<> // Tableau
         void CAttributeTemplate<ARRAY(double, 1)>::fromString(const StdString & str)
      {
         ARRAY_CREATE(array_sptr, double, 1, [1]);
         CArray<double, 1> & array = *array_sptr;
         this->setValue(array_sptr);

         StdIStringStream iss(str) ;
         char c = '\0'; int size = 0;
         double d = 0.,valsup = 0., valinf = 0.;
         std::vector<double> vect;

         iss >> d; vect.push_back(d);
         size = vect.size();
         if (!iss.eof ())
         {
            iss >> c;
            switch (c)
            {
               case ',' : // Le tableau est généré valeur par valeur.
                  iss.unget();
                  while(!iss.eof ())
                  { // On récupère chacune des valeurs une par une jusqu'à ce que le buffer soit vide.
                     iss >> c >> d;
                     if (c != ',')
                        ERROR("CAttributeTemplate<ARRAY(...)>::fromString(const StdString & str)",
                              << "[ str = " << str << " ] bad definition of array !");
                     vect.push_back(d);
                  }
                  size = vect.size();
                  break;
               case '(' : // Le tableau est généré automatiquement.
                  if (!iss.eof ())
                  { // on récupère la borne supérieure
                     valinf = d;
                     iss >> size >> c >> d;
                     if ((c != ')') || (size <= 0))
                        ERROR("CAttributeTemplate<ARRAY(...)>::fromString(const StdString & str)",
                              << "[ str = " << str << " ] bad definition of array !");
                     valsup = d;
                  }
                  d = (valsup - valinf) / (double)(size - 1);
                  for (int j = 1; j <= size; j++)
                     vect.push_back(valinf + j * d);
                  break;
               default :
                  ERROR("CAttributeTemplate<ARRAY(...)>::fromString(const StdString & str)",
                        << "[ str = " << str << " ] bad definition of array !");
            }
         }

         array.resize(boost::extents[size]);
         for (int i = 0; i < size; i++)
            array[i] = vect[i]; 

      }

      //---------------------------------------------------------------

      /// Spécialisations des templates pour la fonction [toBinary] ///

      template <> // Chaîne de caractères.
         void CAttributeTemplate<StdString>::toBinary (StdOStream & os) const
      {
         StdString str = this->getValue();
         StdSize size = str.size();
         os.write (reinterpret_cast<const char*>(&size) , sizeof(StdSize));
         os.write (str.data(), size * sizeof(char));
      }

      template <> // Entier
         void CAttributeTemplate<int>::toBinary(StdOStream & os) const
      {
         int value = this->getValue();
         os.write (reinterpret_cast<const char*>(&value) , sizeof(int));
      }

      template <> // Booléen
         void CAttributeTemplate<bool>::toBinary(StdOStream & os) const
      {
         bool value = this->getValue();
         os.write (reinterpret_cast<const char*>(&value) , sizeof(bool));
      }

      template <> // Double
         void CAttributeTemplate<double>::toBinary(StdOStream & os) const
      {
         double value = this->getValue();
         os.write (reinterpret_cast<const char*>(&value) , sizeof(double));
      }

      //---------------------------------------------------------------

      /// Spécialisations des templates pour la fonction [fromBinary] ///

      template <> // Chaîne de caractères.
         void CAttributeTemplate<StdString>::fromBinary(StdIStream & is)
      {
         StdSize size = 0;
         is.read (reinterpret_cast<char*>(&size), sizeof(StdSize));
         StdString value(size, ' ');
         is.read (const_cast<char *>(value.data()), size * sizeof(char));
         this->setValue(value);
      }

      template <> // Entier
         void CAttributeTemplate<int>::fromBinary(StdIStream & is)
      {
         int value = 0;
         is.read (reinterpret_cast<char*>(&value), sizeof(int));
         this->setValue(value);
      }

      template <> // Booléen
         void CAttributeTemplate<bool>::fromBinary(StdIStream & is)
      {
         bool value = false;
         is.read (reinterpret_cast<char*>(&value), sizeof(bool));
         this->setValue(value);
      }

      template <> // Double
         void CAttributeTemplate<double>::fromBinary(StdIStream & is)
      {
         double value = 0.;
         is.read (reinterpret_cast<char*>(&value), sizeof(double));
         this->setValue(value);
      }
      ///--------------------------------------------------------------
*/
} // namespace xios
