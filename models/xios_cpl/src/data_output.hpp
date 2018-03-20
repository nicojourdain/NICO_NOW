#ifndef __XMLIO_DATA_OUTPUT__
#define __XMLIO_DATA_OUTPUT__

/// xios headers ///
#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "grid.hpp"
#include "field.hpp"


namespace xios
{
      /// ////////////////////// Déclarations ////////////////////// ///
      class CDataOutput
      {
         public :

            /// Définition de type ///
            typedef enum { ONE_FILE = 0, MULTI_GROUP, MULTI_FILE } EDataOutputType;

            /// Ecriture ///
            void writeFile     (CFile*  file);
            void syncFile     (void);
            void closeFile     (void);
            void writeField    (CField* field);
            void writeFieldGrid(CField* field);
            void writeTimeDimension(void);
            void writeFieldData(CField* field);

            virtual void definition_start(void) = 0;
            virtual void definition_end(void)   = 0;

            virtual ~CDataOutput(void);

         protected:

            /// Ecriture ///
            void writeGrid(CGrid* grid);
            void writeGrid(CDomain* domain,
                           CAxis*   axis);
            void writeGrid(CDomain* domain);

            virtual void writeFile_       (CFile*     file)   = 0;
            virtual void closeFile_       (void)                                            = 0;
            virtual void syncFile_       (void)                                            = 0;
            virtual void writeField_      (CField*    field)  = 0;
            virtual void writeFieldData_  (CField*    field)  = 0;
            virtual void writeDomain_     (CDomain*   domain) = 0;
            virtual void writeTimeDimension_ (void) = 0;
            virtual void writeAxis_       (CAxis*     axis)   = 0;
            virtual void writeTimeAxis_   (CField*    field,
                                           const shared_ptr<CCalendar> cal)    = 0;

            /// Propriétés protégées ///
            EDataOutputType type;

      }; // class CDataOutput

} // namespace xios

#endif //__XMLIO_DATA_OUTPUT__
