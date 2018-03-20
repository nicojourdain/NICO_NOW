#ifndef __XMLIO_CContext__
#define __XMLIO_CContext__

/// xios headers ///
#include "xmlioserver_spl.hpp"
//#include "node_type.hpp"
#include "calendar.hpp"

#include "declare_group.hpp"
//#include "context_client.hpp"
//#include "context_server.hpp"
#include "data_output.hpp"

#include "mpi.hpp"


namespace xios {
   class CContextClient ;
   class CContextServer ;
   
   
   /// ////////////////////// Déclarations ////////////////////// ///
   class CContextGroup;
   class CContextAttributes;
   class CContext;
   class CFile;
   ///--------------------------------------------------------------

   // Declare/Define CFileAttribute
   BEGIN_DECLARE_ATTRIBUTE_MAP(CContext)
#  include "context_attribute.conf"
   END_DECLARE_ATTRIBUTE_MAP(CContext)

   ///--------------------------------------------------------------

   class CContext
      : public CObjectTemplate<CContext>
      , public CContextAttributes
   {
         public :
         enum EEventId
         {
           EVENT_ID_CLOSE_DEFINITION,EVENT_ID_UPDATE_CALENDAR,
           EVENT_ID_CREATE_FILE_HEADER,EVENT_ID_CONTEXT_FINALIZE
         } ;
         
         /// typedef ///
         typedef CObjectTemplate<CContext>   SuperClass;
         typedef CContextAttributes SuperClassAttribute;

      public :

         typedef CContextAttributes RelAttributes;
         typedef CContext           RelGroup;

         //---------------------------------------------------------

      public :

         /// Constructeurs ///
         CContext(void);
         explicit CContext(const StdString & id);
         CContext(const CContext & context);       // Not implemented yet.
         CContext(const CContext * const context); // Not implemented yet.

         /// Destructeur ///
         virtual ~CContext(void);

         //---------------------------------------------------------

      public :
      
         /// Mutateurs ///
         void setCalendar(boost::shared_ptr<CCalendar> newCalendar);
      
         /// Accesseurs ///
         boost::shared_ptr<CCalendar>      getCalendar(void) const;

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);         
         static ENodeType GetType(void);         

         static CContextGroup* GetContextGroup(void);

      public :

         /// Traitements ///
         virtual void solveDescInheritance(bool apply, const CAttributeMap * const parent = 0);
         void solveFieldRefInheritance(bool apply);
         void solveCalendar(void);

         /// Autres méthodes statiques ///
         static void ShowTree(StdOStream & out = std::clog);
         static void CleanTree(void);

         /// Test ///
         virtual bool hasChild(void) const;

         bool eventLoop(void) ;
         bool serverLoop(void) ;
         void clientLoop(void) ;
         void initServer(MPI_Comm intraComm, MPI_Comm interComm) ;
         void initClient(MPI_Comm intraComm, MPI_Comm interComm) ;
         CContextServer* server ;
         CContextClient* client ;
         bool hasClient ;
         bool hasServer ;
         void finalize(void) ;
         void closeDefinition(void) ;
         void findAllEnabledFields(void);
         void solveAllGridRef(void);
         void solveAllOperation(void);
         void solveAllInheritance(bool apply=true) ;
         void findEnabledFiles(void);
         void closeAllFile(void) ;
         void updateCalendar(int step) ;
         void createFileHeader(void ) ;
      // dispatch event
         static bool dispatchEvent(CEventServer& event) ;
         void sendCloseDefinition(void) ;
         void sendUpdateCalendar(int step) ;
         void sendCreateFileHeader(void) ;
         static void recvUpdateCalendar(CEventServer& event) ;
         void recvUpdateCalendar(CBufferIn& buffer) ;
         static void recvCloseDefinition(CEventServer& event) ;
         static void recvCreateFileHeader(CEventServer& event) ;
         void recvCreateFileHeader(CBufferIn& buffer) ;
         static CContext* getCurrent(void) ;
         static CContextGroup* getRoot(void) ;
         static void setCurrent(const string& id) ;
         static CContext* create(const string& id = "") ;
         
      public :
      
         /// Autres ///
         virtual void parse(xml::CXMLNode & node);

         virtual StdString toString(void) const;
//         virtual void toBinary  (StdOStream & os) const;
//         virtual void fromBinary(StdIStream & is);
         
      public :
      
         boost::shared_ptr<CCalendar>      calendar;
 
         std::vector<CFile*> enabledFiles;
         static shared_ptr<CContextGroup> root ;


   }; // class CContext

   ///--------------------------------------------------------------

   // Declare/Define CContextGroup and CContextDefinition
   DECLARE_GROUP(CContext);

   ///--------------------------------------------------------------

} // namespace xios

#endif // __XMLIO_CContext__
