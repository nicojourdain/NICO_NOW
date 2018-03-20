#ifndef __XMLIO_CDomain__
#define __XMLIO_CDomain__

/// xios headers ///
#include "xmlioserver_spl.hpp"
#include "group_factory.hpp"

#include "declare_group.hpp"
#include "event_client.hpp"
#include "event_server.hpp"
#include "buffer_in.hpp"
#include "array_new.hpp"
#include "attribute_array.hpp"

namespace xios {
   
   /// ////////////////////// Déclarations ////////////////////// ///

   class CDomainGroup;
   class CDomainAttributes;
   class CDomain;

   ///--------------------------------------------------------------

   // Declare/Define CDomainAttribute
   BEGIN_DECLARE_ATTRIBUTE_MAP(CDomain)
#  include "domain_attribute.conf"
   END_DECLARE_ATTRIBUTE_MAP(CDomain)

   ///--------------------------------------------------------------

   class CDomain
      : public CObjectTemplate<CDomain>
      , public CDomainAttributes
   {
         enum EEventId
         {
           EVENT_ID_SERVER_ATTRIBUT, EVENT_ID_LON_LAT
         } ;
         
         /// typedef ///
         typedef CObjectTemplate<CDomain>   SuperClass;
         typedef CDomainAttributes SuperClassAttribute;

      public :

         typedef CDomainAttributes RelAttributes;
         typedef CDomainGroup      RelGroup;

         /// Constructeurs ///
         CDomain(void);
         explicit CDomain(const StdString & id);
         CDomain(const CDomain & domain);       // Not implemented yet.
         CDomain(const CDomain * const domain); // Not implemented yet.

         /// Vérifications ///
         void checkAttributes(void);

      private :

         void checkGlobalDomain(void);

         void checkLocalIDomain(void);
         void checkLocalJDomain(void);

         void checkMask(void);
         void checkDomainData(void);
         void checkCompression(void);
         
         void checkZoom(void);

         void completeMask(void);

      public :
      
         /// Autres ///
//         virtual void fromBinary(StdIStream & is);

         /// Accesseurs ///
         CArray<int, 2> getLocalMask(void) const;
         
         const std::set<StdString> & getRelFiles(void) const;

         const std::vector<int> & getIBeginSub(void) const;
         const std::vector<int> & getIEndSub(void) const;
         const std::vector<int> & getJBeginSub(void) const;
         const std::vector<int> & getJEndSub(void) const;

         const std::vector<int> & getIBeginZoomSub(void) const;
         const std::vector<int> & getJBeginZoomSub(void) const;
         const std::vector<int> & getNiZoomSub(void) const;
         const std::vector<int> & getNjZoomSub(void) const;
         
         const std::vector<CArray<double,1>* > & getLonValueSub(void) const;
         const std::vector<CArray<double,1>* > & getLatValueSub(void) const;

         /// Test ///
         bool IsWritten(const StdString & filename) const;
         bool hasZoom(void) const;
         bool isEmpty(void) const;
         
         
         int ni_client,ibegin_client,iend_client ;
         int zoom_ni_client,zoom_ibegin_client,zoom_iend_client ;

         int nj_client,jbegin_client,jend_client ;
         int zoom_nj_client,zoom_jbegin_client,zoom_jend_client ;

         int ni_srv,ibegin_srv,iend_srv ;
         int zoom_ni_srv,zoom_ibegin_srv,zoom_iend_srv ;

         int nj_srv,jbegin_srv,jend_srv ;
         int zoom_nj_srv,zoom_jbegin_srv,zoom_jend_srv ;

         CArray<double, 1> lonvalue_srv, latvalue_srv ;
         
         
        vector<int> connectedServer,nbSenders ;
        vector<int> ib_srv, ie_srv, in_srv ;
        vector<int> jb_srv, je_srv, jn_srv ;
         
      public :
      
         /// Mutateur ///
         void addRelFile(const StdString & filename);
         void completeLonLatServer(void);
         void completeLonLatClient(void);
         void sendServerAttribut(void) ;
         void sendLonLat(void) ;
         void computeConnectedServer(void) ;
         static bool dispatchEvent(CEventServer& event) ;
         static void recvLonLat(CEventServer& event) ;
         static void recvServerAttribut(CEventServer& event) ;
         void recvLonLat(CBufferIn& buffer) ;
         void recvServerAttribut(CBufferIn& buffer) ;
         
         /// Destructeur ///
         virtual ~CDomain(void);

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);
         
         static ENodeType GetType(void);

         CArray<int, 2> local_mask;
         bool isCurvilinear ;
       private :

         /// Proriétés protégées ///
         bool isChecked;
         std::set<StdString> relFiles;

         std::vector<int> ibegin_sub, iend_sub, jbegin_sub, jend_sub;
         std::vector<int> ibegin_zoom_sub, jbegin_zoom_sub, ni_zoom_sub, nj_zoom_sub;
         std::vector<CArray<double,1>* > lonvalue_sub, latvalue_sub;
         

   }; // class CDomain

   ///--------------------------------------------------------------

   // Declare/Define CDomainGroup and CDomainDefinition
   DECLARE_GROUP(CDomain);

   ///--------------------------------------------------------------

} // namespace xios

#endif // __XMLIO_CDomain__
