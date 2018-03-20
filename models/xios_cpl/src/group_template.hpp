#ifndef __XMLIO_CGroupTemplate__
#define __XMLIO_CGroupTemplate__

#include "xmlioserver_spl.hpp"
#include "declare_attribute.hpp"
#include "event_server.hpp"
#include "object_template.hpp"

namespace xios
{

   /// ////////////////////// Déclarations ////////////////////// ///
   template <class U, class V, class W>
      class CGroupTemplate
         : public CObjectTemplate<V>, public virtual W
   {
         /// Friend ///
         friend class CGroupFactory;

         /// Typedef ///
         typedef U Child;
         typedef V Derived, Group;
         typedef W SuperClassAttribute;
         typedef CObjectTemplate<V> SuperClass;

      public :
      
         enum EEventId
         {
           EVENT_ID_CREATE_CHILD=200, EVENT_ID_CREATE_CHILD_GROUP
         } ;

         /// Spécifique ///
         DECLARE_ATTRIBUTE(StdString, group_ref)

         /// Accesseurs ///
         const xios_map<StdString,Group*>& getGroupMap(void) const;
         const vector<Child*>& getChildList(void) const;

         void getAllChildren(vector<Child*> & allc) const;
         vector<Child*> getAllChildren(void) const;

         /// Autres ///
         virtual StdString toString(void) const;
         virtual void fromString(const StdString & str);
         
//         virtual void toBinary  (StdOStream & os) const;
//         virtual void fromBinary(StdIStream & is);

         virtual void parse(xml::CXMLNode & node);
         virtual void parse(xml::CXMLNode & node, bool withAttr);
         
         /// Test ///
         virtual bool hasChild(void) const;

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);

         /// Traitements ///
         virtual void solveDescInheritance(bool apply, const CAttributeMap * const parent = 0);
         void solveRefInheritance(void);
//         static bool has(const string & id); 
//         static boost::shared_ptr<V> get(const string& id) ;
//         static boost::shared_ptr<V> create(const string& id=string("")) ;
         U* createChild(const string& id="") ;
          
         void addChild(U* child) ; 
         V* createChildGroup(const string& id="") ; 
         void addChildGroup(V* childGroup) ; 
         static bool dispatchEvent(CEventServer& event) ;
         void sendCreateChild(const string& id="") ;
         void sendCreateChildGroup(const string& id="") ;
         static void recvCreateChild(CEventServer& event) ;
         void recvCreateChild(CBufferIn& buffer) ;
         static void recvCreateChildGroup(CEventServer& event) ;
         void recvCreateChildGroup(CBufferIn& buffer) ;
         
         /// Destructeur ///
         virtual ~CGroupTemplate(void);

      protected :

         /// Constructeurs ///
         CGroupTemplate(void);
         CGroupTemplate(const StdString & id);
         CGroupTemplate(const CGroupTemplate<U, V, W> & group,
                        bool withAttrList = true, bool withId = true); // Not implemented yet.
         CGroupTemplate(const CGroupTemplate<U, V, W> * const group);  // Not implemented yet.

      private :

         /// Propriétés ///
         xios_map<StdString, Child* > childMap;
         vector<Child*> childList;

         xios_map<StdString,Group* > groupMap;
         vector<Group* > groupList;

   }; // class CGroupTemplate
} // namespace xios

//#include "group_template_impl.hpp"

#endif // __XMLIO_CGroupTemplate__
