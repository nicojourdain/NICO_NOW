#ifndef __XMLIO_CGroupTemplate_impl__
#define __XMLIO_CGroupTemplate_impl__

#include "xmlioserver_spl.hpp"
#include "event_server.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "context.hpp"
#include "event_client.hpp"
#include "context_client.hpp"
#include "message.hpp"
#include "type.hpp"
#include "type_util.hpp"


namespace xios
{

   /// ////////////////////// Définitions ////////////////////// ///

   template <class U, class V, class W>
      CGroupTemplate<U, V, W>::CGroupTemplate(void)
         : CObjectTemplate<V>() //, V()
         , childMap(), childList()
         , groupMap(), groupList()
   { /* Ne rien faire de plus */ }

   template <class U, class V, class W>
      CGroupTemplate<U, V, W>::CGroupTemplate(const StdString & id)
         : CObjectTemplate<V>(id) //, V()
         , childMap(), childList()
         , groupMap(), groupList()
   { /* Ne rien faire de plus */ }

   template <class U, class V, class W>
      CGroupTemplate<U, V, W>::~CGroupTemplate(void)
   { /* Ne rien faire de plus */ }
   
   ///--------------------------------------------------------------
/*
   template <class U, class V, class W>
      void CGroupTemplate<U, V, W>::toBinary(StdOStream & os) const
   {
      SuperClass::toBinary(os);
      
      const StdSize grpnb = this->groupList.size();
      const StdSize chdnb = this->childList.size();
      ENodeType cenum = U::GetType();
      ENodeType genum = V::GetType();
      
      os.write (reinterpret_cast<const char*>(&grpnb) , sizeof(StdSize));
      os.write (reinterpret_cast<const char*>(&chdnb) , sizeof(StdSize));      
      
      typename std::vector<V*>::const_iterator  
         itg = this->groupList.begin(), endg = this->groupList.end();
      typename std::vector<U*>::const_iterator 
         itc = this->childList.begin(), endc = this->childList.end();
            
      for (; itg != endg; itg++)
      { 
         V* group = *itg;
         bool hid = group->hasId();
         
         os.write (reinterpret_cast<const char*>(&genum), sizeof(ENodeType));      
         os.write (reinterpret_cast<const char*>(&hid), sizeof(bool));
         
         if (hid)
         {
            const StdString & id = group->getId();
            const StdSize size   = id.size();
               
            os.write (reinterpret_cast<const char*>(&size), sizeof(StdSize));
            os.write (id.data(), size * sizeof(char));         
         }              
         group->toBinary(os);
      }
            
      for (; itc != endc; itc++)
      { 
         U* child = *itc;
         bool hid = child->hasId();
         
         os.write (reinterpret_cast<const char*>(&cenum), sizeof(ENodeType));
         os.write (reinterpret_cast<const char*>(&hid), sizeof(bool));
         
         if (hid)
         {
            const StdString & id = child->getId();
            const StdSize size   = id.size();
               
            os.write (reinterpret_cast<const char*>(&size), sizeof(StdSize));
            os.write (id.data(), size * sizeof(char));         
         }         
         child->toBinary(os);
      }
      
   }
   
   template <class U, class V, class W>
      void CGroupTemplate<U, V, W>::fromBinary(StdIStream & is)
   {
      SuperClass::fromBinary(is);
      
      V* group_ptr = (this->hasId())
         ? V::get(this->getId())
         : V::get((V*)this);
      
      StdSize grpnb = 0;
      StdSize chdnb = 0;
      ENodeType renum = Unknown;
      
      is.read (reinterpret_cast<char*>(&grpnb), sizeof(StdSize));
      is.read (reinterpret_cast<char*>(&chdnb), sizeof(StdSize));
      
      for (StdSize i = 0; i < grpnb; i++)
      {
         bool hid = false;
         is.read (reinterpret_cast<char*>(&renum), sizeof(ENodeType));
         is.read (reinterpret_cast<char*>(&hid), sizeof(bool));
         
         if (renum != V::GetType())
            ERROR("CGroupTemplate<U, V, W>::fromBinary(StdIStream & is)",
                  << "[ renum = " << renum << "] Bad type !");
                        
         if (hid)
         {
            StdSize size  = 0;
            is.read (reinterpret_cast<char*>(&size), sizeof(StdSize));
            StdString id(size, ' ');
            is.read (const_cast<char *>(id.data()), size * sizeof(char));
            CGroupFactory::CreateGroup(group_ptr->getShared(), id)->fromBinary(is);
         }
         else
         {
            CGroupFactory::CreateGroup(group_ptr->getShared())->fromBinary(is);
         }
      }
      
      for (StdSize j = 0; j < chdnb; j++)
      {
         bool hid = false;
         is.read (reinterpret_cast<char*>(&renum), sizeof(ENodeType));
         is.read (reinterpret_cast<char*>(&hid), sizeof(bool));
         
         if (renum != U::GetType())
            ERROR("CGroupTemplate<U, V, W>::fromBinary(StdIStream & is)",
                  << "[ renum = " << renum << "] Bad type !");
                  
         if (hid)
         {
            StdSize size  = 0;
            is.read (reinterpret_cast<char*>(&size), sizeof(StdSize));
            StdString id(size, ' ');
            is.read (const_cast<char *>(id.data()), size * sizeof(char));
            CGroupFactory::CreateChild(group_ptr->getShared(), id)->fromBinary(is);            
         }
         else
         {
            CGroupFactory::CreateChild(group_ptr->getShared())->fromBinary(is);
         }   
      }
   }
*/
   //--------------------------------------------------------------

   template <class U, class V, class W>
      StdString CGroupTemplate<U, V, W>::toString(void) const
   {
      StdOStringStream oss;
      StdString name = (this->getId().compare(V::GetDefName()) != 0)
                     ? V::GetName() : V::GetDefName();

      oss << "<" << name << " ";
      if (this->hasId() && (this->getId().compare(V::GetDefName()) != 0))
         oss << " id=\"" << this->getId() << "\" ";
         
      if (this->hasChild())
      {
         oss << SuperClassAttribute::toString() << ">" << std::endl;
         
         typename std::vector<V*>::const_iterator 
            itg = this->groupList.begin(), endg = this->groupList.end();
         typename std::vector<U*>::const_iterator 
            itc = this->childList.begin(), endc = this->childList.end();
            
         for (; itg != endg; itg++)
         { 
            V* group = *itg;
            oss << *group << std::endl;
         }
            
         for (; itc != endc; itc++)
         { 
            U* child = *itc;
            oss << *child << std::endl;
         }
            
         oss << "</" << name << " >";
      }
      else
      {
         oss << SuperClassAttribute::toString() << "/>";
      }
      return (oss.str());
   }

   template <class U, class V, class W>
      void CGroupTemplate<U, V, W>::fromString(const StdString & str)
   { 
      ERROR("CGroupTemplate<U, V, W>::toString(void)",
            << "[ str = " << str << "] Not implemented yet !");
   }

   //---------------------------------------------------------------

   template <class U, class V, class W>
      StdString CGroupTemplate<U, V, W>::GetName(void)
   { 
      return (U::GetName().append("_group")); 
   }

   template <class U, class V, class W>
      StdString CGroupTemplate<U, V, W>::GetDefName(void)
   { 
      return (U::GetName().append("_definition")); 
   }
   
   //---------------------------------------------------------------   

   template <class U, class V, class W>
      const std::vector<U*>&
         CGroupTemplate<U, V, W>::getChildList(void) const
   { 
      return (this->childList); 
   }

   //---------------------------------------------------------------

   template <class U, class V, class W>
      const xios_map<StdString, V*>&
         CGroupTemplate<U, V, W>::getGroupMap(void) const
   { 
      return (this->groupMap);
   }

   //---------------------------------------------------------------

   template <class U, class V, class W>
      bool CGroupTemplate<U, V, W>::hasChild(void) const
   { 
      return ((groupList.size() + childList.size()) > 0); 
   }

   //---------------------------------------------------------------

   template <class U, class V, class W>
      void CGroupTemplate<U, V, W>::parse(xml::CXMLNode & node)
   { 
      this->parse(node, true); 
   }
   
   //---------------------------------------------------------------
   
   template <class U, class V, class W>
      void CGroupTemplate<U, V, W>::solveDescInheritance(bool apply, const CAttributeMap * const parent)
   {
      if (parent != NULL)
         SuperClassAttribute::setAttributes(parent, apply);
         
      typename std::vector<U*>::const_iterator 
         itc = this->childList.begin(), endc = this->childList.end();
      typename std::vector<V*>::const_iterator 
         itg = this->groupList.begin(), endg = this->groupList.end();
             
      for (; itc != endc; itc++)
      { 
         U* child = *itc;
         child->solveDescInheritance(apply,this);
      }
            
      for (; itg != endg; itg++)
      { 
         V* group = *itg;
         if (apply) group->solveRefInheritance();
         group->solveDescInheritance(apply,this);
      }
   }

   //---------------------------------------------------------------

   template <class U, class V, class W>
      void CGroupTemplate<U, V, W>::getAllChildren(std::vector<U*>& allc) const
   {
      allc.insert (allc.end(), childList.begin(), childList.end());
      typename std::vector<V*>::const_iterator 
         itg = this->groupList.begin(), endg = this->groupList.end();
         
      for (; itg != endg; itg++)
      { 
         V* group = *itg;
         group->getAllChildren(allc);
      }
   }

   //---------------------------------------------------------------

   template <class U, class V, class W>
      std::vector<U*> CGroupTemplate<U, V, W>::getAllChildren(void) const
   { 
      std::vector<U*> allc;
      this->getAllChildren(allc);
      return (allc);
   }

   //---------------------------------------------------------------

   template <class U, class V, class W>
      void CGroupTemplate<U, V, W>::solveRefInheritance(void)
   { /* Ne rien faire de plus */ }
   
//   template <class U, class V, class W>
//   bool CGroupTemplate<U, V, W>::has(const string& id) 
//   {
//       return CObjectFactory::HasObject<V>(id) ;
//   }

//   template <class U, class V, class W>
//   boost::shared_ptr<V> CGroupTemplate<U, V, W>::get(const string& id) 
//   {
//       return CObjectFactory::GetObject<V>(id) ;
//   }

//   template <class U, class V, class W>
//   boost::shared_ptr<V> CGroupTemplate<U, V, W>::get() 
//   {
//       return CObjectFactory::GetObject<V>(this) ;
//   }
   
//   template <class U, class V, class W>
//   boost::shared_ptr<V> CGroupTemplate<U, V, W>::create(const string& id) 
//   {
//       return CObjectFactory::CreateObject<V>(id) ;
//   }
   ///--------------------------------------------------------------

  
   template <class U, class V, class W>
   U* CGroupTemplate<U, V, W>::createChild(const string& id) 
  {
    return CGroupFactory::CreateChild<V>(this->getShared(), id).get() ;
  }

   template <class U, class V, class W>
   void CGroupTemplate<U, V, W>::addChild(U* child) 
  {
    return CGroupFactory::AddChild<V>(this->getShared(),child->getShared()) ;
  }
  
   template <class U, class V, class W>
   V* CGroupTemplate<U, V, W>::createChildGroup(const string& id) 
  {
    return CGroupFactory::CreateGroup<V>(this->getShared(), id).get() ;
  }

   template <class U, class V, class W>
   void CGroupTemplate<U, V, W>::addChildGroup(V* childGroup) 
  {
    return CGroupFactory::AddGroup<V>(this->getShared(), childGroup->getShared()) ;
  }


   template <class U, class V, class W>
   void CGroupTemplate<U, V, W>::sendCreateChild(const string& id)
   {
    CContext* context=CContext::getCurrent() ;
    
    if (! context->hasServer )
    {
       CContextClient* client=context->client ;

       CEventClient event(this->getType(),EVENT_ID_CREATE_CHILD) ;   
       if (client->isServerLeader())
       {
         CMessage msg ;
         msg<<this->getId() ;
         msg<<id ;
         event.push(client->getServerLeader(),1,msg) ;
         client->sendEvent(event) ;
       }
       else client->sendEvent(event) ;
    }
      
   }
   
   template <class U, class V, class W>
   void CGroupTemplate<U, V, W>::sendCreateChildGroup(const string& id)
   {
    CContext* context=CContext::getCurrent() ;
    if (! context->hasServer )
    {
       CContextClient* client=context->client ;

       CEventClient event(this->getType(),EVENT_ID_CREATE_CHILD_GROUP) ;   
       if (client->isServerLeader())
       {
         CMessage msg ;
         msg<<this->getId() ;
         msg<<id ;
         event.push(client->getServerLeader(),1,msg) ;
         client->sendEvent(event) ;
       }
       else client->sendEvent(event) ;
    }
      
   }
   
   template <class U, class V, class W>
   void CGroupTemplate<U, V, W>::recvCreateChild(CEventServer& event)
   {
      
      CBufferIn* buffer=event.subEvents.begin()->buffer;
      string id;
      *buffer>>id ;
      V::get(id)->recvCreateChild(*buffer) ;
   }
   
   
   template <class U, class V, class W>
   void CGroupTemplate<U, V, W>::recvCreateChild(CBufferIn& buffer)
   {
      string id ;
      buffer>>id ;
      createChild(id) ;
   }

   template <class U, class V, class W>
   void CGroupTemplate<U, V, W>::recvCreateChildGroup(CEventServer& event)
   {
      
      CBufferIn* buffer=event.subEvents.begin()->buffer;
      string id;
      *buffer>>id ;
      V::get(id)->recvCreateChildGroup(*buffer) ;
   }
   
   
   template <class U, class V, class W>
   void CGroupTemplate<U, V, W>::recvCreateChildGroup(CBufferIn& buffer)
   {
      string id ;
      buffer>>id ;
      createChildGroup(id) ;
   }
   

   template <class U, class V, class W>
   bool CGroupTemplate<U, V, W>::dispatchEvent(CEventServer& event)
   {
      if (CObjectTemplate<V>::dispatchEvent(event)) return true ;
      else
      {
        switch(event.type)
        {
           case EVENT_ID_CREATE_CHILD :
             recvCreateChild(event) ;
             return true ;
             break ;
         
           case EVENT_ID_CREATE_CHILD_GROUP :
             recvCreateChildGroup(event) ;
             return true ;
             break ;       
         
           default :
           return false ;
        }
      }
   }

   template <class U, class V, class W>
      void CGroupTemplate<U, V, W>::parse(xml::CXMLNode & node, bool withAttr)
   {

      StdString name = node.getElementName();
      xml::THashAttributes attributes = node.getAttributes();
      if (withAttr)
      {
         CGroupTemplate<U, V, W>::SuperClass::parse(node);
         if (attributes.end() != attributes.find("src"))
         {
            StdIFStream ifs ( attributes["src"].c_str() , StdIFStream::in );
            if (!ifs.good())
               ERROR("CGroupTemplate<U, V, W>::parse(xml::CXMLNode & node, bool withAttr)",
                     << "[ filename = " << attributes["src"] << " ] Bad xml stream !");
            xml::CXMLParser::ParseInclude(ifs, *this);
         }
      }

      // PARSING POUR GESTION DES ENFANTS
           V* group_ptr = (this->hasId()) 
         ? V::get(this->getId())
         : boost::polymorphic_downcast<V*>(this);

      if (!(node.goToChildElement()))
      {
         if (this->hasId())
         {
            DEBUG(<< "L'objet de type \'" << V::GetName()
                  << "\' nommé \'" << this->getId()
                  << "\' ne contient pas d\'enfant !");
         }
      }
      else
      {
         do { // Parcours pour traitement.

            StdString name = node.getElementName();
            attributes.clear();
            attributes = node.getAttributes();

            if (name.compare(V::GetName()) == 0)
            {
               if (attributes.end() == attributes.find("id"))
                  CGroupFactory::CreateGroup(group_ptr->getShared())->parse(node);
               else
                  CGroupFactory::CreateGroup(group_ptr->getShared(), attributes["id"])->parse(node);
               continue;
            }

            if (name.compare(U::GetName()) == 0)
            {
               if (attributes.end() == attributes.find("id"))
                  CGroupFactory::CreateChild(group_ptr->getShared())->parse(node);
               else
                  CGroupFactory::CreateChild(group_ptr->getShared(), attributes["id"])->parse(node);
               continue;
            }

            DEBUG(<< "Dans le contexte \'" << CContext::getCurrent()->getId()
                  << "\', un objet de type \'" << V::GetName()
                  << "\' ne peut contenir qu'un objet de type \'" << V::GetName()
                  << "\' ou de type \'" << U::GetName()
                  << "\' (reçu : " << name << ") !");

         } while (node.goToNextElement());
         node.goToParentElement(); // Retour au parent
      }
   }
} // namespace xios


#endif // __XMLIO_CGroupTemplate_impl__
