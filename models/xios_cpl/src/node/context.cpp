#include "context.hpp"
#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"

#include "calendar_type.hpp"
#include "duration.hpp"

#include "context_client.hpp"
#include "context_server.hpp"
#include "nc4_data_output.hpp"
#include "node_type.hpp"
#include "message.hpp"
#include "type.hpp"
#include "xmlioserver_spl.hpp"

namespace xios {
  
  shared_ptr<CContextGroup> CContext::root ;
   
   /// ////////////////////// Définitions ////////////////////// ///

   CContext::CContext(void)
      : CObjectTemplate<CContext>(), CContextAttributes()
      , calendar(),hasClient(false),hasServer(false)
   { /* Ne rien faire de plus */ }

   CContext::CContext(const StdString & id)
      : CObjectTemplate<CContext>(id), CContextAttributes()
      , calendar(),hasClient(false),hasServer(false)
   { /* Ne rien faire de plus */ }

   CContext::~CContext(void)
   { 
     if (hasClient) delete client ;
     if (hasServer) delete server ;
   }

   //----------------------------------------------------------------

   StdString CContext::GetName(void)   { return (StdString("context")); }
   StdString CContext::GetDefName(void){ return (CContext::GetName()); }
   ENodeType CContext::GetType(void)   { return (eContext); }

   //----------------------------------------------------------------

   CContextGroup* CContext::getRoot(void)
   {  
      if (root.get()==NULL) root=shared_ptr<CContextGroup>(new CContextGroup(xml::CXMLNode::GetRootName())) ;
      return root.get(); 
   }
   

   //----------------------------------------------------------------

   boost::shared_ptr<CCalendar> CContext::getCalendar(void) const
   {
      return (this->calendar);
   }
   
   //----------------------------------------------------------------
   
   void CContext::setCalendar(boost::shared_ptr<CCalendar> newCalendar)
   {
      this->calendar = newCalendar;
      calendar_type.setValue(this->calendar->getId());
      start_date.setValue(this->calendar->getInitDate().toString());
   }
   
   //----------------------------------------------------------------

   void CContext::solveCalendar(void)
   {
      if (this->calendar.get() != NULL) return;
      if (calendar_type.isEmpty() || start_date.isEmpty())
         ERROR(" CContext::solveCalendar(void)",
               << "[ context id = " << this->getId() << " ] "
               << "Impossible to define a calendar (an attribute is missing).");

#define DECLARE_CALENDAR(MType  , mtype)                              \
   if (calendar_type.getValue().compare(#mtype) == 0)                 \
   {                                                                  \
      if (time_origin.isEmpty())                                       \
        this->calendar =  boost::shared_ptr<CCalendar>          \
           (new C##MType##Calendar(start_date.getValue()));     \
      else this->calendar =  boost::shared_ptr<CCalendar>       \
           (new C##MType##Calendar(start_date.getValue(),time_origin.getValue()));     \
      if (!this->timestep.isEmpty())                                  \
       this->calendar->setTimeStep                                    \
          (CDuration::FromString(this->timestep.getValue()));   \
      return;                                                         \
   }
#include "calendar_type.conf"

      ERROR("CContext::solveCalendar(void)",
            << "[ calendar_type = " << calendar_type.getValue() << " ] "
            << "The calendar is not defined !");
   }
   
   //----------------------------------------------------------------

   void CContext::parse(xml::CXMLNode & node)
   {
      CContext::SuperClass::parse(node);

      // PARSING POUR GESTION DES ENFANTS
      xml::THashAttributes attributes = node.getAttributes();

      if (attributes.end() != attributes.find("src"))
      {
         StdIFStream ifs ( attributes["src"].c_str() , StdIFStream::in );
         if (!ifs.good())
            ERROR("CContext::parse(xml::CXMLNode & node)",
                  << "[ filename = " << attributes["src"] << " ] Bad xml stream !");
         xml::CXMLParser::ParseInclude(ifs, *this);
      }

      if (node.getElementName().compare(CContext::GetName()))
         DEBUG("Le noeud is wrong defined but will be considered as a context !");

      if (!(node.goToChildElement()))
      {
         DEBUG("Le context ne contient pas d'enfant !");
      }
      else
      {
         do { // Parcours des contextes pour traitement.

            StdString name = node.getElementName();
            attributes.clear();
            attributes = node.getAttributes();

            if (attributes.end() != attributes.find("id"))
            { DEBUG(<< "Definition node has an id,"
                    << "it will not be taking account !"); }

#define DECLARE_NODE(Name_, name_)    \
   if (name.compare(C##Name_##Definition::GetDefName()) == 0) \
   { C##Name_##Definition::create(C##Name_##Definition::GetDefName()) -> parse(node) ; continue; }
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"

            DEBUG(<< "The element \'"     << name
                  << "\' in the context \'" << CContext::getCurrent()->getId()
                  << "\' is not a definition !");

         } while (node.goToNextElement());

         node.goToParentElement(); // Retour au parent
      }
   }

   //----------------------------------------------------------------

   void CContext::ShowTree(StdOStream & out)
   {
      StdString currentContextId = CContext::getCurrent() -> getId() ;
      std::vector<CContext*> def_vector =
         CContext::getRoot()->getChildList();
      std::vector<CContext*>::iterator
         it = def_vector.begin(), end = def_vector.end();

      out << "<? xml version=\"1.0\" ?>" << std::endl;
      out << "<"  << xml::CXMLNode::GetRootName() << " >" << std::endl;
      
      for (; it != end; it++)
      {
         CContext* context = *it;         
         CContext::setCurrent(context->getId());         
         out << *context << std::endl;
      }
      
      out << "</" << xml::CXMLNode::GetRootName() << " >" << std::endl;
      CContext::setCurrent(currentContextId);  
   }
   
   //----------------------------------------------------------------
/*
   void CContext::toBinary(StdOStream & os) const
   {
      SuperClass::toBinary(os);

#define DECLARE_NODE(Name_, name_)                                         \
   {                                                                       \
      ENodeType renum = C##Name_##Definition::GetType();                   \
      bool val = C##Name_##Definition::has(C##Name_##Definition::GetDefName()); \
      os.write (reinterpret_cast<const char*>(&renum), sizeof(ENodeType)); \
      os.write (reinterpret_cast<const char*>(&val), sizeof(bool));        \
      if (val) C##Name_##Definition::get(C##Name_##Definition::GetDefName())->toBinary(os);   \
   }   
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"
   }
*/
   //----------------------------------------------------------------
/*
   void CContext::fromBinary(StdIStream & is)
   {
      SuperClass::fromBinary(is);
#define DECLARE_NODE(Name_, name_)                                         \
   {                                                                       \
      bool val = false;                                                    \
      ENodeType renum = Unknown;                                           \
      is.read (reinterpret_cast<char*>(&renum), sizeof(ENodeType));        \
      is.read (reinterpret_cast<char*>(&val), sizeof(bool));               \
      if (renum != C##Name_##Definition::GetType())                        \
         ERROR("CContext::fromBinary(StdIStream & is)",                    \
               << "[ renum = " << renum << "] Bad type !");                \
      if (val) C##Name_##Definition::create(C##Name_##Definition::GetDefName()) -> fromBinary(is); \
   }   
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"
      
   }
 */
   
   //----------------------------------------------------------------

   StdString CContext::toString(void) const
   {
      StdOStringStream oss;
      oss << "<" << CContext::GetName()
          << " id=\"" << this->getId() << "\" "
          << SuperClassAttribute::toString() << ">" << std::endl;
      if (!this->hasChild())
      {
         //oss << "<!-- No definition -->" << std::endl; // fait planter l'incrémentation
      }
      else
      {

#define DECLARE_NODE(Name_, name_)    \
   if (C##Name_##Definition::has(C##Name_##Definition::GetDefName())) \
   oss << * C##Name_##Definition::get(C##Name_##Definition::GetDefName()) << std::endl;
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"

      }

      oss << "</" << CContext::GetName() << " >";

      return (oss.str());
   }

   //----------------------------------------------------------------

   void CContext::solveDescInheritance(bool apply, const CAttributeMap * const UNUSED(parent))
   {
#define DECLARE_NODE(Name_, name_)    \
   if (C##Name_##Definition::has(C##Name_##Definition::GetDefName())) \
     C##Name_##Definition::get(C##Name_##Definition::GetDefName())->solveDescInheritance(apply);
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"
   }

   //----------------------------------------------------------------

   bool CContext::hasChild(void) const
   {
      return (
#define DECLARE_NODE(Name_, name_)    \
   C##Name_##Definition::has(C##Name_##Definition::GetDefName())   ||
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"
      false);
}

   //----------------------------------------------------------------

   void CContext::solveFieldRefInheritance(bool apply)
   {
      if (!this->hasId()) return;
      vector<CField*> allField = CField::getAll() ;
//              = CObjectTemplate<CField>::GetAllVectobject(this->getId());
      std::vector<CField*>::iterator 
         it = allField.begin(), end = allField.end();
            
      for (; it != end; it++)
      {
         CField* field = *it;
         field->solveRefInheritance(apply);
      }
   }

   //----------------------------------------------------------------

   void CContext::CleanTree(void)
   {
#define DECLARE_NODE(Name_, name_) C##Name_##Group::ClearAllAttributes();
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"
   }
   ///---------------------------------------------------------------
   
   void CContext::initClient(MPI_Comm intraComm, MPI_Comm interComm)
   {
     hasClient=true ;
     client = new CContextClient(this,intraComm, interComm) ;
   } 

   void CContext::initServer(MPI_Comm intraComm,MPI_Comm interComm)
   {
     hasServer=true ;
     server = new CContextServer(this,intraComm,interComm) ;
   } 

   bool CContext::eventLoop(void)
   {
     return server->eventLoop() ;
   } 
   
   void CContext::finalize(void)
   {
      if (hasClient && !hasServer)
      {
         client->finalize() ;
      }
      if (hasServer)
      {
        closeAllFile() ;
      }
   }
       
       
 
   
   void CContext::closeDefinition(void)
   {
      if (hasClient && !hasServer) sendCloseDefinition() ;
      
      solveCalendar();         
         
      // Résolution des héritages pour le context actuel.
      this->solveAllInheritance();

      //Initialisation du vecteur 'enabledFiles' contenant la liste des fichiers à sortir.
      this->findEnabledFiles();

      //Recherche des champs à sortir (enable à true + niveau de sortie correct)
      // pour chaque fichier précédemment listé.
      this->findAllEnabledFields();

      // Résolution des références de grilles pour chacun des champs.
      this->solveAllGridRef();

      // Traitement des opérations.
      this->solveAllOperation();

      // Nettoyage de l'arborescence
      CleanTree();
      if (hasClient) sendCreateFileHeader() ;
   }
   
   void CContext::findAllEnabledFields(void)
   {
     for (unsigned int i = 0; i < this->enabledFiles.size(); i++)
     (void)this->enabledFiles[i]->getEnabledFields();
   }

   void CContext::solveAllGridRef(void)
   {
     for (unsigned int i = 0; i < this->enabledFiles.size(); i++)
     this->enabledFiles[i]->solveEFGridRef();
   }

   void CContext::solveAllOperation(void)
   {
      for (unsigned int i = 0; i < this->enabledFiles.size(); i++)
      this->enabledFiles[i]->solveEFOperation();
   }

   void CContext::solveAllInheritance(bool apply)
   {
     // Résolution des héritages descendants (càd des héritages de groupes)
     // pour chacun des contextes.
      solveDescInheritance(apply);

     // Résolution des héritages par référence au niveau des fichiers.
      const vector<CFile*> allFiles=CFile::getAll() ;

      for (unsigned int i = 0; i < allFiles.size(); i++)
         allFiles[i]->solveFieldRefInheritance(apply);
   }

   void CContext::findEnabledFiles(void)
   {
      const std::vector<CFile*> allFiles = CFile::getAll();

      for (unsigned int i = 0; i < allFiles.size(); i++)
         if (!allFiles[i]->enabled.isEmpty()) // Si l'attribut 'enabled' est défini.
         {
            if (allFiles[i]->enabled.getValue()) // Si l'attribut 'enabled' est fixé à vrai.
               enabledFiles.push_back(allFiles[i]);
         }
         else enabledFiles.push_back(allFiles[i]); // otherwise true by default
               

      if (enabledFiles.size() == 0)
         DEBUG(<<"Aucun fichier ne va être sorti dans le contexte nommé \""
               << getId() << "\" !");
   }

   void CContext::closeAllFile(void)
   {
     std::vector<CFile*>::const_iterator
            it = this->enabledFiles.begin(), end = this->enabledFiles.end();
         
     for (; it != end; it++)
     {
       info(30)<<"Closing File : "<<(*it)->getId()<<endl;
       (*it)->close();
     }
   }
   
   bool CContext::dispatchEvent(CEventServer& event)
   {
      
      if (SuperClass::dispatchEvent(event)) return true ;
      else
      {
        switch(event.type)
        {
           case EVENT_ID_CLOSE_DEFINITION :
             recvCloseDefinition(event) ;
             return true ;
             break ;
           case EVENT_ID_UPDATE_CALENDAR :
             recvUpdateCalendar(event) ;
             return true ;
             break ;
           case EVENT_ID_CREATE_FILE_HEADER :
             recvCreateFileHeader(event) ;
             return true ;
             break ;
           default :
             ERROR("bool CContext::dispatchEvent(CEventServer& event)",
                    <<"Unknown Event") ;
           return false ;
         }
      }
   }
   
   void CContext::sendCloseDefinition(void)
   {

     CEventClient event(getType(),EVENT_ID_CLOSE_DEFINITION) ;   
     if (client->isServerLeader())
     {
       CMessage msg ;
       msg<<this->getId() ;
       event.push(client->getServerLeader(),1,msg) ;
       client->sendEvent(event) ;
     }
     else client->sendEvent(event) ;
   }
   
   void CContext::recvCloseDefinition(CEventServer& event)
   {
      
      CBufferIn* buffer=event.subEvents.begin()->buffer;
      string id;
      *buffer>>id ;
      get(id)->closeDefinition() ;
   }
   
   void CContext::sendUpdateCalendar(int step)
   {
     if (!hasServer)
     {
       CEventClient event(getType(),EVENT_ID_UPDATE_CALENDAR) ;   
       if (client->isServerLeader())
       {
         CMessage msg ;
         msg<<this->getId()<<step ;
         event.push(client->getServerLeader(),1,msg) ;
         client->sendEvent(event) ;
       }
       else client->sendEvent(event) ;
     }
   }
   
   void CContext::recvUpdateCalendar(CEventServer& event)
   {
      
      CBufferIn* buffer=event.subEvents.begin()->buffer;
      string id;
      *buffer>>id ;
      get(id)->recvUpdateCalendar(*buffer) ;
   }
   
   void CContext::recvUpdateCalendar(CBufferIn& buffer)
   {
      int step ;
      buffer>>step ;
      updateCalendar(step) ;
   }
   
   void CContext::sendCreateFileHeader(void)
   {

     CEventClient event(getType(),EVENT_ID_CREATE_FILE_HEADER) ;   
     if (client->isServerLeader())
     {
       CMessage msg ;
       msg<<this->getId() ;
       event.push(client->getServerLeader(),1,msg) ;
       client->sendEvent(event) ;
     }
     else client->sendEvent(event) ;
   }
   
   void CContext::recvCreateFileHeader(CEventServer& event)
   {
      
      CBufferIn* buffer=event.subEvents.begin()->buffer;
      string id;
      *buffer>>id ;
      get(id)->recvCreateFileHeader(*buffer) ;
   }
   
   void CContext::recvCreateFileHeader(CBufferIn& buffer)
   {
      createFileHeader() ;
   }
   
   void CContext::updateCalendar(int step)
   {
      info(50)<<"updateCalendar : before : "<<calendar->getCurrentDate()<<endl ;
      calendar->update(step) ;
      info(50)<<"updateCalendar : after : "<<calendar->getCurrentDate()<<endl ;
   }
 
   void CContext::createFileHeader(void )
   {
      vector<CFile*>::const_iterator it ;
         
      for (it=enabledFiles.begin(); it != enabledFiles.end(); it++)
      {
         (*it)->initFile();
      }
   } 
   
   CContext* CContext::getCurrent(void)
   {
     return CObjectFactory::GetObject<CContext>(CObjectFactory::GetCurrentContextId()).get() ;
   }
   
   void CContext::setCurrent(const string& id)
   {
     CObjectFactory::SetCurrentContextId(id);
     CGroupFactory::SetCurrentContextId(id);
   }
   
  CContext* CContext::create(const StdString& id)
  {
    CContext::setCurrent(id) ;
 
    bool hasctxt = CContext::has(id);
    CContext* context = CObjectFactory::CreateObject<CContext>(id).get();
    getRoot() ;
    if (!hasctxt) CGroupFactory::AddChild(root, context->getShared());

#define DECLARE_NODE(Name_, name_) \
    C##Name_##Definition::create(C##Name_##Definition::GetDefName());
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"

    return (context);
  }
} // namespace xios
