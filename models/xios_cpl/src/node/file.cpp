#include "file.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "object_factory.hpp"
#include "data_output.hpp"
#include "context.hpp"
#include "context_server.hpp"
#include "nc4_data_output.hpp"
#include "calendar_util.hpp"
#include "date.hpp"
#include "message.hpp"
#include "type.hpp"
#include "xmlioserver_spl.hpp"
#include "context_client.hpp"
#include "mpi.hpp"

namespace xios {
   
   /// ////////////////////// Définitions ////////////////////// ///

   CFile::CFile(void)
      : CObjectTemplate<CFile>(), CFileAttributes()
      , vFieldGroup(), data_out(), enabledFields(), fileComm(MPI_COMM_NULL)
   { setVirtualFieldGroup() ;}

   CFile::CFile(const StdString & id)
      : CObjectTemplate<CFile>(id), CFileAttributes()
      , vFieldGroup(), data_out(), enabledFields(), fileComm(MPI_COMM_NULL)
   { setVirtualFieldGroup() ;}

   CFile::~CFile(void)
   { /* Ne rien faire de plus */ }

   ///---------------------------------------------------------------

   StdString CFile::GetName(void)   { return (StdString("file")); }
   StdString CFile::GetDefName(void){ return (CFile::GetName()); }
   ENodeType CFile::GetType(void)   { return (eFile); }

   //----------------------------------------------------------------

   boost::shared_ptr<CDataOutput> CFile::getDataOutput(void) const
   {
      return (data_out);
   }

   CFieldGroup* CFile::getVirtualFieldGroup(void) const
   {
      return (this->vFieldGroup);
   }

   std::vector<CField*> CFile::getAllFields(void) const
   {
      return (this->vFieldGroup->getAllChildren());
   }

   //----------------------------------------------------------------

   std::vector<CField*> CFile::getEnabledFields(int default_outputlevel, 
                                                int default_level,
                                                bool default_enabled)
   {
      if (!this->enabledFields.empty())
         return (this->enabledFields);

      const int _outputlevel =
         (!output_level.isEmpty()) ? output_level.getValue() : default_outputlevel;
      std::vector<CField*>::iterator it;
      this->enabledFields = this->getAllFields();

      std::vector<CField*> newEnabledFields;
      
      for ( it = this->enabledFields.begin() ; it != this->enabledFields.end(); it++ )
      {
         if (!(*it)->enabled.isEmpty()) // Si l'attribut 'enabled' est défini ...
         {
            if (! (*it)->enabled.getValue()) continue;
//            { it--; this->enabledFields.erase(it+1); continue; }
         }
         else // Si l'attribut 'enabled' n'est pas défini ...
         {
            if (!default_enabled) continue ;
//            { it--; this->enabledFields.erase(it+1); continue; }
         }

         if (!(*it)->level.isEmpty()) // Si l'attribut 'level' est défini ...
         {
            if ((*it)->level.getValue() > _outputlevel) continue ;
//            { it--; this->enabledFields.erase(it+1); continue; }
         }
         else // Si l'attribut 'level' n'est pas défini ...
         {
            if (default_level > _outputlevel) continue ;
//            { it--; this->enabledFields.erase(it+1); continue; }
         }
 
//         CField* field_tmp=(*it).get() ;
//         shared_ptr<CField> sptfield=*it ;
//         field_tmp->refObject.push_back(sptfield) ;
         newEnabledFields.push_back(*it) ;
         // Le champ est finalement actif, on y ajoute sa propre reference.
         (*it)->refObject.push_back(*it);
         // Le champ est finalement actif, on y ajoute la référence au champ de base.
         (*it)->setRelFile(CFile::get(this));
         (*it)->baseRefObject->refObject.push_back(*it);
         // A faire, ajouter les references intermediaires...
      }
      enabledFields=newEnabledFields ;

      return (this->enabledFields);
   }

   //----------------------------------------------------------------

   void CFile::setVirtualFieldGroup(CFieldGroup* newVFieldGroup)
   { 
      this->vFieldGroup = newVFieldGroup; 
   }

   //----------------------------------------------------------------

   void CFile::setVirtualFieldGroup(void)
   {
      this->setVirtualFieldGroup(CFieldGroup::create());
   }

   //----------------------------------------------------------------
   bool CFile::isSyncTime(void)
   {
     CContext* context = CContext::getCurrent() ;
     CDate& currentDate=context->calendar->getCurrentDate() ;
     if (! sync_freq.isEmpty())
     {
       if (*lastSync+syncFreq < currentDate)
       {
         *lastSync=currentDate ;
         return true ;
        }
      }
      return false ;
    }
    
   void CFile::initFile(void)
   {
      CContext* context = CContext::getCurrent() ;
      CDate& currentDate=context->calendar->getCurrentDate() ;
      CContextServer* server=context->server ;
            
      if (! sync_freq.isEmpty()) syncFreq = CDuration::FromString(sync_freq.getValue());
      if (! split_freq.isEmpty()) splitFreq = CDuration::FromString(split_freq.getValue());
      if (! output_freq.isEmpty()) outputFreq = CDuration::FromString(output_freq.getValue());
      lastSync=new CDate(currentDate) ;
      lastSplit=new CDate(currentDate) ;
      isOpen=false ;

      allDomainEmpty=true ;
      set<CDomain*> setDomain ;

      std::vector<CField*>::iterator it, end = this->enabledFields.end();
      for (it = this->enabledFields.begin() ;it != end; it++)
      {
         CField* field = *it;
         allDomainEmpty&=field->grid->domain->isEmpty() ;
         setDomain.insert(field->grid->domain) ;
      }
      nbDomain=setDomain.size() ;

      // create sub communicator for file  
      int color=allDomainEmpty?0:1 ;
      MPI_Comm_split(server->intraComm,color,server->intraCommRank,&fileComm) ;
      if (allDomainEmpty) MPI_Comm_free(&fileComm) ;
      //
      
    }
    
    void CFile::checkFile(void)
    {
      if (!isOpen) createHeader() ;
      checkSync() ;
      checkSplit() ;
    }
      
     
   bool CFile::checkSync(void)
   {
     CContext* context = CContext::getCurrent() ;
     CDate& currentDate=context->calendar->getCurrentDate() ;
     if (! sync_freq.isEmpty())
     {
       if (*lastSync+syncFreq < currentDate)
       {
         *lastSync=currentDate ;
         data_out->syncFile() ;
         return true ;
        }
      }
      return false ;
    }
    
    
    bool CFile::checkSplit(void)
    {
      CContext* context = CContext::getCurrent() ;
      CDate& currentDate=context->calendar->getCurrentDate() ;
      if (! split_freq.isEmpty())
      {
        if (currentDate > *lastSplit+splitFreq)
        {
          *lastSplit=*lastSplit+splitFreq ;    
          std::vector<CField*>::iterator it, end = this->enabledFields.end();
          for (it = this->enabledFields.begin() ;it != end; it++)  (*it)->resetNStep() ;
          createHeader() ;
          return true ;
        }
      }
      return false ;
    }
    
   void CFile::createHeader(void)
   {
      CContext* context = CContext::getCurrent() ;
      CContextServer* server=context->server ;
     
      if (!allDomainEmpty)
      {
         StdString filename = (!name.isEmpty()) ?   name.getValue() : getId();
         StdOStringStream oss;
         oss << filename;
         if (!name_suffix.isEmpty()) oss << name_suffix.getValue();
//         if (!split_freq.isEmpty()) oss<<"_"<<lastSplit->getStryyyymmdd()<<"-"<< (*lastSplit+(splitFreq-1*Second)).getStryyyymmdd();
//         if (!split_freq.isEmpty()) oss<<"_"<<lastSplit->getStr("%y_%mo_%d")<<"-"<< (*lastSplit+(splitFreq-1*Second)).getStr("%y_%mo_%d");
         if (!split_freq.isEmpty())
         {
           string splitFormat ;
           if (split_freq_format.isEmpty())
           {
             if (splitFreq.second!=0) splitFormat="%y%mo%d%h%mi%s";
             else if (splitFreq.minute!=0) splitFormat="%y%mo%d%h%mi";
             else if (splitFreq.hour!=0) splitFormat="%y%mo%d%h";
             else if (splitFreq.day!=0) splitFormat="%y%mo%d";
             else if (splitFreq.month!=0) splitFormat="%y%mo";
             else splitFormat="%y";
           }
           else splitFormat=split_freq_format ;
           oss<<"_"<<lastSplit->getStr(splitFormat)<<"-"<< (*lastSplit+(splitFreq-1*Second)).getStr(splitFormat);
         }
           
         bool multifile=true ;
         if (!type.isEmpty())
         {
           if (type==type_attr::one_file) multifile=false ;
           else if (type==type_attr::multiple_file) multifile=true ;

         } 
#ifndef USING_NETCDF_PAR
         if (!multifile)
         {
            info(0)<<"!!! Warning -> Using non parallel version of netcdf, switching in multiple_file mode for file : "<<filename<<" ..."<<endl ;
            multifile=true ;
          }
#endif
         if (multifile) 
         {
            int commSize, commRank ;
            MPI_Comm_size(fileComm,&commSize) ;
            MPI_Comm_rank(fileComm,&commRank) ;
            
            if (server->intraCommSize > 1) 
            {
              oss << "_"  ;
              int width=0 ; int n=commSize-1 ;
              while(n != 0) { n=n/10 ; width++ ;}
              if (!min_digits.isEmpty()) 
                if (width<min_digits) width=min_digits ;
              oss.width(width) ;
              oss.fill('0') ;
              oss<<right<< commRank;
            }
         }
         oss << ".nc";

         if (isOpen) data_out->closeFile() ;
         bool isCollective=true ;
         if (!par_access.isEmpty())
         {
           if (par_access.getValue()=="independent") isCollective=false ;
           else if (par_access.getValue()=="collective") isCollective=true ;
           else 
           {
             ERROR("void Context::createDataOutput(void)",
                        "incorrect file <par_access> attribut : must be <collective> or <indepedent>, "
                        <<"having : <"<<type.getValue()<<">") ;
           }
         }
         data_out=shared_ptr<CDataOutput>(new CNc4DataOutput(oss.str(), false, fileComm, multifile, isCollective));
         isOpen=true ;

         data_out->writeFile(CFile::get(this));
         std::vector<CField*>::iterator it, end = this->enabledFields.end();
         for (it = this->enabledFields.begin() ;it != end; it++)
         {
            CField* field = *it;
            this->data_out->writeFieldGrid(field);
         }
         this->data_out->writeTimeDimension();
         
         for (it = this->enabledFields.begin() ;it != end; it++)
         {
            CField* field = *it;
            this->data_out->writeField(field);
         }
         
         this->data_out->definition_end();
      }
   }

   void CFile::close(void)
   {
     delete lastSync ;
     delete lastSplit ;
     if (!allDomainEmpty)
       if (isOpen) 
       {
         this->data_out->closeFile();
       }
      if (fileComm != MPI_COMM_NULL) MPI_Comm_free(&fileComm) ;
   }
   //----------------------------------------------------------------

   void CFile::parse(xml::CXMLNode & node)
   {
      SuperClass::parse(node);
      if (node.goToChildElement() & this->hasId())
      { // Si la définition du fichier intégre des champs et si le fichier est identifié.
         node.goToParentElement();
//         this->setVirtualFieldGroup(this->getId());
         this->getVirtualFieldGroup()->parse(node, false);
      }
   }
   //----------------------------------------------------------------

   StdString CFile::toString(void) const
   {
      StdOStringStream oss;

      oss << "<" << CFile::GetName() << " ";
      if (this->hasId())
         oss << " id=\"" << this->getId() << "\" ";
      oss << SuperClassAttribute::toString() << ">" << std::endl;
      if (this->getVirtualFieldGroup() != NULL)
         oss << *this->getVirtualFieldGroup() << std::endl;
      oss << "</" << CFile::GetName() << " >";
      return (oss.str());
   }

   //----------------------------------------------------------------
   
   void CFile::solveDescInheritance(bool apply, const CAttributeMap * const parent)
   {
      SuperClassAttribute::setAttributes(parent,apply);
      this->getVirtualFieldGroup()->solveDescInheritance(apply, NULL);
   }

   //----------------------------------------------------------------

   void CFile::solveFieldRefInheritance(bool apply)
   {
      // Résolution des héritages par référence de chacun des champs contenus dans le fichier.
      std::vector<CField*> allF = this->getAllFields();
      for (unsigned int i = 0; i < allF.size(); i++)
         allF[i]->solveRefInheritance(apply);
   }

   //----------------------------------------------------------------

   void CFile::solveEFGridRef(void)
   {
      for (unsigned int i = 0; i < this->enabledFields.size(); i++)
         this->enabledFields[i]->solveGridReference();
   }

   //----------------------------------------------------------------

   void CFile::solveEFOperation(void)
   {
      for (unsigned int i = 0; i < this->enabledFields.size(); i++)
         this->enabledFields[i]->solveOperation();
   }
   
   //---------------------------------------------------------------
/*
   void CFile::toBinary  (StdOStream & os) const
   {
      ENodeType genum = CFileGroup::GetType();
      bool hasVFG = (this->getVirtualFieldGroup() != NULL);
      SuperClass::toBinary(os);
      
      os.write (reinterpret_cast<const char*>(&genum) , sizeof(ENodeType));
      os.write (reinterpret_cast<const char*>(&hasVFG) , sizeof(bool));
      
      if (hasVFG)this->getVirtualFieldGroup()->toBinary(os);
         
   }
   
   //----------------------------------------------------------------
   
   void CFile::fromBinary(StdIStream & is)
   {
      ENodeType renum = Unknown;
      bool hasVFG = false;
      SuperClass::fromBinary(is);
      
      is.read (reinterpret_cast<char*>(&renum), sizeof(ENodeType));
      is.read (reinterpret_cast<char*>(&hasVFG), sizeof(bool));
      
      if (renum != CFileGroup::GetType())
         ERROR("CFile::fromBinary(StdIStream & is)",
               << "[ renum = " << renum << "] Bad type !");
      
//      this->setVirtualFieldGroup(this->getId());
      if (hasVFG)this->getVirtualFieldGroup()->fromBinary(is);
      
   }
*/

   CField* CFile::addField(const string& id)
   {
     return vFieldGroup->createChild(id) ;
   }

   CFieldGroup* CFile::addFieldGroup(const string& id)
   {
     return vFieldGroup->createChildGroup(id) ;
   }
   
  
   void CFile::sendAddField(const string& id)
   {
    CContext* context=CContext::getCurrent() ;
    
    if (! context->hasServer )
    {
       CContextClient* client=context->client ;

       CEventClient event(this->getType(),EVENT_ID_ADD_FIELD) ;   
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
   
   void CFile::sendAddFieldGroup(const string& id)
   {
    CContext* context=CContext::getCurrent() ;
    if (! context->hasServer )
    {
       CContextClient* client=context->client ;

       CEventClient event(this->getType(),EVENT_ID_ADD_FIELD_GROUP) ;   
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
   
   void CFile::recvAddField(CEventServer& event)
   {
      
      CBufferIn* buffer=event.subEvents.begin()->buffer;
      string id;
      *buffer>>id ;
      get(id)->recvAddField(*buffer) ;
   }
   
   
   void CFile::recvAddField(CBufferIn& buffer)
   {
      string id ;
      buffer>>id ;
      addField(id) ;
   }

   void CFile::recvAddFieldGroup(CEventServer& event)
   {
      
      CBufferIn* buffer=event.subEvents.begin()->buffer;
      string id;
      *buffer>>id ;
      get(id)->recvAddFieldGroup(*buffer) ;
   }
   
   
   void CFile::recvAddFieldGroup(CBufferIn& buffer)
   {
      string id ;
      buffer>>id ;
      addFieldGroup(id) ;
   }
   

   bool CFile::dispatchEvent(CEventServer& event)
   {
      if (SuperClass::dispatchEvent(event)) return true ;
      else
      {
        switch(event.type)
        {
           case EVENT_ID_ADD_FIELD :
             recvAddField(event) ;
             return true ;
             break ;
         
           case EVENT_ID_ADD_FIELD_GROUP :
             recvAddFieldGroup(event) ;
             return true ;
             break ;       
         
           default :
              ERROR("bool CFile::dispatchEvent(CEventServer& event)", <<"Unknown Event") ;
           return false ;
        }
      }
   }
   
   
   
   
   ///---------------------------------------------------------------

} // namespace xios
