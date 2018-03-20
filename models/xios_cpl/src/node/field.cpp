#include "field.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"

#include "node_type.hpp"
#include "calendar_util.hpp"
#include "message.hpp"
#include "xmlioserver_spl.hpp"
#include "type.hpp"
#include "context_client.hpp"

namespace xios{
   
   /// ////////////////////// Définitions ////////////////////// ///

   CField::CField(void)
      : CObjectTemplate<CField>(), CFieldAttributes()
      , refObject(), baseRefObject()
      , grid(), file()
      , freq_operation(), freq_write()
      , nstep(0)
      , last_Write(), last_operation()
      , foperation()
      { /* Ne rien faire de plus */ }

   CField::CField(const StdString & id)
      : CObjectTemplate<CField>(id), CFieldAttributes()
      , refObject(), baseRefObject()
      , grid(), file()
      , freq_operation(), freq_write()
      , nstep(0)
      , last_Write(), last_operation()
      , foperation()
   { /* Ne rien faire de plus */ }

   CField::~CField(void)
   {
//      this->grid.reset() ;
//      this->file.reset() ;
      this->foperation.reset() ;
   }

   //----------------------------------------------------------------

   bool CField::updateDataServer
      (const CDate & currDate,
       const std::deque< CArray<double, 1>* > storedClient)
   {
      const CDate opeDate      = *last_operation + freq_operation;
      const CDate writeDate    = *last_Write     + freq_write; 
      
      if (opeDate <= currDate)
      {
         if (this->data.numElements() != this->grid->storeIndex[0]->numElements())
         {
            this->data.resize(this->grid->storeIndex[0] ->numElements());
         }  
         CArray<double,1> input(data.numElements()) ;
         this->grid->inputFieldServer(storedClient, input);          
         (*this->foperation)(input);
         *last_operation = currDate;
      }
      if (writeDate < (currDate + freq_operation))
      {
         this->foperation->final();
         this->incrementNStep();
         *last_Write = writeDate;
         return (true);        
      }
      return (false);
   }
   
   bool CField::dispatchEvent(CEventServer& event)
  {
     
    if (SuperClass::dispatchEvent(event)) return true ;
    else
    {
      switch(event.type)
      {
        case EVENT_ID_UPDATE_DATA :
          recvUpdateData(event) ;
          return true ;
          break ;
 
        default :
          ERROR("bool CField::dispatchEvent(CEventServer& event)",<<"Unknown Event") ;
          return false ;
      }
    }
  }
  
  void CField::sendUpdateData(void)
  {
    CContext* context = CContext::getCurrent() ;
    CContextClient* client=context->client ;
    
    CEventClient event(getType(),EVENT_ID_UPDATE_DATA) ;
    
    map<int,CArray<int, 1>* >::iterator it ;
    list<shared_ptr<CMessage> > list_msg ;
    list< CArray<double,1>* > list_data ;
    
    for(it=grid->storeIndex_toSrv.begin();it!=grid->storeIndex_toSrv.end();it++)
    {
      int rank=(*it).first ;
      CArray<int,1>& index = *(it->second) ;
      CArray<double,1> data_tmp(index.numElements()) ;
      
      for(int n=0;n<data_tmp.numElements();n++) data_tmp(n)=data(index(n)) ;
      list_msg.push_back(shared_ptr<CMessage>(new CMessage)) ;
      list_data.push_back(new CArray<double,1>(data_tmp)) ;
      *list_msg.back()<<getId()<<*list_data.back() ;
      event.push(rank,grid->nbSenders[rank],*list_msg.back()) ;
    }
    client->sendEvent(event) ;
    
    for(list< CArray<double,1>* >::iterator it=list_data.begin();it!=list_data.end();it++) delete *it ;
  }
  
  void CField::recvUpdateData(CEventServer& event)
  {
    vector<int> ranks ;
    vector<CBufferIn*> buffers ;
      
    list<CEventServer::SSubEvent>::iterator it ;
    string fieldId ;

    for (it=event.subEvents.begin();it!=event.subEvents.end();++it)
    {
      int rank=it->rank;
      CBufferIn* buffer=it->buffer;
      *buffer>>fieldId ;
      ranks.push_back(rank) ;
      buffers.push_back(buffer) ;
    }
    get(fieldId)->recvUpdateData(ranks,buffers) ;   
  }
  
  void  CField::recvUpdateData(vector<int>& ranks, vector<CBufferIn*>& buffers)
  {
    
    if (data_srv.empty())
    {
      for(map<int, CArray<int, 1>* >::iterator it=grid->out_i_fromClient.begin();it!=grid->out_i_fromClient.end();it++)
      {
        int rank=it->first ;
        CArray<double,1> data_tmp(it->second->numElements()) ;
        data_srv.insert( pair<int, CArray<double,1>* >(rank, new CArray<double,1>(data_tmp) ) ) ;
        foperation_srv.insert(pair<int,boost::shared_ptr<func::CFunctor> >(rank,boost::shared_ptr<func::CFunctor>(new func::CInstant(*data_srv[rank])))) ;
      }
    }

    CContext* context = CContext::getCurrent() ;
    const CDate & currDate = context->getCalendar()->getCurrentDate();
    const CDate opeDate      = *last_operation_srv + freq_operation_srv;
    const CDate writeDate    = *last_Write_srv     + freq_write_srv; 
    

    
    if (opeDate <= currDate)
    {
      for(int n=0;n<ranks.size();n++)
      {
        CArray<double,1> data_tmp ;
        *buffers[n]>>data_tmp ;
        (*foperation_srv[ranks[n]])(data_tmp) ;
      }
      *last_operation_srv = currDate;
    }
     
    if (writeDate < (currDate + freq_operation_srv))
    {
      for(int n=0;n<ranks.size();n++)
      {
        this->foperation_srv[ranks[n]]->final();
      }
      
      *last_Write_srv = writeDate;
      writeField() ;
      *lastlast_Write_srv=*last_Write_srv;
    }
  }
  
  void CField::writeField(void)
  {
    if (! getRelFile()->allDomainEmpty )
      if (! grid->domain->isEmpty() || getRelFile()->type == CFile::type_attr::one_file)
      {
        getRelFile()->checkFile();
        this->incrementNStep();
        getRelFile()->getDataOutput()->writeFieldData(CField::get(this));
      }
  }
   //----------------------------------------------------------------

   void CField::setRelFile(CFile* _file)
   { 
      this->file = _file; 
   }

   //----------------------------------------------------------------

   StdString CField::GetName(void)   { return (StdString("field")); }
   StdString CField::GetDefName(void){ return (CField::GetName()); }
   ENodeType CField::GetType(void)   { return (eField); }

   //----------------------------------------------------------------

   CGrid* CField::getRelGrid(void) const
   { 
      return (this->grid); 
   }

   //----------------------------------------------------------------

   CFile* CField::getRelFile(void) const
   { 
      return (this->file);
   }
   
   StdSize CField::getNStep(void) const
   {
      return (this->nstep);
   }
   
   void CField::incrementNStep(void)
   {
      this->nstep++;
   }
 
   void CField::resetNStep(void)
   {
      this->nstep=0;
   }

   //----------------------------------------------------------------

   CField* CField::getDirectFieldReference(void) const
   {
      if (this->field_ref.isEmpty())
         return (this->getBaseFieldReference());

      if (! CField::has(this->field_ref.getValue()))
         ERROR("CField::getDirectFieldReference(void)",
               << "[ ref_name = " << this->field_ref.getValue() << "]"
               << " invalid field name !");

      return (CField::get(this->field_ref.getValue()));
   }

   //----------------------------------------------------------------

   CField* CField::getBaseFieldReference(void) const
   { 
      return (baseRefObject); 
   }

   //----------------------------------------------------------------

   const std::vector<CField*>& CField::getAllReference(void) const 
   { 
      return (refObject);
   }

   //----------------------------------------------------------------

   const StdString & CField::getBaseFieldId(void) const
   { 
      return (this->getBaseFieldReference()->getId());
   }
   
   //----------------------------------------------------------------
   
   const CDuration & CField::getFreqOperation(void) const
   {
      return (this->freq_operation);
   }
   
   //----------------------------------------------------------------
   const CDuration & CField::getFreqWrite(void) const
   {
      return (this->freq_write);
   }
   
   //----------------------------------------------------------------
         
   boost::shared_ptr<func::CFunctor> CField::getFieldOperation(void) const
   {
      return (this->foperation);
   }

   //----------------------------------------------------------------

   bool CField::hasDirectFieldReference(void) const
   { 
     return (!this->field_ref.isEmpty()); 
   }
   
   bool CField::isActive(void) const
   { 
      return (!this->refObject.empty()); 
   }
   //----------------------------------------------------------------
   
   CArray<double, 1> CField::getData(void) const
   {
      return(this->data);
   }

   //----------------------------------------------------------------

   boost::shared_ptr<CDate> CField::getLastWriteDate(void) const
   {
      return(this->last_Write);
   }

   //----------------------------------------------------------------

   boost::shared_ptr<CDate> CField::getLastOperationDate(void) const
   {
      return(this->last_operation);
   }

   //----------------------------------------------------------------

   void CField::solveRefInheritance(bool apply)
   {
      std::set<CField *> sset;
      CField* refer_sptr;
      CField * refer_ptr = this;
      
      this->baseRefObject = CField::get(this);
      
      while (refer_ptr->hasDirectFieldReference())
      {
         refer_sptr = refer_ptr->getDirectFieldReference();
         refer_ptr  = refer_sptr;

         if(sset.end() != sset.find(refer_ptr))
         {
            DEBUG (<< "Dépendance circulaire stoppée pour l'objet de type CField sur "
                   << "\"" + refer_ptr->getId() + "\" !");
            break;
         }

         SuperClassAttribute::setAttributes(refer_ptr, apply);
         sset.insert(refer_ptr);
         baseRefObject = refer_sptr;
//ym         refObject.push_back(refer_sptr);
      }
   }

   //----------------------------------------------------------------

   void  CField::solveOperation(void)
   {
      using namespace func;
       
      StdString id = this->getBaseFieldReference()->getId();
      CContext* context = CContext::getCurrent();
      
      if (freq_op.isEmpty()) freq_op=string("1ts") ;
      
      if (operation.isEmpty()  || this->file->output_freq.isEmpty())
      {
         ERROR("CField::solveOperation(void)",
               << "[ id = " << id << "]"
               << "Impossible to define an operation for this field !");
      }
      
      CDuration freq_offset_ = NoneDu;
      if (!freq_offset.isEmpty())
      {
         freq_offset_ = CDuration::FromString(freq_offset.getValue());
      }
      else
      {
         freq_offset.setValue(NoneDu.toString());
      }  

//      if (CXIOSManager::GetStatus() == CXIOSManager::LOC_SERVER)
      if (context->hasServer)
      {
         this->freq_operation_srv =
             CDuration::FromString(this->file->output_freq.getValue());
         this->freq_write_srv     =
             CDuration::FromString(this->file->output_freq.getValue());
         this->lastlast_Write_srv     = boost::shared_ptr<CDate>
                        (new CDate(context->getCalendar()->getInitDate()));
         this->last_Write_srv     = boost::shared_ptr<CDate>
                        (new CDate(context->getCalendar()->getInitDate()));
         this->last_operation_srv = boost::shared_ptr<CDate>
                        (new CDate(context->getCalendar()->getInitDate()));
//         this->foperation_srv     =
//             boost::shared_ptr<func::CFunctor>(new CInstant(this->data_srv));
             
         const CDuration toffset = this->freq_operation_srv - freq_offset_ - context->getCalendar()->getTimeStep(); 
         *this->last_operation_srv   = *this->last_operation_srv - toffset; 
      }
      
      if (context->hasClient)
      {                  
         this->freq_operation = CDuration::FromString(freq_op.getValue());
         this->freq_write     = CDuration::FromString(this->file->output_freq.getValue());
         this->last_Write     = boost::shared_ptr<CDate>
                        (new CDate(context->getCalendar()->getInitDate()));
         this->last_operation = boost::shared_ptr<CDate>
                        (new CDate(context->getCalendar()->getInitDate()));
                        
         const CDuration toffset = this->freq_operation - freq_offset_ - context->getCalendar()->getTimeStep(); 
         *this->last_operation   = *this->last_operation - toffset;  
      
        if (operation.get()=="once") isOnceOperation=true ;
        else isOnceOperation=false;
        isFirstOperation=true;

#define DECLARE_FUNCTOR(MType, mtype)              \
   if  (operation.getValue().compare(#mtype) == 0) \
   {                                               \
      boost::shared_ptr<func::CFunctor>            \
            foperation_(new C##MType(this->data)); \
      this->foperation = foperation_;              \
      return;                                      \
   }
   
#include "functor_type.conf"
         
         ERROR("CField::solveOperation(void)",
               << "[ operation = " << operation.getValue() << "]"
               << "The operation is not defined !");
      }
      

   }
   
   //----------------------------------------------------------------
/*
   void CField::fromBinary(StdIStream & is)
   {
      SuperClass::fromBinary(is);
#define CLEAR_ATT(name_)\
      SuperClassAttribute::operator[](#name_)->reset()

         CLEAR_ATT(domain_ref);
         CLEAR_ATT(axis_ref);
#undef CLEAR_ATT

   }
*/
   //----------------------------------------------------------------

   void CField::solveGridReference(void)
   {
      CDomain* domain;
      CAxis* axis;

      if (!domain_ref.isEmpty())
      {
         if (CDomain::has(domain_ref.getValue()))
            domain = CDomain::get(domain_ref.getValue()) ;
         else
            ERROR("CField::solveGridReference(void)",
                  << "Reference to the domain \'"
                  << domain_ref.getValue() << "\' is wrong") ;
      }

      if (!axis_ref.isEmpty())
      {
         if (CAxis::has(axis_ref.getValue()))
            axis = CAxis::get(axis_ref.getValue()) ;
         else
            ERROR("CField::solveGridReference(void)",
                  << "Reference to the axis \'"
                  << axis_ref.getValue() <<"\' is wrong") ;
      }

      if (!grid_ref.isEmpty())
      {
         if (CGrid::has(grid_ref.getValue()))
            this->grid = CGrid::get(grid_ref.getValue()) ;
         else
            ERROR("CField::solveGridReference(void)",
                  << "Reference to the grid \'"
                  << grid_ref.getValue() << "\' is wrong");
      }
      
      if (grid_ref.isEmpty() &&  domain_ref.isEmpty())
      {
            ERROR("CField::solveGridReference(void)",
                  << "The horizontal domain for this field is not defined");

     }
     
     CType<string> goodDomain ;
     CType<string> goodAxis ;
     if (!grid_ref.isEmpty())
     {
       if (!grid->domain_ref.isEmpty()) goodDomain=grid->domain_ref ;
       if (!grid->axis_ref.isEmpty()) goodAxis=grid->axis_ref ;
     }
     if (!domain_ref.isEmpty()) goodDomain=domain_ref ;
     if (!axis_ref.isEmpty()) goodAxis=axis_ref ;
     
     
     if (goodDomain.isEmpty()) 
     {
       ERROR("CField::solveGridReference(void)", << "The horizontal domain for this field is not defined");
     }
     else 
     {
       if (CDomain::has(goodDomain)) domain = CDomain::get(goodDomain) ;
       else ERROR("CField::solveGridReference(void)",<< "Reference to the domain \'"<<goodDomain.get() << "\' is wrong") ;
     }
 
     if (!goodAxis.isEmpty())
     {
       if (CAxis::has(goodAxis))  axis = CAxis::get(goodAxis) ;
       else  ERROR("CField::solveGridReference(void)", << "Reference to the axis \'"
                  << goodAxis.get() <<"\' is wrong") ;
     } 
    
     bool nothingToDo=false ;
     
     if (!grid_ref.isEmpty())
     {
       if (!grid->domain_ref.isEmpty() && goodDomain.get() == grid->domain_ref.get())
         if (goodAxis.isEmpty()) nothingToDo=true ;
         else if (!grid->axis_ref.isEmpty()) 
                 if (grid->axis_ref.get()==goodAxis.get()) nothingToDo=true ;
     }
     
     if (!nothingToDo)
     {
       if (!goodAxis.isEmpty())
       {
         this->grid = CGrid::createGrid(domain, axis) ;
         this->grid_ref.setValue(this->grid->getId());
       }
       else
       {
         this->grid = CGrid::createGrid(domain) ;
         this->grid_ref.setValue(this->grid->getId());
       }    
     }

     grid->solveReference() ;
   }


   ///-------------------------------------------------------------------

   template <>
      void CGroupTemplate<CField, CFieldGroup, CFieldAttributes>::solveRefInheritance(void)
   {
      if (this->group_ref.isEmpty()) return;
      StdString gref = this->group_ref.getValue();

      if (!CFieldGroup::has(gref))
         ERROR("CGroupTemplate<CField, CFieldGroup, CFieldAttributes>::solveRefInheritance(void)",
               << "[ gref = " << gref << "]"
               << " invalid group name !");

      CFieldGroup* group = CFieldGroup::get(gref);
      CFieldGroup* owner = CFieldGroup::get(boost::polymorphic_downcast<CFieldGroup*>(this));

      std::vector<CField*> allChildren  = group->getAllChildren();
      std::vector<CField*>::iterator 
         it = allChildren.begin(), end = allChildren.end();
      
      for (; it != end; it++)
      {
         CField* child = *it;
         if (child->hasId()) owner->createChild()->field_ref.setValue(child->getId()) ;
            
      }
   }
   
   void CField::outputField(CArray<double,3>& fieldOut)
   {
      map<int, CArray<double,1>* >::iterator it;
      for(it=data_srv.begin();it!=data_srv.end();it++)
         grid->outputField(it->first,*it->second, fieldOut) ;
      
   }
   
   void CField::outputField(CArray<double,2>& fieldOut)
   {
      map<int, CArray<double,1>* >::iterator it;

      for(it=data_srv.begin();it!=data_srv.end();it++)
      {
         grid->outputField(it->first,*it->second, fieldOut) ;
      }
   }
   ///-------------------------------------------------------------------

} // namespace xios
