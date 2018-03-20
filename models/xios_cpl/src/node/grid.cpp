
#include "grid.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "message.hpp"
#include <iostream>
#include "xmlioserver_spl.hpp"
#include "type.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "array_new.hpp"

namespace xios {

   /// ////////////////////// Définitions ////////////////////// ///

   CGrid::CGrid(void)
      : CObjectTemplate<CGrid>(), CGridAttributes()
      , withAxis(false), isChecked(false), axis(), domain()
      , storeIndex(1), out_i_index(1), out_j_index(1), out_l_index(1)
   { /* Ne rien faire de plus */ }

   CGrid::CGrid(const StdString & id)
      : CObjectTemplate<CGrid>(id), CGridAttributes()
      , withAxis(false), isChecked(false), axis(), domain()
      , storeIndex(1), out_i_index(1), out_j_index(1), out_l_index(1)
   { /* Ne rien faire de plus */ }

   CGrid::~CGrid(void)
   { 
 //     this->axis.reset() ;
//      this->domain.reset() ;
    deque< CArray<int, 1>* >::iterator it ;
    
    for(deque< CArray<int,1>* >::iterator it=storeIndex.begin(); it!=storeIndex.end();it++)  delete *it ;
    for(deque< CArray<int,1>* >::iterator it=out_i_index.begin();it!=out_i_index.end();it++) delete *it ;
    for(deque< CArray<int,1>* >::iterator it=out_j_index.begin();it!=out_j_index.end();it++) delete *it ;
    for(deque< CArray<int,1>* >::iterator it=out_l_index.begin();it!=out_l_index.end();it++) delete *it ;

    for(map<int,CArray<int,1>* >::iterator it=out_i_fromClient.begin();it!=out_i_fromClient.end();it++) delete it->second ;
    for(map<int,CArray<int,1>* >::iterator it=out_j_fromClient.begin();it!=out_j_fromClient.end();it++) delete it->second ;
    for(map<int,CArray<int,1>* >::iterator it=out_l_fromClient.begin();it!=out_l_fromClient.end();it++) delete it->second ;

   }

   ///---------------------------------------------------------------

   StdString CGrid::GetName(void)    { return (StdString("grid")); }
   StdString CGrid::GetDefName(void) { return (CGrid::GetName()); }
   ENodeType CGrid::GetType(void)    { return (eGrid); }

   //----------------------------------------------------------------

   const std::deque< CArray<int,1>* > & CGrid::getStoreIndex(void) const
   { 
      return (this->storeIndex );
   }

   //---------------------------------------------------------------

   const std::deque< CArray<int,1>* > & CGrid::getOutIIndex(void)  const
   { 
      return (this->out_i_index ); 
   }

   //---------------------------------------------------------------

   const std::deque< CArray<int,1>* > & CGrid::getOutJIndex(void)  const
   { 
      return (this->out_j_index ); 
   }

   //---------------------------------------------------------------

   const std::deque< CArray<int,1>* > & CGrid::getOutLIndex(void)  const
   { 
      return (this->out_l_index ); 
   }

   //---------------------------------------------------------------

   const CAxis*   CGrid::getRelAxis  (void) const
   { 
      return (this->axis ); 
   }

   //---------------------------------------------------------------

   const CDomain* CGrid::getRelDomain(void) const
   { 
      return (this->domain ); 
   }

   //---------------------------------------------------------------

   bool CGrid::hasAxis(void) const 
   { 
      return (this->withAxis); 
   }

   //---------------------------------------------------------------

   StdSize CGrid::getDimension(void) const
   {
      return ((this->withAxis)?3:2);
   }

   //---------------------------------------------------------------

/*
   std::vector<StdSize> CGrid::getLocalShape(void) const
   {
      std::vector<StdSize> retvalue;
      retvalue.push_back(domain->zoom_ni_loc.getValue());
      retvalue.push_back(domain->zoom_nj_loc.getValue());
      if (this->withAxis)
         retvalue.push_back(this->axis->zoom_size.getValue());
      return (retvalue);
   }
*/
   //---------------------------------------------------------------
   
/*
   StdSize CGrid::getLocalSize(void) const
   {
      StdSize retvalue = 1;
      std::vector<StdSize> shape_ = this->getLocalShape();
      for (StdSize s = 0; s < shape_.size(); s++)
         retvalue *= shape_[s];
      return (retvalue);
   }
*/
   //---------------------------------------------------------------
/*
   std::vector<StdSize> CGrid::getGlobalShape(void) const
   {
      std::vector<StdSize> retvalue;
      retvalue.push_back(domain->ni.getValue());
      retvalue.push_back(domain->nj.getValue());
      if (this->withAxis)
         retvalue.push_back(this->axis->size.getValue());
      return (retvalue);
   }
*/
   //---------------------------------------------------------------

/*   
   StdSize CGrid::getGlobalSize(void) const
   {
      StdSize retvalue = 1;
      std::vector<StdSize> shape_ = this->getGlobalShape();
      for (StdSize s = 0; s < shape_.size(); s++)
         retvalue *= shape_[s];
      return (retvalue);
   }
*/
   StdSize CGrid::getDataSize(void) const
   {
      StdSize retvalue=domain->data_ni.getValue() ;
      if (domain->data_dim.getValue()==2) retvalue*=domain->data_nj.getValue() ;
      if (this->withAxis) retvalue*=this->axis->size.getValue() ;

      return (retvalue);
   }

   //---------------------------------------------------------------

   void CGrid::solveReference(void)
   {
      if (this->isChecked) return;
      CContext* context = CContext::getCurrent() ;
      CContextClient* client=context->client ;
      
      this->solveDomainRef() ;
      this->solveAxisRef() ;
        
      if (context->hasClient)
      {
         checkMask() ;
         this->computeIndex() ;

         this->storeIndex.push_front(new CArray<int,1>() );
         this->out_i_index.push_front(new CArray<int,1>());
         this->out_j_index.push_front(new CArray<int,1>());
         this->out_l_index.push_front(new CArray<int,1>());
      }
//      this->computeIndexServer();
      this->isChecked = true;
   }


   void CGrid::checkMask(void)
   {
      using namespace std;

      unsigned int niu = domain->ni, nju = domain->nj;
      unsigned int nlu = 1 ;
      if (hasAxis()) nlu=axis->size ;

      if (!mask.isEmpty())
      {
         if ((mask.extent(0) != niu) ||
             (mask.extent(1) != nju) ||
             (mask.extent(2) != nlu))
             ERROR("CGrid::checkAttributes(void)",
                  <<"The mask has not the same size than the local grid"<<endl 
                  <<"Local size is "<<niu<<"x"<<nju<<"x"<<nlu<<endl
                  <<"Mask size is "<<mask.extent(0)<<"x"<<mask.extent(1)<<"x"<<mask.extent(2));
      }
      else 
      {
        mask.resize(niu,nju,nlu) ;
        mask=true  ;
      }
     
      CArray<bool,2>& domainMask = domain->mask ;
      for (int l=0; l < nlu ; l++)
        for (int j=0; j < nju ; j++)
          for(int i=0; i<niu ; i++) mask(i,j,l) = mask(i,j,l) && domainMask(i,j) ;
        
      
   }
   
   //---------------------------------------------------------------

   void CGrid::solveDomainRef(void)
   {
      if (!domain_ref.isEmpty())
      {
         if (CDomain::has(domain_ref.getValue()))
         {
            this->domain = CDomain::get(domain_ref.getValue()) ;
            domain->checkAttributes() ;
         }
         else ERROR("CGrid::solveDomainRef(void)",
                     << "Wrong domain reference") ;
      }
      else ERROR("CGrid::solveDomainRef(void)",
                  << "Domain reference is not defined") ;
   }

   //---------------------------------------------------------------

   void CGrid::solveAxisRef(void)
   {
      if (!axis_ref.isEmpty())
      {
         this->withAxis = true ;
         if (CAxis::get(axis_ref.getValue()))
         {
            this->axis = CAxis::get(axis_ref.getValue()) ;
            axis->checkAttributes() ;
         }
         else ERROR("CGrid::solveAxisRef(void)",
                    << "Wrong axis reference") ;
      }
      else withAxis = false ;
   }

   //---------------------------------------------------------------

   void CGrid::computeIndex(void)
   {    
   
      const int ni   = domain->ni.getValue() ,
                nj   = domain->nj.getValue() ,
                size = (this->hasAxis()) ? axis->size.getValue() : 1 ,
                lbegin = (this->hasAxis()) ? axis->zoom_begin.getValue()-1 : 0 ,
                lend = (this->hasAxis()) ? axis->zoom_end.getValue()-1 : 0 ;


      const int data_dim     = domain->data_dim.getValue() ,
                data_n_index = domain->data_n_index.getValue() ,
                data_ibegin  = domain->data_ibegin.getValue() ,
                data_jbegin  = (data_dim == 2)
                             ? domain->data_jbegin.getValue() : -1;

      CArray<int,1> data_i_index = domain->data_i_index ;
      CArray<int,1> data_j_index = domain->data_j_index ;
      

      int indexCount = 0;

      for(int l = 0; l < size ; l++)
      {
         for(int n = 0, i = 0, j = 0; n < data_n_index; n++)
         {
            int temp_i = data_i_index(n) + data_ibegin,
                temp_j = (data_dim == 1) ? -1
                       : data_j_index(n) + data_jbegin;
            i = (data_dim == 1) ? (temp_i - 1) % ni
                                : (temp_i - 1) ;
            j = (data_dim == 1) ? (temp_i - 1) / ni
                                : (temp_j - 1) ;

            if ((l >=lbegin && l<= lend) &&
                (i >= 0 && i < ni) &&
                (j >= 0 && j < nj) && mask(i,j,l))
               indexCount++ ;
         }
      }
      
      storeIndex[0]  = new CArray<int,1>(indexCount) ;
      out_i_index[0] = new CArray<int,1>(indexCount) ;
      out_j_index[0] = new CArray<int,1>(indexCount) ;
      out_l_index[0] = new CArray<int,1>(indexCount) ;
      
      storeIndex_client.resize(indexCount) ;
      out_i_client.resize(indexCount) ;
      out_j_client.resize(indexCount) ;
      out_l_client.resize(indexCount) ;
      
      
      for(int count = 0, indexCount = 0,  l = 0; l < size; l++)
      {
         for(int n = 0, i = 0, j = 0; n < data_n_index; n++, count++)
         {
            int temp_i = data_i_index(n) + data_ibegin,
                temp_j = (data_dim == 1) ? -1
                       : data_j_index(n) + data_jbegin;
            i = (data_dim == 1) ? (temp_i - 1) % ni
                                : (temp_i - 1) ;
            j = (data_dim == 1) ? (temp_i - 1) / ni
                                : (temp_j - 1) ;

            if ((l >= lbegin && l <= lend) &&
                (i >= 0 && i < ni) &&
                (j >= 0 && j < nj) && mask(i,j,l))
            {
               (*storeIndex[0])(indexCount) = count ;
               (*out_l_index[0])(indexCount) = l ;
               (*out_i_index[0])(indexCount) = i ;
               (*out_j_index[0])(indexCount) = j ;
               
               storeIndex_client(indexCount) = count ;
               out_i_client(indexCount)=i+domain->ibegin_client-1 ;
               out_j_client(indexCount)=j+domain->jbegin_client-1 ;
               out_l_client(indexCount)=l-lbegin ;
               indexCount++ ;
            }
         }
      }
      sendIndex() ;


   }

   //----------------------------------------------------------------

   CGrid* CGrid::createGrid(CDomain* domain)
   {
      StdString new_id = StdString("__") + domain->getId() + StdString("__") ;
      CGrid* grid = CGridGroup::get("grid_definition")->createChild(new_id) ;
      grid->domain_ref.setValue(domain->getId());
      return (grid);
   }

   CGrid* CGrid::createGrid(CDomain* domain, CAxis* axis)
   {
      StdString new_id = StdString("__") + domain->getId() +
                         StdString("_") + axis->getId() + StdString("__") ;
      CGrid* grid = CGridGroup::get("grid_definition")->createChild(new_id) ;
      grid->domain_ref.setValue(domain->getId());
      grid->axis_ref.setValue(axis->getId());
      return (grid);
   }

   //----------------------------------------------------------------

   void CGrid::outputField(int rank, const CArray<double, 1>& stored,  CArray<double, 3>& field) 
   {
      CArray<int,1>& out_i=*out_i_fromClient[rank] ;
      CArray<int,1>& out_j=*out_j_fromClient[rank] ;
      CArray<int,1>& out_l=*out_l_fromClient[rank] ;
      
      for(StdSize n = 0; n < stored.numElements(); n++)
         field(out_i(n), out_j(n), out_l(n)) = stored(n) ;
   }

   void CGrid::outputField(int rank, const CArray<double, 1>& stored,  CArray<double, 2>& field) 
   {
      CArray<int,1>& out_i=*out_i_fromClient[rank] ;
      CArray<int,1>& out_j=*out_j_fromClient[rank] ;
      
      for(StdSize n = 0; n < stored.numElements(); n++)
         field(out_i(n), out_j(n)) = stored(n) ;   }

   //---------------------------------------------------------------

   void CGrid::outputField(int rank,const CArray<double, 1>& stored,  CArray<double, 1>& field)
   {
      CArray<int,1>& out_i=*out_i_fromClient[rank] ;
 
      for(StdSize n = 0; n < stored.numElements(); n++)
         field(out_i(n)) = stored(n) ;
   }

   //----------------------------------------------------------------
  

   void CGrid::storeField_arr
      (const double * const data, CArray<double, 1>& stored) const
   {
      const StdSize size = storeIndex_client.numElements() ;

      stored.resize(size) ;
      for(StdSize i = 0; i < size; i++) stored(i) = data[storeIndex_client(i)] ;
   }
   
   //---------------------------------------------------------------

  void CGrid::sendIndex(void)
  {
    CContext* context = CContext::getCurrent() ;
    CContextClient* client=context->client ;
    
    CEventClient event(getType(),EVENT_ID_INDEX) ;
    int rank ;
    list<shared_ptr<CMessage> > list_msg ;
    list< CArray<int,1>* > list_out_i,list_out_j,list_out_l ;
     
    for(int ns=0;ns<domain->connectedServer.size();ns++)
    {
       rank=domain->connectedServer[ns] ;
       int ib=domain->ib_srv[ns] ;
       int ie=domain->ie_srv[ns] ;
       int jb=domain->jb_srv[ns] ;
       int je=domain->je_srv[ns] ;
       
       int i,j ;
       int nb=0 ;
       for(int k=0;k<storeIndex_client.numElements();k++)
       {
         i=out_i_client(k) ;
         j=out_j_client(k) ;
         if (i>=ib-1 && i<=ie-1 && j>=jb-1 && j<=je-1) nb++ ; 
       }
       
       CArray<int,1> storeIndex(nb) ;
       CArray<int,1> out_i(nb) ;
       CArray<int,1> out_j(nb) ;
       CArray<int,1> out_l(nb) ;
 
       
       nb=0 ;
       for(int k=0;k<storeIndex_client.numElements();k++)
       {
         i=out_i_client(k) ;
         j=out_j_client(k) ;
         if (i>=ib-1 && i<=ie-1 && j>=jb-1 && j<=je-1) 
         {
            storeIndex(nb)=k ;
            out_i(nb)=out_i_client(k) ;
            out_j(nb)=out_j_client(k) ;
            out_l(nb)=out_l_client(k) ;
            nb++ ;
         }
       }
       
       storeIndex_toSrv.insert( pair<int,CArray<int,1>* >(rank,new CArray<int,1>(storeIndex) )) ;
       nbSenders.insert(pair<int,int>(rank,domain->nbSenders[ns])) ;
       list_msg.push_back(shared_ptr<CMessage>(new CMessage)) ;
       list_out_i.push_back(new CArray<int,1>(out_i)) ;
       list_out_j.push_back(new CArray<int,1>(out_j)) ;
       list_out_l.push_back(new CArray<int,1>(out_l)) ;

       *list_msg.back()<<getId()<<*list_out_i.back()<<*list_out_j.back()<<*list_out_l.back() ;
       event.push(rank,domain->nbSenders[ns],*list_msg.back()) ;
    }
    client->sendEvent(event) ;

    for(list<CArray<int,1>* >::iterator it=list_out_i.begin();it!=list_out_i.end();it++) delete *it ;
    for(list<CArray<int,1>* >::iterator it=list_out_j.begin();it!=list_out_j.end();it++) delete *it ;
    for(list<CArray<int,1>* >::iterator it=list_out_l.begin();it!=list_out_l.end();it++) delete *it ;
    
  }
  
  void CGrid::recvIndex(CEventServer& event)
  {
    list<CEventServer::SSubEvent>::iterator it ;
    for (it=event.subEvents.begin();it!=event.subEvents.end();++it)
    {
      int rank=it->rank;
      CBufferIn* buffer=it->buffer;
      string domainId ;
      *buffer>>domainId ;
      get(domainId)->recvIndex(rank,*buffer) ;
    }
  }
  
  void CGrid::recvIndex(int rank, CBufferIn& buffer)
  {
    CArray<int,1> out_i ;
    CArray<int,1> out_j ;
    CArray<int,1> out_l ;
    
    buffer>>out_i>>out_j>>out_l ;
    
    out_i -= domain->zoom_ibegin_srv-1 ;
    out_j -= domain->zoom_jbegin_srv-1 ;
    
    out_i_fromClient.insert(pair< int,CArray<int,1>* >(rank,new CArray<int,1>(out_i) )) ;
    out_j_fromClient.insert(pair< int,CArray<int,1>* >(rank,new CArray<int,1>(out_j) )) ;
    out_l_fromClient.insert(pair< int,CArray<int,1>* >(rank,new CArray<int,1>(out_l) )) ;
  }

  bool CGrid::dispatchEvent(CEventServer& event)
  {
     
    if (SuperClass::dispatchEvent(event)) return true ;
    else
    {
      switch(event.type)
      {
        case EVENT_ID_INDEX :
          recvIndex(event) ;
          return true ;
          break ;
 
        default :
          ERROR("bool CDomain::dispatchEvent(CEventServer& event)",
                <<"Unknown Event") ;
          return false ;
      }
    }
  }

   void CGrid::inputFieldServer(const std::deque< CArray<double, 1>* > storedClient, CArray<double, 1>&  storedServer) const
   {
      if ((this->storeIndex.size()-1 ) != storedClient.size())
         ERROR("void CGrid::inputFieldServer(const std::deque< CArray<double, 1>* > storedClient, CArray<double, 1>&  storedServer) const",
                << "[ Expected received field = " << (this->storeIndex.size()-1) << ", "
                << "[ received fiedl = "    << storedClient.size() << "] "
                << "Data from clients are missing!") ;
      storedServer.resize(storeIndex[0]->numElements());
         
      for (StdSize i = 0, n = 0; i < storedClient.size(); i++)
         for (StdSize j = 0; j < storedClient[i]->numElements(); j++)
            storedServer(n++) = (*storedClient[i])(j);
   }

   void CGrid::outputFieldToServer(CArray<double,1>& fieldIn, int rank, CArray<double,1>& fieldOut)
   {
     CArray<int,1>& index = *storeIndex_toSrv[rank] ;
     int nb=index.numElements() ;
     fieldOut.resize(nb) ;
     
     for(int k=0;k<nb;k++) fieldOut(k)=fieldIn(index(k)) ;
    }
   ///---------------------------------------------------------------

} // namespace xios
