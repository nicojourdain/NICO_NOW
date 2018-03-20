#include "context_server.hpp"
#include "buffer_in.hpp"
#include "type.hpp"
#include "context.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "attribute_template.hpp"
#include "domain.hpp"
#include "field.hpp"
#include "file.hpp"
#include "grid.hpp"
#include "mpi.hpp"
#include "tracer.hpp"
#include "timer.hpp"
#include "cxios.hpp"



namespace xios
{

  CContextServer::CContextServer(CContext* parent,MPI_Comm intraComm_,MPI_Comm interComm_)
  {
    context=parent ;
    intraComm=intraComm_ ;
    MPI_Comm_size(intraComm,&intraCommSize) ;
    MPI_Comm_rank(intraComm,&intraCommRank) ;
    interComm=interComm_ ;
    int flag ;
    MPI_Comm_test_inter(interComm,&flag) ;
    if (flag) MPI_Comm_remote_size(interComm,&commSize);
    else  MPI_Comm_size(interComm,&commSize) ;
    currentTimeLine=0 ;
    finished=false ;
  }
  void CContextServer::setPendingEvent(void)
  {
    pendingEvent=true ;
  }
  
  bool CContextServer::hasPendingEvent(void)
  {
    return pendingEvent ;
  }
  
  bool CContextServer::eventLoop(void)
  {
    listen() ;
    checkPendingRequest() ;
    processEvents() ;
    return finished ;
  }

  void CContextServer::listen(void)
  {
    int rank;
    int flag ;
    int count ;
    char * addr ;
    MPI_Status status; 
    map<int,CServerBuffer*>::iterator it;
    
    for(rank=0;rank<commSize;rank++)
    {
      if (pendingRequest.find(rank)==pendingRequest.end())
      {
        traceOff() ;
        MPI_Iprobe(rank,20,interComm,&flag,&status);     
        traceOn() ;
        if (flag==true)
        {
          it=buffers.find(rank) ;
          if (it==buffers.end()) 
            it=(buffers.insert(pair<int,CServerBuffer*>(rank,new CServerBuffer))).first ;
          MPI_Get_count(&status,MPI_CHAR,&count) ;
          if (it->second->isBufferFree(count))
          {
            addr=(char*)it->second->getBuffer(count) ;
            MPI_Irecv(addr,count,MPI_CHAR,rank,20,interComm,&pendingRequest[rank]) ;
            bufferRequest[rank]=addr ;
          }
        }
      }
    }
  }
  
  void CContextServer::checkPendingRequest(void)
  {
    map<int,MPI_Request>::iterator it;
    list<int> recvRequest ;
    list<int>::iterator itRecv;
    int rank ;
    int flag ;
    int count ;
    MPI_Status status ;
    
    for(it=pendingRequest.begin();it!=pendingRequest.end();it++)
    {
      rank=it->first ;
      traceOff() ;
      MPI_Test(& it->second, &flag, &status) ;
      traceOn() ;
      if (flag==true)
      {
        recvRequest.push_back(rank) ;
        MPI_Get_count(&status,MPI_CHAR,&count) ;
        processRequest(rank,bufferRequest[rank],count) ;
      }
    }
    
    for(itRecv=recvRequest.begin();itRecv!=recvRequest.end();itRecv++) 
    {
      pendingRequest.erase(*itRecv) ;
      bufferRequest.erase(*itRecv) ;
    }
  }
  
  void CContextServer::processRequest(int rank, char* buff,int count)
  {
    
    CBufferIn buffer(buff,count) ;
    char* startBuffer,endBuffer ;
    int size, offset ;
    size_t timeLine ;
    map<size_t,CEventServer*>::iterator it ;
       
    while(count>0)
    {
      char* startBuffer=(char*)buffer.ptr() ;
      CBufferIn newBuffer(startBuffer,buffer.remain()) ;
      newBuffer>>size>>timeLine ;

      it=events.find(timeLine) ;
      if (it==events.end()) it=events.insert(pair<int,CEventServer*>(timeLine,new CEventServer)).first ;
      it->second->push(rank,buffers[rank],startBuffer,size) ;

      buffer.advance(size) ;
      count=buffer.remain() ;           
    } 
  
  }
    
  void CContextServer::processEvents(void)
  {
    map<size_t,CEventServer*>::iterator it ;
    CEventServer* event ;
    
    it=events.find(currentTimeLine) ;
    if (it!=events.end()) 
    {
      event=it->second ;
      if (event->isFull())
      {
         CTimer::get("Process events").resume() ;
         dispatchEvent(*event) ;
         CTimer::get("Process events").suspend() ;
         pendingEvent=false ;
         delete event ;
         events.erase(it) ;
         currentTimeLine++ ;
       }
     }
   }
       
  CContextServer::~CContextServer()
  {
    map<int,CServerBuffer*>::iterator it ;
    for(it=buffers.begin();it!=buffers.end();++it) delete it->second ; 
  } 


  void CContextServer::dispatchEvent(CEventServer& event)
  {
    string contextName ;
    string buff ;
    int MsgSize ;
    int rank ;
    list<CEventServer::SSubEvent>::iterator it ;
    CContext::setCurrent(context->getId()) ;
        
    if (event.classId==CContext::GetType() && event.type==CContext::EVENT_ID_CONTEXT_FINALIZE)
    {
      info(20)<<"Server Side context <"<<context->getId()<<"> finalized"<<endl ;
      context->finalize() ;
      finished=true ;
      report(0)<< " Memory report : Context <"<<context->getId()<<"> : server side : total memory used for buffer "<<buffers.size()*CXios::bufferSize<<" bytes"<<endl ;
    }
    else if (event.classId==CContext::GetType()) CContext::dispatchEvent(event) ;
    else if (event.classId==CContextGroup::GetType()) CContextGroup::dispatchEvent(event) ;
    else if (event.classId==CDomain::GetType()) CDomain::dispatchEvent(event) ;
    else if (event.classId==CDomainGroup::GetType()) CDomainGroup::dispatchEvent(event) ;
    else if (event.classId==CAxis::GetType()) CAxis::dispatchEvent(event) ;
    else if (event.classId==CAxisGroup::GetType()) CAxisGroup::dispatchEvent(event) ;
    else if (event.classId==CGrid::GetType()) CGrid::dispatchEvent(event) ;
    else if (event.classId==CGridGroup::GetType()) CGridGroup::dispatchEvent(event) ;
    else if (event.classId==CField::GetType()) CField::dispatchEvent(event) ;
    else if (event.classId==CFieldGroup::GetType()) CFieldGroup::dispatchEvent(event) ;
    else if (event.classId==CFile::GetType()) CFile::dispatchEvent(event) ;
    else if (event.classId==CFileGroup::GetType()) CFileGroup::dispatchEvent(event) ;
    else
    {
      ERROR("void CContextServer::dispatchEvent(CEventServer& event)",<<" Bad event class Id"<<endl) ;
    }
  }
}
