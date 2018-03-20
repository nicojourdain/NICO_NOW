#include "xmlioserver_spl.hpp"
#include "context_client.hpp"
#include "context_server.hpp"
#include "event_client.hpp"
#include "buffer_out.hpp"
#include "buffer_client.hpp"
#include "type.hpp"
#include "message.hpp"
#include "event_client.hpp"
#include "context.hpp"
#include "mpi.hpp"
#include "timer.hpp"
#include "cxios.hpp"

namespace xios
{


    CContextClient::CContextClient(CContext* parent,MPI_Comm intraComm_, MPI_Comm interComm_)
    {
      context=parent ;
      intraComm=intraComm_ ;
      interComm=interComm_ ;
      MPI_Comm_rank(intraComm,&clientRank) ;
      MPI_Comm_size(intraComm,&clientSize) ;
      
      int flag ;
      MPI_Comm_test_inter(interComm,&flag) ;
      if (flag) MPI_Comm_remote_size(interComm,&serverSize);
      else  MPI_Comm_size(interComm,&serverSize) ;
     
      timeLine=0 ;

    }


    void CContextClient::sendEvent(CEventClient& event)
    {
      list<int>::iterator itServer ;
      list<int> ranks ;
      list<int> sizes ;  
      list<int>::iterator itSize ;
      
      ranks=event.getRanks() ;
      if (! event.isEmpty())
      {
        sizes=event.getSizes() ;
        CMessage msg ;

        msg<<*(sizes.begin())<<timeLine ;
        for(list<int>::iterator it=sizes.begin();it!=sizes.end();it++) *it+=msg.size() ;
        list<CBufferOut*> buffList=getBuffers(ranks,sizes) ;
      
        list<CBufferOut*>::iterator it ;      
        for(it=buffList.begin(),itSize=sizes.begin();it!=buffList.end();++it,++itSize)
        {
          **it<<*itSize<<timeLine ;
        }
        event.send(buffList) ;
        checkBuffers(ranks) ;
      }

      if (context->hasServer) waitEvent(ranks) ;
      timeLine++ ;
    }
      
    void CContextClient::waitEvent(list<int>& ranks)
    {
      context->server->setPendingEvent() ;
      while(checkBuffers(ranks))
      {
        context->server->listen() ;
        context->server->checkPendingRequest() ;
      }

      while(context->server->hasPendingEvent())
      {
       context->server->eventLoop() ;
      }
      
    }

    list<CBufferOut*> CContextClient::getBuffers(list<int>& serverList, list<int>& sizeList)
    {
      list<int>::iterator itServer,itSize ;
      list<CClientBuffer*> bufferList ; 
      map<int,CClientBuffer*>::iterator it ; 
      list<CClientBuffer*>::iterator itBuffer ; 
      list<CBufferOut*>  retBuffer ;
      bool free ;

      for(itServer=serverList.begin();itServer!=serverList.end();itServer++) 
      {
        it=buffers.find(*itServer) ;
        if (it==buffers.end()) 
        {
          newBuffer(*itServer) ;
          it=buffers.find(*itServer) ;
        }         
        bufferList.push_back(it->second) ;
      }
      free=false ;

      CTimer::get("Blocking time").resume();
      while(!free)
      {
        free=true ;
        for(itBuffer=bufferList.begin(),itSize=sizeList.begin(); itBuffer!=bufferList.end();itBuffer++,itSize++)
        {
          (*itBuffer)->checkBuffer() ;
         free&=(*itBuffer)->isBufferFree(*itSize) ;
        }
      }
      CTimer::get("Blocking time").suspend();

      for(itBuffer=bufferList.begin(),itSize=sizeList.begin(); itBuffer!=bufferList.end();itBuffer++,itSize++)
      {
        retBuffer.push_back((*itBuffer)->getBuffer(*itSize)) ;
      }
      return retBuffer ;             
   
   }
     
   void CContextClient::newBuffer(int rank)
   {
      buffers[rank]=new CClientBuffer(interComm,rank) ;
   } 

   bool CContextClient::checkBuffers(void)
   {
      map<int,CClientBuffer*>::iterator itBuff ;
      bool pending=false ;
      for(itBuff=buffers.begin();itBuff!=buffers.end();itBuff++) pending|=itBuff->second->checkBuffer() ;
      return pending ;
   } 

   void CContextClient::releaseBuffers(void)
   {
      map<int,CClientBuffer*>::iterator itBuff ;
      for(itBuff=buffers.begin();itBuff!=buffers.end();itBuff++) delete itBuff->second ;
   } 

   bool CContextClient::checkBuffers(list<int>& ranks)
   {
      list<int>::iterator it ;
      bool pending=false ;
      for(it=ranks.begin();it!=ranks.end();it++) pending|=buffers[*it]->checkBuffer() ;
      return pending ;
   } 

   int CContextClient::getServerLeader(void)
   {
     int clientByServer=clientSize/serverSize ;
     int remain=clientSize%serverSize ;
     
     if (clientRank<(clientByServer+1)*remain)
     {
       return clientRank/(clientByServer+1) ;
     }
     else
     {
       int rank=clientRank-(clientByServer+1)*remain ;
       int nbServer=serverSize-remain ;
       return remain+rank/clientByServer ;
     }
   }      

   bool CContextClient::isServerLeader(void)
   {
     int clientByServer=clientSize/serverSize ;
     int remain=clientSize%serverSize ;
     
     if (clientRank<(clientByServer+1)*remain)
     {
       if (clientRank%(clientByServer+1)==0) return true ;
       else return false ;
     }
     else
     {
       int rank=clientRank-(clientByServer+1)*remain ;
       int nbServer=serverSize-remain ;
       if  (rank%clientByServer==0) return true ;
       else return false ;
     } 
   }
     
   void CContextClient::finalize(void)
   {
      
     map<int,CClientBuffer*>::iterator itBuff ;
     bool stop=true ;

     CEventClient event(CContext::GetType(),CContext::EVENT_ID_CONTEXT_FINALIZE) ;   
     if (isServerLeader())
     {
       CMessage msg ;
       event.push(getServerLeader(),1,msg) ;
       sendEvent(event) ;
     }
     else sendEvent(event) ;
 
     CTimer::get("Blocking time").resume();
     while(stop)
     {
       checkBuffers() ;
       stop=false ;
       for(itBuff=buffers.begin();itBuff!=buffers.end();itBuff++) stop|=itBuff->second->hasPendingRequest() ;
     }
     CTimer::get("Blocking time").suspend();
     report(0)<< " Memory report : Context <"<<context->getId()<<"> : client side : total memory used for buffer "<<buffers.size()*CXios::bufferSize<<" bytes"<<endl ;
     
     releaseBuffers() ;
   }
}      
