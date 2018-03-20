#include "xmlioserver_spl.hpp"
#include "event_client.hpp"
#include "buffer_out.hpp"
#include "message.hpp"
#include "type.hpp"
#include "mpi.hpp"

namespace xios
{
   CEventClient::CEventClient(int classId_,int typeId_)
   {
     classId=classId_ ;
     typeId=typeId_ ;
   }
   
   void CEventClient::push(int rank,int nbSender,CMessage & msg)
   {
     nbSenders.push_back(nbSender) ;
     ranks.push_back(rank) ;
     messages.push_back(&msg) ;
   }

   bool CEventClient::isEmpty(void)
   {
     return ranks.empty() ;
   }

   list<int> CEventClient::getRanks(void)
   {
     return ranks ;
   }
   
   list<int> CEventClient::getSizes(void)
   {
     list<CMessage*>::iterator it ;
     list<int> sizes ;
     size_t headerSize=sizeof(int)+sizeof(classId)+sizeof(typeId) ;
     
     for(it=messages.begin();it!=messages.end();++it) sizes.push_back((*it)->size()+headerSize) ;
     return sizes ;
   }
   
   void CEventClient::send(list<CBufferOut*>& buffers)
   {
     list<CBufferOut*>::iterator itBuff ;
     list<CMessage*>::iterator itMsg ;
     list<int>::iterator itSenders ;
     
     for(itBuff=buffers.begin(),itMsg=messages.begin(),itSenders=nbSenders.begin();itBuff!=buffers.end();++itBuff,++itMsg,++itSenders)
     {
       **itBuff<<*itSenders<<classId<<typeId<<**itMsg ;
     }
   }   
/*
   CEventClient::CEventClient(CContextClient& client_,int nbSender_,list<int>& serverList_)
   {
     client=&client_ ;
     nbSender=nbSender_ ;
     serverList=serverList_ ;
     
     client->registerEvent(*this) ;
   }

   list<CBufferOut*> CEventClient::newEvent(int classId, int type, list<int> sizes)
   {
     list<int>::iterator it ;
     list<CBufferOut*>::iterator itBuff;
     
     
     CMessage msg;
     
     msg<<nbSender<<classId<<type ;    
     
     for(it=sizes.begin();it!=sizes.end();it++) *it+=msg.size() ;
     list<CBufferOut*> buffers=client->newEvent(*this,sizes) ;

     for(itBuff=buffers.begin();itBuff!=buffers.end();itBuff++) *(*itBuff)<<msg ;  
     
     return buffers ;
   
   }
          
   void CEventClient::send(void)
   {
     client->sendEvent(*this) ;
   }

*/   
   
}
