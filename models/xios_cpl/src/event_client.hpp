#ifndef __EVENT_CLIENT_HPP__
#define __EVENT_CLIENT_HPP__

#include "xmlioserver_spl.hpp"
#include "buffer_out.hpp"
#include "message.hpp"

namespace xios
{
 
  class CEventClient
  {
    public:
    
//    CEventClient(CContextClient& client,int nbSender,list<int>& serverList);  
    CEventClient(int classId, int typeId);
    void push(int rank,int nbSender, CMessage& msg) ;  

//    list<CBufferOut*> newEvent(int classId, int type, list<int> sizes) ;
    list<int> getRanks(void) ;
    list<int> getSizes(void) ;
    void send(list<CBufferOut*>&) ;   
    bool isEmpty(void) ;
    list<int> ranks ;
    list<int> nbSenders ;
    list<CMessage*> messages ;
//    CContextClient* client ;
    int classId ;
    int typeId ;
  } ;

}

#endif
