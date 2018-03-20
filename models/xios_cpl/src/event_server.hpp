#ifndef __EVENT_SERVER_HPP__
#define __EVENT_SERVER_HPP__

#include "xmlioserver_spl.hpp"
#include "buffer_in.hpp"
#include "buffer_server.hpp"

namespace xios
{

  class CEventServer
  {
    public:
    
    int classId ;
    int type ;
    int nbSender ;


    void push(int rank,CServerBuffer* serverBuffer ,char* startBuffer,int size) ;

    struct SSubEvent
    {
      int rank ;
      CServerBuffer* serverBuffer ;
      CBufferIn*  buffer ;
      int size ;
    } ;
    
    list<SSubEvent> subEvents ;    
    
    bool isFull(void) ;
    ~CEventServer() ; 
  } ;

}

#endif
