#ifndef __CONTEXT_SERVER_HPP__
#define __CONTEXT_SERVER_HPP__
#include "xmlioserver_spl.hpp"
#include "event_server.hpp"
#include "buffer_server.hpp"
#include "mpi.hpp"

namespace xios
{
  class CContext ;
  
  class CContextServer
  {
    public:
    
    CContextServer(CContext* parent,MPI_Comm intraComm,MPI_Comm interComm) ;
    bool eventLoop(void) ;
    void listen(void) ;
    void checkPendingRequest(void) ;
    void processRequest(int rank, char* buff,int count) ;
    void processEvents(void) ;
    void dispatchEvent(CEventServer& event) ;
    void setPendingEvent(void) ;
    bool hasPendingEvent(void) ;
    
    MPI_Comm intraComm ;
    int intraCommSize ;
    int intraCommRank ;
    
    MPI_Comm interComm ;
    int commSize ;
  
    map<int,CServerBuffer*> buffers ;
    map<int,MPI_Request> pendingRequest ;
    map<int,char*> bufferRequest ;
    
    map<size_t,CEventServer*> events ;
    size_t currentTimeLine ;
    CContext* context ;
    
    bool finished ;
    bool pendingEvent ;
    ~CContextServer() ;    
  } ;

}

#endif
