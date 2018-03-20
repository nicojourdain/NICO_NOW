#ifndef __CONTEXT_CLIENT_HPP__
#define __CONTEXT_CLIENT_HPP__

#include "xmlioserver_spl.hpp"
#include "buffer_out.hpp"
#include "buffer_client.hpp"
#include "event_client.hpp"
#include "mpi.hpp"

namespace xios
{
  class CContext ;
  
  class CContextClient
  {
  
    public:
    CContextClient(CContext* parent,MPI_Comm intraComm, MPI_Comm interComm) ;
//    void registerEvent(CEventClient& event) ;

//    list<CBufferOut*> newEvent(CEventClient& event,list<int>& sizes) ;  
    void sendEvent(CEventClient& event) ;

    list<CBufferOut*> getBuffers(list<int>& serverlist, list<int>& sizeList) ;
    void newBuffer(int rank) ;
    size_t timeLine ;
    int clientRank ;
    int clientSize ;
    int serverSize ;
//    set<int> connectedServer ;
    MPI_Comm interComm ;
    MPI_Comm intraComm ;
    map<int,CClientBuffer*> buffers ;
    bool checkBuffers(list<int>& ranks) ;
    bool checkBuffers(void);
    void releaseBuffers(void);
    void closeContext(void) ;
    bool isServerLeader(void) ;
    int getServerLeader(void) ;
    void finalize(void) ;
    void waitEvent(list<int>& ranks) ;

    CContext* context ;
//    bool locked ;
    
  } ;




}



#endif
