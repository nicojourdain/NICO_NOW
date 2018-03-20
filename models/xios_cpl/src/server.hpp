#ifndef __SERVER_HPP__
#define __SERVER_HPP__

#include "xmlioserver_spl.hpp"
#include "context.hpp"
#include "mpi.hpp"

namespace xios
{
    class CServer
    {
       public:
       
       static void initialize(void) ;
       static void finalize(void) ;
       static void eventLoop(void) ;
       static void contextEventLoop(void) ;
       static void listenContext(void) ;
       static void listenFinalize(void) ;
       static void recvContextMessage(void* buff,int count) ;
       static void listenRootContext(void) ;
       static void listenRootFinalize(void) ;
       static void registerContext(void* buff,int count, int leaderRank=0) ;
       
       static MPI_Comm intraComm ;
       static list<MPI_Comm> interComm ;

       struct contextMessage
       {
         int nbRecv ;
         int leaderRank ;
       } ;
       
      static bool isRoot ;
      static int rank ;
      static map<string,CContext*> contextList ;
      static bool finished ;
      static bool is_MPI_Initialized ;
    } ;
}

#endif
