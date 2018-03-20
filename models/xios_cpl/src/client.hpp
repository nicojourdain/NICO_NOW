#ifndef __CLIENT_HPP__
#define __CLIENT_HPP__

#include "xmlioserver_spl.hpp"
#include "mpi.hpp"

namespace xios
{                      
    class CClient
    {
       public:
       
       static void initialize(const string& codeId,MPI_Comm& localComm,MPI_Comm& returnComm) ;
       static void finalize(void) ;
       static void registerContext(const string& id,MPI_Comm contextComm) ;

       static MPI_Comm intraComm ;
       static MPI_Comm interComm ;
       static int serverLeader;
       static bool is_MPI_Initialized ;
    } ;
}

#endif
