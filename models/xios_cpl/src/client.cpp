#include "xmlioserver_spl.hpp"
#include "cxios.hpp"
#include "client.hpp"
#include <boost/functional/hash.hpp>
#include "type.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "oasis_cinterface.hpp"
#include "mpi.hpp"
#include "timer.hpp"
#include "buffer_client.hpp"

namespace xios
{                      

    MPI_Comm CClient::intraComm ;
    MPI_Comm CClient::interComm ;
    int CClient::serverLeader ;
    bool CClient::is_MPI_Initialized ;
    
    
    void CClient::initialize(const string& codeId,MPI_Comm& localComm,MPI_Comm& returnComm)
    {
      int initialized ;
      MPI_Initialized(&initialized) ;
      if (initialized) is_MPI_Initialized=true ;
      else is_MPI_Initialized=false ;
      
// don't use OASIS
      if (!CXios::usingOasis)
      {
// localComm doesn't given
        if (localComm == MPI_COMM_NULL)
        {
          if (!is_MPI_Initialized) 
          {
            int argc=0;
            char** argv=NULL;
            MPI_Init(&argc,&argv) ;
          }
          CTimer::get("XIOS").resume() ;
          CTimer::get("XIOS init").resume() ;
          boost::hash<string> hashString ;    
    
          unsigned long hashClient=hashString(codeId) ;
          unsigned long hashServer=hashString(CXios::xiosCodeId) ;
          unsigned long* hashAll ;
          int rank ;
          int size ;
          int myColor ;
          int i,c ;
          MPI_Comm newComm ;
      
          MPI_Comm_size(CXios::globalComm,&size) ;
          MPI_Comm_rank(CXios::globalComm,&rank);
          hashAll=new unsigned long[size] ;
     
          MPI_Allgather(&hashClient,1,MPI_LONG,hashAll,1,MPI_LONG,CXios::globalComm) ;

          map<unsigned long, int> colors ;
          map<unsigned long, int> leaders ;
      
          for(i=0,c=0;i<size;i++)
          {
            if (colors.find(hashAll[i])==colors.end())
            {
              colors[hashAll[i]] =c ;
              leaders[hashAll[i]]=i ;
              c++ ;
            }
          }
     
          myColor=colors[hashClient] ;
      
          MPI_Comm_split(CXios::globalComm,myColor,rank,&intraComm) ;

          if (CXios::usingServer)
          {     
            int clientLeader=leaders[hashClient] ;
            serverLeader=leaders[hashServer] ;
            MPI_Intercomm_create(intraComm,0,CXios::globalComm,serverLeader,0,&interComm) ;
          }
          else
          {
            MPI_Comm_dup(intraComm,&interComm) ;
          }
          delete [] hashAll ;
        }
        // localComm argument is given
        else 
        {
          if (CXios::usingServer)
          {          
            //ERROR("void CClient::initialize(const string& codeId,MPI_Comm& localComm,MPI_Comm& returnComm)", << " giving a local communictor is not compatible with using server mode") ;
          }
          else
          {
            MPI_Comm_dup(localComm,&intraComm) ;
            MPI_Comm_dup(intraComm,&interComm) ;
          }
        }
      }
      // using OASIS
      else
      {
        // localComm doesn't given
        if (localComm == MPI_COMM_NULL)
        {
          if (!is_MPI_Initialized) oasis_init(codeId) ;
          oasis_get_localcomm(intraComm) ;
        }
        else MPI_Comm_dup(localComm,&intraComm) ;
        CTimer::get("XIOS").resume() ;
        CTimer::get("XIOS init").resume() ;
  
        if (CXios::usingServer) 
        {
          MPI_Status status ;
          int rank ;
          MPI_Comm_rank(intraComm,&rank) ;

          oasis_get_intercomm(interComm,CXios::xiosCodeId) ;
          if (rank==0) MPI_Recv(&serverLeader,1, MPI_INT, 0, 0, interComm, &status) ;
          MPI_Bcast(&serverLeader,1,MPI_INT,0,intraComm) ;
           
        }
        else MPI_Comm_dup(intraComm,&interComm) ;
      }
          
      MPI_Comm_dup(intraComm,&returnComm) ;
    }
    
    
    void CClient::registerContext(const string& id,MPI_Comm contextComm)
    {
      CContext::setCurrent(id) ;
      CContext* context=CContext::create(id) ;
        
      if (!CXios::isServer)
      {
        int size,rank,globalRank ;
        size_t message_size ;
        int leaderRank ;
        MPI_Comm contextInterComm ;
      
        MPI_Comm_size(contextComm,&size) ;
        MPI_Comm_rank(contextComm,&rank) ;
        MPI_Comm_rank(CXios::globalComm,&globalRank) ;
        if (rank!=0) globalRank=0 ;
      
    
        CMessage msg ;
        msg<<id<<size<<globalRank ;

        int messageSize=msg.size() ;
        void * buff = new char[messageSize] ;
        CBufferOut buffer(buff,messageSize) ;
        buffer<<msg ;
      
        MPI_Send(buff,buffer.count(),MPI_CHAR,serverLeader,1,CXios::globalComm) ;
        delete [] buff ;
      
        MPI_Intercomm_create(contextComm,0,CXios::globalComm,serverLeader,10+globalRank,&contextInterComm) ;
        info(10)<<"Register new Context : "<<id<<endl ;
 
        MPI_Comm inter ;
        MPI_Intercomm_merge(contextInterComm,0,&inter) ;
        MPI_Barrier(inter) ;

        context->initClient(contextComm,contextInterComm) ;
      }
      else
      {
        MPI_Comm contextInterComm ;
        MPI_Comm_dup(contextComm,&contextInterComm) ;
        context->initClient(contextComm,contextInterComm) ;
        context->initServer(contextComm,contextInterComm) ;
      }
    }
    
    void CClient::finalize(void)
    {
      int rank ;
      int msg=0 ;
      if (!CXios::isServer)
      {
        MPI_Comm_rank(intraComm,&rank) ;  
        if (rank==0) 
        {
          MPI_Send(&msg,1,MPI_INT,0,0,interComm) ;
        }
      }
      
     CTimer::get("XIOS finalize").suspend() ;
     CTimer::get("XIOS").suspend() ;

      if (!is_MPI_Initialized)
      {
        if (CXios::usingOasis) oasis_finalize();
        else MPI_Finalize() ;
      }
      info(20) << "Client side context is finalized"<<endl ;
      report(0) <<" Performance report : total time spent for XIOS : "<< CTimer::get("XIOS").getCumulatedTime()<<" s"<<endl ; 
      report(0)<< " Performance report : time spent for waiting free buffer : "<< CTimer::get("Blocking time").getCumulatedTime()<<" s"<<endl ;
      report(0)<< " Performance report : Ratio : "<< CTimer::get("Blocking time").getCumulatedTime()/CTimer::get("XIOS").getCumulatedTime()*100.<<" %"<<endl ;
      report(0)<< " Performance report : This ratio must be close to zero. Otherwise it may be usefull to increase buffer size or numbers of server"<<endl ;
      report(0)<< " Memory report : Current buffer_size : "<<CXios::bufferSize<<endl ;
      report(0)<< " Memory report : Minimum buffer size required : "<<maxRequestSize*2<<endl ;
      report(0)<< " Memory report : increasing it by a factor will increase performance, depending of the volume of data wrote in file at each time step of the file"<<endl ;
   }
}
