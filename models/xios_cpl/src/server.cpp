#include "xmlioserver_spl.hpp"
#include "cxios.hpp"
#include "server.hpp"
#include "type.hpp"
#include "context.hpp"
#include "object_template.hpp"
#include "oasis_cinterface.hpp"
#include <boost/functional/hash.hpp>
#include <boost/algorithm/string.hpp>
#include "mpi.hpp"
#include "tracer.hpp"
#include "timer.hpp"

namespace xios
{                      
    MPI_Comm CServer::intraComm ;
    list<MPI_Comm> CServer::interComm ;
    bool CServer::isRoot ;
    int CServer::rank ;
    map<string,CContext*> CServer::contextList ;    
    bool CServer::finished=false ;
    bool CServer::is_MPI_Initialized ;
    
    void CServer::initialize(void)
    {
      int initialized ;
      MPI_Initialized(&initialized) ;
      if (initialized) is_MPI_Initialized=true ;
      else is_MPI_Initialized=false ;
            
      // Not using OASIS
      if (!CXios::usingOasis)
      {
        
        if (!is_MPI_Initialized) 
        {
          int argc=0;
          char** argv=NULL;
          MPI_Init(&argc,&argv) ;
        }
        CTimer::get("XIOS").resume() ;
 
        boost::hash<string> hashString ;    
      
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
     
        MPI_Allgather(&hashServer,1,MPI_LONG,hashAll,1,MPI_LONG,CXios::globalComm) ;

        map<unsigned long, int> colors ;
        map<unsigned long, int> leaders ;
        map<unsigned long, int>::iterator it ;
      
        for(i=0,c=0;i<size;i++)
        {
          if (colors.find(hashAll[i])==colors.end())
          {
            colors[hashAll[i]]=c ;
            leaders[hashAll[i]]=i ;
            c++ ;
          }
        }
     
        myColor=colors[hashServer] ;
        MPI_Comm_split(MPI_COMM_WORLD,myColor,rank,&intraComm) ;

        int serverLeader=leaders[hashServer] ;
        int clientLeader;
     
         serverLeader=leaders[hashServer] ;
         for(it=leaders.begin();it!=leaders.end();it++)
         {
           if (it->first!=hashServer)
           {
             clientLeader=it->second ;
           
             MPI_Intercomm_create(intraComm,0,CXios::globalComm,clientLeader,0,&newComm) ;
             interComm.push_back(newComm) ;
           }
         }

         delete [] hashAll ;
      }
      // using OASIS
      else
      {
        int rank ,size;
        if (!is_MPI_Initialized) oasis_init(CXios::xiosCodeId) ;
        CTimer::get("XIOS").resume() ;
        oasis_get_localcomm(intraComm) ;
        MPI_Comm_rank(intraComm,&rank) ;
        MPI_Comm_size(intraComm,&size) ;
        string codesId=CXios::getin<string>("oasis_codes_id") ;
         
        vector<string> splitted ;
        boost::split( splitted, codesId, boost::is_space(), boost::token_compress_on ) ;
        vector<string>::iterator it ;

        MPI_Comm newComm ;
        int globalRank ;
        MPI_Comm_rank(CXios::globalComm,&globalRank);
        
        for(it=splitted.begin();it!=splitted.end();it++)
        {
          oasis_get_intercomm(newComm,*it) ;
          if (rank==0) MPI_Send(&globalRank,1,MPI_INT,0,0,newComm) ;
          MPI_Comm_remote_size(newComm,&size);
          interComm.push_back(newComm) ;
        }
      oasis_enddef() ;
      }
      
      int rank ;
      MPI_Comm_rank(intraComm,&rank) ;
      if (rank==0) isRoot=true;
      else isRoot=false; 
      eventLoop() ;
      finalize() ;
    }
    
    void CServer::finalize(void)
    {
      CTimer::get("XIOS").suspend() ;
      if (!is_MPI_Initialized)
      { 
        if (CXios::usingOasis) oasis_finalize();
        else MPI_Finalize() ;
      }
      report(0)<<"Performance report : Time spent for XIOS : "<<CTimer::get("XIOS server").getCumulatedTime()<<endl  ;
      report(0)<<"Performance report : Time spent in processing events : "<<CTimer::get("Process events").getCumulatedTime()<<endl  ;
      report(0)<<"Performance report : Ratio : "<<CTimer::get("Process events").getCumulatedTime()/CTimer::get("XIOS server").getCumulatedTime()*100.<<"%"<<endl  ;
    }
    
     void CServer::eventLoop(void)
     {
       bool stop=false ;
       
       CTimer::get("XIOS server").resume() ;
       while(!stop)
       {
         if (isRoot)
         {
           listenContext();
           if (!finished) listenFinalize() ;
         }
         else
         {
           listenRootContext();
           if (!finished) listenRootFinalize() ;
         }
       
         contextEventLoop() ;
         if (finished && contextList.empty()) stop=true ;
       }
       CTimer::get("XIOS server").suspend() ;
     }
     
     void CServer::listenFinalize(void)
     {
        list<MPI_Comm>::iterator it;
        int msg ;
        int flag ;
        
        for(it=interComm.begin();it!=interComm.end();it++)
        {
           MPI_Status status ;
           traceOff() ;
           MPI_Iprobe(0,0,*it,&flag,&status) ;
           traceOn() ;
           if (flag==true)
           {
              MPI_Recv(&msg,1,MPI_INT,0,0,*it,&status) ;
              info(20)<<" CServer : Receive client finalize"<<endl ;
              interComm.erase(it) ;
              break ;
            }
         }
         
         if (interComm.empty())
         {
           int i,size ;
           MPI_Comm_size(intraComm,&size) ;
           MPI_Request* requests= new MPI_Request[size-1] ;
           MPI_Status* status= new MPI_Status[size-1] ;
          
           for(int i=1;i<size;i++) MPI_Isend(&msg,1,MPI_INT,i,4,intraComm,&requests[i-1]) ;
           MPI_Waitall(size-1,requests,status) ;

           finished=true ;
           delete [] requests ;
           delete [] status ;
         }
     }
      
     
     void CServer::listenRootFinalize()
     {
        int flag ;
        MPI_Status status ;
        int msg ;
        
        traceOff() ;
        MPI_Iprobe(0,4,intraComm, &flag, &status) ;
        traceOn() ;
        if (flag==true)
        {
           MPI_Recv(&msg,1,MPI_INT,0,4,intraComm,&status) ;
           finished=true ;
        }
      }
      
     void CServer::listenContext(void)
     {
       
       MPI_Status status ;
       int flag ;
       static void* buffer ;
       static MPI_Request request ;
       static bool recept=false ;
       int rank ;
       int count ;
       
       if (recept==false)
       {
         traceOff() ;
         MPI_Iprobe(MPI_ANY_SOURCE,1,CXios::globalComm, &flag, &status) ;
         traceOn() ;
         if (flag==true) 
         {
           rank=status.MPI_SOURCE ;
           MPI_Get_count(&status,MPI_CHAR,&count) ;
           buffer=new char[count] ;
           MPI_Irecv(buffer,count,MPI_CHAR,rank,1,CXios::globalComm,&request) ;
           recept=true ;   
         }
       }
       else
       {
         traceOff() ;
         MPI_Test(&request,&flag,&status) ;
         traceOn() ;
         if (flag==true)
         {
           rank=status.MPI_SOURCE ;
           MPI_Get_count(&status,MPI_CHAR,&count) ;
           recvContextMessage(buffer,count) ;
           delete [] buffer ;
           recept=false ;         
         }
       }
     }
     
     void CServer::recvContextMessage(void* buff,int count)
     {
 
       
       static map<string,contextMessage> recvContextId ;
       map<string,contextMessage>::iterator it ;
       
       CBufferIn buffer(buff,count) ;
       string id ;
       int clientLeader ;
       int nbMessage ;

       buffer>>id>>nbMessage>>clientLeader ;
                       
       it=recvContextId.find(id) ;
       if (it==recvContextId.end())
       {         
         contextMessage msg={0,0} ;
         pair<map<string,contextMessage>::iterator,bool> ret ;
         ret=recvContextId.insert(pair<string,contextMessage>(id,msg)) ;
         it=ret.first ;
       }  
       it->second.nbRecv+=1 ;
       it->second.leaderRank+=clientLeader ;
         
       if (it->second.nbRecv==nbMessage)
       { 
         int size ;
         MPI_Comm_size(intraComm,&size) ;
         MPI_Request* requests= new MPI_Request[size-1] ;
         MPI_Status* status= new MPI_Status[size-1] ;
         
         for(int i=1;i<size;i++)
         {
            MPI_Isend(buff,count,MPI_CHAR,i,2,intraComm,&requests[i-1]) ;
         }
         MPI_Waitall(size-1,requests,status) ;
         registerContext(buff,count,it->second.leaderRank) ;

         recvContextId.erase(it) ;
         delete [] requests ;
         delete [] status ;

       }
     }     
     
     void CServer::listenRootContext(void)
     {
       
       MPI_Status status ;
       int flag ;
       static void* buffer ;
       static MPI_Request request ;
       static bool recept=false ;
       int rank ;
       int count ;
       const int root=0 ;
       
       if (recept==false)
       {
         traceOff() ;
         MPI_Iprobe(root,2,intraComm, &flag, &status) ;
         traceOn() ;
         if (flag==true) 
         {
           MPI_Get_count(&status,MPI_CHAR,&count) ;
           buffer=new char[count] ;
           MPI_Irecv(buffer,count,MPI_CHAR,root,2,intraComm,&request) ;
           recept=true ;   
         }
       }
       else
       {
         MPI_Test(&request,&flag,&status) ;
         if (flag==true)
         {
           MPI_Get_count(&status,MPI_CHAR,&count) ;
           registerContext(buffer,count) ;
           delete [] buffer ;
           recept=false ;         
         }
       }
     } 
     
     
     
     void CServer::registerContext(void* buff,int count, int leaderRank)
     {
     
       string contextId;
       CBufferIn buffer(buff,count) ;

       buffer>>contextId ;
       MPI_Comm contextIntercomm ;
       MPI_Intercomm_create(intraComm,0,CXios::globalComm,leaderRank,10+leaderRank,&contextIntercomm) ;
       
       info(20)<<"CServer : Register new Context : "<<contextId<<endl  ;
       MPI_Comm inter ;
       MPI_Intercomm_merge(contextIntercomm,1,&inter) ;
       MPI_Barrier(inter) ;
       if (contextList.find(contextId)!=contextList.end()) 
        ERROR("void CServer::registerContext(void* buff,int count, int leaderRank)",
              <<"Context has already been registred") ;
      
      CContext* context=CContext::create(contextId) ;
      contextList[contextId]=context ;
      context->initServer(intraComm,contextIntercomm) ;
             
     }    
     
     
     void CServer::contextEventLoop(void)
     {
       bool finished ;
       map<string,CContext*>::iterator it ;
       for(it=contextList.begin();it!=contextList.end();it++) 
       {
         finished=it->second->eventLoop() ;
         if (finished)
         {
           contextList.erase(it) ;
           break ;
         }
       }
         
     }
     
}
