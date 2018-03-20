#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "log.hpp"
#include "buffer_out.hpp"
#include "buffer_client.hpp"
#include "cxios.hpp"
#include "mpi.hpp"
#include "tracer.hpp"

namespace xios
{
 
  size_t maxRequestSize=0 ;
  
  CClientBuffer::CClientBuffer(MPI_Comm interComm_,int serverRank_)
  {
    bufferSizeByServer=CXios::bufferSize ;
    info(10)<<"bufferSizeByServer "<<bufferSizeByServer<<endl ;
    interComm=interComm_ ;
    serverRank=serverRank_ ;
    bufferSize=bufferSizeByServer/2 ;
    buffer[0]=new char[bufferSize] ; // transform it with MPI_ALLOC_MEM later
    buffer[1]=new char[bufferSize] ;
    current=0 ;
    count=0 ;
    pending=false ;
    retBuffer=new CBufferOut(buffer[current],bufferSize) ;
  }
  
  CClientBuffer::~CClientBuffer()
  {
   delete [] buffer[0] ;
   delete [] buffer[1] ;
   delete retBuffer ;
  }
  
  int CClientBuffer::remain(void)
  {
    return bufferSize-count ;
  }
  
  bool CClientBuffer::isBufferFree(int size)
  {
    if (size>maxRequestSize) maxRequestSize=size ;
    
    if (size>bufferSize) ERROR("CClientBuffer::hasSpace(int size)",
                               <<"request size is too big for buffer, increase buffer client size"<<endl
                               <<"Current buffer_size : "<<CXios::bufferSize<<endl
                               <<"buffer_size must be > "<<size*2<<endl)
 
    if (size<=remain()) return true ;
    else return false ;
  }
    
  
  CBufferOut*  CClientBuffer::getBuffer(int size)
  {
    if (size<=remain())
    {
      retBuffer->realloc(buffer[current]+count,size) ;
      count+=size ;
      return retBuffer ;
    }
    else
    {
       ERROR("CBufferOut*  CClientBuffer::getSpace(int size) ;",
               <<"No ennough space in buffer, that may not happen...");
       return NULL ;
    }
 
  }  
  
  bool CClientBuffer::checkBuffer(void)
  {
    MPI_Status status ;
    int flag ;
    
    if (pending)
    {
      traceOff() ;
      MPI_Test(&request,&flag,&status) ;
      traceOn() ;
      if (flag==true) pending=false ;
    }

    if (!pending)
    {
      if (count>0)
      {
        MPI_Issend(buffer[current],count,MPI_CHAR,serverRank,20,interComm,&request) ;
        pending=true ;
        if (current==1) current=0 ;
        else current=1 ;
        count=0 ;
      }
    }
    return pending ;
  }
  
  bool CClientBuffer::hasPendingRequest(void)
  {
    if (pending) return true ;
    else if (count>0) return true ;
    else return false ;
  }
    
  
  
}    
    
