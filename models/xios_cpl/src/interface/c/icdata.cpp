/* ************************************************************************** *
 *      Copyright © IPSL/LSCE, xios, Avril 2010 - Octobre 2011         *
 * ************************************************************************** */

#include <boost/multi_array.hpp>
#include <boost/shared_ptr.hpp>
#include <string>
#include <iostream>


#include "xmlioserver.hpp"
#include "oasis_cinterface.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"

#include "icutil.hpp"
#include "cxios.hpp"
#include "client.hpp"
#include "field.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "mpi.hpp"
#include "timer.hpp"
#include "array_new.hpp"


extern "C"
{
// /////////////////////////////// Définitions ////////////////////////////// //

   // ----------------------- Redéfinition de types ----------------------------
   
   typedef enum { NETCDF4 = 0 } XFileType;
   
   typedef xios::CContext * XContextPtr;

   // -------------------- Traitement des données ------------------------------
   void cxios_init_server(void)
   {
     CXios::initServerSide();      
   }

   void cxios_init_client(const char * client_id , int len_client_id, MPI_Fint* f_local_comm, MPI_Fint* f_return_comm )
   {
      std::string str; 
      MPI_Comm local_comm ;
      MPI_Comm return_comm ;
      
      if (!cstr2string(client_id, len_client_id, str)) return;

      int initialized ;
      MPI_Initialized(&initialized) ;
      if (initialized) local_comm=MPI_Comm_f2c(*f_local_comm) ;
      else local_comm=MPI_COMM_NULL ;
      CXios::initClientSide(str,local_comm,return_comm);
      *f_return_comm=MPI_Comm_c2f(return_comm) ;
      CTimer::get("XIOS init").suspend() ;
      CTimer::get("XIOS").suspend() ;
   }

   void cxios_context_initialize(const char * context_id , int len_context_id, MPI_Fint* f_comm)
   {
     std::string str; 
     MPI_Comm comm ;
     
     if (!cstr2string(context_id, len_context_id, str)) return;
     CTimer::get("XIOS").resume() ;
     CTimer::get("XIOS init context").resume() ;
     comm=MPI_Comm_f2c(*f_comm) ;
     CClient::registerContext(str,comm) ;
     CTimer::get("XIOS init context").suspend() ;
     CTimer::get("XIOS").suspend() ;
   }
 
    void cxios_context_close_definition()
   {
     CTimer::get("XIOS").resume() ;
     CTimer::get("XIOS close definition").resume() ;
     CContext* context = CContext::getCurrent() ;
     context->closeDefinition() ;
     CTimer::get("XIOS close definition").suspend() ;
     CTimer::get("XIOS").suspend() ;
   }  

   void cxios_context_finalize()
   {
     CTimer::get("XIOS").resume() ;
     CTimer::get("XIOS context finalize").resume() ;
     CContext* context = CContext::getCurrent() ;
     context->finalize() ;
     CTimer::get("XIOS context finalize").suspend() ;
     CTimer::get("XIOS").suspend() ;
   }
   
   void cxios_finalize()
   {
     CTimer::get("XIOS").resume() ;
     CTimer::get("XIOS finalize").resume() ;
     CXios::clientFinalize() ;
   }

   void cxios_solve_inheritance()
   {
     CTimer::get("XIOS").resume() ;
     CContext* context = CContext::getCurrent() ;
     context->solveAllInheritance(false) ;
     CTimer::get("XIOS").suspend() ;
   } 
   
   // ---------------------- Ecriture des données ------------------------------
   
   void cxios_write_data_k81(const char * fieldid, int fieldid_size, double * data_k8, int data_Xsize)
   {
      std::string fieldid_str;
      if (!cstr2string(fieldid, fieldid_size, fieldid_str)) return; 
      
      CTimer::get("XIOS").resume() ;
      CTimer::get("XIOS send field").resume() ;
      CContext* context = CContext::getCurrent() ;
      if (!context->hasServer) context->client->checkBuffers() ;
      CArray<double,(StdSize)1> data(data_k8,shape(data_Xsize),neverDeleteData) ;
      CField::get(fieldid_str)->setData(data) ;
      CField toto ;
      toto.setData(data) ;
      CTimer::get("XIOS send field").suspend() ;
      CTimer::get("XIOS").suspend() ;
   }
   
   void cxios_write_data_k82(const char * fieldid, int fieldid_size, double * data_k8, int data_Xsize, int data_Ysize)
   {
      std::string fieldid_str;
      if (!cstr2string(fieldid, fieldid_size, fieldid_str)) return;
      
      CTimer::get("XIOS").resume() ;
      CTimer::get("XIOS send field").resume() ;
      CContext* context = CContext::getCurrent() ;
      if (!context->hasServer) context->client->checkBuffers() ;
      
      CArray<double,2>data(data_k8,shape(data_Xsize,data_Ysize),neverDeleteData) ;
      CField::get(fieldid_str)->setData(data) ;
      CTimer::get("XIOS send field").suspend() ;
      CTimer::get("XIOS").suspend() ;
   }
   
   void cxios_write_data_k83(const char * fieldid, int fieldid_size, double * data_k8, int data_Xsize, int data_Ysize, int data_Zsize)
   {
      std::string fieldid_str;
      if (!cstr2string(fieldid, fieldid_size, fieldid_str)) return; 

      CTimer::get("XIOS").resume() ;
      CTimer::get("XIOS send field").resume() ;
      CContext* context = CContext::getCurrent() ;
      if (!context->hasServer) context->client->checkBuffers() ;

      CArray<double,3>data(data_k8,shape(data_Xsize,data_Ysize,data_Zsize),neverDeleteData) ;
      CField::get(fieldid_str)->setData(data) ;
      CTimer::get("XIOS send field").suspend() ;
      CTimer::get("XIOS").suspend() ;
   }
   
   void cxios_write_data_k41(const char * fieldid, int fieldid_size, float * data_k4, int data_Xsize)
   {
      std::string fieldid_str;
     if (!cstr2string(fieldid, fieldid_size, fieldid_str)) return; 

      CTimer::get("XIOS").resume() ;
      CTimer::get("XIOS send field").resume() ;
      CContext* context = CContext::getCurrent() ;
      if (!context->hasServer) context->client->checkBuffers() ;

      CArray<float,1> data_tmp(data_k4,shape(data_Xsize),neverDeleteData) ;
      CArray<double,1> data(data_Xsize) ;
      data=data_tmp ;
      CField::get(fieldid_str)->setData(data) ;
      CTimer::get("XIOS send field").suspend() ;
      CTimer::get("XIOS").suspend() ;
   }
   
   void cxios_write_data_k42(const char * fieldid, int fieldid_size, float * data_k4, int data_Xsize, int data_Ysize)
   {
      std::string fieldid_str;
      if (!cstr2string(fieldid, fieldid_size, fieldid_str)) return; 

      CTimer::get("XIOS").resume() ;
      CTimer::get("XIOS send field").resume() ;
      CContext* context = CContext::getCurrent() ;
      if (!context->hasServer) context->client->checkBuffers() ;

      CArray<float,2> data_tmp(data_k4,shape(data_Xsize,data_Ysize),neverDeleteData) ;
      CArray<double,2> data(data_Xsize,data_Ysize) ;
      data=data_tmp ;
      CField::get(fieldid_str)->setData(data) ;
      CTimer::get("XIOS send field").suspend() ;
      CTimer::get("XIOS").suspend() ;
   }
   
   void cxios_write_data_k43(const char * fieldid, int fieldid_size, float * data_k4, int data_Xsize, int data_Ysize, int data_Zsize)
   {
      std::string fieldid_str;
 
      if (!cstr2string(fieldid, fieldid_size, fieldid_str)) return; 
 
      CTimer::get("XIOS").resume() ;
      CTimer::get("XIOS send field").resume() ;
      CContext* context = CContext::getCurrent() ;
      if (!context->hasServer) context->client->checkBuffers() ;

      CArray<float,3> data_tmp(data_k4,shape(data_Xsize,data_Ysize,data_Zsize),neverDeleteData) ;
      CArray<double,3> data(data_Xsize,data_Ysize,data_Zsize) ;
      data=data_tmp ;
 
      CField::get(fieldid_str)->setData(data) ;
      CTimer::get("XIOS send field").suspend() ;
      CTimer::get("XIOS").suspend() ;

    } 

} // extern "C"
