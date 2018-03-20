#ifndef __OASIS_CINTERFACE__
#define __OASIS_CINTERFACE__
#include <string>
#include "mpi.hpp"

extern "C"
{

  void fxios_oasis_init(const char* server_id,int str_len) ;
  void fxios_oasis_enddef(void) ;
  void fxios_oasis_finalize(void) ;
  void fxios_oasis_get_localcomm(MPI_Fint* f_comm) ;
  void fxios_oasis_get_intracomm(MPI_Fint* f_comm_client_server,const char* client_id,int str_len) ;
  void fxios_oasis_get_intercomm(MPI_Fint* f_comm_client_server,const char* client_id,int str_len) ;
}
 
namespace xios
{
  void oasis_init(const std::string& server_id) ;
  void oasis_enddef(void) ;
  void oasis_finalize(void) ;
  void oasis_get_localcomm(MPI_Comm& comm) ;
  void oasis_get_intracomm(MPI_Comm& comm_client_server,const std::string& server_id) ;
  void oasis_get_intercomm(MPI_Comm& comm_client_server,const std::string& server_id) ;
}
#endif
