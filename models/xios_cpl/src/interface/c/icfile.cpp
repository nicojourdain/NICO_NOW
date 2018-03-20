/* ************************************************************************** *
 *      Copyright © IPSL/LSCE, xios, Avril 2010 - Octobre 2011         *
 * ************************************************************************** */

#include <boost/multi_array.hpp>
#include <boost/shared_ptr.hpp>

#include "xmlioserver.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"

#include "icutil.hpp"
#include "timer.hpp"
#include "file.hpp"

extern "C"
{
// /////////////////////////////// Définitions ////////////////////////////// //

   // ----------------------- Redéfinition de types ----------------------------
   
   typedef xios::CFile      * XFilePtr;
   typedef xios::CFileGroup * XFileGroupPtr;

   // ------------------------ Création des handle -----------------------------
   
   void cxios_file_handle_create (XFilePtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;
      CTimer::get("XIOS").resume() ;
      *_ret = CFile::get(id);
      CTimer::get("XIOS").suspend() ;
   }
   
   void cxios_filegroup_handle_create (XFileGroupPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;
      CTimer::get("XIOS").resume() ;
      *_ret = CFileGroup::get(id);
      CTimer::get("XIOS").suspend() ;
   }

   // -------------------- Vérification des identifiants -----------------------

   void cxios_file_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;
      CTimer::get("XIOS").resume() ;
      *_ret = CFile::has(id);
      CTimer::get("XIOS").suspend() ;
   }

   void cxios_filegroup_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;
      CTimer::get("XIOS").resume() ;
      *_ret = CFileGroup::has(id);
      CTimer::get("XIOS").suspend() ;
   }
} // extern "C"
