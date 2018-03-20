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
#include "grid.hpp"

extern "C"
{
// /////////////////////////////// Définitions ////////////////////////////// //

   // ----------------------- Redéfinition de types ----------------------------
   
   typedef xios::CGrid      * XGridPtr;
   typedef xios::CGridGroup * XGridGroupPtr;

   // ------------------------ Création des handle -----------------------------
  
   void cxios_grid_handle_create (XGridPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;
      CTimer::get("XIOS").resume() ;
      *_ret = CGrid::get(id);
      CTimer::get("XIOS").suspend() ;
   }
   
   void cxios_gridgroup_handle_create (XGridGroupPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;
      CTimer::get("XIOS").resume() ;
      *_ret = CGridGroup::get(id);
      CTimer::get("XIOS").suspend() ;
   }

   // -------------------- Vérification des identifiants -----------------------

   void cxios_grid_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;
      CTimer::get("XIOS").resume() ;
      *_ret = CGrid::has(id);
      CTimer::get("XIOS").suspend() ;
   }

   void cxios_gridgroup_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;
      CTimer::get("XIOS").resume() ;
      *_ret = CGridGroup::has(id);
      CTimer::get("XIOS").suspend() ;
   }
} // extern "C"
