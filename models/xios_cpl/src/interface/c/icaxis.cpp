/* ************************************************************************** *
 *      Copyright © IPSL/LSCE, xios, Avril 2010 - Octobre 2011         *
 * ************************************************************************** */

#include <boost/multi_array.hpp>
#include <boost/shared_ptr.hpp>

#include "xmlioserver.hpp"

#include "object_template.hpp"
#include "group_template.hpp"
#include "attribute_template.hpp"

#include "icutil.hpp"
#include "timer.hpp"
#include "axis.hpp"

extern "C"
{
// /////////////////////////////// Définitions ////////////////////////////// //

   // ----------------------- Redéfinition de types ----------------------------
   
   typedef xios::CAxis      * XAxisPtr;
   typedef xios::CAxisGroup * XAxisGroupPtr;

   // ------------------------ Création des handle -----------------------------
   
   void cxios_axis_handle_create (XAxisPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;
      CTimer::get("XIOS").resume() ;
      *_ret = xios::CAxis::get(id);
      CTimer::get("XIOS").suspend() ;
   }
   
   void cxios_axisgroup_handle_create (XAxisGroupPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;
      CTimer::get("XIOS").resume() ;
      *_ret = xios::CAxisGroup::get(id);
      CTimer::get("XIOS").suspend() ;
    }

   // -------------------- Vérification des identifiants -----------------------

   void cxios_axis_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      CTimer::get("XIOS").resume() ;
      *_ret = xios::CAxis::has(id);
      CTimer::get("XIOS").suspend() ;
   }

   void cxios_axisgroup_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      CTimer::get("XIOS").resume() ;
      *_ret = xios::CAxisGroup::has(id);
      CTimer::get("XIOS").suspend() ;

   }
   
} // extern "C"
