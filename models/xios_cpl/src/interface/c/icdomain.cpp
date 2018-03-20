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
#include "domain.hpp"

extern "C"
{
// /////////////////////////////// Définitions ////////////////////////////// //

   // ----------------------- Redéfinition de types ----------------------------

   typedef xios::CDomain      * XDomainPtr;
   typedef xios::CDomainGroup * XDomainGroupPtr;

   // ------------------------ Création des handle -----------------------------
   
   void cxios_domain_handle_create (XDomainPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;
      CTimer::get("XIOS").resume() ;
      *_ret = CDomain::get(id);
      CTimer::get("XIOS").suspend() ;
   }
   
   void cxios_domaingroup_handle_create (XDomainGroupPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;

      CTimer::get("XIOS").resume() ;
      *_ret = CDomainGroup::get(id);
      CTimer::get("XIOS").suspend() ;
   }

   // -------------------- Vérification des identifiants -----------------------

   void cxios_domain_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;

      CTimer::get("XIOS").resume() ;
      *_ret = CDomain::has(id);
      CTimer::get("XIOS").suspend() ;
   }

   void cxios_domaingroup_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;
      CTimer::get("XIOS").resume() ;
      *_ret = CDomainGroup::has(id);
      CTimer::get("XIOS").suspend() ;
   }
} // extern "C"
