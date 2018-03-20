/* ************************************************************************** *
 *      Copyright © IPSL/LSCE, xios, Avril 2010 - Octobre 2011         *
 * ************************************************************************** */

#include <boost/multi_array.hpp>
#include <boost/shared_ptr.hpp>

#include "xmlioserver.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"

#include "calendar_type.hpp"

#include "icutil.hpp"
#include "timer.hpp"
#include "context.hpp"

extern "C"
{
// /////////////////////////////// Définitions ////////////////////////////// //

   // ----------------------- Redéfinition de types ----------------------------

   typedef enum { D360 = 0 , ALLLEAP, NOLEAP, JULIAN, GREGORIAN } XCalendarType ;

   typedef xios::CContext * XContextPtr;

   // ------------------------ Création des handle -----------------------------
   
   void cxios_context_handle_create (XContextPtr * _ret, const char * _id, int _id_len)
   {
      std::string id; 
      if (!cstr2string(_id, _id_len, id)) return;
      CTimer::get("XIOS").resume() ;
      
      std::vector<xios::CContext*> def_vector =
            xios::CContext::getRoot()->getChildList();

      for (std::size_t i = 0; i < def_vector.size(); i++)
      {
          if (def_vector[i]->getId().compare(id) == 0)
          {
            *_ret = def_vector[i];
             CTimer::get("XIOS").suspend() ;
            return;
          }
      }
       CTimer::get("XIOS").suspend() ;
       ERROR("void cxios_context_handle_create (XContextPtr * _ret, const char * _id, int _id_len)",
             << "Context "<<id<<"  unknown");
      // Lever une exeception ici
   }
   
   // ------------------------ Changements de contextes ------------------------
   
   void cxios_context_set_current(XContextPtr context, bool withswap)
   {
      CTimer::get("XIOS").resume() ;
      CContext::setCurrent(context->getId());
      CTimer::get("XIOS").suspend() ;
   }
   
 
   // -------------------- Vérification des identifiants -----------------------

   void cxios_context_valid_id (bool * _ret, const char * _id, int _id_len)
   {
      std::string id;
      if (!cstr2string(_id, _id_len, id)) return;
      
      CTimer::get("XIOS").resume() ;
      std::vector<xios::CContext*> def_vector =
            xios::CContext::getRoot()->getChildList();

      for (std::size_t i = 0; i < def_vector.size(); i++)
	   {
          if (def_vector[i]->getId().compare(id) == 0)
          *_ret = true;
      }
     *_ret = false;
     CTimer::get("XIOS").suspend() ;
   }
} // extern "C"
