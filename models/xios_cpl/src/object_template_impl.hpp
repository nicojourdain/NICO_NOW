#ifndef __XMLIO_CObjectTemplate_impl__
#define __XMLIO_CObjectTemplate_impl__

#include "xmlioserver_spl.hpp"
#include "context_client.hpp"
#include "object_factory.hpp"
#include "context.hpp"
#include "buffer_in.hpp"
#include "attribute.hpp"
#include "event_client.hpp"
#include "object_template.hpp"
#include "context_client.hpp"
#include "indent.hpp"
#include "type_util.hpp"
#include "message.hpp"
#include "type.hpp"
#include "type_util.hpp"
#include "group_template.hpp"

namespace xios
{
   /// ////////////////////// Définitions ////////////////////// ///
   template <class T>
      xios_map<StdString,
      xios_map<StdString,
      boost::shared_ptr<T> > > CObjectTemplate<T>::AllMapObj;

   template <class T>
      xios_map<StdString,
      std::vector<boost::shared_ptr<T> > > CObjectTemplate<T>::AllVectObj;

   template <class T>
      xios_map<StdString,long int> CObjectTemplate<T>::GenId;

   template <class T>
      CObjectTemplate<T>::CObjectTemplate(void)
         : CAttributeMap()
         , CObject()
   { /* Ne rien faire de plus */ }

   template <class T>
      CObjectTemplate<T>::CObjectTemplate(const StdString & id)
         : CAttributeMap()
         , CObject(id)
   { /* Ne rien faire de plus */ }

   template <class T>
      CObjectTemplate<T>::CObjectTemplate
         (const CObjectTemplate<T> & object, bool withAttrList, bool withId)
         : CAttributeMap()
         , CObject()
   {
      if (object.hasId() && withId)
         this->setId(object.getId());
      ERROR("CObjectTemplate<T> construtor 3", << "Not completly implemented yet !");
   }
   
   template <class T>
      CObjectTemplate<T>::~CObjectTemplate(void)
   { /* Ne rien faire de plus */ }

   ///--------------------------------------------------------------

   template <class T>
      std::vector<boost::shared_ptr<T> > &
         CObjectTemplate<T>::GetAllVectobject(const StdString & contextId)
   { 
      return (CObjectTemplate<T>::AllVectObj[contextId]); 
   }
   
   //---------------------------------------------------------------
   
   template <class T>
      StdString CObjectTemplate<T>::toString(void) const
   {
      StdOStringStream oss;
      oss << "<" << T::GetName();
      if (this->hasId())
         oss << " id=\"" << this->getId() << "\"";
      oss << " " << SuperClassMap::toString() << "/>";
      return (oss.str());
   }

   template <class T>
      void CObjectTemplate<T>::fromString(const StdString & str)
   { 
      ERROR("CObjectTemplate<T>::fromString(str)",
            << "[ str = " << str << "] Not implemented yet !"); 
   }
   
   //---------------------------------------------------------------

/*   
   template <class T>
      void CObjectTemplate<T>::toBinary(StdOStream & os) const
   {
      SuperClassMap::toBinary(os);    
   }
      
   template <class T>
      void CObjectTemplate<T>::fromBinary(StdIStream & is)
   {
      SuperClassMap::fromBinary(is); 
   }
*/

   //---------------------------------------------------------------

   template <class T>
      void CObjectTemplate<T>::parse(xml::CXMLNode & node)
   {
      xml::THashAttributes attributes = node.getAttributes();
      CAttributeMap::setAttributes(attributes);
   }

   //---------------------------------------------------------------

   template <class T>
      ENodeType CObjectTemplate<T>::getType(void) const
   {
      return (T::GetType());
   }
  
   template <class T>
   string CObjectTemplate<T>::getName(void) const
   {
      return (T::GetName());
   }
  
   //---------------------------------------------------------------

   template <class T>
      bool CObjectTemplate<T>::hasChild(void) const
   { 
      return (false); 
   }

   //---------------------------------------------------------------

   template <class T>
      void CObjectTemplate<T>::solveDescInheritance(bool apply, const CAttributeMap * const parent)
   { 
      SuperClassMap::setAttributes(parent, apply); 
   }

   //---------------------------------------------------------------

   template <class T>
      void CObjectTemplate<T>::ClearAllAttributes(void)
   {
      vector<T*> avect = CObjectTemplate<T>::getAll() ;
      typename vector<T*>::iterator
            it = avect.begin(), end = avect.end();

      for (;it != end; it++)
      {
         CAttributeMap & amap = **it;
         amap.clearAllAttributes();
      }
   }

   template <class T>
   void CObjectTemplate<T>::sendAttributToServer(const string& id)
   {
      CAttributeMap & attrMap = *this;
      CAttribute* attr=attrMap[id] ;
      sendAttributToServer(*attr) ;
   }

  template <class T>
  void CObjectTemplate<T>::sendAttributToServer(CAttribute& attr)
  {
    CContext* context=CContext::getCurrent() ;
    
    if (!context->hasServer)
    {
       CContextClient* client=context->client ;

       CEventClient event(getType(),EVENT_ID_SEND_ATTRIBUTE) ;   
       if (client->isServerLeader())
       {
         CMessage msg ;
         msg<<this->getId() ;
         msg<<attr.getName() ;
         msg<<attr ;
         event.push(client->getServerLeader(),1,msg) ;
         client->sendEvent(event) ;
       }
       else client->sendEvent(event) ;
    }
      
  }
   
  template <class T>
  void CObjectTemplate<T>::recvAttributFromClient(CEventServer& event)
  {
      
    CBufferIn* buffer=event.subEvents.begin()->buffer;
    string id,attrId;
    *buffer>>id ;
    CAttributeMap & attrMap = *get(id);
    *buffer>>attrId ;
    CAttribute* attr=attrMap[attrId] ;
    info(50)<<"attribut recu "<<attrId<<"  " ;
    if (attr->isEmpty()) info(50)<<"--> empty"<<endl ;
    else info(50) /*<attr->getValue()*/<<endl ;
    *buffer>>*attr ;
     info(50)<<"attribut recu "<<attrId<<"  " ;
    if (attr->isEmpty()) info(50)<<"--> empty"<<endl ;
    else info(50) /*attr->getValue()*/<<endl ;
  }

   template <class T>
   bool CObjectTemplate<T>::dispatchEvent(CEventServer& event)
   {
      switch(event.type)
      {
         case EVENT_ID_SEND_ATTRIBUTE :
           recvAttributFromClient(event) ;
           return true ;
           break ;
       
         default :
         return false ;
//           ERROR("void CObjectTemplate<T>::recvEvent(CEventServer& event)",
//                 <<"Unknown Event") ;
      }
   }
   
   template <typename T>
   bool CObjectTemplate<T>::has(const string & id)
   {
     return CObjectFactory::HasObject<T>(id) ;
   }

   template <typename T>
   bool CObjectTemplate<T>::has(const string& contextId, const string & id)
   {
     return CObjectFactory::HasObject<T>(contextId,id) ;
   }

   template <typename T>
   T* CObjectTemplate<T>::get(const string & id)
   {
     return CObjectFactory::GetObject<T>(id).get() ;
   }

   template <typename T>
   T* CObjectTemplate<T>::get(const T* ptr)
   {
     return CObjectFactory::GetObject<T>(ptr).get() ;
   }
   
   template <typename T>
   shared_ptr<T> CObjectTemplate<T>::getShared(const T* ptr)
   {
     return CObjectFactory::GetObject<T>(ptr) ;
   }

   template <typename T>
   shared_ptr<T> CObjectTemplate<T>::getShared(void)
   {
     return CObjectFactory::GetObject<T>((T*)this) ;
   }
   
   template <typename T>
   const vector<T*> CObjectTemplate<T>::getAll()
   {
     const vector< shared_ptr<T> >& shared_vect= CObjectFactory::GetObjectVector<T>();
     vector<T*> vect ;
    
     typename vector<shared_ptr<T> >::const_iterator it;
     for(it=shared_vect.begin();it!=shared_vect.end();++it) vect.push_back(it->get()) ;
     return vect ;
   }

   template <typename T>
   const vector<T*> CObjectTemplate<T>::getAll(const string & id)
   {
     const vector< shared_ptr<T> >& shared_vect= CObjectFactory::GetObjectVector<T>(id);
     vector<T*> vect ;
    
     typename vector<shared_ptr<T> >::const_iterator it;
     for(it=shared_vect.begin();it!=shared_vect.end();++it) vect.push_back(it->get()) ;
     return vect ;
   }

   template <typename T>
   T* CObjectTemplate<T>::get(const string& contextId, const string & id)
   {
     return CObjectFactory::GetObject<T>(contextId,id).get() ;
   }

   template <typename T>
   T* CObjectTemplate<T>::create(const string & id)
   {
     return CObjectFactory::CreateObject<T>(id).get() ;
   }   ///--------------------------------------------------------------

  template <typename T>
  T* CObjectTemplate<T>::get(void)
  {
    return CObjectFactory::GetObject<T>((T*)this).get() ;
  }
  
   template <typename T>
   void CObjectTemplate<T>::generateCInterface(ostream& oss)
   {
     string className=getName() ;
     int found=className.find_first_of("_") ;
     if (found!=string::npos) className.replace(found,1,0,'x') ;
     
     oss<<"/* ************************************************************************** *"<<iendl ;
     oss<<" *               Interface auto generated - do not modify                   *"<<iendl ;
     oss<<" * ************************************************************************** */"<<iendl;
     oss<<iendl ;
     oss<<"#include <boost/multi_array.hpp>"<<iendl ;
     oss<<"#include <boost/shared_ptr.hpp>"<<iendl ;
     oss<<"#include \"xmlioserver.hpp\""<<iendl;
     oss<<"#include \"attribute_template.hpp\""<<iendl ;
     oss<<"#include \"object_template.hpp\""<<iendl;
     oss<<"#include \"group_template.hpp\""<<iendl ;
     oss<<"#include \"icutil.hpp\""<<iendl ;
     oss<<"#include \"timer.hpp\""<<iendl ;
     oss<<"#include \"node_type.hpp\""<<iendl ;
     oss<<iendl ;
     oss<<"extern \"C\""<<iendl ;
     oss<<"{"<<iendl++ ;
     oss<<"typedef xios::"<<getStrType<T>()<<"*  "<<className<<"_Ptr;"<<iendl;
     oss<<iendl ;
     SuperClassMap::generateCInterface(oss,className) ;
     oss<<iendl-- ;
     oss<<"}"<<iendl ;
   }

   template <typename T>
   void CObjectTemplate<T>::generateFortran2003Interface(ostream& oss)
   {
     string className=getName() ;
     int found=className.find_first_of("_") ;
     if (found!=string::npos) className.replace(found,1,0,'x') ;
     
     oss<<"! * ************************************************************************** *"<<iendl ;
     oss<<"! *               Interface auto generated - do not modify                     *"<<iendl ;
     oss<<"! * ************************************************************************** *"<<iendl;
     oss<<iendl ;
     oss<<"MODULE "<<className<<"_interface_attr"<<iendl++ ;
     oss<<"USE, INTRINSIC :: ISO_C_BINDING"<<iendl ;
     oss<<iendl ;
     oss<<"INTERFACE ! Do not call directly / interface FORTRAN 2003 <-> C99"<<iendl++ ;
     oss<<iendl ;   
     oss<<iendl ;
     SuperClassMap::generateFortran2003Interface(oss,className) ;
     oss<<"END INTERFACE"<<iendl-- ;
     oss<<iendl-- ;
     oss<<"END MODULE "<<className<<"_interface_attr"<<iendl ;
   }
  
   template <typename T>
   void CObjectTemplate<T>::generateFortranInterface(ostream& oss)
   {
     string className=getName() ;
     int found=className.find_first_of('_') ;
     if (found!=string::npos) className.erase(found,1) ;
     string superClassName=getName();
     found=superClassName.find("_group") ;
     if (found!=string::npos) superClassName.erase(found,6) ;
     
     oss<<"! * ************************************************************************** *"<<iendl ;
     oss<<"! *               Interface auto generated - do not modify                     *"<<iendl ;
     oss<<"! * ************************************************************************** *"<<iendl;
     oss<<"#include \"xios_fortran_prefix.hpp\""<<iendl ;
     oss<<iendl ;
     oss<<"MODULE i"<<className<<"_attr"<<iendl++ ;
     oss<<"USE, INTRINSIC :: ISO_C_BINDING"<<iendl ;
     oss<<"USE i"<<superClassName<<iendl ;
     oss<<"USE "<<className<<"_interface_attr"<<iendl ;
//     oss<<"TYPE txios("<<className<<")"<<iendl ;
//     oss<<"  INTEGER(kind = C_INTPTR_T) :: daddr"<<iendl ;
//     oss<<"END TYPE txios("<<className<<")"<<iendl ;
     oss<<iendl-- ;
     oss<<"CONTAINS"<<iendl++ ;
     oss<<iendl ;
     SuperClassMap::generateFortranInterface_id(oss,className) ;
     oss<<iendl ;
     SuperClassMap::generateFortranInterface_hdl(oss,className) ;
     oss<<iendl ;
     SuperClassMap::generateFortranInterface_hdl_(oss,className) ;
     oss<<iendl ;
     SuperClassMap::generateFortranInterfaceGet_id(oss,className) ;
     oss<<iendl ;
     SuperClassMap::generateFortranInterfaceGet_hdl(oss,className) ;
     oss<<iendl ;
     SuperClassMap::generateFortranInterfaceGet_hdl_(oss,className) ;
     oss<<iendl ;
     SuperClassMap::generateFortranInterfaceIsDefined_id(oss,className) ;
     oss<<iendl ;
     SuperClassMap::generateFortranInterfaceIsDefined_hdl(oss,className) ;
     oss<<iendl ;
     SuperClassMap::generateFortranInterfaceIsDefined_hdl_(oss,className) ;     
     oss<<iendl-- ;
     oss<<"END MODULE i"<<className<<"_attr"<<iendl ;
   }


} // namespace xios

#endif // __XMLIO_CObjectTemplate_impl__
