
#ifndef __FIELD_IMPL_HPP__
#define __FIELD_IMPL_HPP__

#include "xmlioserver_spl.hpp"
#include "field.hpp"
#include "context.hpp"
#include "grid.hpp"
#include "timer.hpp"
#include "array_new.hpp"


namespace xios {

   template <int N>
   void CField::setData(const CArray<double, N>& _data)
   {
     const std::vector<CField*>& refField=getAllReference();
     std::vector<CField*>::const_iterator  it = refField.begin(), end = refField.end();
     
     for (; it != end; it++) (*it)->updateData(_data) ;
    }
    
   template <int N>
      bool CField::updateData(const CArray<double, N>& _data)
   {        
      CContext* context=CContext::getCurrent();
      const CDate & currDate = context->getCalendar()->getCurrentDate();
      const CDate opeDate      = *last_operation + freq_operation;
      const CDate writeDate    = *last_Write     + freq_write;
      bool doOperation, doWrite; 
         

   
      info(50) << "CField::updateData " << currDate <<  " : send data to " << this->getBaseFieldId() << std::endl;
      info(50) << "Next operation "  << opeDate<<std::endl;
      
      doOperation = (opeDate <= currDate) ;
      if (isOnceOperation)
        if (isFirstOperation) doOperation=true ;
        else doOperation=false ;
      
      if (doOperation)
      {
         if (this->data.numElements() != this->grid->storeIndex_client.numElements())
         {
            this->data.resize(this->grid->storeIndex_client.numElements());
         }
            
         CArray<double,1> input(data.numElements()) ;
         this->grid->inputField(_data, input);          
         (*this->foperation)(input);
         
         *last_operation = currDate;
         info(50) << "(*last_operation = currDate) : " << *last_operation << " = " << currDate << std::endl; 
      }
      
      
      doWrite = (writeDate < (currDate + freq_operation)) ;
      if (isOnceOperation)
      { 
        if(isFirstOperation) 
        {
          doWrite=true ;
          isFirstOperation=false ;
        }
        else doWrite=false ;
      }
      
      if (doWrite)
      {
         this->foperation->final();
         *last_Write = writeDate;
         info(50) << "(*last_Write = currDate) : " << *last_Write << " = " << currDate	<< std::endl;
         CTimer::get("XIOS Send Data").resume() ;
         sendUpdateData() ;
         CTimer::get("XIOS Send Data").suspend() ;
         return (true);        
      }

      return (false);
   }

} // namespace xios

#endif
