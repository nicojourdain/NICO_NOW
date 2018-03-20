#ifndef __XMLIO_CField__
#define __XMLIO_CField__

/// xios headers ///
#include "xmlioserver_spl.hpp"
#include "group_factory.hpp"
#include "functor.hpp"
#include "functor_type.hpp"
#include "duration.hpp"
#include "date.hpp"
#include "declare_group.hpp"
#include "calendar_util.hpp"
#include "array_new.hpp"
#include "attribute_array.hpp"
//#include "context.hpp"


namespace xios {
   
   /// ////////////////////// Déclarations ////////////////////// ///

   class CFieldGroup;
   class CFieldAttributes;
   class CField;

   class CFile;
   class CGrid;
   class CContext ;
   ///--------------------------------------------------------------

   // Declare/Define CFieldAttribute
   BEGIN_DECLARE_ATTRIBUTE_MAP(CField)
#  include "field_attribute.conf"
   END_DECLARE_ATTRIBUTE_MAP(CField)

   ///--------------------------------------------------------------
   class CField
      : public CObjectTemplate<CField>
      , public CFieldAttributes
   {
         /// friend ///
         friend class CFile;

         /// typedef ///
         typedef CObjectTemplate<CField>   SuperClass;
         typedef CFieldAttributes SuperClassAttribute;

      public :

         typedef CFieldAttributes RelAttributes;
         typedef CFieldGroup      RelGroup;

         enum EEventId
         {
           EVENT_ID_UPDATE_DATA
         } ;
         
         /// Constructeurs ///
         CField(void);
         explicit CField(const StdString & id);
         CField(const CField & field);       // Not implemented yet.
         CField(const CField * const field); // Not implemented yet.

         /// Accesseurs ///
         CField* getDirectFieldReference(void) const;
         CField* getBaseFieldReference(void)   const;
         const std::vector<CField*> & getAllReference(void) const;

         CGrid* getRelGrid(void) const ;
         CFile* getRelFile(void) const ;

      public :

         StdSize getNStep(void) const;

         const CDuration & getFreqOperation(void) const;
         const CDuration & getFreqWrite(void) const;

         boost::shared_ptr<CDate> getLastWriteDate(void) const;
         boost::shared_ptr<CDate> getLastOperationDate(void) const;

         boost::shared_ptr<func::CFunctor> getFieldOperation(void) const;
         
         CArray<double, 1> getData(void) const;

         const StdString & getBaseFieldId(void) const;

         /// Mutateur ///
         void setRelFile(CFile* _file);
         void incrementNStep(void);
         void resetNStep() ;

         template <int N> bool updateData(const CArray<double, N>&   data);
         
         bool updateDataServer
               (const CDate & currDate,
                const std::deque< CArray<double, 1>* > storedClient);
 
       public :

         /// Test ///
         bool hasDirectFieldReference(void) const;
         bool isActive(void) const;

         /// Traitements ///
         void solveRefInheritance(bool apply);
         void solveGridReference(void);
         void solveOperation(void);

//         virtual void fromBinary(StdIStream & is);

         /// Destructeur ///
         virtual ~CField(void);

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);
         
         static ENodeType GetType(void);
         
        template <int N> void setData(const CArray<double, N>& _data) ;
        static bool dispatchEvent(CEventServer& event) ;
        void sendUpdateData(void) ;
        static void recvUpdateData(CEventServer& event) ;
        void recvUpdateData(vector<int>& ranks, vector<CBufferIn*>& buffers) ;
        void writeField(void) ;
        void outputField(CArray<double,3>& fieldOut) ;
        void outputField(CArray<double,2>& fieldOut) ;
        
      public :

         /// Propriétés privées ///
         
         std::vector<CField*> refObject;
         CField* baseRefObject;
         CGrid*  grid ;
         CFile*  file;

         CDuration freq_operation, freq_write;
         CDuration freq_operation_srv, freq_write_srv;

         StdSize nstep;
         boost::shared_ptr<CDate>    last_Write, last_operation;
         boost::shared_ptr<CDate>    lastlast_Write_srv,last_Write_srv, last_operation_srv;
         
         boost::shared_ptr<func::CFunctor> foperation;
         map<int,boost::shared_ptr<func::CFunctor> > foperation_srv;
         
         CArray<double, 1> data;
         map<int, CArray<double,1>* > data_srv ;
         bool isOnceOperation ;
         bool isFirstOperation ;

   }; // class CField

   ///--------------------------------------------------------------

   // Declare/Define CFieldGroup and CFieldDefinition
   DECLARE_GROUP(CField);

   ///-----------------------------------------------------------------

   template <>
      void CGroupTemplate<CField, CFieldGroup, CFieldAttributes>::solveRefInheritance(void);

   ///-----------------------------------------------------------------
} // namespace xios


#endif // __XMLIO_CField__
