#ifndef __XMLIO_CGrid__
#define __XMLIO_CGrid__

/// xios headers ///
#include "xmlioserver_spl.hpp"
#include "group_factory.hpp"

#include "declare_group.hpp"
#include "domain.hpp"
#include "axis.hpp"
#include "array_new.hpp"
#include "attribute_array.hpp"

namespace xios {
   
   /// ////////////////////// Déclarations ////////////////////// ///

   class CGridGroup;
   class CGridAttributes;
   class CGrid;

   ///--------------------------------------------------------------

   // Declare/Define CGridAttribute
   BEGIN_DECLARE_ATTRIBUTE_MAP(CGrid)
#  include "grid_attribute.conf"
   END_DECLARE_ATTRIBUTE_MAP(CGrid)

   ///--------------------------------------------------------------

   class CGrid
      : public CObjectTemplate<CGrid>
      , public CGridAttributes
   {
         /// typedef ///
         typedef CObjectTemplate<CGrid>   SuperClass;
         typedef CGridAttributes SuperClassAttribute;

      public :

         typedef CGridAttributes RelAttributes;
         typedef CGridGroup      RelGroup;

         enum EEventId
         {
           EVENT_ID_INDEX
         } ;
         
         /// Constructeurs ///
         CGrid(void);
         explicit CGrid(const StdString & id);
         CGrid(const CGrid & grid);       // Not implemented yet.
         CGrid(const CGrid * const grid); // Not implemented yet.

         /// Traitements ///
         void solveReference(void);

 //        virtual void toBinary  (StdOStream & os) const;
//         virtual void fromBinary(StdIStream & is);

         /// Tests ///
         bool hasAxis(void) const;

      public :

         /// Accesseurs ///
         const std::deque< CArray<int, 1>* > & getStoreIndex(void) const;
         const std::deque< CArray<int, 1>* > & getOutIIndex(void)  const;
         const std::deque< CArray<int, 1>* > & getOutJIndex(void)  const;
         const std::deque< CArray<int, 1>* > & getOutLIndex(void)  const;

         const CAxis*   getRelAxis  (void) const;
         const CDomain* getRelDomain(void) const;

         StdSize getDimension(void) const;
         
//         StdSize getLocalSize(void) const;
//         StdSize getGlobalSize(void) const;
         StdSize  getDataSize(void) const;
//         std::vector<StdSize> getLocalShape(void) const;
//         std::vector<StdSize> getGlobalShape(void) const;

         /// Entrées-sorties de champs ///
         template <int n>
            void inputField(const CArray<double,n>& field, CArray<double,1>& stored) const;
            
         void inputFieldServer(const std::deque< CArray<double, 1>* > storedClient,
                               CArray<double, 1>&  storedServer) const;

         void outputField(int rank, const CArray<double,1>& stored,  CArray<double,3>& field)  ;
         void outputField(int rank, const CArray<double,1>& stored,  CArray<double,2>& field)  ;
         void outputField(int rank, const CArray<double,1>& stored,  CArray<double,1>& field)  ; 
   
         /// Destructeur ///
         virtual ~CGrid(void);

      public :

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);
         
         static ENodeType GetType(void);

         /// Instanciateurs Statiques ///
         static CGrid* createGrid(CDomain* domain);
         static CGrid* createGrid(CDomain* domain, CAxis* axis);

      public :

         /// Entrées-sorties de champs (interne) ///
         void storeField_arr(const double * const data, CArray<double,1>& stored) const;

         /// Traitements protégés ///
         void computeIndexServer(void);
         void computeIndex(void);
         void solveDomainRef(void);
         void solveAxisRef(void);

         static bool dispatchEvent(CEventServer& event) ;
         void outputFieldToServer(CArray<double,1>& fieldIn, int rank, CArray<double,1>& fieldOut) ;
         static void recvIndex(CEventServer& event) ;
         void recvIndex(int rank, CBufferIn& buffer) ;
         void sendIndex(void) ;
         
      public:

         /// Propriétés privées ///
         bool withAxis ;
         bool isChecked;

         CAxis*   axis ;
         CDomain* domain ;

         std::deque< CArray<int, 1>* > storeIndex ;
         std::deque< CArray<int, 1>* > out_i_index ;
         std::deque< CArray<int, 1>* > out_j_index ;
         std::deque< CArray<int, 1>* > out_l_index ;
         
        CArray<int, 1>  storeIndex_client ;
        CArray<int, 1>  out_i_client ;
        CArray<int, 1>  out_j_client ;
        CArray<int, 1>  out_l_client ;
         
         map<int, CArray<int, 1>* >  storeIndex_toSrv ;
         map<int,int> nbSenders ;
//         std::deque<ARRAY(int, 1)> out_i_toSrv ;
//         std::deque<ARRAY(int, 1)> out_j_toSrv ;
//         std::deque<ARRAY(int, 1)> out_l_toSrv ;
         
         map<int, CArray<int, 1>* > out_i_fromClient ;
         map<int, CArray<int, 1>* > out_j_fromClient ;
         map<int, CArray<int, 1>* > out_l_fromClient ;
         void checkMask(void) ;
   }; // class CGrid

   ///--------------------------------------------------------------

   template <int n>
      void CGrid::inputField(const CArray<double,n>& field, CArray<double,1>& stored) const
   {
      if (this->getDataSize() != field.numElements())
         ERROR("void CGrid::inputField(const  CArray<double,n>& field, CArray<double,1>& stored) const",
                << "[ Awaiting size of the data = " << this->getDataSize()       << ", "
                << "Received data size = "      << field.numElements() << " ] "
                << "The array of data has not the good size !")
      this->storeField_arr(field.dataFirst(), stored) ;
   }

   ///--------------------------------------------------------------

   // Declare/Define CGridGroup and CGridDefinition
   DECLARE_GROUP(CGrid);

   ///--------------------------------------------------------------

} // namespace xios

#endif // __XMLIO_CGrid__
