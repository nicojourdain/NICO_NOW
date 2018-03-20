#ifndef __XMLIO_INETCDF4__
#define __XMLIO_INETCDF4__

/// xios headers ///
#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "array_new.hpp"

#include "mpi.hpp"
#include "netcdf.hpp"

#ifndef UNLIMITED_DIM
   #define UNLIMITED_DIM (size_t)(-1)
#endif  //UNLIMITED_DIM

namespace xios
{
      /// ////////////////////// Déclarations ////////////////////// ///
      typedef std::vector<StdString> CVarPath;

      class CINetCDF4
      {
         public :

            /// Constructeurs ///
            CINetCDF4(void);                             // Not implemented.
            CINetCDF4(const StdString & filename);
            CINetCDF4(const CINetCDF4 & inetcdf4);       // Not implemented.
            CINetCDF4(const CINetCDF4 * const inetcdf4); // Not implemented.

         //-------------------------------------------------------------

            /// Accesseurs ///
            StdSize getNbOfTimestep(const CVarPath  * const path = NULL);

            StdString getUnlimitedDimensionName(const CVarPath  * const path = NULL);

            StdString getCoordinatesId(const StdString & name,
                                       const CVarPath  * const path = NULL);

            StdString getBoundsId(const StdString & name,
                                  const CVarPath  * const path = NULL);

            StdString getLonCoordName(const StdString & varname,
                                      const CVarPath  * const path = NULL);
            StdString getLatCoordName(const StdString & varname,
                                       const CVarPath  * const path = NULL);
            StdString getVertCoordName(const StdString & varname,
                                       const CVarPath  * const path = NULL);

            std::set<StdString> getCoordVariables(const CVarPath * const path = NULL);
            std::set<StdString> getBoundVariables(const CVarPath * const path = NULL);

            std::list<StdString> getGroups   (const CVarPath * const path = NULL);
            std::list<StdString> getVariables(const CVarPath * const path = NULL);

            std::list<StdString> getDataVariables(bool _is3D       = true,
                                                  bool _isRecti    = true,
                                                  bool _isCurvi    = true,
                                                  bool _isUnstr    = true,
                                                  bool _isCellData = true,
                                                  bool _isTemporal = true,
                                                  const CVarPath * const path = NULL);

            std::list<StdString> getAttributes
                                (const StdString * const var  = NULL,
                                 const CVarPath  * const path = NULL);

            std::list<StdString> getDimensionsList
                                (const StdString * const var  = NULL,
                                 const CVarPath  * const path = NULL);

            std::list<StdString> getCoordinatesIdList(const StdString & name,
                                                      const CVarPath  * const path = NULL);

            std::map<StdString, StdSize> getDimensions (const StdString * const var  = NULL,
                                 			const CVarPath  * const path = NULL);

            StdSize getNbVertex(const StdString & name,
                                const CVarPath  * const path = NULL);
         //-------------------------------------------------------------

            template <class T>
               T getMissingValue(const StdString & name,
                                 const CVarPath  * const path = NULL);

            template <class T>
               T getAttributeValue(const StdString & name,
                                   const StdString * const var  = NULL,
                                   const CVarPath  * const path = NULL);

            template <class T>
               void getData(CArray<T, 1>& data,
                            const StdString & var,
                            const CVarPath  * const path = NULL,
                            StdSize record   = UNLIMITED_DIM);

         //-------------------------------------------------------------

            /// Tests ///
            bool hasMissingValue(const StdString & name,
                                 const CVarPath  * const path = NULL);

            bool hasAttribute(const StdString & name,
                              const StdString * const var  = NULL,
                              const CVarPath  * const path = NULL);

            bool hasVariable(const StdString & name,
                             const CVarPath  * const path = NULL);

            bool hasCoordinates(const StdString & name,
                                const CVarPath  * const path = NULL);

            bool hasTemporalDim(const CVarPath  * const path = NULL);

            bool hasBounds(const StdString & name,
                           const CVarPath  * const path = NULL);

         //-------------------------------------------------------------

            bool isBound(const StdString & name,
                         const CVarPath  * const path = NULL);
            bool isCoordinate(const StdString & name,
                              const CVarPath  * const path = NULL);
            bool isRectilinear(const StdString & name,
                               const CVarPath  * const path = NULL);
            bool isCurvilinear(const StdString & name,
                               const CVarPath  * const path = NULL);
            bool isUnknown(const StdString & name,
                               const CVarPath  * const path = NULL);
            bool isUnstructured(const StdString & name,
                                const CVarPath  * const path = NULL);

            bool isTemporal(const StdString & name,
                            const CVarPath  * const path = NULL);
            bool is3Dim(const StdString & name,
                        const CVarPath  * const path = NULL);
            bool isCellGrid(const StdString & name,
                            const CVarPath  * const path = NULL);

         //-------------------------------------------------------------

            /// Destructeur ///
            virtual ~CINetCDF4(void);

         protected :

         //-------------------------------------------------------------

            /// Accesseurs ///
            int getGroup   (const CVarPath * const path = NULL);
            int getVariable(const StdString & varname,
                            const CVarPath * const path = NULL);
            int getDimension(const StdString & dimname,
                             const CVarPath  * const path = NULL);
            int getUnlimitedDimension(const CVarPath  * const path = NULL);
            int getAttributeId(const StdString & name,
                               const StdString * const var  = NULL,
                               const CVarPath  * const path = NULL);

            std::pair<nc_type , StdSize> getAttribute(const StdString & attname,
                                                      const StdString * const var  = NULL,
                                                      const CVarPath  * const path = NULL);

         //-------------------------------------------------------------

            void getDataInfo(const StdString & var, const CVarPath  * const path, StdSize record,
                             std::vector<StdSize> & start, std::vector<StdSize> & count,
                             StdSize & array_size);

         private :

            /// Vérification des erreurs NetCDF ///
            static void CheckError(int status);

            int ncidp; // Identifiant de fichier netcdf.

      }; // class CINetCDF4

      ///--------------------------------------------------------------
      template <>
         StdString CINetCDF4::getAttributeValue
            (const StdString & name, const StdString * const var,
                                     const CVarPath  * const path);

      template <>
         std::vector<double> CINetCDF4::getAttributeValue
            (const StdString & name, const StdString * const var,
                                     const CVarPath  * const path);

      template <>
         std::vector<float> CINetCDF4::getAttributeValue
            (const StdString & name, const StdString * const var,
                                     const CVarPath  * const path);

      template <>
         std::vector<int>  CINetCDF4::getAttributeValue
            (const StdString & name, const StdString * const var,
                                     const CVarPath  * const path);

      template <>
         std::vector<char>  CINetCDF4::getAttributeValue
            (const StdString & name, const StdString * const var,
                                     const CVarPath  * const path);

      //---------------------------------------------------------------
      template <>
         int CINetCDF4::getMissingValue
            (const StdString & name, const CVarPath  * const path);

      template <>
         double CINetCDF4::getMissingValue
            (const StdString & name, const CVarPath  * const path);

      template <>
         float CINetCDF4::getMissingValue
            (const StdString & name, const CVarPath  * const path);

      //---------------------------------------------------------------
      template <>
         void CINetCDF4::getData(CArray<int, 1>& data, const StdString & var,
                                 const CVarPath  * const path, StdSize record);

      template <>
         void CINetCDF4::getData(CArray<double, 1>& data, const StdString & var,
                                 const CVarPath  * const path, StdSize record);

      template <>
         void CINetCDF4::getData(CArray<float, 1>& data, const StdString & var,
                                 const CVarPath  * const path, StdSize record);
      ///--------------------------------------------------------------

} // namespace xmlioserver

#endif //__XMLIO_INETCDF4__
