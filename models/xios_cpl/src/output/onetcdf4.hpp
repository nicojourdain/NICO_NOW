#ifndef __XMLIO_INETCDF4__
#define __XMLIO_INETCDF4__

/// xios headers ///
#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "data_output.hpp"
#include "array_new.hpp"
#include "mpi.hpp"
#include "netcdf.hpp"

#ifndef UNLIMITED_DIM
   #define UNLIMITED_DIM (size_t)(-1)
#endif  //UNLIMITED_DIM

namespace xios
{
      /// ////////////////////// Déclarations ////////////////////// ///
      class CONetCDF4
         : public virtual CDataOutput
      {
         public :

            /// Définition de type ///
            typedef std::vector<StdString> CONetCDF4Path;

            /// Constructeurs ///
            CONetCDF4(const StdString & filename, bool exist, const MPI_Comm * comm = NULL, bool multifile=true);

            CONetCDF4(const CONetCDF4 & onetcdf4);       // Not implemented.
            CONetCDF4(const CONetCDF4 * const onetcdf4); // Not implemented.


            /// Initialisation ///
            void initialize(const StdString & filename, bool exist, const MPI_Comm * comm, bool multifile);
            void close(void) ;
            void sync(void) ;
            void definition_start(void);
            void definition_end(void);

            /// Mutateurs ///
            void setCurrentPath(const CONetCDF4Path & path);

            int addGroup(const StdString & name);
            int addDimension(const StdString& name, const StdSize size = UNLIMITED_DIM);
            int addVariable(const StdString & name, nc_type type,
                            const std::vector<StdString> & dim);
                            
      //----------------------------------------------------------------
         public :
         
            template <class T>
               void setDefaultValue(const StdString & varname, const T * value = NULL);
         
            template <class T>
               void addAttribute
                  (const StdString & name, const T & value, const StdString * varname = NULL);

            /// Ecriture des données ///
            template <class T, int ndim>
               void writeData(const CArray<T,ndim>& data, const StdString & name,
                              bool collective, StdSize record,
                              const std::vector<StdSize> * start = NULL,
                              const std::vector<StdSize> * count = NULL);

            void writeData(const CArray<int, 2>& data, const StdString & name);     
            void writeTimeAxisData(const CArray<double,1>& data, const StdString & name,
                                   bool collective, StdSize record, bool Isroot) ;
            /// Accesseur ///
            const CONetCDF4Path & getCurrentPath(void) const;

            /// Destructeur ///
            virtual ~CONetCDF4(void);
            
      //----------------------------------------------------------------
      
         protected :

            /// Ecriture ///
            virtual void writeField_ (CField*  field)  = 0;
            virtual void writeDomain_(CDomain* domain) = 0;
            virtual void writeAxis_  (CAxis*   axis)   = 0;

            /// Accesseurs ///
            int getCurrentGroup(void);
            int getGroup(const CONetCDF4Path & path);
            int getVariable(const StdString & varname);
            int getDimension(const StdString & dimname);
            std::vector<StdSize>   getDimensions       (const StdString & varname);
            std::vector<StdString> getDimensionsIdList (const StdString * varname);
            int       getUnlimitedDimension(void);
            StdString getUnlimitedDimensionName(void);

            bool varExist(const StdString & varname);

      //----------------------------------------------------------------
      
         private :
         
            template <class T>
               void writeData_(int grpid, int varid,
                               const std::vector<StdSize> & sstart,
                               const std::vector<StdSize> & scount, T * data);

            void getWriteDataInfos(const StdString & name, StdSize record, StdSize & array_size,
                                   std::vector<StdSize> & sstart,
                                   std::vector<StdSize> & scount,
                                   const std::vector<StdSize> * start,
                                   const std::vector<StdSize> * count);

            /// Vérification des erreurs NetCDF ///
            void CheckError(int status);

            /// Propriétés privées ///
            CONetCDF4Path path;
            int ncidp;
            bool wmpi;
            map<int,size_t> timeAxis ;
      }; // class CONetCDF4

      ///---------------------------------------------------------------
           


} // namespace xios

#endif //__XMLIO_INETCDF4__
