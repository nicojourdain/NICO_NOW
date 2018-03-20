#include "onetcdf4.hpp"
#include "group_template.hpp"
#include "mpi.hpp"
#include "netcdf.hpp"

namespace xios
{
      /// ////////////////////// Définitions ////////////////////// ///

      CONetCDF4::CONetCDF4
         (const StdString & filename, bool exist, const MPI_Comm * comm, bool multifile)
            : path()
      {
         this->wmpi = (comm != NULL) && !multifile;
         this->initialize(filename, exist, comm,multifile);
      }
      
      //---------------------------------------------------------------
      
      

      CONetCDF4::~CONetCDF4(void)
      {
//         CheckError(nc_close(this->ncidp));
      }

      ///--------------------------------------------------------------

      void CONetCDF4::initialize
         (const StdString & filename, bool exist, const MPI_Comm * comm, bool multifile)
      {
         if (!exist)
         {
            if (comm != NULL)
            {
               if (!multifile) CheckError(xios::nc_create_par(filename.c_str(), NC_NETCDF4|NC_MPIIO, *comm, MPI_INFO_NULL, &this->ncidp));
               else CheckError(nc_create(filename.c_str(), NC_NETCDF4, &this->ncidp));
            }
            else CheckError(nc_create(filename.c_str(), NC_NETCDF4, &this->ncidp));
         }
         else
         {
            if (comm != NULL)
            {
               if (!multifile) CheckError(xios::nc_open_par(filename.c_str(), NC_NETCDF4|NC_MPIIO, *comm, MPI_INFO_NULL, &this->ncidp));
               else CheckError(nc_open(filename.c_str(), NC_NETCDF4, &this->ncidp));
            }
            else  CheckError(nc_open(filename.c_str(), NC_NETCDF4, &this->ncidp));
         }
      }
      
      void CONetCDF4::close()
      {
        CheckError(nc_close(this->ncidp));
      }
      
      //---------------------------------------------------------------
      
      void CONetCDF4::definition_start(void)
      { 
         CheckError(nc_redef(this->ncidp));
      }
      
      //---------------------------------------------------------------
      
      void CONetCDF4::definition_end(void)
      { 
         CheckError(nc_enddef(this->ncidp));
      }
      
      //---------------------------------------------------------------
      
      void CONetCDF4::CheckError(int status)
      {
         if (status != NC_NOERR)
         {
            StdString errormsg (nc_strerror(status)); // fuite mémoire ici ?
            ERROR("CONetCDF4::CheckError(int status)",
                  << "[ status = " << status << " ] " << errormsg);
         }
      }

      //---------------------------------------------------------------
      
      int CONetCDF4::getCurrentGroup(void)
      {
         return (this->getGroup(this->getCurrentPath()));
      }
      
      //---------------------------------------------------------------
      
      int CONetCDF4::getGroup(const CONetCDF4Path & path)
      {
         int retvalue = this->ncidp;
         
         CONetCDF4Path::const_iterator
            it  = path.begin(), end = path.end();

         for (;it != end; it++)
         {
            const StdString & groupid = *it;
            CheckError(nc_inq_ncid(retvalue, const_cast<char*>(groupid.c_str()), &retvalue));
         }
         return (retvalue);
      }
      
      //---------------------------------------------------------------
      
      int CONetCDF4::getVariable(const StdString & varname)
      {
         int varid = 0;
         int grpid = this->getCurrentGroup();
         CheckError(nc_inq_varid (grpid, varname.c_str(), &varid));
         return (varid);
      }
      
      //---------------------------------------------------------------
      
      int CONetCDF4::getDimension(const StdString & dimname)
      {
         int dimid = 0;
         int grpid = this->getCurrentGroup();
         CheckError(nc_inq_dimid (grpid, dimname.c_str(), &dimid));
         return (dimid);
      }
      
      //---------------------------------------------------------------
      
      int CONetCDF4::getUnlimitedDimension(void)
      {
         int dimid = 0;
         int grpid = this->getCurrentGroup();
         CheckError(nc_inq_unlimdim (grpid, &dimid));
         return (dimid);
      }
      
      StdString CONetCDF4::getUnlimitedDimensionName(void)
      {
         char full_name_in[NC_MAX_NAME +1];
         int grpid = this->getGroup(path);
         int dimid = this->getUnlimitedDimension();
                              
         if (dimid == -1) return (std::string());
            CheckError(nc_inq_dimname(grpid, dimid, full_name_in));
                                          
         StdString dimname(full_name_in);
         return (dimname);
      }
      
      //---------------------------------------------------------------
      
      std::vector<StdSize> CONetCDF4::getDimensions(const StdString & varname)
      {
         StdSize size = 0;
         std::vector<StdSize> retvalue;
         int grpid = this->getCurrentGroup();
         int varid = this->getVariable(varname);
         int nbdim = 0, *dimid = NULL;

         CheckError(nc_inq_varndims(grpid, varid, &nbdim));
         dimid = new int[nbdim]();
         CheckError(nc_inq_vardimid(grpid, varid, dimid));

         for (int i = 0; i < nbdim; i++)
         {
            CheckError(nc_inq_dimlen (grpid, dimid[i], &size));
            if (size == NC_UNLIMITED)
                size = UNLIMITED_DIM;
            retvalue.push_back(size);
         }
         delete [] dimid;
         return (retvalue);
      }

      std::vector<std::string> CONetCDF4::getDimensionsIdList (const std::string * _varname)
      {
         char full_name_in[NC_MAX_NAME +1];
         int nbdim = 0, *dimid = NULL;
         int grpid = this->getCurrentGroup();
         int varid = (_varname != NULL) ? this->getVariable(*_varname) : NC_GLOBAL;
         std::vector<std::string> retvalue;
                                   
         if (_varname != NULL)
         {
            CheckError(nc_inq_varndims(grpid, varid, &nbdim));
            dimid = new int[nbdim]();
            CheckError(nc_inq_vardimid(grpid, varid, dimid));
         }
         else
         {
            CheckError(nc_inq_dimids(grpid, &nbdim, NULL, 1));
            dimid = new int[nbdim]();
            CheckError(nc_inq_dimids(grpid, NULL, dimid, 1));
         }
                                        
         for (int i = 0; i < nbdim; i++)
         {
            CheckError(nc_inq_dimname(grpid, dimid[i], full_name_in));
            std::string dimname(full_name_in);
            retvalue.push_back(dimname);
         }
         delete [] dimid;
                                                                                                                                                      
         return (retvalue);
      }


      //---------------------------------------------------------------

      const CONetCDF4::CONetCDF4Path & CONetCDF4::getCurrentPath(void) const
      { return (this->path); }

      void CONetCDF4::setCurrentPath(const CONetCDF4Path & path)
      { this->path = path; }

      //---------------------------------------------------------------

      int CONetCDF4::addGroup(const StdString & name)
      {
         int retvalue = 0;
         int grpid = this->getCurrentGroup();
         CheckError(nc_def_grp(grpid, const_cast<char*>(name.c_str()), &retvalue));
         return (retvalue);
      }
      
      //---------------------------------------------------------------
      
      int CONetCDF4::addDimension(const StdString& name, const StdSize size)
      {
         int retvalue = 0;
         int grpid = this->getCurrentGroup();
         if (size != UNLIMITED_DIM)
            CheckError(nc_def_dim (grpid, name.c_str(), size, &retvalue));
         else
            CheckError(nc_def_dim (grpid, name.c_str(), NC_UNLIMITED, &retvalue));
         return (retvalue);
      }
      
      //---------------------------------------------------------------
      
      int CONetCDF4::addVariable(const StdString & name, nc_type type,
                                  const std::vector<StdString> & dim)
      {
         int varid = 0;
         std::vector<int> dimids;
         std::vector<StdSize> dimsizes ;
         StdSize size ;
         
         int grpid = this->getCurrentGroup();
         
         std::vector<StdString>::const_iterator
            it  = dim.begin(), end = dim.end();

         for (;it != end; it++)
         {
            const StdString & dimid = *it;
            dimids.push_back(this->getDimension(dimid));
            CheckError(nc_inq_dimlen (grpid, this->getDimension(dimid), &size));
            if (size==NC_UNLIMITED) size=1 ;
            dimsizes.push_back(size) ;
         }
         
         CheckError(nc_def_var (grpid, name.c_str(), type, dimids.size(), &(dimids[0]), &varid));
         CheckError(nc_def_var_chunking (grpid, varid, NC_CHUNKED, &(dimsizes[0])));
         CheckError(nc_def_var_fill(grpid, varid, true, NULL));
         return (varid);
      }

      //---------------------------------------------------------------

      template <>
         void CONetCDF4::addAttribute
            (const StdString & name, const StdString & value, const StdString * varname )
      {
         int grpid = this->getCurrentGroup();
         int varid = (varname == NULL) ? NC_GLOBAL : this->getVariable(*varname);
         CheckError(nc_put_att(grpid, varid, name.c_str(), NC_CHAR, value.size(), value.c_str()));
         //CheckError(nc_put_att_string(grpid, varid, name.c_str(), 1, &str));
      }
      
      //---------------------------------------------------------------
      
      template <>
         void CONetCDF4::addAttribute
            (const StdString & name, const double & value, const StdString * varname )
      {
         int grpid = this->getCurrentGroup();
         int varid = (varname == NULL) ? NC_GLOBAL : this->getVariable(*varname);
         CheckError(nc_put_att_double(grpid, varid, name.c_str(), NC_DOUBLE,1, &value));
      }

       template <>
         void CONetCDF4::addAttribute
            (const StdString & name, const CArray<double,1>& value, const StdString * varname )
      {
         int grpid = this->getCurrentGroup();
         int varid = (varname == NULL) ? NC_GLOBAL : this->getVariable(*varname);
         CheckError(nc_put_att_double(grpid, varid, name.c_str(), NC_DOUBLE,value.numElements(), value.dataFirst()));
      }     
      //---------------------------------------------------------------
      
      template <>
         void CONetCDF4::addAttribute
            (const StdString & name, const float & value, const StdString * varname )
      {
         int grpid = this->getCurrentGroup();
         int varid = (varname == NULL) ? NC_GLOBAL : this->getVariable(*varname);
         CheckError(nc_put_att_float(grpid, varid, name.c_str(), NC_FLOAT, 1, &value));
      }

       template <>
         void CONetCDF4::addAttribute
            (const StdString & name, const CArray<float,1>& value, const StdString * varname )
      {
         int grpid = this->getCurrentGroup();
         int varid = (varname == NULL) ? NC_GLOBAL : this->getVariable(*varname);
         CheckError(nc_put_att_float(grpid, varid, name.c_str(), NC_FLOAT,value.numElements(), value.dataFirst()));
      }     
      
      //---------------------------------------------------------------
      
      template <>
         void CONetCDF4::addAttribute
            (const StdString & name, const int & value, const StdString * varname )
      {
         int grpid = this->getCurrentGroup();
         int varid = (varname == NULL) ? NC_GLOBAL : this->getVariable(*varname);
         CheckError(nc_put_att_int(grpid, varid, name.c_str(), NC_INT,1, &value));
      }

       template <>
         void CONetCDF4::addAttribute
            (const StdString & name, const CArray<int,1>& value, const StdString * varname )
      {
         int grpid = this->getCurrentGroup();
         int varid = (varname == NULL) ? NC_GLOBAL : this->getVariable(*varname);
         CheckError(nc_put_att_int(grpid, varid, name.c_str(), NC_INT,value.numElements(), value.dataFirst()));
      }     
      //---------------------------------------------------------------

      void CONetCDF4::getWriteDataInfos(const StdString & name, StdSize record, StdSize & array_size,
                                        std::vector<StdSize> & sstart,
                                        std::vector<StdSize> & scount,
                                        const std::vector<StdSize> * start,
                                        const std::vector<StdSize> * count)
      {   
         std::vector<std::size_t> sizes  = this->getDimensions(name);
         std::vector<std::string> iddims = this->getDimensionsIdList (&name);   
         std::vector<std::size_t>::const_iterator
            it  = sizes.begin(), end = sizes.end();
         int i = 0;

         if (iddims.begin()->compare(this->getUnlimitedDimensionName()) == 0)
         {
            sstart.push_back(record);
            scount.push_back(1); 
            if ((start == NULL) &&
                (count == NULL)) i++;
            it++;
         }

         for (;it != end; it++)
         {      
            if ((start != NULL) && (count != NULL))
            {
               sstart.push_back((*start)[i]);
               scount.push_back((*count)[i]);
               array_size *= (*count)[i];
               i++;
            }
            else
            {
               sstart.push_back(0);
               scount.push_back(sizes[i]);
               array_size *= sizes[i];
               i++;
            }
         }
         
      }
      
                     
 
      template <>
         void CONetCDF4::writeData_(int grpid, int varid,
                                    const std::vector<StdSize> & sstart,
                                    const std::vector<StdSize> & scount, const double * data)
      {
         CheckError(nc_put_vara_double(grpid, varid, &(sstart[0]), &(scount[0]), data));
//         sync() ;
      }
      
      //---------------------------------------------------------------
      
      template <>
         void CONetCDF4::writeData_(int grpid, int varid,
                                    const std::vector<StdSize> & sstart,
                                    const std::vector<StdSize> & scount, const int * data)
      {
          CheckError(nc_put_vara_int(grpid, varid, &(sstart[0]), &(scount[0]), data));
//          sync() ;
      }
      
      //---------------------------------------------------------------
      
      template <>
         void CONetCDF4::writeData_(int grpid, int varid,
                                    const std::vector<StdSize> & sstart,
                                    const std::vector<StdSize> & scount, const float * data)
      {
          CheckError(nc_put_vara_float(grpid, varid, &(sstart[0]), &(scount[0]), data));
//          sync() ;
      }

      //---------------------------------------------------------------

      void CONetCDF4::writeData(const CArray<int, 2>& data, const StdString & name)
      {
         int grpid = this->getCurrentGroup();
         int varid = this->getVariable(name);
         StdSize array_size = 1;
         std::vector<StdSize> sstart, scount;

         this->getWriteDataInfos(name, 0, array_size,  sstart, scount, NULL, NULL);
         this->writeData_(grpid, varid, sstart, scount, data.dataFirst());
      }

      void CONetCDF4::writeTimeAxisData(const CArray<double, 1>& data, const StdString & name,
                                        bool collective, StdSize record, bool isRoot)
      {
         int grpid = this->getCurrentGroup();
         int varid = this->getVariable(name);
         
         map<int,size_t>::iterator it=timeAxis.find(varid) ;
         if (it==timeAxis.end()) timeAxis[varid]=record ;
         else 
         {
           if (it->second >= record) return ;
           else it->second =record ;
         }
         
         StdSize array_size = 1;
         std::vector<StdSize> sstart, scount;
         
         if (this->wmpi && collective)
         CheckError(nc_var_par_access(grpid, varid, NC_COLLECTIVE));
         if (this->wmpi && !collective)
         CheckError(nc_var_par_access(grpid, varid, NC_INDEPENDENT));
         
         this->getWriteDataInfos(name, record, array_size,  sstart, scount, NULL, NULL);
         if (using_netcdf_internal)  if (!isRoot) { sstart[0]=sstart[0]+1 ; scount[0]=0 ;}
         this->writeData_(grpid, varid, sstart, scount, data.dataFirst());
       }

      //---------------------------------------------------------------
      
      bool CONetCDF4::varExist(const StdString & varname)
      {
         int varid = 0;
         int grpid = this->getCurrentGroup();
         return (nc_inq_varid (grpid, varname.c_str(), &varid) == NC_NOERR);
      }

      void CONetCDF4::sync(void)
      {
         CheckError(nc_sync(this->ncidp)) ;
      } 
      ///--------------------------------------------------------------
 } // namespace xios
