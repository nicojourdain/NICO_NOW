#include "xmlioserver.hpp"
#include "generate_interface.hpp"
#include "indent.hpp"
#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "context.hpp"
#include "file.hpp"

int main (int argc, char ** argv, char ** UNUSED (env))
{
//  string path="./src/attr_interface/" ;
  string path="./interface/" ;
  
  CContext* context=CContext::create("interface") ;
  CAxis axis ;
  CAxisGroup axisgroup ;
  CField field;
  CFieldGroup fieldgroup ;
  CDomain domain ;
  CDomainGroup domaingroup ;
  CGrid grid ;
  CGridGroup gridgroup ;
  
  CFile afile;
  CFileGroup filegroup;
  
  ostringstream oss ;
  ofstream file;
  
  file.open((path+"axis_interface_attr.f90").c_str()); 
  axis.generateFortran2003Interface(file) ;
  file.close();
  
  file.open((path+"icaxis_attr.cpp").c_str()); 
  axis.generateCInterface(file) ;
  file.close();
  
  file.open((path+"iaxis_attr.F90").c_str()); 
  axis.generateFortranInterface(file) ;
  file.close();
  
  file.open((path+"axisgroup_interface_attr.f90").c_str()); 
  axisgroup.generateFortran2003Interface(file) ;
  file.close();
  
  file.open((path+"icaxisgroup_attr.cpp").c_str()); 
  axisgroup.generateCInterface(file) ;
  file.close();
  
  file.open((path+"iaxisgroup_attr.F90").c_str()); 
  axisgroup.generateFortranInterface(file) ;
  file.close();
  
  file.open((path+"domain_interface_attr.f90").c_str()); 
  domain.generateFortran2003Interface(file) ;
  file.close();
  
  file.open((path+"icdomain_attr.cpp").c_str()); 
  domain.generateCInterface(file) ;
  file.close();
  
  file.open((path+"idomain_attr.F90").c_str()); 
  domain.generateFortranInterface(file) ;
  file.close();
  
  file.open((path+"domaingroup_interface_attr.f90").c_str()); 
  domaingroup.generateFortran2003Interface(file) ;
  file.close();
  
  file.open((path+"icdomaingroup_attr.cpp").c_str()); 
  domaingroup.generateCInterface(file) ;
  file.close();
  
  file.open((path+"idomaingroup_attr.F90").c_str()); 
  domaingroup.generateFortranInterface(file) ;
  file.close();
  
  
  file.open((path+"grid_interface_attr.f90").c_str()); 
  grid.generateFortran2003Interface(file) ;
  file.close();
  
  file.open((path+"icgrid_attr.cpp").c_str()); 
  grid.generateCInterface(file) ;
  file.close();
  
  file.open((path+"igrid_attr.F90").c_str()); 
  grid.generateFortranInterface(file) ;
  file.close();
  
  file.open((path+"gridgroup_interface_attr.f90").c_str()); 
  gridgroup.generateFortran2003Interface(file) ;
  file.close();
  
  file.open((path+"icgridgroup_attr.cpp").c_str()); 
  gridgroup.generateCInterface(file) ;
  file.close();
  
  file.open((path+"igridgroup_attr.F90").c_str()); 
  gridgroup.generateFortranInterface(file) ;
  file.close();
  
  
  file.open((path+"field_interface_attr.f90").c_str()); 
  field.generateFortran2003Interface(file) ;
  file.close();
  
  file.open((path+"icfield_attr.cpp").c_str()); 
  field.generateCInterface(file) ;
  file.close();
  
  file.open((path+"ifield_attr.F90").c_str()); 
  field.generateFortranInterface(file) ;
  file.close();
  
  file.open((path+"fieldgroup_interface_attr.f90").c_str()); 
  fieldgroup.generateFortran2003Interface(file) ;
  file.close();
  
  file.open((path+"icfieldgroup_attr.cpp").c_str()); 
  fieldgroup.generateCInterface(file) ;
  file.close();
  
  file.open((path+"ifieldgroup_attr.F90").c_str()); 
  fieldgroup.generateFortranInterface(file) ;
  file.close();
  


  file.open((path+"file_interface_attr.f90").c_str()); 
  afile.generateFortran2003Interface(file) ;
  file.close();
  
  file.open((path+"icfile_attr.cpp").c_str()); 
  afile.generateCInterface(file) ;
  file.close();
  
  file.open((path+"ifile_attr.F90").c_str()); 
  afile.generateFortranInterface(file) ;
  file.close();
  
  file.open((path+"filegroup_interface_attr.f90").c_str()); 
  filegroup.generateFortran2003Interface(file) ;
  file.close();
  
  file.open((path+"icfilegroup_attr.cpp").c_str()); 
  filegroup.generateCInterface(file) ;
  file.close();
  
  file.open((path+"ifilegroup_attr.F90").c_str()); 
  filegroup.generateFortranInterface(file) ;
  file.close();
  
 
  file.open((path+"context_interface_attr.f90").c_str()); 
  context->generateFortran2003Interface(file) ;
  file.close();
  
  file.open((path+"iccontext_attr.cpp").c_str()); 
  context->generateCInterface(file) ;
  file.close();
  
  file.open((path+"icontext_attr.F90").c_str()); 
  context->generateFortranInterface(file) ;
  file.close();
  
}
