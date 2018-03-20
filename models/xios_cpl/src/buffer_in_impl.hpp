#ifndef __BUFFER_IN_IMPL_HPP__
#define __BUFFER_IN_IMPL_HPP__

namespace xios
{

// template spectialisation : CBufferIn::get

    template <> bool CBufferIn::get<char>(char& data) { return get_template(data) ; }  
    template <> bool CBufferIn::get<bool>(bool& data) { return get_template(data) ; }  
    template <> bool CBufferIn::get<int>(int& data)   { return get_template(data) ; }
    template <> bool CBufferIn::get<short>(short& data) { return get_template(data) ; }  
    template <> bool CBufferIn::get<long>(long& data)  { return get_template(data) ; }  
    template <> bool CBufferIn::get<uint>(uint& data)  { return get_template(data) ; }  
    template <> bool CBufferIn::get<ushort>(ushort& data) { return get_template(data) ; }  
    template <> bool CBufferIn::get<ulong>(ulong& data) { return get_template(data) ; }  
    template <> bool CBufferIn::get<float>(float& data) { return get_template(data) ; }  
    template <> bool CBufferIn::get<double>(double& data) { return get_template(data) ; }  
    template <> bool CBufferIn::get<long double>(long double& data) { return get_template(data) ;}  

    template <> bool CBufferIn::get<char>(char* data, size_t n) { return get_template(data,n) ; }  
    template <> bool CBufferIn::get<bool>(bool* data, size_t n) { return get_template(data,n) ; }  
    template <> bool CBufferIn::get<int>(int* data, size_t n)   { return get_template(data,n) ; }
    template <> bool CBufferIn::get<short>(short* data, size_t n) { return get_template(data,n) ; }  
    template <> bool CBufferIn::get<long>(long* data, size_t n)  { return get_template(data,n) ; }  
    template <> bool CBufferIn::get<uint>(uint* data, size_t n)  { return get_template(data,n) ; }  
    template <> bool CBufferIn::get<ushort>(ushort* data, size_t n) { return get_template(data,n) ; }  
    template <> bool CBufferIn::get<ulong>(ulong* data, size_t n) { return get_template(data,n) ; }  
    template <> bool CBufferIn::get<float>(float* data, size_t n) { return get_template(data,n) ; }  
    template <> bool CBufferIn::get<double>(double* data, size_t n) { return get_template(data,n) ; }  
    template <> bool CBufferIn::get<long double>(long double* data, size_t n) { return get_template(data,n) ;}  

    template <> bool CBufferIn::advance<char>(size_t n) { return advance_template<char>(n) ; }  
    template <> bool CBufferIn::advance<bool>(size_t n) { return advance_template<bool>(n) ; }  
    template <> bool CBufferIn::advance<int>(size_t n)   { return advance_template<int>(n) ; }
    template <> bool CBufferIn::advance<short>(size_t n) { return advance_template<short>(n) ; }  
    template <> bool CBufferIn::advance<long>(size_t n)  { return advance_template<long>(n) ; }  
    template <> bool CBufferIn::advance<uint>(size_t n)  { return advance_template<uint>(n) ; }  
    template <> bool CBufferIn::advance<ushort>(size_t n) { return advance_template<ushort>(n) ; }  
    template <> bool CBufferIn::advance<ulong>(size_t n) { return advance_template<ulong>(n) ; }  
    template <> bool CBufferIn::advance<float>(size_t n) { return advance_template<float>(n) ; }  
    template <> bool CBufferIn::advance<double>(size_t n) { return advance_template<double>(n) ; }  
    template <> bool CBufferIn::advance<long double>(size_t n) { return advance_template<long double>(n) ;}  

    template <class T>
    bool CBufferIn::get_template(T& data)
    {
      return get_template<T>(&data,1) ;
    }
        
    template <class T>
    bool CBufferIn::get_template(T* data, size_t n)
    {
      bool ret;
      char* dataBuff ;
  
      size_t dataSize=sizeof(T)*n ;
  
      if (count_+dataSize<=size_)
      {
        dataBuff=(char*) data ;
        for(size_t i=0;i<dataSize;i++) dataBuff[i]=current[i] ;
        current+=dataSize ;
        count_+=dataSize ;
        ret=true ;
      }
      else ret=false ;
  
      return ret ;
    }

    template <class T>
    bool CBufferIn::advance_template(size_t n)
    {
      bool ret;
      char* dataBuff ;
  
      size_t dataSize=sizeof(T)*n ;
  
      if (count_+dataSize<=size_)
      {
        current+=dataSize ;
        count_+=dataSize ;
        ret=true ;
      }
      else ret=false ;
  
      return ret ;
    }    

}
  

#endif
