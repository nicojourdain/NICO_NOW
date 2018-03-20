#ifndef __BUFFER_OUT_IMPL_HPP__
#define __BUFFER_OUT_IMPL_HPP__

namespace xios
{

// template spectialisation : CBufferIn::put
    template <> bool CBufferOut::put<char>(const char& data) { return put_template(data) ; }  
    template <> bool CBufferOut::put<bool>(const bool& data) { return put_template(data) ; }  
    template <> bool CBufferOut::put<int>(const int& data)   { return put_template(data) ; }
    template <> bool CBufferOut::put<short>(const short& data) { return put_template(data) ; }  
    template <> bool CBufferOut::put<long>(const long& data)  { return put_template(data) ; }  
    template <> bool CBufferOut::put<uint>(const uint& data)  { return put_template(data) ; }  
    template <> bool CBufferOut::put<ushort>(const ushort& data) { return put_template(data) ; }  
    template <> bool CBufferOut::put<ulong>(const ulong& data) { return put_template(data) ; }  
    template <> bool CBufferOut::put<float>(const float& data) { return put_template(data) ; }  
    template <> bool CBufferOut::put<double>(const double& data) { return put_template(data) ; }  
    template <> bool CBufferOut::put<long double>(const long double& data) { return put_template(data) ;}  

    template <> bool CBufferOut::put<char>(const char* data, size_t n) { return put_template(data,n) ; }  
    template <> bool CBufferOut::put<bool>(const bool* data, size_t n) { return put_template(data,n) ; }  
    template <> bool CBufferOut::put<int>(const int* data, size_t n)   { return put_template(data,n) ; }
    template <> bool CBufferOut::put<short>(const short* data, size_t n) { return put_template(data,n) ; }  
    template <> bool CBufferOut::put<long>(const long* data, size_t n)  { return put_template(data,n) ; }  
    template <> bool CBufferOut::put<uint>(const uint* data, size_t n)  { return put_template(data,n) ; }  
    template <> bool CBufferOut::put<ushort>(const ushort* data, size_t n) { return put_template(data,n) ; }  
    template <> bool CBufferOut::put<ulong>(const ulong* data, size_t n) { return put_template(data,n) ; }  
    template <> bool CBufferOut::put<float>(const float* data, size_t n) { return put_template(data,n) ; }  
    template <> bool CBufferOut::put<double>(const double* data, size_t n) { return put_template(data,n) ; }  
    template <> bool CBufferOut::put<long double>(const long double* data, size_t n) { return put_template(data,n) ;}  


    template <> bool CBufferOut::advance<char>(size_t n) { return advance_template<char>(n) ; }  
    template <> bool CBufferOut::advance<bool>(size_t n) { return advance_template<bool>(n) ; }  
    template <> bool CBufferOut::advance<int>(size_t n)   { return advance_template<int>(n) ; }
    template <> bool CBufferOut::advance<short>(size_t n) { return advance_template<short>(n) ; }  
    template <> bool CBufferOut::advance<long>(size_t n)  { return advance_template<long>(n) ; }  
    template <> bool CBufferOut::advance<uint>(size_t n)  { return advance_template<uint>(n) ; }  
    template <> bool CBufferOut::advance<ushort>(size_t n) { return advance_template<ushort>(n) ; }  
    template <> bool CBufferOut::advance<ulong>(size_t n) { return advance_template<ulong>(n) ; }  
    template <> bool CBufferOut::advance<float>(size_t n) { return advance_template<float>(n) ; }  
    template <> bool CBufferOut::advance<double>(size_t n) { return advance_template<double>(n) ; }  
    template <> bool CBufferOut::advance<long double>(size_t n) { return advance_template<long double>(n) ;}  

    template <class T>
    bool CBufferOut::put_template(const T& data)
    {
      return put_template<T>(&data,1);
    }
    
    template <class T>
    bool CBufferOut::put_template(const T* data, size_t n)
    {
      bool ret;
      char* dataBuff ;
  
      size_t dataSize=sizeof(T)*n ;
  
      if (count_+dataSize<=size_)
      {
        dataBuff=(char*) data ;
        for(size_t i=0;i<dataSize;i++) current[i]=dataBuff[i] ;
        current+=dataSize ;
        count_+=dataSize;
        ret=true ;
      }
      else ret=false ;
  
      return ret ;
    }

    template <class T>
    bool CBufferOut::advance_template(size_t n)
    {
      bool ret;
      char* dataBuff ;
  
      size_t dataSize=sizeof(T)*n ;
  
      if (count_+dataSize<=size_)
      {
        current+=dataSize ;
        count_+=dataSize;
        ret=true ;
      }
      else ret=false ;
  
      return ret ;
    }
    
}
  

#endif
