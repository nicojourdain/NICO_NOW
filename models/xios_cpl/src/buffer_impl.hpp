#ifndef __BUFFER_IMPL_HPP__
#define __BUFFER_IMPL_HPP__

namespace xios
{

// template spectialisation : CBuffer::put
    template <> bool CBuffer::put<char>(const char& data) { return put_template(data) ; }  
    template <> bool CBuffer::put<int>(const int& data)   { return put_template(data) ; }
    template <> bool CBuffer::put<short>(const short& data) { return put_template(data) ; }  
    template <> bool CBuffer::put<long>(const long& data)  { return put_template(data) ; }  
    template <> bool CBuffer::put<uint>(const uint& data)  { return put_template(data) ; }  
    template <> bool CBuffer::put<ushort>(const ushort& data) { return put_template(data) ; }  
    template <> bool CBuffer::put<ulong>(const ulong& data) { return put_template(data) ; }  
    template <> bool CBuffer::put<float>(const float& data) { return put_template(data) ; }  
    template <> bool CBuffer::put<double>(const double& data) { return put_template(data) ; }  
    template <> bool CBuffer::put<long double>(const long double& data) { return put_template(data) ;}  

    template <> bool CBuffer::put<char>(const char* data, size_t n) { return put_template(data,n) ; }  
    template <> bool CBuffer::put<int>(const int* data, size_t n)   { return put_template(data,n) ; }
    template <> bool CBuffer::put<short>(const short* data, size_t n) { return put_template(data,n) ; }  
    template <> bool CBuffer::put<long>(const long* data, size_t n)  { return put_template(data,n) ; }  
    template <> bool CBuffer::put<uint>(const uint* data, size_t n)  { return put_template(data,n) ; }  
    template <> bool CBuffer::put<ushort>(const ushort* data, size_t n) { return put_template(data,n) ; }  
    template <> bool CBuffer::put<ulong>(const ulong* data, size_t n) { return put_template(data,n) ; }  
    template <> bool CBuffer::put<float>(const float* data, size_t n) { return put_template(data,n) ; }  
    template <> bool CBuffer::put<double>(const double* data, size_t n) { return put_template(data,n) ; }  
    template <> bool CBuffer::put<long double>(const long double* data, size_t n) { return put_template(data,n) ;}  


    template <> bool CBuffer::put_ptr<char>(const char*& data, size_t n) { return put_ptr_template(data,n) ; }  
    template <> bool CBuffer::put_ptr<int>(const int*& data, size_t n)   { return put_ptr_template(data,n) ; }
    template <> bool CBuffer::put_ptr<short>(const short*& data, size_t n) { return put_ptr_template(data,n) ; }  
    template <> bool CBuffer::put_ptr<long>(const long*& data, size_t n)  { return put_ptr_template(data,n) ; }  
    template <> bool CBuffer::put_ptr<uint>(const uint*& data, size_t n)  { return put_ptr_template(data,n) ; }  
    template <> bool CBuffer::put_ptr<ushort>(const ushort*& data, size_t n) { return put_ptr_template(data,n) ; }  
    template <> bool CBuffer::put_ptr<ulong>(const ulong*& data, size_t n) { return put_ptr_template(data,n) ; }  
    template <> bool CBuffer::put_ptr<float>(const float*& data, size_t n) { return put_ptr_template(data,n) ; }  
    template <> bool CBuffer::put_ptr<double>(const double*& data, size_t n) { return put_ptr_template(data,n) ; }  
    template <> bool CBuffer::put_ptr<long double>(const long double*& data, size_t n) { return put_ptr_template(data,n) ;}  


// template spectialisation : CBuffer::get
    template <> bool CBuffer::get<char>(char& data) { return get_template(data) ; }  
    template <> bool CBuffer::get<int>(int& data)   { return get_template(data) ; }
    template <> bool CBuffer::get<short>(short& data) { return get_template(data) ; }  
    template <> bool CBuffer::get<long>(long& data)  { return get_template(data) ; }  
    template <> bool CBuffer::get<uint>(uint& data)  { return get_template(data) ; }  
    template <> bool CBuffer::get<ushort>(ushort& data) { return get_template(data) ; }  
    template <> bool CBuffer::get<ulong>(ulong& data) { return get_template(data) ; }  
    template <> bool CBuffer::get<float>(float& data) { return get_template(data) ; }  
    template <> bool CBuffer::get<double>(double& data) { return get_template(data) ; }  
    template <> bool CBuffer::get<long double>(long double& data) { return get_template(data) ;}  

    template <> bool CBuffer::get<char>(char* data, size_t n) { return get_template(data,n) ; }  
    template <> bool CBuffer::get<int>(int* data, size_t n)   { return get_template(data,n) ; }
    template <> bool CBuffer::get<short>(short* data, size_t n) { return get_template(data,n) ; }  
    template <> bool CBuffer::get<long>(long* data, size_t n)  { return get_template(data,n) ; }  
    template <> bool CBuffer::get<uint>(uint* data, size_t n)  { return get_template(data,n) ; }  
    template <> bool CBuffer::get<ushort>(ushort* data, size_t n) { return get_template(data,n) ; }  
    template <> bool CBuffer::get<ulong>(ulong* data, size_t n) { return get_template(data,n) ; }  
    template <> bool CBuffer::get<float>(float* data, size_t n) { return get_template(data,n) ; }  
    template <> bool CBuffer::get<double>(double* data, size_t n) { return get_template(data,n) ; }  
    template <> bool CBuffer::get<long double>(long double* data, size_t n) { return get_template(data,n) ;}  

    template <> bool CBuffer::get_ptr<char>(char*& data, size_t n) { return get_ptr_template(data,n) ; }  
    template <> bool CBuffer::get_ptr<int>(int*& data, size_t n)   { return get_ptr_template(data,n) ; }
    template <> bool CBuffer::get_ptr<short>(short*& data, size_t n) { return get_ptr_template(data,n) ; }  
    template <> bool CBuffer::get_ptr<long>(long*& data, size_t n)  { return get_ptr_template(data,n) ; }  
    template <> bool CBuffer::get_ptr<uint>(uint*& data, size_t n)  { return get_ptr_template(data,n) ; }  
    template <> bool CBuffer::get_ptr<ushort>(ushort*& data, size_t n) { return get_ptr_template(data,n) ; }  
    template <> bool CBuffer::get_ptr<ulong>(ulong*& data, size_t n) { return get_ptr_template(data,n) ; }  
    template <> bool CBuffer::get_ptr<float>(float*& data, size_t n) { return get_ptr_template(data,n) ; }  
    template <> bool CBuffer::get_ptr<double>(double*& data, size_t n) { return get_ptr_template(data,n) ; }  
    template <> bool CBuffer::get_ptr<long double>(long double*& data, size_t n) { return get_ptr_template(data,n) ;}  
    
    
    template <class T>
    bool CBuffer::put_template(const T& data)
    {
      return put_template<T>(&data,1);
    }
    
    template <class T>
    bool CBuffer::put_template(const T* data, size_t n)
    {
      bool ret;
      char* dataBuff ;
  
      size_t dataSize=sizeof(T)*n ;
  
      if (count+dataSize<=size)
      {
        dataBuff=(char*) data ;
        for(size_t i=0;i<dataSize;i++) write[i]=dataBuff[i] ;
        write+=dataSize ;
        count+=dataSize;
        ret=true ;
      }
      else ret=false ;
  
      return ret ;
    }

    template <class T>
    bool CBuffer::put_ptr_template(const T*& data, size_t n)
    {
      bool ret;
      char* dataBuff ;
  
      size_t dataSize=sizeof(T)*n ;
  
      if (count+dataSize<=size)
      {
        data=(T*) write ;
        write+=dataSize ;
        count+=dataSize;
        ret=true ;
      }
      else ret=false ;
  
      return ret ;
    }


    template <class T>
    bool CBuffer::get_template(T& data)
    {
      return get_template<T>(&data,1) ;
      
    }
        
    template <class T>
    bool CBuffer::get_template(T* data, size_t n)
    {
      bool ret;
      char* dataBuff ;
  
      size_t dataSize=sizeof(T)*n ;
  
      if (read+dataSize<=buffer+size)
      {
        dataBuff=(char*) data ;
        for(size_t i=0;i<dataSize;i++) dataBuff[i]=read[i] ;
        read+=dataSize ;
        ret=true ;
      }
      else ret=false ;
  
      return ret ;
    }

    template <class T>
    bool CBuffer::get_ptr_template(T*& data, size_t n)
    {
      bool ret;
      char* dataBuff ;
  
      size_t dataSize=sizeof(T)*n ;
  
      if (read+dataSize<=buffer+size)
      {
        data=(T*) read ;
        read+=dataSize ;
        ret=true ;
      }
      else ret=false ;
  
      return ret ;
    }    

}
  

#endif
