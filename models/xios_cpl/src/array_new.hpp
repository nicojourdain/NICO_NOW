#ifndef __XIOS_ARRAY_HPP__
#define __XIOS_ARRAY_HPP__

#define BZ_COLUMN_MAJOR_ARRAY
#include <blitz/array.h>
#include "buffer_in.hpp"
#include "buffer_out.hpp"
#include "message.hpp"

using namespace blitz ;
namespace xios
{

    
template <typename T_numtype,int N_rank>
class CArray : public Array<T_numtype,N_rank>, public virtual CBaseType
{
  public :
  using Array<T_numtype,N_rank>::operator = ;
  typedef typename Array<T_numtype,N_rank>::T_default_storage T_default_storage;
  
    template<typename T_expr> explicit CArray(_bz_ArrayExpr<T_expr> expr) : Array<T_numtype,N_rank>(expr) {} 

    CArray(GeneralArrayStorage<N_rank> storage=T_default_storage()) : Array<T_numtype,N_rank>(storage) {}

    explicit CArray(int length0,GeneralArrayStorage<N_rank> storage=T_default_storage() ) : Array<T_numtype,N_rank>(length0,storage ) {}

    CArray(int length0, int length1, GeneralArrayStorage<N_rank> storage=T_default_storage() ) : Array<T_numtype,N_rank>(length0, length1, storage ) {}

    CArray(int length0, int length1, int length2, GeneralArrayStorage<N_rank> storage=T_default_storage() ) : Array<T_numtype,N_rank>(length0, length1, length2, storage ) {}

    CArray(int length0, int length1, int length2, int length3, GeneralArrayStorage<N_rank> storage=T_default_storage() )
           : Array<T_numtype,N_rank>(length0, length1, length2, length3, storage ) {}
    
    CArray(int length0, int length1, int length2, int length3, int length4, GeneralArrayStorage<N_rank> storage=T_default_storage() )
           : Array<T_numtype,N_rank>(length0,length1, length2, length3, length4, storage ) {}

    CArray(int length0, int length1, int length2, int length3, int length4, int length5, GeneralArrayStorage<N_rank> storage=T_default_storage() )
           : Array<T_numtype,N_rank>(length0, length1, length2, length3, length4, length5, storage ) {}
        
    CArray(int length0, int length1, int length2, int length3, int length4, int length5, int length6, GeneralArrayStorage<N_rank> storage=T_default_storage() )
           : Array<T_numtype,N_rank>(length0, length1, length2, length3, length4, length5, length6, storage ) {}

    CArray(int length0, int length1, int length2, int length3, int length4, int length5, int length6, int length7,
           GeneralArrayStorage<N_rank> storage=T_default_storage() ) : Array<T_numtype,N_rank>(length0, length1, length2, length3, length4, length5, length6, length7, storage ) {}

    CArray(int length0, int length1, int length2, int length3, int length4, int length5, int length6,
           int length7, int length8, GeneralArrayStorage<N_rank> storage=T_default_storage() )
           : Array<T_numtype,N_rank>(length0, length1, length2, length3, length4, length5, length6, length7, length8, storage ) {}

    CArray(int length0, int length1, int length2, int length3, int length4,
           int length5, int length6, int length7, int length8, int length9, GeneralArrayStorage<N_rank> storage=T_default_storage() )
          : Array<T_numtype,N_rank>(length0, length1, length2, length3, length4, length5, length6, length7, length8, length9, storage ) {}
          
    CArray(int length0, int length1, int length2, int length3, int length4, int length5, int length6,
           int length7, int length8, int length9, int length10, GeneralArrayStorage<N_rank> storage=T_default_storage() )
           : Array<T_numtype,N_rank>(length0, length1, length2, length3, length4, length5, length6, length7, length8,
                   length9, length10, storage ) {}
           

    CArray(T_numtype* restrict dataFirst, TinyVector<int, N_rank> shape, GeneralArrayStorage<N_rank> storage=T_default_storage() )
          : Array<T_numtype,N_rank>(dataFirst, shape, storage ) {}

    CArray(T_numtype* restrict dataFirst, TinyVector<int, N_rank> shape, TinyVector<diffType, N_rank> stride, 
           GeneralArrayStorage<N_rank> storage=T_default_storage()) : Array<T_numtype,N_rank>(dataFirst, shape, stride, storage ) {}

    CArray(T_numtype* restrict dataFirst, TinyVector<int, N_rank> shape, preexistingMemoryPolicy deletionPolicy,
          GeneralArrayStorage<N_rank> storage=T_default_storage()) : Array<T_numtype,N_rank>(dataFirst, shape, deletionPolicy, storage) {}

    CArray(T_numtype* restrict dataFirst, TinyVector<int, N_rank> shape, TinyVector<diffType, N_rank> stride,
           preexistingMemoryPolicy deletionPolicy, GeneralArrayStorage<N_rank> storage=T_default_storage())
          : Array<T_numtype,N_rank>(dataFirst, shape, stride, deletionPolicy, storage) {}

    CArray(const TinyVector<int, N_rank>& extent, GeneralArrayStorage<N_rank> storage=T_default_storage())
          : Array<T_numtype,N_rank>(extent, storage) {}

    CArray(const TinyVector<int, N_rank>& lbounds, const TinyVector<int, N_rank>& extent,
           const GeneralArrayStorage<N_rank>& storage ) : Array<T_numtype,N_rank>(lbounds, extent, storage ) {} 

    CArray(Range r0, GeneralArrayStorage<N_rank> storage=T_default_storage() ) : CArray(r0, storage ) {}

    CArray(Range r0, Range r1, GeneralArrayStorage<N_rank> storage=T_default_storage() ) : Array<T_numtype,N_rank>(r0, r1, storage ) {}

    CArray(Range r0, Range r1, Range r2, GeneralArrayStorage<N_rank> storage=T_default_storage() ) 
           : Array<T_numtype,N_rank>(r0, r1, r2, storage ) {}

    CArray(Range r0, Range r1, Range r2, Range r3, GeneralArrayStorage<N_rank> storage=T_default_storage() )
          : Array<T_numtype,N_rank>(r0, r1, r2, r3, storage ) {}

    CArray(Range r0, Range r1, Range r2, Range r3, Range r4, GeneralArrayStorage<N_rank> storage=T_default_storage() )
          : Array<T_numtype,N_rank>(r0, r1, r2, r3, r4, storage ) {}

    CArray(Range r0, Range r1, Range r2, Range r3, Range r4, Range r5, GeneralArrayStorage<N_rank> storage=T_default_storage() )
          : Array<T_numtype,N_rank>(r0, r1, r2, r3, r4, r5, storage ) {}

    CArray(Range r0, Range r1, Range r2, Range r3, Range r4, Range r5, Range r6,
           GeneralArrayStorage<N_rank> storage=T_default_storage() ) : Array<T_numtype,N_rank>(r0, r1, r2, r3, r4, r5, r6, storage ) {}

    CArray(Range r0, Range r1, Range r2, Range r3, Range r4, Range r5, Range r6, Range r7,
           GeneralArrayStorage<N_rank> storage=T_default_storage() ) : Array<T_numtype,N_rank>(r0, r1, r2, r3, r4, r5, r6, r7, storage ) {}

    CArray(Range r0, Range r1, Range r2, Range r3, Range r4, Range r5,
           Range r6, Range r7, Range r8, GeneralArrayStorage<N_rank> storage=T_default_storage() )
          : Array<T_numtype,N_rank>(r0, r1, r2, r3, r4, r5, r6, r7, r8, storage ) {}

    CArray(Range r0, Range r1, Range r2, Range r3, Range r4, Range r5,
           Range r6, Range r7, Range r8, Range r9, GeneralArrayStorage<N_rank> storage=T_default_storage() )
          : Array<T_numtype,N_rank>(r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, storage ) {}

    CArray(Range r0, Range r1, Range r2, Range r3, Range r4, Range r5, Range r6, Range r7,
           Range r8, Range r9, Range r10, GeneralArrayStorage<N_rank> storage=T_default_storage() )
          : Array<T_numtype,N_rank>(r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, storage ) {} 

    CArray(const Array<T_numtype, N_rank>& array) : Array<T_numtype,N_rank>(array) {}

    CArray(const TinyVector<int,N_rank-1>& shape, int lastExtent, const GeneralArrayStorage<N_rank>& storage)
          : Array<T_numtype,N_rank>(shape, lastExtent, storage) {}

    CArray(Array<T_numtype, N_rank>& array, Range r0) : Array<T_numtype,N_rank>(array, r0) {}

    CArray(Array<T_numtype, N_rank>& array, Range r0, Range r1) : Array<T_numtype,N_rank>(array, r0, r1) {}

    CArray(Array<T_numtype, N_rank>& array, Range r0, Range r1, Range r2) : Array<T_numtype,N_rank>( array, r0, r1, r2) {}

    CArray(Array<T_numtype, N_rank>& array, Range r0, Range r1, Range r2, Range r3) : Array<T_numtype,N_rank>(array, r0, r1, r2, r3) {}

    CArray(Array<T_numtype, N_rank>& array, Range r0, Range r1, Range r2,
           Range r3, Range r4) : Array<T_numtype,N_rank>(array, r0, r1, r2, r3,  r4) {}

    CArray(Array<T_numtype, N_rank>& array, Range r0, Range r1, Range r2,
           Range r3, Range r4, Range r5) : Array<T_numtype,N_rank>( array, r0, r1, r2, r3, r4, r5) {}

    CArray(Array<T_numtype, N_rank>& array, Range r0, Range r1, Range r2, Range r3, 
           Range r4, Range r5, Range r6) : Array<T_numtype,N_rank>( array, r0, r1, r2, r3, r4, r5, r6) {}

    CArray(Array<T_numtype, N_rank>& array, Range r0, Range r1, Range r2, Range r3, Range r4,
           Range r5, Range r6, Range r7) : Array<T_numtype,N_rank>(array, r0, r1, r2, r3, r4, r5, r6, r7) {}

    CArray(Array<T_numtype, N_rank>& array, Range r0, Range r1, Range r2, Range r3, Range r4, Range r5,
           Range r6, Range r7, Range r8) : Array<T_numtype,N_rank>(array, r0, r1, r2, r3, r4, r5, r6, r7, r8) {}

    CArray(Array<T_numtype, N_rank>& array, Range r0, Range r1, Range r2, Range r3, Range r4, Range r5,
           Range r6, Range r7, Range r8, Range r9) : Array<T_numtype,N_rank>(array, r0, r1, r2, r3, r4, r5, r6, r7, r8, r9) {}

    CArray(Array<T_numtype, N_rank>& array, Range r0, Range r1, Range r2, Range r3, Range r4, Range r5, Range r6,
           Range r7, Range r8, Range r9, Range r10) : Array<T_numtype,N_rank>(array, r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10) {}

    CArray(Array<T_numtype, N_rank>& array, const RectDomain<N_rank>& subdomain) : Array<T_numtype,N_rank>(array, subdomain) {}

    CArray(Array<T_numtype, N_rank>& array, const StridedDomain<N_rank>& subdomain) : Array<T_numtype,N_rank>(array, subdomain) {} 

    template<int N_rank2, typename R0, typename R1, typename R2, typename R3, typename R4, typename R5,
                          typename R6, typename R7, typename R8, typename R9, typename R10>
    CArray(Array<T_numtype,N_rank2>& array, R0 r0, R1 r1, R2 r2, R3 r3, R4 r4, R5 r5, R6 r6, R7 r7, R8 r8, R9 r9, R10 r10)
          : Array<T_numtype,N_rank>(array, r0,r1, r2, r3, r4, r5, r6, r7, r8, r9, r10) {}

    virtual ~CArray() {}
    virtual void fromString(const string& str) { istringstream iss(str) ;  iss>>*this ; }
    virtual string toString(void) const { ostringstream oss ; oss<<*this ; return oss.str() ; }
    virtual void reset(void) { this->free(); }
    virtual bool isEmpty(void) const { if (this->numElements()==0) return true; else return false; }
    virtual size_t size(void) const { return (this->dimensions()+1)*sizeof(int)+sizeof(size_t) + this->numElements()*sizeof(T_numtype) ;}
 
    virtual CBaseType* clone(void) const { return new CArray(*this); }
       
    virtual bool toBuffer(CBufferOut& buffer) const
    {
      bool ret ;
      ret=buffer.put(this->dimensions()) ;
      ret&=buffer.put(this->shape().data(),this->dimensions()) ;
      ret&=buffer.put(this->numElements()) ;
      ret&=buffer.put(this->dataFirst(),this->numElements()) ;
      return ret ;
    }
    
   
   virtual bool fromBuffer(CBufferIn& buffer)
   {

    bool ret ;
    int numDim ;
    TinyVector<int,N_rank> vect;
    size_t ne;
      
    ret=buffer.get(numDim) ;
    ret&=buffer.get(vect.data(),N_rank) ;
    this->resize(vect) ;
    ret&=buffer.get(ne) ;
    ret&=buffer.get(this->dataFirst(),ne) ;
     return ret ;
  }

} ;
 
  template <typename T_numtype,int N_rank> inline CBufferOut& operator<<(CBufferOut& buffer, const CArray<T_numtype,N_rank>& array)
  {
    if (!array.toBuffer(buffer)) ERROR(" template <typename T_numtype,int N_rank> inline CBufferOut& operator<<(CBufferOut& buffer, const CArray& array)",
                                        << "Buffer remain size is to low for size type") ;
    return buffer ;
  }

  template <typename T_numtype,int N_rank> inline CBufferIn& operator>>(CBufferIn& buffer, CArray<T_numtype, N_rank>& array)
  {
    if (!array.fromBuffer(buffer)) ERROR("template <typename T_numtype,int N_rank> inline CBufferIn& operator>>(CBufferIn& buffer, CArray& array)",
                                        <<"Buffer remain size is to low for size type");
    return buffer ;
  }
 
  template <typename T_numtype,int N_rank> inline CMessage& operator<<(CMessage& msg, const CArray<T_numtype, N_rank>& array)
  {
    msg.push(array) ;
    return msg ;
  }

  template <typename T_numtype,int N_rank> inline CMessage& operator<<(CMessage& msg, CArray<T_numtype, N_rank>& array)
  {
    msg.push(array) ;
    return msg ;
  }

 
}

#endif
