#ifndef __XIOS_ENUM__
#define __XIOS_ENUM__

#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"
#include "base_type.hpp"
#include "message.hpp"

#define __INLINE__ inline


namespace xios
{
  class CEnumBase
  {
  } ;
  
  template <typename T> class CEnum_ref ;
 
  template <typename T> 
  class CEnum : public  virtual CBaseType, public T
  {
    public:
    typedef typename T::t_enum T_enum ;
    CEnum(void) ;
    __INLINE__ CEnum(const T_enum& val) ;
    __INLINE__ CEnum(const CEnum& type) ;
    __INLINE__ CEnum(const CEnum_ref<T>& type) ;
    virtual ~CEnum() { _reset() ; }

    __INLINE__ T_enum& get(void) ;
    __INLINE__ const T_enum& get(void) const;

    __INLINE__ void set(const T_enum& val) ;
    __INLINE__ void set(const CEnum& val) ;
    __INLINE__ void set(const CEnum_ref<T>& val) ;
    __INLINE__ CEnum& operator = (const T_enum& val) ;
    __INLINE__ CEnum& operator = (const CEnum& val) ;
    __INLINE__ CEnum& operator = (const CEnum_ref<T>& val) ;
    __INLINE__ operator T_enum&() ;
        
    inline virtual CBaseType* clone(void) const   { return _clone(); }
    virtual void fromString(const string& str)   { _fromString(str); }
    virtual string toString(void) const { return _toString(); }
    virtual bool fromBuffer(CBufferIn& buffer) { return _fromBuffer(buffer) ; }
    virtual bool toBuffer(CBufferOut& buffer) const { return _toBuffer(buffer); }
    virtual void reset(void) { _reset(); }
    virtual bool isEmpty() const { return _isEmpty(); } 
    virtual size_t size(void) const { return _size(); }
    
    __INLINE__ void allocate(void) ;
    __INLINE__ void checkEmpty(void) const;
       
    T_enum* ptrValue ;
    bool empty ;
      
    friend class CEnum_ref<T> ; 
 
    private :
 
    __INLINE__ CEnum* _clone(void) const;
    __INLINE__ void _fromString(const string& str) ;
    __INLINE__ string _toString(void) const;
    __INLINE__ bool _fromBuffer(CBufferIn& buffer) ;
    __INLINE__ bool _toBuffer(CBufferOut& buffer) const;
    __INLINE__ void _reset(void) ;
    __INLINE__ bool _isEmpty() const ;  
    __INLINE__ size_t _size(void) const ;
 
  } ;
  
  
  
  template <typename T> 
  class CEnum_ref : public  virtual CBaseType, public T
  {
    public:
    
    typedef typename T::t_enum T_enum ;
    __INLINE__ CEnum_ref(void) ;
    __INLINE__ CEnum_ref(T_enum& val) ;
    __INLINE__ CEnum_ref(CEnum<T>& type) ;
    __INLINE__ CEnum_ref(const CEnum_ref& type) ;
    virtual ~CEnum_ref() {};

    __INLINE__ T_enum& get(void) const;

    __INLINE__ void set(const T_enum& val) const ;
    __INLINE__ void set(const CEnum<T>& val) const ;
    __INLINE__ void set(const CEnum_ref& val) const ;

    __INLINE__ void set_ref(T_enum& val) ;
    __INLINE__ void set_ref(CEnum<T>& val) ;
    __INLINE__ void set_ref(const CEnum_ref& val) ;
    
    __INLINE__ const CEnum_ref& operator = (T_enum& val) const ;
    __INLINE__ const CEnum_ref& operator = (CEnum<T>& val) const ;
    __INLINE__ const CEnum_ref& operator = (const CEnum_ref& val) const;
    __INLINE__ operator T_enum&() const;
    bool operator == (const CEnum_ref &other) {return this->get()==other.get() ;}

    inline virtual CBaseType* clone(void) const   { return _clone(); }
    virtual void fromString(const string& str)   { _fromString(str); }
    virtual void fromString(const string& str) const  { _fromString(str); }
    virtual string toString(void) const { return _toString(); }
    virtual bool fromBuffer(CBufferIn& buffer) { return _fromBuffer(buffer) ; }
    virtual bool fromBuffer(CBufferIn& buffer) const { return _fromBuffer(buffer); }
    virtual bool toBuffer(CBufferOut& buffer) const { return _toBuffer(buffer); }
    virtual void reset(void) { _reset(); }
    virtual bool isEmpty() const { return _isEmpty(); } 
    virtual size_t size(void) const { return _size(); }

    __INLINE__ void checkEmpty(void) const;
    

    T_enum mutable * ptrValue ;
    bool empty ;
    friend class CEnum<T> ;
    
    private :
    
    __INLINE__ CEnum_ref* _clone(void) const;
    __INLINE__ void _fromString(const string& str) ;
    __INLINE__ void _fromString(const string& str) const;
    __INLINE__ string _toString(void) const;
    __INLINE__ bool _fromBuffer(CBufferIn& buffer) ;
    __INLINE__ bool _fromBuffer(CBufferIn& buffer) const ;
    __INLINE__ bool _toBuffer(CBufferOut& buffer) const;
    __INLINE__ void _reset(void) ;
    __INLINE__ bool _isEmpty() const ;  
    __INLINE__  size_t _size(void) const ;
  } ;
  
  template <typename T> __INLINE__ CBufferOut& operator<<(CBufferOut& buffer, const CEnum<T>& type) ;
  template <typename T> __INLINE__ CBufferOut& operator<<(CBufferOut& buffer, const typename T::t_enum & type) ;  
  template <typename T> __INLINE__ CBufferIn& operator>>(CBufferIn& buffer, CEnum<T>& type) ;
//  template <typename T> __INLINE__ CMessage& operator<<(CMessage& msg, const CEnum<T>& type) ;
  template <typename T> __INLINE__ CMessage& operator<<(CMessage& msg, const typename T::t_enum & type) ;

  template <typename T> __INLINE__ CBufferOut& operator<<(CBufferOut& buffer, const CEnum<T>& type) ;
  template <typename T> __INLINE__ CBufferOut& operator<<(CBufferOut& buffer, const typename T::t_enum & type);
  template <typename T> __INLINE__ CBufferIn& operator>>(CBufferIn& buffer, CEnum<T>& type);
//  template <typename T> __INLINE__ CMessage& operator<<(CMessage& msg, const CEnum<T>& type);
  template <typename T> __INLINE__ CMessage& operator<<(CMessage& msg, const typename T::t_enum & type);
}

#  ifdef __INLINE__
#    include "enum_impl.hpp"
#    include "enum_ref_impl.hpp"
#  endif

#endif
