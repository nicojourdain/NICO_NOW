#ifndef __XIOS_TYPE__
#define __XIOS_TYPE__

#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"
#include "base_type.hpp"


namespace xios
{

  template <typename T> class CType_ref ;
    
  template <typename T> 
  class CType : public  virtual CBaseType
  {
    public:
  
    CType(void) ;
    CType(const T& val) ;
    CType(const CType& type) ;
    CType(const CType_ref<T>& type) ;
    virtual ~CType() { _reset() ; }

    T& get(void) ;
    const T& get(void) const;

    void set(const T& val) ;
    void set(const CType& val) ;
    void set(const CType_ref<T>& val) ;
    CType& operator = (const T& val) ;
    CType& operator = (const CType& val) ;
    CType& operator = (const CType_ref<T>& val) ;
    operator T&() ;
    
    inline virtual CBaseType* clone(void) const   { return _clone(); }
    virtual void fromString(const string& str)   { _fromString(str); }
    virtual string toString(void) const { return _toString(); }
    virtual bool fromBuffer(CBufferIn& buffer) { return _fromBuffer(buffer) ; }
    virtual bool toBuffer(CBufferOut& buffer) const { return _toBuffer(buffer); }
    virtual void reset(void) { _reset(); }
    virtual bool isEmpty() const { return _isEmpty(); } 
    virtual size_t size(void) const { return _size(); }
    
    void allocate(void) ;
    void checkEmpty(void) const;
       
    T* ptrValue ;
    bool empty ;
      
    friend class CType_ref<T> ; 
 
    private :
 
    CType* _clone(void) const;
    void _fromString(const string& str) ;
    string _toString(void) const;
    bool _fromBuffer(CBufferIn& buffer) ;
    bool _toBuffer(CBufferOut& buffer) const;
    void _reset(void) ;
    bool _isEmpty() const ;  
    size_t _size(void) const ;
 
  } ;


  template<typename T> class CType ;
    
  template <typename T> 
  class CType_ref : public virtual CBaseType
  {
    public:
  
    CType_ref(void) ;
    CType_ref(T& val) ;
    CType_ref(CType<T>& type) ;
    CType_ref(const CType_ref& type) ;
    virtual ~CType_ref() {};

    T& get(void) const;

    void set(const T& val) const ;
    void set(const CType<T>& val) const ;
    void set(const CType_ref& val) const ;

    void set_ref(T& val) ;
    void set_ref(CType<T>& val) ;
    void set_ref(const CType_ref& val) ;
    
    const CType_ref& operator = (T& val) const ;
    const CType_ref& operator = (CType<T>& val) const ;
    const CType_ref& operator = (const CType_ref& val) const;
    operator T&() const;

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

    void checkEmpty(void) const;
    

    T mutable * ptrValue ;
    bool empty ;
    friend class CType<T> ;
    
    private :
    
    CType_ref* _clone(void) const;
    void _fromString(const string& str) ;
    void _fromString(const string& str) const;
    string _toString(void) const;
    bool _fromBuffer(CBufferIn& buffer) ;
    bool _fromBuffer(CBufferIn& buffer) const ;
    bool _toBuffer(CBufferOut& buffer) const;
    void _reset(void) ;
    bool _isEmpty() const ;  
    size_t _size(void) const ;
  } ;
  
  
  class CMessage ;
  
  template <typename T>
  CBufferOut& operator<<(CBufferOut& buffer, const CType<T>& type) ;

  template <typename T>
  CBufferOut& operator<<(CBufferOut& buffer, const CType_ref<T>& type) ;
  
  template <typename T>
  CBufferOut& operator<<(CBufferOut& buffer, const T& type) ;
  
  template <typename T>
  CBufferOut& operator<<(CBufferOut& buffer, T& type) ;


  template <typename T>
  CBufferIn& operator>>(CBufferIn& buffer, CType<T>& type) ;

  template <typename T>
  CBufferIn& operator>>(CBufferIn& buffer, const CType_ref<T>& type) ;

  template <typename T>
  CBufferIn& operator>>(CBufferIn& buffer, T& type) ;

/*
  template <typename T>
  CMessage& operator<<(CMessage& msg, const CType<T>& type) ;

  template <typename T>
  CMessage& operator<<(CMessage& msg, const CType_ref<T>& type) ;
*/  
  
  template <typename T>
  CMessage& operator<<(CMessage& msg, const T& type) ;

  template <typename T>
  CMessage& operator<<(CMessage& msg, T& type) ;

}


//#include "test_type_impl.hpp"
//#include "test_type_specialisation.hpp"

#endif

