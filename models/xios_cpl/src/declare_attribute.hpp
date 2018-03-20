#ifndef __XMLIO_DECLARE_ATTRIBUTE__
#define __XMLIO_DECLARE_ATTRIBUTE__

/// ///////////////////////////// Macros ///////////////////////////// ///

#define DECLARE_ATTRIBUTE(type, name)                             \
   class name##_attr : public CAttributeTemplate<type>            \
   {                                                              \
      public :                                                    \
         name##_attr(void)                                          \
            : CAttributeTemplate<type>                            \
            (#name, *CAttributeMap::Current)                      \
         { /* Ne rien faire de plus */ }                          \
         type operator=(const type & value)                       \
         { return (CAttributeTemplate<type>::operator=(value)); } \
         virtual ~name##_attr(void)                                 \
         { /* Ne rien faire de plus */ }                          \
   } name;

#define DECLARE_ARRAY(T_num, T_rank, name)                        \
   class name##_attr : public CAttributeArray<T_num, T_rank>      \
   {                                                              \
      public :                                                    \
         using CAttributeArray<T_num, T_rank>::operator = ;       \
         name##_attr(void) : CAttributeArray<T_num, T_rank> (#name, *CAttributeMap::Current) {} \
         virtual ~name##_attr(void) {}                            \
   } name;
   
#define DECLARE_CLASS_ENUM(name)                                   \
   class name##_attr : public CAttributeEnum<Enum_##name>          \
   {                                                              \
      public :                                                    \
         name##_attr(void) : CAttributeEnum<Enum_##name>(#name, *CAttributeMap::Current) { } \
         virtual ~name##_attr(void) {}                           \
   } name;
   
#define DECLARE_ENUM2(name,arg1,arg2)                             \
   class Enum_##name                                              \
   {                                                              \
     public:                                                      \
     enum t_enum { arg1=0, arg2} ;                                \
     const char** getStr(void) const { static const char * enumStr[] = { #arg1, #arg2 } ; return enumStr ; }   \
     int getSize(void) const { return 2 ; }                       \
   } ;                                                            \
   DECLARE_CLASS_ENUM(name)
   
#define DECLARE_ENUM3(name,arg1,arg2,arg3)                             \
   class Enum_##name                                              \
   {                                                              \
     public:                                                      \
     enum t_enum { arg1=0, arg2, arg3} ;                                \
     const char** getStr(void) const { static const char * enumStr[] = { #arg1, #arg2, #arg3 } ; return enumStr ; }   \
     int getSize(void) const { return 3 ; }                       \
   } ;                                                            \
   DECLARE_CLASS_ENUM(name)

#define DECLARE_ENUM4(name,arg1,arg2,arg3,arg4)                             \
   class Enum_##name                                              \
   {                                                              \
     public:                                                      \
     enum t_enum { arg1=0, arg2, arg3,arg4} ;                                \
     const char** getStr(void) const { static const char * enumStr[] = { #arg1, #arg2, #arg3,#arg4 } ; return enumStr ; }   \
     int getSize(void) const { return 4 ; }                       \
   } ;                                                            \
   DECLARE_CLASS_ENUM(name)

#define DECLARE_ENUM5(name,arg1,arg2,arg3,arg4,arg5)                             \
   class Enum_##name                                              \
   {                                                              \
     public:                                                      \
     enum t_enum { arg1=0, arg2, arg3,arg4,arg5} ;                                \
     const char** getStr(void) const { static const char * enumStr[] = { #arg1, #arg2, #arg3,#arg4,#arg5 } ; return enumStr ; }   \
     int getSize(void) const { return 5 ; }                       \
   } ;                                                            \
   DECLARE_CLASS_ENUM(name)

#define DECLARE_ENUM6(name,arg1,arg2,arg3,arg4,arg5,arg6)                             \
   class Enum_##name                                              \
   {                                                              \
     public:                                                      \
     enum t_enum { arg1=0, arg2, arg3,arg4,arg5,arg6} ;                                \
     const char** getStr(void) const { static const char * enumStr[] = { #arg1, #arg2, #arg3,#arg4,#arg5,#arg6 } ; return enumStr ; }   \
     int getSize(void) const { return 6 ; }                       \
   } ;                                                            \
   DECLARE_CLASS_ENUM(name)

#define DECLARE_ENUM7(name,arg1,arg2,arg3,arg4,arg5,arg6,arg7)                             \
   class Enum_##name                                              \
   {                                                              \
     public:                                                      \
     enum t_enum { arg1=0, arg2, arg3,arg4,arg5,arg6,arg7} ;                                \
     const char** getStr(void) const { static const char * enumStr[] = { #arg1, #arg2, #arg3,#arg4,#arg5,#arg6,#arg7 } ; return enumStr ; }   \
     int getSize(void) const { return 7 ; }                       \
   } ;                                                            \
   DECLARE_CLASS_ENUM(name)

#define DECLARE_ENUM8(name,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8)                             \
   class Enum_##name                                              \
   {                                                              \
     public:                                                      \
     enum t_enum { arg1=0, arg2, arg3,arg4,arg5,arg6,arg7,arg8} ;                                \
     const char** getStr(void) const { static const char * enumStr[] = { #arg1, #arg2, #arg3,#arg4,#arg5,#arg6,#arg7,#arg8 } ; return enumStr ; }   \
     int getSize(void) const { return 8 ; }                       \
   } ;                                                            \
   DECLARE_CLASS_ENUM(name)

  #define DECLARE_ENUM9(name,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)                             \
   class Enum_##name                                              \
   {                                                              \
     public:                                                      \
     enum t_enum { arg1=0, arg2, arg3,arg4,arg5,arg6,arg7,arg8,arg9} ;                                \
     const char** getStr(void) const { static const char * enumStr[] = { #arg1, #arg2, #arg3,#arg4,#arg5,#arg6,#arg7,#arg8,#arg9 } ; return enumStr ; }   \
     int getSize(void) const { return 8 ; }                       \
   } ;                                                            \
   DECLARE_CLASS_ENUM(name) 


#define BEGIN_DECLARE_ATTRIBUTE_MAP(type)                  \
   class type##Attributes : public virtual CAttributeMap   \
   {                                                       \
      public :

#define END_DECLARE_ATTRIBUTE_MAP(type)            \
         type##Attributes (void) : CAttributeMap() \
         { /* Ne rien faire de plus */ }           \
         virtual ~type##Attributes (void)          \
         { /* Ne rien faire de plus */ }           \
   };

#endif // __XMLIO_DECLARE_ATTRIBUTE__
