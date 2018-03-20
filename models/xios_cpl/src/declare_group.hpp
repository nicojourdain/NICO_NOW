#ifndef __XMLIO_DECLARE_GROUP__
#define __XMLIO_DECLARE_GROUP__

/// ///////////////////////////// Macros ///////////////////////////// ///

#define DECLARE_GROUP(type)                                           \
   class type##Group                                                  \
      : public CGroupTemplate<type, type##Group, type##Attributes>    \
   {                                                                  \
      public:                                                         \
         typedef type              RelChild;                          \
         typedef type##Group       RelGroup;                          \
         typedef type##Attributes  RelAttributes;                     \
                                                                      \
         type##Group(void)                                            \
            : CGroupTemplate<RelChild, RelGroup, RelAttributes> ()    \
         { /* Ne rien faire de plus */ }                              \
         type##Group(const StdString& _id)                            \
            : CGroupTemplate<RelChild, RelGroup, RelAttributes> (_id) \
         { /* Ne rien faire de plus */ }                              \
                                                                      \
         static ENodeType GetType(void)                               \
         { return static_cast<ENodeType>(RelChild::GetType()+1); }    \
                                                                      \
         virtual ~type##Group(void)                                   \
         { /* Ne rien faire de plus */ }                              \
   };                                                                 \
   typedef type##Group type##Definition
   
#define DECLARE_GROUP_PARSE_REDEF(type)                                  \
   class type##Group                                                     \
      : public CGroupTemplate<type, type##Group, type##Attributes>       \
   {                                                                     \
      public:                                                            \
         typedef type              RelChild;                             \
         typedef type##Group       RelGroup;                             \
         typedef type##Attributes  RelAttributes;                        \
         typedef CGroupTemplate<type, type##Group, type##Attributes>     \
                 SuperClass;                                             \
                                                                         \
         type##Group(void)                                               \
            : CGroupTemplate<RelChild, RelGroup, RelAttributes> ()       \
         { /* Ne rien faire de plus */ }                                 \
         type##Group(const StdString& _id)                               \
            : CGroupTemplate<RelChild, RelGroup, RelAttributes> (_id)    \
         { /* Ne rien faire de plus */ }                                 \
                                                                         \
         static ENodeType GetType(void)                                  \
         { return static_cast<ENodeType>(RelChild::GetType()+1); }       \
                                                                         \
         virtual void parse(xml::CXMLNode & node, bool withAttr = true); \
                                                                         \
         virtual ~type##Group(void)                                      \
         { /* Ne rien faire de plus */ }                                 \
   };                                                                    \
   typedef type##Group type##Definition

#endif // __XMLIO_DECLARE_GROUP__
