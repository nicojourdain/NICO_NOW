#ifndef __XMLIO_SPL__
#define __XMLIO_SPL__

/// standard C++ headers ///
#include <utility>
#include <string>
#include <algorithm>

// standard C
#include <cstring>
#include <cstdlib>
#include <cmath>
#include <ctime>

// Conteneurs.
#include <vector>
#include <set>
#include <stack>
#include <list>
#include <map>
#include <deque>
#include <valarray>
// Flux.
#include <iostream>
#include <fstream>
#include <sstream>

/// boost headers ///
//#include <boost/unordered_map.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/cast.hpp>
/// Map ///
#define xios_map std::map

/// Macro ///
#define UNUSED(parameter)

/// Définition de types (issus de la bibliothèque standard)///
typedef std::ostringstream StdOStringStream;
typedef std::istringstream StdIStringStream;
typedef std::ofstream      StdOFStream;
typedef std::ifstream      StdIFStream;
typedef std::ostream       StdOStream;
typedef std::istream       StdIStream;
typedef std::string        StdString;
typedef std::size_t        StdSize;

typedef  unsigned short int   ushort;
typedef  unsigned int         uint;
typedef  unsigned long int    ulong;

/// xios headers ///
#include "configure.hpp"
#include "log.hpp"
using namespace std;
using namespace boost ;


#endif //__XMLIO_SPL__
