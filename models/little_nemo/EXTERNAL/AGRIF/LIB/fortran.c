/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0

/* Substitute the variable and function names.  */
#define yyparse fortranparse
#define yylex   fortranlex
#define yyerror fortranerror
#define yylval  fortranlval
#define yychar  fortranchar
#define yydebug fortrandebug
#define yynerrs fortrannerrs


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     TOK_BINARY_OP = 258,
     TOK_NEQV = 259,
     TOK_EQV = 260,
     TOK_XOR = 261,
     TOK_OR = 262,
     TOK_AND = 263,
     TOK_NOT = 264,
     TOK_NE = 265,
     TOK_EQ = 266,
     TOK_GE = 267,
     TOK_LE = 268,
     TOK_GT = 269,
     TOK_LT = 270,
     TOK_UNARY_OP = 271,
     TOK_DSLASH = 272,
     TOK_SLASH = 273,
     TOK_DASTER = 274,
     TOK_SEP = 275,
     TOK_SEMICOLON = 276,
     TOK_NEXTLINE = 277,
     TOK_PARAMETER = 278,
     TOK_RESULT = 279,
     TOK_ONLY = 280,
     TOK_INCLUDE = 281,
     TOK_SUBROUTINE = 282,
     TOK_PROGRAM = 283,
     TOK_FUNCTION = 284,
     TOK_OMP = 285,
     TOK_DOLLAR = 286,
     TOK_FORMAT = 287,
     TOK_MAX = 288,
     TOK_TANH = 289,
     TOK_WHERE = 290,
     TOK_ELSEWHERE = 291,
     TOK_ENDWHERE = 292,
     TOK_MAXVAL = 293,
     TOK_TRIM = 294,
     TOK_SUM = 295,
     TOK_SQRT = 296,
     TOK_CASE = 297,
     TOK_SELECTCASE = 298,
     TOK_FILE = 299,
     TOK_END = 300,
     TOK_ERR = 301,
     TOK_DONOTTREAT = 302,
     TOK_ENDDONOTTREAT = 303,
     TOK_EXIST = 304,
     TOK_MIN = 305,
     TOK_FLOAT = 306,
     TOK_EXP = 307,
     TOK_COS = 308,
     TOK_COSH = 309,
     TOK_ACOS = 310,
     TOK_NINT = 311,
     TOK_CYCLE = 312,
     TOK_SIN = 313,
     TOK_SINH = 314,
     TOK_ASIN = 315,
     TOK_EQUIVALENCE = 316,
     TOK_BACKSPACE = 317,
     TOK_LOG = 318,
     TOK_TAN = 319,
     TOK_ATAN = 320,
     TOK_RECURSIVE = 321,
     TOK_ABS = 322,
     TOK_MOD = 323,
     TOK_SIGN = 324,
     TOK_MINLOC = 325,
     TOK_MAXLOC = 326,
     TOK_EXIT = 327,
     TOK_MINVAL = 328,
     TOK_PUBLIC = 329,
     TOK_PRIVATE = 330,
     TOK_ALLOCATABLE = 331,
     TOK_RETURN = 332,
     TOK_THEN = 333,
     TOK_ELSEIF = 334,
     TOK_ELSE = 335,
     TOK_ENDIF = 336,
     TOK_PRINT = 337,
     TOK_PLAINGOTO = 338,
     TOK_CONSTRUCTID = 339,
     TOK_LOGICALIF = 340,
     TOK_PLAINDO = 341,
     TOK_CONTAINS = 342,
     TOK_ENDDO = 343,
     TOK_MODULE = 344,
     TOK_ENDMODULE = 345,
     TOK_DOWHILE = 346,
     TOK_ALLOCATE = 347,
     TOK_OPEN = 348,
     TOK_CLOSE = 349,
     TOK_INQUIRE = 350,
     TOK_WRITE = 351,
     TOK_READ = 352,
     TOK_REWIND = 353,
     TOK_DEALLOCATE = 354,
     TOK_NULLIFY = 355,
     TOK_FIN = 356,
     TOK_DEBUT = 357,
     TOK_DIMENSION = 358,
     TOK_ENDSELECT = 359,
     TOK_EXTERNAL = 360,
     TOK_INTENT = 361,
     TOK_INTRINSIC = 362,
     TOK_NAMELIST = 363,
     TOK_CASEDEFAULT = 364,
     TOK_OPTIONAL = 365,
     TOK_POINTER = 366,
     TOK_CONTINUE = 367,
     TOK_SAVE = 368,
     TOK_TARGET = 369,
     TOK_QUOTE = 370,
     TOK_IMPLICIT = 371,
     TOK_NONE = 372,
     TOK_CALL = 373,
     TOK_STAT = 374,
     TOK_POINT_TO = 375,
     TOK_COMMON = 376,
     TOK_GLOBAL = 377,
     TOK_INTERFACE = 378,
     TOK_ENDINTERFACE = 379,
     TOK_LEFTAB = 380,
     TOK_RIGHTAB = 381,
     TOK_PAUSE = 382,
     TOK_PROCEDURE = 383,
     TOK_STOP = 384,
     TOK_NAMEEQ = 385,
     TOK_REAL8 = 386,
     TOK_OUT = 387,
     TOK_INOUT = 388,
     TOK_IN = 389,
     TOK_USE = 390,
     TOK_TRUE = 391,
     TOK_FALSE = 392,
     TOK_LABEL = 393,
     TOK_TYPE = 394,
     TOK_TYPEPAR = 395,
     TOK_ENDTYPE = 396,
     TOK_REAL = 397,
     TOK_INTEGER = 398,
     TOK_LOGICAL = 399,
     TOK_DOUBLEPRECISION = 400,
     TOK_DOUBLEREAL = 401,
     TOK_ENDSUBROUTINE = 402,
     TOK_ENDFUNCTION = 403,
     TOK_ENDPROGRAM = 404,
     TOK_ENDUNIT = 405,
     TOK_CHARACTER = 406,
     TOK_CHAR_CONSTANT = 407,
     TOK_CHAR_CUT = 408,
     TOK_DATA = 409,
     TOK_CHAR_INT = 410,
     TOK_CHAR_MESSAGE = 411,
     TOK_CSTREAL = 412,
     TOK_CSTREALDP = 413,
     TOK_CSTREALQP = 414,
     TOK_SFREAL = 415,
     TOK_COMPLEX = 416,
     TOK_DOUBLECOMPLEX = 417,
     TOK_NAME = 418,
     TOK_NAME_CHAR = 419,
     TOK_PROBTYPE = 420,
     TOK_INTERPTYPE = 421,
     TOK_VARTYPE = 422,
     TOK_BC = 423,
     TOK_OP = 424,
     TOK_CSTINT = 425,
     TOK_COMMENT = 426,
     TOK_FILENAME = 427
   };
#endif
/* Tokens.  */
#define TOK_BINARY_OP 258
#define TOK_NEQV 259
#define TOK_EQV 260
#define TOK_XOR 261
#define TOK_OR 262
#define TOK_AND 263
#define TOK_NOT 264
#define TOK_NE 265
#define TOK_EQ 266
#define TOK_GE 267
#define TOK_LE 268
#define TOK_GT 269
#define TOK_LT 270
#define TOK_UNARY_OP 271
#define TOK_DSLASH 272
#define TOK_SLASH 273
#define TOK_DASTER 274
#define TOK_SEP 275
#define TOK_SEMICOLON 276
#define TOK_NEXTLINE 277
#define TOK_PARAMETER 278
#define TOK_RESULT 279
#define TOK_ONLY 280
#define TOK_INCLUDE 281
#define TOK_SUBROUTINE 282
#define TOK_PROGRAM 283
#define TOK_FUNCTION 284
#define TOK_OMP 285
#define TOK_DOLLAR 286
#define TOK_FORMAT 287
#define TOK_MAX 288
#define TOK_TANH 289
#define TOK_WHERE 290
#define TOK_ELSEWHERE 291
#define TOK_ENDWHERE 292
#define TOK_MAXVAL 293
#define TOK_TRIM 294
#define TOK_SUM 295
#define TOK_SQRT 296
#define TOK_CASE 297
#define TOK_SELECTCASE 298
#define TOK_FILE 299
#define TOK_END 300
#define TOK_ERR 301
#define TOK_DONOTTREAT 302
#define TOK_ENDDONOTTREAT 303
#define TOK_EXIST 304
#define TOK_MIN 305
#define TOK_FLOAT 306
#define TOK_EXP 307
#define TOK_COS 308
#define TOK_COSH 309
#define TOK_ACOS 310
#define TOK_NINT 311
#define TOK_CYCLE 312
#define TOK_SIN 313
#define TOK_SINH 314
#define TOK_ASIN 315
#define TOK_EQUIVALENCE 316
#define TOK_BACKSPACE 317
#define TOK_LOG 318
#define TOK_TAN 319
#define TOK_ATAN 320
#define TOK_RECURSIVE 321
#define TOK_ABS 322
#define TOK_MOD 323
#define TOK_SIGN 324
#define TOK_MINLOC 325
#define TOK_MAXLOC 326
#define TOK_EXIT 327
#define TOK_MINVAL 328
#define TOK_PUBLIC 329
#define TOK_PRIVATE 330
#define TOK_ALLOCATABLE 331
#define TOK_RETURN 332
#define TOK_THEN 333
#define TOK_ELSEIF 334
#define TOK_ELSE 335
#define TOK_ENDIF 336
#define TOK_PRINT 337
#define TOK_PLAINGOTO 338
#define TOK_CONSTRUCTID 339
#define TOK_LOGICALIF 340
#define TOK_PLAINDO 341
#define TOK_CONTAINS 342
#define TOK_ENDDO 343
#define TOK_MODULE 344
#define TOK_ENDMODULE 345
#define TOK_DOWHILE 346
#define TOK_ALLOCATE 347
#define TOK_OPEN 348
#define TOK_CLOSE 349
#define TOK_INQUIRE 350
#define TOK_WRITE 351
#define TOK_READ 352
#define TOK_REWIND 353
#define TOK_DEALLOCATE 354
#define TOK_NULLIFY 355
#define TOK_FIN 356
#define TOK_DEBUT 357
#define TOK_DIMENSION 358
#define TOK_ENDSELECT 359
#define TOK_EXTERNAL 360
#define TOK_INTENT 361
#define TOK_INTRINSIC 362
#define TOK_NAMELIST 363
#define TOK_CASEDEFAULT 364
#define TOK_OPTIONAL 365
#define TOK_POINTER 366
#define TOK_CONTINUE 367
#define TOK_SAVE 368
#define TOK_TARGET 369
#define TOK_QUOTE 370
#define TOK_IMPLICIT 371
#define TOK_NONE 372
#define TOK_CALL 373
#define TOK_STAT 374
#define TOK_POINT_TO 375
#define TOK_COMMON 376
#define TOK_GLOBAL 377
#define TOK_INTERFACE 378
#define TOK_ENDINTERFACE 379
#define TOK_LEFTAB 380
#define TOK_RIGHTAB 381
#define TOK_PAUSE 382
#define TOK_PROCEDURE 383
#define TOK_STOP 384
#define TOK_NAMEEQ 385
#define TOK_REAL8 386
#define TOK_OUT 387
#define TOK_INOUT 388
#define TOK_IN 389
#define TOK_USE 390
#define TOK_TRUE 391
#define TOK_FALSE 392
#define TOK_LABEL 393
#define TOK_TYPE 394
#define TOK_TYPEPAR 395
#define TOK_ENDTYPE 396
#define TOK_REAL 397
#define TOK_INTEGER 398
#define TOK_LOGICAL 399
#define TOK_DOUBLEPRECISION 400
#define TOK_DOUBLEREAL 401
#define TOK_ENDSUBROUTINE 402
#define TOK_ENDFUNCTION 403
#define TOK_ENDPROGRAM 404
#define TOK_ENDUNIT 405
#define TOK_CHARACTER 406
#define TOK_CHAR_CONSTANT 407
#define TOK_CHAR_CUT 408
#define TOK_DATA 409
#define TOK_CHAR_INT 410
#define TOK_CHAR_MESSAGE 411
#define TOK_CSTREAL 412
#define TOK_CSTREALDP 413
#define TOK_CSTREALQP 414
#define TOK_SFREAL 415
#define TOK_COMPLEX 416
#define TOK_DOUBLECOMPLEX 417
#define TOK_NAME 418
#define TOK_NAME_CHAR 419
#define TOK_PROBTYPE 420
#define TOK_INTERPTYPE 421
#define TOK_VARTYPE 422
#define TOK_BC 423
#define TOK_OP 424
#define TOK_CSTINT 425
#define TOK_COMMENT 426
#define TOK_FILENAME 427




/* Copy the first part of user declarations.  */
#line 36 "fortran.y"

#define YYMAXDEPTH 1000
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "decl.h"
extern int line_num_fortran;
extern int line_num_fortran_common;
char *tmp;
char c_selectorname[LONG_C];
char ligne[LONG_C];
char truename[LONGNOM];
char identcopy[LONG_C];
int c_selectorgiven=0;
int incom;
listvar *curlistvar;
typedim c_selectordim;
listcouple *coupletmp;
listdim *parcoursdim;
int removeline=0;
listvar *test;


/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 59 "fortran.y"
{
       char      nac[LONG_C];
       char      na[LONGNOM];
       listdim  *d;
       listvar  *l;
       listnom  *ln;
       listcouple  *lc;
       listname *lnn;
       typedim   dim1;
       variable *v;
       }
/* Line 193 of yacc.c.  */
#line 483 "fortran.tab.c"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 496 "fortran.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  2
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   6070

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  190
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  184
/* YYNRULES -- Number of rules.  */
#define YYNRULES  555
/* YYNRULES -- Number of states.  */
#define YYNSTATES  982

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   427

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     187,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   183,     2,     2,     2,   189,     2,     2,
     179,   180,    23,    21,     3,    22,     2,   188,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     4,     2,
     185,     5,   186,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   181,     2,   182,     2,   184,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
     141,   142,   143,   144,   145,   146,   147,   148,   149,   150,
     151,   152,   153,   154,   155,   156,   157,   158,   159,   160,
     161,   162,   163,   164,   165,   166,   167,   168,   169,   170,
     171,   172,   173,   174,   175,   176,   177,   178
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,     7,    10,    13,    15,    19,    23,
      25,    29,    32,    35,    39,    43,    46,    49,    52,    54,
      56,    58,    60,    61,    62,    65,    66,    68,    69,    70,
      72,    73,    78,    81,    88,    93,    96,    98,    99,   101,
     103,   104,   107,   111,   112,   115,   119,   121,   125,   127,
     129,   132,   137,   140,   143,   148,   151,   153,   155,   157,
     159,   161,   163,   165,   167,   169,   171,   176,   180,   184,
     187,   191,   192,   194,   196,   198,   200,   202,   204,   206,
     208,   210,   212,   214,   216,   218,   220,   222,   224,   226,
     228,   230,   232,   234,   236,   238,   240,   242,   244,   248,
     252,   258,   260,   264,   268,   271,   276,   278,   282,   283,
     286,   289,   293,   295,   297,   299,   304,   311,   316,   318,
     322,   325,   329,   335,   339,   341,   342,   345,   347,   352,
     356,   359,   363,   367,   371,   375,   376,   378,   381,   384,
     388,   394,   398,   399,   402,   405,   407,   413,   419,   422,
     426,   429,   433,   435,   439,   442,   446,   452,   454,   457,
     459,   463,   466,   468,   472,   473,   475,   477,   481,   485,
     488,   490,   494,   497,   500,   501,   508,   516,   517,   520,
     523,   527,   531,   533,   534,   537,   542,   546,   550,   555,
     558,   560,   562,   564,   566,   568,   570,   572,   574,   575,
     578,   580,   584,   585,   588,   592,   594,   598,   601,   605,
     607,   609,   611,   613,   614,   618,   619,   622,   627,   629,
     633,   635,   637,   639,   642,   644,   649,   651,   653,   655,
     657,   659,   661,   663,   665,   667,   669,   670,   674,   676,
     680,   682,   684,   687,   690,   694,   696,   698,   700,   704,
     706,   708,   712,   716,   721,   726,   730,   735,   740,   744,
     749,   754,   759,   764,   769,   774,   779,   784,   789,   794,
     799,   804,   809,   813,   818,   823,   828,   833,   835,   839,
     841,   843,   845,   848,   851,   854,   856,   858,   861,   864,
     867,   870,   873,   876,   879,   882,   885,   888,   891,   895,
     898,   902,   905,   908,   911,   914,   917,   920,   923,   924,
     926,   929,   932,   935,   937,   939,   941,   943,   944,   946,
     949,   954,   960,   965,   969,   973,   975,   978,   980,   984,
     986,   988,   992,   998,  1003,  1007,  1010,  1013,  1015,  1017,
    1019,  1021,  1023,  1025,  1027,  1029,  1032,  1035,  1037,  1040,
    1042,  1044,  1045,  1047,  1053,  1054,  1056,  1058,  1060,  1061,
    1065,  1069,  1070,  1076,  1079,  1084,  1091,  1098,  1100,  1102,
    1104,  1108,  1112,  1114,  1118,  1122,  1124,  1126,  1132,  1138,
    1143,  1145,  1148,  1151,  1154,  1157,  1159,  1162,  1168,  1170,
    1172,  1175,  1181,  1183,  1186,  1190,  1195,  1197,  1199,  1201,
    1203,  1205,  1207,  1209,  1211,  1215,  1219,  1223,  1226,  1229,
    1230,  1236,  1244,  1245,  1248,  1250,  1252,  1253,  1255,  1257,
    1259,  1261,  1264,  1266,  1268,  1270,  1276,  1282,  1285,  1288,
    1291,  1294,  1296,  1297,  1302,  1309,  1311,  1315,  1318,  1321,
    1324,  1325,  1329,  1330,  1332,  1335,  1337,  1339,  1343,  1345,
    1348,  1350,  1352,  1355,  1358,  1361,  1365,  1368,  1370,  1371,
    1373,  1376,  1379,  1380,  1383,  1387,  1391,  1395,  1399,  1401,
    1405,  1407,  1409,  1413,  1415,  1417,  1419,  1423,  1426,  1431,
    1436,  1439,  1442,  1444,  1446,  1448,  1450,  1452,  1454,  1456,
    1458,  1460,  1464,  1466,  1468,  1472,  1476,  1480,  1484,  1487,
    1491,  1494,  1497,  1500,  1503,  1507,  1509,  1511,  1513,  1515,
    1519,  1520,  1522,  1525,  1530,  1533,  1536,  1542,  1543,  1545,
    1548,  1550,  1552,  1554,  1558,  1562,  1566,  1570,  1574,  1578,
    1580,  1582,  1584,  1588,  1594,  1600,  1606,  1612,  1620,  1622,
    1631,  1634,  1636,  1638,  1642,  1644,  1646,  1648,  1653,  1655,
    1659,  1660,  1665,  1667,  1671,  1675
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
     191,     0,    -1,    -1,   191,   192,    -1,   187,   198,    -1,
     199,   193,    -1,   177,    -1,   197,   200,   205,    -1,     1,
     205,   201,    -1,   194,    -1,   193,    27,   194,    -1,   203,
     196,    -1,   212,   196,    -1,   206,   207,   196,    -1,   315,
     200,   205,    -1,   195,   196,    -1,   298,     4,    -1,   198,
     200,    -1,    53,    -1,    54,    -1,    36,    -1,    37,    -1,
      -1,    -1,   144,   201,    -1,    -1,   177,    -1,    -1,    -1,
      72,    -1,    -1,   202,    33,   204,   208,    -1,    34,   204,
      -1,   202,    35,   204,   208,    30,   209,    -1,   202,    35,
     204,   208,    -1,    95,   169,    -1,   169,    -1,    -1,    32,
      -1,   158,    -1,    -1,   179,   180,    -1,   179,   210,   180,
      -1,    -1,   179,   180,    -1,   179,   210,   180,    -1,   211,
      -1,   210,     3,   211,    -1,   169,    -1,    23,    -1,   257,
     222,    -1,   145,   213,   221,   324,    -1,   147,   324,    -1,
     117,   216,    -1,   224,   179,   249,   180,    -1,   224,   249,
      -1,   243,    -1,   228,    -1,   253,    -1,   239,    -1,   241,
      -1,   240,    -1,   308,    -1,   251,    -1,   237,    -1,   234,
      -1,    68,   179,   280,   180,    -1,   111,   221,   242,    -1,
     113,   221,   215,    -1,    67,   217,    -1,   225,   226,   187,
      -1,    -1,   275,    -1,    46,    -1,    40,    -1,    44,    -1,
      56,    -1,    79,    -1,    45,    -1,    47,    -1,    62,    -1,
      57,    -1,    58,    -1,    59,    -1,    60,    -1,    61,    -1,
      64,    -1,    65,    -1,    66,    -1,    69,    -1,    70,    -1,
      71,    -1,    74,    -1,    75,    -1,    76,    -1,    77,    -1,
     169,    -1,   214,    -1,   215,     3,   214,    -1,   179,   220,
     180,    -1,   216,     3,   179,   220,   180,    -1,   218,    -1,
     217,     3,   218,    -1,   179,   219,   180,    -1,   298,   276,
      -1,   219,     3,   298,   276,    -1,   280,    -1,   220,     3,
     280,    -1,    -1,     4,     4,    -1,   255,   256,    -1,   223,
     204,   208,    -1,    35,    -1,    29,    -1,   160,    -1,   169,
      24,   227,    24,    -1,   226,   248,   169,    24,   227,    24,
      -1,   231,    24,   227,    24,    -1,   232,    -1,   232,     3,
     227,    -1,   229,   230,    -1,   229,   247,   230,    -1,   228,
     248,   247,   248,   230,    -1,   228,     3,   230,    -1,   119,
      -1,    -1,   169,   276,    -1,   169,    -1,   169,   179,   280,
     180,    -1,   231,     3,   231,    -1,   233,   299,    -1,   232,
      21,   232,    -1,   232,    22,   232,    -1,   232,    23,   232,
      -1,   232,   188,   232,    -1,    -1,   284,    -1,   235,   236,
      -1,   114,   298,    -1,   114,   247,   298,    -1,   235,   248,
     247,   248,   298,    -1,   235,     3,   298,    -1,    -1,   129,
     324,    -1,   130,   324,    -1,   109,    -1,   238,   248,   169,
     276,   264,    -1,   239,     3,   169,   276,   264,    -1,    81,
     187,    -1,    81,   221,   242,    -1,    80,   187,    -1,    80,
     221,   242,    -1,   169,    -1,   242,     3,   169,    -1,   244,
     245,    -1,   244,   247,   245,    -1,   243,   248,   247,   248,
     245,    -1,   127,    -1,   128,   127,    -1,   246,    -1,   245,
       3,   246,    -1,   169,   276,    -1,    20,    -1,    24,   169,
      24,    -1,    -1,     3,    -1,   250,    -1,   249,     3,   250,
      -1,   169,     5,   280,    -1,   134,   252,    -1,   169,    -1,
     252,     3,   169,    -1,   122,   123,    -1,   122,   137,    -1,
      -1,   271,   254,   169,   276,   264,   305,    -1,   255,     3,
     254,   169,   276,   264,   305,    -1,    -1,   262,   266,    -1,
     261,   259,    -1,   263,    23,   176,    -1,   258,   268,   180,
      -1,   146,    -1,    -1,    23,   176,    -1,    23,   179,   260,
     180,    -1,   179,   260,   180,    -1,   169,   269,   270,    -1,
     169,     5,   269,   270,    -1,   269,   270,    -1,   157,    -1,
     263,    -1,   149,    -1,   148,    -1,   167,    -1,   151,    -1,
     168,    -1,   150,    -1,    -1,    23,   265,    -1,   280,    -1,
     179,    23,   180,    -1,    -1,    23,   267,    -1,   179,   268,
     180,    -1,   280,    -1,   179,    23,   180,    -1,   169,   269,
      -1,   169,     5,   269,    -1,   169,    -1,   176,    -1,   280,
      -1,    23,    -1,    -1,     3,   169,   269,    -1,    -1,     4,
       4,    -1,     3,   272,     4,     4,    -1,   273,    -1,   272,
       3,   273,    -1,    29,    -1,   275,    -1,    82,    -1,   109,
     276,    -1,   111,    -1,   112,   179,   274,   180,    -1,   113,
      -1,   116,    -1,   117,    -1,   119,    -1,   120,    -1,   140,
      -1,   138,    -1,   139,    -1,    80,    -1,    81,    -1,    -1,
     179,   277,   180,    -1,   278,    -1,   277,     3,   278,    -1,
     279,    -1,     4,    -1,   280,     4,    -1,     4,   280,    -1,
     280,     4,   279,    -1,    23,    -1,   280,    -1,   283,    -1,
     179,   280,   180,    -1,   307,    -1,   281,    -1,    46,   282,
     180,    -1,    39,   282,   180,    -1,    40,   179,   282,   180,
      -1,    44,   179,   282,   180,    -1,    56,   282,   180,    -1,
      79,   179,   282,   180,    -1,    45,   179,   280,   180,    -1,
      47,   280,   180,    -1,   148,   179,   282,   180,    -1,    62,
     179,   280,   180,    -1,    57,   179,   280,   180,    -1,    58,
     179,   280,   180,    -1,    59,   179,   280,   180,    -1,    60,
     179,   280,   180,    -1,    61,   179,   280,   180,    -1,    64,
     179,   280,   180,    -1,    65,   179,   280,   180,    -1,    66,
     179,   280,   180,    -1,    69,   179,   280,   180,    -1,    70,
     179,   280,   180,    -1,    71,   179,   280,   180,    -1,    73,
     280,   180,    -1,    74,   179,   282,   180,    -1,    75,   179,
     282,   180,    -1,    76,   179,   282,   180,    -1,    77,   179,
     282,   180,    -1,   280,    -1,   282,     3,   280,    -1,   288,
      -1,   299,    -1,   293,    -1,   280,   285,    -1,   284,   280,
      -1,    12,   280,    -1,    21,    -1,    22,    -1,    21,   280,
      -1,    22,   280,    -1,    23,   280,    -1,    25,   280,    -1,
      14,   280,    -1,     8,   280,    -1,    17,   280,    -1,   186,
     280,    -1,    18,   280,    -1,   185,   280,    -1,    15,   280,
      -1,   186,     5,   280,    -1,    16,   280,    -1,   185,     5,
     280,    -1,    13,   280,    -1,     7,   280,    -1,     9,   280,
      -1,    10,   280,    -1,    11,   280,    -1,    24,   286,    -1,
       5,   287,    -1,    -1,   280,    -1,     5,   280,    -1,    24,
     280,    -1,     5,   280,    -1,   280,    -1,   298,    -1,   292,
      -1,   290,    -1,    -1,   291,    -1,   291,   302,    -1,   292,
     179,   294,   180,    -1,   292,   179,   294,   180,   302,    -1,
     298,   179,   294,   180,    -1,   288,   189,   288,    -1,   131,
     360,   132,    -1,   289,    -1,   289,   295,    -1,   296,    -1,
     295,     3,   296,    -1,   280,    -1,   297,    -1,   280,     4,
     280,    -1,   280,     4,   280,     4,   280,    -1,     4,   280,
       4,   280,    -1,     4,     4,   280,    -1,     4,   280,    -1,
     280,     4,    -1,     4,    -1,   169,    -1,   142,    -1,   143,
      -1,   176,    -1,   163,    -1,   164,    -1,   165,    -1,   299,
     169,    -1,   300,   301,    -1,   158,    -1,   300,   158,    -1,
     162,    -1,   159,    -1,    -1,   302,    -1,   179,   303,     4,
     303,   180,    -1,    -1,   280,    -1,   187,    -1,   280,    -1,
      -1,   306,     5,   280,    -1,   306,   126,   280,    -1,    -1,
     179,   283,     3,   283,   180,    -1,   309,   310,    -1,   309,
     310,     3,   311,    -1,   309,   310,     3,    31,     4,   187,
      -1,   309,   310,     3,    31,     4,   313,    -1,   141,    -1,
     169,    -1,   312,    -1,   311,     3,   312,    -1,   169,   126,
     169,    -1,   314,    -1,   313,     3,   314,    -1,   169,   126,
     169,    -1,   169,    -1,   328,    -1,    98,   179,   366,   370,
     180,    -1,   105,   179,   369,   370,   180,    -1,   106,   179,
     371,   180,    -1,   317,    -1,   318,   324,    -1,   316,   324,
      -1,   319,   324,    -1,    96,   324,    -1,   321,    -1,   372,
     328,    -1,    41,   179,   280,   180,   304,    -1,    42,    -1,
      43,    -1,   372,    84,    -1,    85,   179,   280,   180,    84,
      -1,    86,    -1,    87,   324,    -1,    48,   320,   180,    -1,
      49,   179,   280,   180,    -1,   115,    -1,   110,    -1,    93,
      -1,   153,    -1,   156,    -1,   155,    -1,   154,    -1,   280,
      -1,   320,     3,   280,    -1,   320,     4,   280,    -1,   326,
     323,   322,    -1,   327,   280,    -1,    94,   325,    -1,    -1,
     373,     5,   280,     3,   280,    -1,   373,     5,   280,     3,
     280,     3,   280,    -1,    -1,   176,   248,    -1,   187,    -1,
     169,    -1,    -1,   169,    -1,    92,    -1,    97,    -1,   118,
      -1,   330,   332,    -1,   365,    -1,   341,    -1,   333,    -1,
      98,   179,   366,   370,   180,    -1,   105,   179,   369,   370,
     180,    -1,    78,   303,    -1,    83,   304,    -1,    63,   304,
      -1,   340,   304,    -1,   331,    -1,    -1,   298,   329,   276,
     276,    -1,   330,   189,   298,   329,   276,   276,    -1,   176,
      -1,   331,     3,   176,    -1,     5,   280,    -1,   126,   280,
      -1,   336,   334,    -1,    -1,   179,   335,   180,    -1,    -1,
     338,    -1,   337,   169,    -1,   124,    -1,   339,    -1,   338,
       3,   339,    -1,   280,    -1,    23,   364,    -1,   133,    -1,
     135,    -1,   349,   345,    -1,   351,   343,    -1,   352,   345,
      -1,   352,   345,   360,    -1,   104,   346,    -1,    38,    -1,
      -1,   356,    -1,   345,   342,    -1,   350,   344,    -1,    -1,
       3,   356,    -1,   179,   347,   180,    -1,   179,   353,   180,
      -1,   179,   298,   180,    -1,   179,   176,   180,    -1,   176,
      -1,   179,   283,   180,    -1,   169,    -1,   348,    -1,   347,
       3,   348,    -1,   353,    -1,    23,    -1,    25,    -1,   298,
     280,   276,    -1,   298,   280,    -1,   298,   280,   189,   330,
      -1,   298,   179,   297,   180,    -1,   298,    23,    -1,   298,
      25,    -1,    99,    -1,   100,    -1,   354,    -1,    23,    -1,
     103,    -1,   101,    -1,    88,    -1,   102,    -1,   354,    -1,
     179,   353,   180,    -1,   288,    -1,   299,    -1,   353,   355,
     353,    -1,   353,    23,   353,    -1,   353,    24,   353,    -1,
     353,    25,   353,    -1,   355,   353,    -1,   353,    20,   353,
      -1,    50,   280,    -1,    55,   280,    -1,    52,   280,    -1,
      51,   280,    -1,   169,     5,   280,    -1,   281,    -1,    21,
      -1,    22,    -1,   358,    -1,   356,     3,   358,    -1,    -1,
     288,    -1,   357,   359,    -1,   179,   356,   180,   359,    -1,
     281,   359,    -1,   299,   359,    -1,   179,   356,     3,   363,
     180,    -1,    -1,   285,    -1,   359,   285,    -1,   283,    -1,
     362,    -1,   361,    -1,   283,     3,   280,    -1,   283,     3,
     362,    -1,   362,     3,   280,    -1,   362,     3,   362,    -1,
     361,     3,   280,    -1,   361,     3,   362,    -1,   283,    -1,
     281,    -1,   307,    -1,   179,   280,   180,    -1,   179,   283,
       3,   363,   180,    -1,   179,   362,     3,   363,   180,    -1,
     179,   361,     3,   363,   180,    -1,   169,     5,   280,     3,
     280,    -1,   169,     5,   280,     3,   280,     3,   280,    -1,
     176,    -1,    89,   179,   280,     3,   280,   180,     3,   280,
      -1,    89,   364,    -1,   367,    -1,   330,    -1,   366,     3,
     367,    -1,   298,    -1,   292,    -1,   368,    -1,   298,   179,
     295,   180,    -1,   367,    -1,   369,     3,   367,    -1,    -1,
       3,   125,     5,   298,    -1,   298,    -1,   371,     3,   298,
      -1,    91,   280,   180,    -1,   298,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   336,   336,   337,   339,   340,   341,   342,   343,   346,
     347,   349,   350,   351,   360,   361,   363,   365,   367,   374,
     379,   380,   382,   384,   385,   387,   388,   394,   397,   398,
     401,   402,   430,   448,   481,   515,   532,   539,   541,   545,
     550,   553,   556,   561,   562,   563,   571,   582,   594,   595,
     597,   605,   613,   619,   620,   635,   648,   649,   655,   656,
     693,   706,   707,   708,   709,   710,   711,   712,   713,   714,
     715,   734,   735,   741,   742,   743,   744,   745,   746,   747,
     748,   749,   750,   751,   752,   753,   754,   755,   756,   757,
     758,   759,   760,   761,   762,   763,   764,   766,   767,   769,
     770,   772,   773,   775,   777,   778,   780,   781,   783,   784,
     786,   870,   907,   913,   918,   923,   937,   951,   966,   973,
     982,   983,   984,   985,   987,   992,   993,   998,  1002,  1007,
    1012,  1014,  1016,  1018,  1020,  1024,  1025,  1028,  1030,  1031,
    1032,  1033,  1035,  1037,  1042,  1047,  1053,  1069,  1086,  1087,
    1089,  1093,  1098,  1102,  1107,  1113,  1124,  1136,  1141,  1147,
    1152,  1156,  1166,  1175,  1185,  1186,  1188,  1192,  1197,  1220,
    1222,  1223,  1225,  1235,  1237,  1239,  1272,  1307,  1309,  1311,
    1315,  1320,  1326,  1334,  1335,  1337,  1338,  1340,  1341,  1342,
    1344,  1349,  1351,  1356,  1360,  1363,  1369,  1371,  1377,  1378,
    1380,  1381,  1383,  1384,  1385,  1387,  1388,  1390,  1391,  1400,
    1404,  1409,  1411,  1414,  1415,  1417,  1418,  1419,  1421,  1422,
    1424,  1428,  1429,  1431,  1436,  1438,  1440,  1441,  1442,  1443,
    1449,  1452,  1453,  1454,  1456,  1458,  1461,  1466,  1470,  1472,
    1476,  1481,  1485,  1490,  1495,  1503,  1504,  1506,  1507,  1509,
    1511,  1515,  1517,  1519,  1521,  1523,  1525,  1527,  1529,  1531,
    1533,  1535,  1537,  1539,  1541,  1543,  1545,  1547,  1549,  1551,
    1553,  1555,  1557,  1559,  1561,  1563,  1565,  1568,  1569,  1573,
    1574,  1576,  1578,  1580,  1582,  1585,  1586,  1588,  1590,  1592,
    1594,  1596,  1598,  1600,  1602,  1604,  1606,  1608,  1610,  1612,
    1614,  1616,  1618,  1620,  1622,  1624,  1626,  1628,  1631,  1632,
    1634,  1636,  1639,  1641,  1645,  1650,  1655,  1661,  1673,  1678,
    1680,  1683,  1687,  1697,  1703,  1706,  1707,  1710,  1711,  1714,
    1715,  1717,  1719,  1722,  1724,  1725,  1726,  1727,  1729,  1787,
    1789,  1790,  1791,  1792,  1793,  1794,  1796,  1798,  1800,  1801,
    1803,  1806,  1807,  1809,  1812,  1813,  1815,  1816,  1818,  1819,
    1827,  1836,  1838,  1841,  1861,  1895,  1915,  1966,  1974,  1977,
    1981,  1991,  2002,  2006,  2016,  2028,  2037,  2038,  2044,  2050,
    2051,  2101,  2119,  2139,  2158,  2203,  2204,  2205,  2206,  2207,
    2208,  2209,  2210,  2211,  2212,  2213,  2214,  2215,  2216,  2260,
    2270,  2279,  2288,  2297,  2298,  2299,  2301,  2302,  2303,  2305,
    2306,  2307,  2308,  2309,  2311,  2312,  2314,  2315,  2317,  2319,
    2326,  2327,  2328,  2329,  2330,  2331,  2337,  2343,  2344,  2345,
    2346,  2347,  2349,  2350,  2360,  2362,  2363,  2365,  2366,  2368,
    2396,  2397,  2399,  2400,  2402,  2432,  2434,  2435,  2438,  2452,
    2455,  2456,  2459,  2460,  2461,  2462,  2463,  2464,  2476,  2477,
    2479,  2480,  2485,  2486,  2488,  2489,  2491,  2492,  2493,  2494,
    2495,  2497,  2498,  2500,  2501,  2502,  2503,  2504,  2505,  2506,
    2507,  2508,  2510,  2511,  2513,  2514,  2517,  2518,  2519,  2522,
    2525,  2526,  2528,  2529,  2530,  2531,  2532,  2533,  2534,  2535,
    2536,  2537,  2538,  2539,  2540,  2541,  2543,  2544,  2546,  2547,
    2549,  2550,  2552,  2553,  2554,  2555,  2556,  2558,  2559,  2560,
    2562,  2563,  2564,  2566,  2568,  2570,  2572,  2574,  2576,  2578,
    2579,  2581,  2583,  2585,  2587,  2589,  2593,  2596,  2603,  2605,
    2606,  2608,  2609,  2610,  2612,  2614,  2615,  2617,  2624,  2625,
    2627,  2628,  2630,  2631,  2639,  2641
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "','", "':'", "'='", "TOK_BINARY_OP",
  "TOK_NEQV", "TOK_EQV", "TOK_XOR", "TOK_OR", "TOK_AND", "TOK_NOT",
  "TOK_NE", "TOK_EQ", "TOK_GE", "TOK_LE", "TOK_GT", "TOK_LT",
  "TOK_UNARY_OP", "TOK_DSLASH", "'+'", "'-'", "'*'", "TOK_SLASH",
  "TOK_DASTER", "TOK_SEP", "TOK_SEMICOLON", "TOK_NEXTLINE",
  "TOK_PARAMETER", "TOK_RESULT", "TOK_ONLY", "TOK_INCLUDE",
  "TOK_SUBROUTINE", "TOK_PROGRAM", "TOK_FUNCTION", "TOK_OMP", "TOK_DOLLAR",
  "TOK_FORMAT", "TOK_MAX", "TOK_TANH", "TOK_WHERE", "TOK_ELSEWHERE",
  "TOK_ENDWHERE", "TOK_MAXVAL", "TOK_TRIM", "TOK_SUM", "TOK_SQRT",
  "TOK_CASE", "TOK_SELECTCASE", "TOK_FILE", "TOK_END", "TOK_ERR",
  "TOK_DONOTTREAT", "TOK_ENDDONOTTREAT", "TOK_EXIST", "TOK_MIN",
  "TOK_FLOAT", "TOK_EXP", "TOK_COS", "TOK_COSH", "TOK_ACOS", "TOK_NINT",
  "TOK_CYCLE", "TOK_SIN", "TOK_SINH", "TOK_ASIN", "TOK_EQUIVALENCE",
  "TOK_BACKSPACE", "TOK_LOG", "TOK_TAN", "TOK_ATAN", "TOK_RECURSIVE",
  "TOK_ABS", "TOK_MOD", "TOK_SIGN", "TOK_MINLOC", "TOK_MAXLOC", "TOK_EXIT",
  "TOK_MINVAL", "TOK_PUBLIC", "TOK_PRIVATE", "TOK_ALLOCATABLE",
  "TOK_RETURN", "TOK_THEN", "TOK_ELSEIF", "TOK_ELSE", "TOK_ENDIF",
  "TOK_PRINT", "TOK_PLAINGOTO", "TOK_CONSTRUCTID", "TOK_LOGICALIF",
  "TOK_PLAINDO", "TOK_CONTAINS", "TOK_ENDDO", "TOK_MODULE",
  "TOK_ENDMODULE", "TOK_DOWHILE", "TOK_ALLOCATE", "TOK_OPEN", "TOK_CLOSE",
  "TOK_INQUIRE", "TOK_WRITE", "TOK_READ", "TOK_REWIND", "TOK_DEALLOCATE",
  "TOK_NULLIFY", "TOK_FIN", "TOK_DEBUT", "TOK_DIMENSION", "TOK_ENDSELECT",
  "TOK_EXTERNAL", "TOK_INTENT", "TOK_INTRINSIC", "TOK_NAMELIST",
  "TOK_CASEDEFAULT", "TOK_OPTIONAL", "TOK_POINTER", "TOK_CONTINUE",
  "TOK_SAVE", "TOK_TARGET", "TOK_QUOTE", "TOK_IMPLICIT", "TOK_NONE",
  "TOK_CALL", "TOK_STAT", "TOK_POINT_TO", "TOK_COMMON", "TOK_GLOBAL",
  "TOK_INTERFACE", "TOK_ENDINTERFACE", "TOK_LEFTAB", "TOK_RIGHTAB",
  "TOK_PAUSE", "TOK_PROCEDURE", "TOK_STOP", "TOK_NAMEEQ", "TOK_REAL8",
  "TOK_OUT", "TOK_INOUT", "TOK_IN", "TOK_USE", "TOK_TRUE", "TOK_FALSE",
  "TOK_LABEL", "TOK_TYPE", "TOK_TYPEPAR", "TOK_ENDTYPE", "TOK_REAL",
  "TOK_INTEGER", "TOK_LOGICAL", "TOK_DOUBLEPRECISION", "TOK_DOUBLEREAL",
  "TOK_ENDSUBROUTINE", "TOK_ENDFUNCTION", "TOK_ENDPROGRAM", "TOK_ENDUNIT",
  "TOK_CHARACTER", "TOK_CHAR_CONSTANT", "TOK_CHAR_CUT", "TOK_DATA",
  "TOK_CHAR_INT", "TOK_CHAR_MESSAGE", "TOK_CSTREAL", "TOK_CSTREALDP",
  "TOK_CSTREALQP", "TOK_SFREAL", "TOK_COMPLEX", "TOK_DOUBLECOMPLEX",
  "TOK_NAME", "TOK_NAME_CHAR", "TOK_PROBTYPE", "TOK_INTERPTYPE",
  "TOK_VARTYPE", "TOK_BC", "TOK_OP", "TOK_CSTINT", "TOK_COMMENT",
  "TOK_FILENAME", "'('", "')'", "'['", "']'", "'!'", "'_'", "'<'", "'>'",
  "'\\n'", "'/'", "'%'", "$accept", "input", "line", "suite_line_list",
  "suite_line", "instr", "fin_line", "keyword", "position", "thislabel",
  "cmnt", "nulcurbuf", "opt_recursive", "entry", "name_routine",
  "writedeclar", "before_include", "filename", "arglist", "arglist1",
  "args", "arg", "spec", "opt_spec", "name_intrinsic",
  "use_intrinsic_list", "list_couple", "list_expr_equi", "expr_equi",
  "list_expr_equi1", "list_expr", "opt_sep", "after_type",
  "before_function", "before_parameter", "before_data", "data",
  "datavallist", "save", "before_save", "varsave", "datanamelist",
  "expr_data", "opt_signe", "namelist", "namelist_action",
  "after_namelist", "interface", "before_dimension", "dimension",
  "private", "public", "use_name_list", "common", "before_common",
  "var_common_list", "var_common", "comblock", "opt_comma", "paramlist",
  "paramitem", "module_proc_stmt", "proc_name_list", "implicit",
  "opt_retour", "dcl", "nodimsgiven", "type", "before_typepar",
  "c_selector", "c_attribute", "before_character", "typespec", "typename",
  "lengspec", "proper_lengspec", "selector", "proper_selector",
  "attribute", "clause", "opt_clause", "options", "attr_spec_list",
  "attr_spec", "intent_spec", "access_spec", "dims", "dimlist", "dim",
  "ubound", "expr", "predefinedfunction", "minmaxlist", "uexpr", "signe",
  "operation", "after_slash", "after_equal", "lhs", "beforefunctionuse",
  "array_ele_substring_func_ref", "begin_array", "structure_component",
  "vec", "funarglist", "funargs", "funarg", "triplet", "ident",
  "simple_const", "string_constant", "opt_substring", "substring",
  "optexpr", "opt_expr", "initial_value", "before_initial",
  "complex_const", "use_stat", "word_use", "module_name", "rename_list",
  "rename_name", "only_list", "only_name", "exec", "word_endsubroutine",
  "word_endunit", "word_endprogram", "word_endfunction", "caselist",
  "boucledo", "do_arg", "opt_int", "opt_name", "optname", "worddo",
  "wordwhile", "iffable", "before_dims", "ident_dims", "int_list",
  "after_ident_dims", "call", "opt_call", "opt_callarglist", "keywordcall",
  "before_call", "callarglist", "callarg", "stop", "io", "option_inlist",
  "option_read", "opt_inlist", "ioctl", "after_rewind", "ctllist",
  "ioclause", "iofctl", "infmt", "read", "write", "fexpr", "unpar_fexpr",
  "addop", "inlist", "opt_lhs", "inelt", "opt_operation", "outlist",
  "out2", "other", "dospec", "label", "goto", "allocation_list",
  "allocate_object", "array_element", "allocate_object_list",
  "opt_stat_spec", "pointer_name_list", "logif", "do_var", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,    44,    58,    61,   258,   259,   260,   261,
     262,   263,   264,   265,   266,   267,   268,   269,   270,   271,
     272,    43,    45,    42,   273,   274,   275,   276,   277,   278,
     279,   280,   281,   282,   283,   284,   285,   286,   287,   288,
     289,   290,   291,   292,   293,   294,   295,   296,   297,   298,
     299,   300,   301,   302,   303,   304,   305,   306,   307,   308,
     309,   310,   311,   312,   313,   314,   315,   316,   317,   318,
     319,   320,   321,   322,   323,   324,   325,   326,   327,   328,
     329,   330,   331,   332,   333,   334,   335,   336,   337,   338,
     339,   340,   341,   342,   343,   344,   345,   346,   347,   348,
     349,   350,   351,   352,   353,   354,   355,   356,   357,   358,
     359,   360,   361,   362,   363,   364,   365,   366,   367,   368,
     369,   370,   371,   372,   373,   374,   375,   376,   377,   378,
     379,   380,   381,   382,   383,   384,   385,   386,   387,   388,
     389,   390,   391,   392,   393,   394,   395,   396,   397,   398,
     399,   400,   401,   402,   403,   404,   405,   406,   407,   408,
     409,   410,   411,   412,   413,   414,   415,   416,   417,   418,
     419,   420,   421,   422,   423,   424,   425,   426,   427,    40,
      41,    91,    93,    33,    95,    60,    62,    10,    47,    37
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   190,   191,   191,   192,   192,   192,   192,   192,   193,
     193,   194,   194,   194,   194,   194,   195,   196,   197,   197,
     197,   197,   198,   199,   199,   200,   200,   201,   202,   202,
     203,   203,   203,   203,   203,   203,   204,   205,   206,   207,
     208,   208,   208,   209,   209,   209,   210,   210,   211,   211,
     212,   212,   212,   212,   212,   212,   212,   212,   212,   212,
     212,   212,   212,   212,   212,   212,   212,   212,   212,   212,
     212,   213,   213,   214,   214,   214,   214,   214,   214,   214,
     214,   214,   214,   214,   214,   214,   214,   214,   214,   214,
     214,   214,   214,   214,   214,   214,   214,   215,   215,   216,
     216,   217,   217,   218,   219,   219,   220,   220,   221,   221,
     222,   222,   223,   224,   225,   226,   226,   226,   227,   227,
     228,   228,   228,   228,   229,   230,   230,   231,   231,   231,
     232,   232,   232,   232,   232,   233,   233,   234,   235,   235,
     235,   235,   236,   237,   237,   238,   239,   239,   240,   240,
     241,   241,   242,   242,   243,   243,   243,   244,   244,   245,
     245,   246,   247,   247,   248,   248,   249,   249,   250,   251,
     252,   252,   253,   253,   254,   255,   255,   256,   257,   257,
     257,   257,   258,   259,   259,   259,   259,   260,   260,   260,
     261,   262,   263,   263,   263,   263,   263,   263,   264,   264,
     265,   265,   266,   266,   266,   267,   267,   268,   268,   268,
     268,   269,   269,   270,   270,   271,   271,   271,   272,   272,
     273,   273,   273,   273,   273,   273,   273,   273,   273,   273,
     273,   274,   274,   274,   275,   275,   276,   276,   277,   277,
     278,   278,   278,   278,   278,   279,   279,   280,   280,   280,
     280,   281,   281,   281,   281,   281,   281,   281,   281,   281,
     281,   281,   281,   281,   281,   281,   281,   281,   281,   281,
     281,   281,   281,   281,   281,   281,   281,   282,   282,   283,
     283,   283,   283,   283,   283,   284,   284,   285,   285,   285,
     285,   285,   285,   285,   285,   285,   285,   285,   285,   285,
     285,   285,   285,   285,   285,   285,   285,   285,   286,   286,
     286,   286,   287,   287,   288,   288,   288,   289,   290,   290,
     290,   290,   291,   292,   293,   294,   294,   295,   295,   296,
     296,   297,   297,   297,   297,   297,   297,   297,   298,   299,
     299,   299,   299,   299,   299,   299,   299,   300,   300,   300,
     300,   301,   301,   302,   303,   303,   304,   304,   305,   305,
     305,   306,   307,   308,   308,   308,   308,   309,   310,   311,
     311,   312,   313,   313,   314,   314,   315,   315,   315,   315,
     315,   315,   315,   315,   315,   315,   315,   315,   315,   315,
     315,   315,   315,   315,   315,   315,   315,   315,   315,   316,
     317,   318,   319,   320,   320,   320,   321,   321,   321,   322,
     322,   322,   323,   323,   324,   324,   325,   325,   326,   327,
     328,   328,   328,   328,   328,   328,   328,   328,   328,   328,
     328,   328,   329,   330,   330,   331,   331,   332,   332,   333,
     334,   334,   335,   335,   336,   337,   338,   338,   339,   339,
     340,   340,   341,   341,   341,   341,   341,   341,   342,   342,
     343,   343,   344,   344,   345,   345,   346,   346,   346,   346,
     346,   347,   347,   348,   348,   348,   348,   348,   348,   348,
     348,   348,   349,   349,   350,   350,   351,   351,   351,   352,
     353,   353,   354,   354,   354,   354,   354,   354,   354,   354,
     354,   354,   354,   354,   354,   354,   355,   355,   356,   356,
     357,   357,   358,   358,   358,   358,   358,   359,   359,   359,
     360,   360,   360,   361,   361,   361,   361,   361,   361,   361,
     361,   362,   362,   362,   362,   362,   363,   363,   364,   365,
     365,   366,   366,   366,   367,   367,   367,   368,   369,   369,
     370,   370,   371,   371,   372,   373
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     2,     2,     1,     3,     3,     1,
       3,     2,     2,     3,     3,     2,     2,     2,     1,     1,
       1,     1,     0,     0,     2,     0,     1,     0,     0,     1,
       0,     4,     2,     6,     4,     2,     1,     0,     1,     1,
       0,     2,     3,     0,     2,     3,     1,     3,     1,     1,
       2,     4,     2,     2,     4,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     4,     3,     3,     2,
       3,     0,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     3,     3,
       5,     1,     3,     3,     2,     4,     1,     3,     0,     2,
       2,     3,     1,     1,     1,     4,     6,     4,     1,     3,
       2,     3,     5,     3,     1,     0,     2,     1,     4,     3,
       2,     3,     3,     3,     3,     0,     1,     2,     2,     3,
       5,     3,     0,     2,     2,     1,     5,     5,     2,     3,
       2,     3,     1,     3,     2,     3,     5,     1,     2,     1,
       3,     2,     1,     3,     0,     1,     1,     3,     3,     2,
       1,     3,     2,     2,     0,     6,     7,     0,     2,     2,
       3,     3,     1,     0,     2,     4,     3,     3,     4,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     2,
       1,     3,     0,     2,     3,     1,     3,     2,     3,     1,
       1,     1,     1,     0,     3,     0,     2,     4,     1,     3,
       1,     1,     1,     2,     1,     4,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     3,     1,     3,
       1,     1,     2,     2,     3,     1,     1,     1,     3,     1,
       1,     3,     3,     4,     4,     3,     4,     4,     3,     4,
       4,     4,     4,     4,     4,     4,     4,     4,     4,     4,
       4,     4,     3,     4,     4,     4,     4,     1,     3,     1,
       1,     1,     2,     2,     2,     1,     1,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     3,     2,
       3,     2,     2,     2,     2,     2,     2,     2,     0,     1,
       2,     2,     2,     1,     1,     1,     1,     0,     1,     2,
       4,     5,     4,     3,     3,     1,     2,     1,     3,     1,
       1,     3,     5,     4,     3,     2,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     1,     2,     1,
       1,     0,     1,     5,     0,     1,     1,     1,     0,     3,
       3,     0,     5,     2,     4,     6,     6,     1,     1,     1,
       3,     3,     1,     3,     3,     1,     1,     5,     5,     4,
       1,     2,     2,     2,     2,     1,     2,     5,     1,     1,
       2,     5,     1,     2,     3,     4,     1,     1,     1,     1,
       1,     1,     1,     1,     3,     3,     3,     2,     2,     0,
       5,     7,     0,     2,     1,     1,     0,     1,     1,     1,
       1,     2,     1,     1,     1,     5,     5,     2,     2,     2,
       2,     1,     0,     4,     6,     1,     3,     2,     2,     2,
       0,     3,     0,     1,     2,     1,     1,     3,     1,     2,
       1,     1,     2,     2,     2,     3,     2,     1,     0,     1,
       2,     2,     0,     2,     3,     3,     3,     3,     1,     3,
       1,     1,     3,     1,     1,     1,     3,     2,     4,     4,
       2,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     1,     1,     3,     3,     3,     3,     2,     3,
       2,     2,     2,     2,     3,     1,     1,     1,     1,     3,
       0,     1,     2,     4,     2,     2,     5,     0,     1,     2,
       1,     1,     1,     3,     3,     3,     3,     3,     3,     1,
       1,     1,     3,     5,     5,     5,     5,     7,     1,     8,
       2,     1,     1,     3,     1,     1,     1,     4,     1,     3,
       0,     4,     1,     3,     3,     1
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,     1,    37,    20,    21,    18,    19,    27,     6,
      22,     3,    25,    30,    27,    24,     4,    26,    37,   113,
      38,     0,   457,     0,   388,   389,     0,     0,     0,     0,
       0,    29,   354,   108,   108,     0,     0,   392,     0,   488,
       0,     0,   418,   398,   416,     0,     0,   419,     0,   482,
     483,   487,   489,   486,     0,     0,     0,   145,   397,   108,
     108,     0,   396,     0,   420,   124,     0,   445,   157,     0,
       0,     0,   450,     0,   451,   367,    71,   182,     0,   193,
     192,   197,   195,   399,   402,   401,   400,   190,   114,   194,
     196,   338,   435,     5,     9,    22,     0,    22,     0,    22,
       0,     0,    57,   125,    65,   142,    64,   164,    59,    61,
      60,    56,     0,    63,    58,   215,     0,   183,   202,   191,
     432,    62,     0,    25,     0,   380,     0,     0,   385,   412,
       0,   376,     0,   431,   424,   440,     0,     0,   423,     0,
       0,     0,   422,     0,     8,     7,    36,    32,     0,     0,
     285,   286,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   339,   340,
       0,   347,   350,   349,   342,   343,   344,   341,     0,   403,
     250,   247,     0,   279,   316,   318,   315,   281,   314,   280,
     351,   249,     0,     0,   356,   357,   429,     0,    69,   101,
       0,   355,   427,     0,   150,     0,   148,     0,   428,     0,
     415,   414,   393,   538,     0,   540,     0,   417,   408,    35,
     384,     0,   470,   468,     0,   456,     0,     0,     0,     0,
     162,     0,     0,   138,     0,    53,   172,   173,   158,   143,
     144,   170,   169,   234,   235,   108,    72,    52,    30,    15,
      25,     0,     0,    11,    39,    22,    12,     0,     0,    55,
     166,   127,   164,     0,   125,     0,   236,   120,   125,   165,
     137,     0,   165,     0,     0,     0,   236,   154,   159,     0,
       0,     0,   112,    50,     0,   177,   174,   209,   210,     0,
       0,     0,   179,     0,     0,   178,     0,    16,   236,   368,
     363,    37,   382,   381,   383,   164,   409,   407,     0,     0,
       0,   421,     0,   442,   439,   444,   430,     0,   452,   506,
     507,   485,     0,     0,     0,     0,   338,     0,   505,   492,
     493,   453,   458,   462,     0,   484,     0,   454,   390,     0,
       0,   432,   386,     0,   284,   277,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   530,   520,   249,     0,   522,   521,     0,
       0,   247,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   308,     0,     0,
       0,   282,   283,     0,   354,   319,   317,   317,   345,   348,
     346,   352,     0,     0,   394,     0,     0,   236,     0,     0,
     109,   152,   151,   149,     0,     0,   554,     0,   545,   432,
     542,   550,   541,   546,   341,   247,   314,   544,   548,   550,
     552,     0,    67,    74,    75,    78,    73,    79,    76,    81,
      82,    83,    84,    85,    80,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    77,    96,    97,    68,     0,
     139,     0,   106,     0,     0,     0,    10,    17,    40,    40,
      13,     0,     0,     0,   135,     0,    70,     0,     0,   135,
     123,   164,     0,   126,   121,   141,   164,   236,   236,   164,
     161,     0,   155,   220,   222,   236,   224,     0,   226,   227,
     228,   229,   230,     0,   218,   221,   216,    40,   174,   110,
       0,     0,   212,   207,   211,   181,   184,     0,   338,     0,
     213,     0,   203,   205,     0,   180,   236,     0,    14,   413,
     555,   406,     0,   437,   438,   432,   436,     0,   448,     0,
     443,   446,   474,   475,     0,   314,     0,   471,   473,   490,
     500,   503,   502,   501,     0,   473,   510,   517,   511,   517,
     460,   459,   517,   508,   510,   461,     0,     0,     0,     0,
       0,   498,   455,     0,     0,     0,     0,   252,     0,     0,
       0,   251,   258,   255,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   272,     0,     0,     0,
       0,     0,     0,   247,     0,     0,     0,   324,     0,     0,
       0,   248,     0,     0,   313,   307,   302,   292,   303,   304,
     305,   301,   291,   297,   299,   293,   295,   287,   288,   289,
       0,     0,   309,   306,   290,     0,   296,     0,   294,   323,
       0,   325,     0,     0,   404,   405,   395,     0,   103,   104,
     102,    66,     0,     0,     0,   317,     0,     0,   467,   469,
     466,     0,     0,     0,   379,     0,   163,     0,    99,     0,
     171,    51,     0,    31,    34,   168,    54,   167,     0,   118,
       0,   136,     0,     0,   127,   129,     0,   125,   241,   245,
       0,   238,   240,   246,     0,   198,   198,     0,   160,   223,
       0,     0,     0,   111,     0,   236,   208,     0,     0,   213,
     186,     0,   189,     0,   204,   433,     0,     0,   364,   369,
       0,   236,   449,   441,     0,     0,   480,   481,   317,   236,
       0,   464,   465,   504,   465,     0,   518,   514,   515,   510,
     512,   463,   499,   495,   496,   497,   494,   550,   550,   387,
     278,   253,   254,   257,   261,   262,   263,   264,   265,   260,
     266,   267,   268,   269,   270,   271,   273,   274,   275,   276,
     256,   248,     0,     0,     0,   523,   524,   527,   528,   525,
     526,   259,   247,   312,   310,   311,   300,   298,   354,   337,
     329,   326,   327,   330,   320,   322,   236,   153,   391,     0,
       0,     0,   543,   377,   549,   378,   553,    98,   107,     0,
      49,    48,    41,     0,    46,    43,   115,   135,   135,   135,
     135,   135,   130,   128,   135,   117,   122,   243,     0,   237,
     242,   140,     0,   146,   147,   156,   232,   233,   231,     0,
     219,   217,   236,   198,   185,   213,   187,     0,   206,     0,
       0,     0,     0,   236,   447,   491,     0,     0,     0,   476,
     472,   473,   510,   517,   519,   509,     0,     0,   338,     0,
       0,     0,   362,     0,     0,   335,   336,     0,   321,   105,
       0,   547,     0,   100,     0,    42,     0,    33,   119,   131,
     132,   133,   134,     0,   239,   244,   246,     0,   199,   200,
     225,   198,   358,   188,   214,   375,   365,   366,   372,   371,
     370,     0,   434,   479,   478,     0,   513,   425,   426,     0,
     533,   535,   534,   353,   334,     0,   331,   328,     0,   551,
      47,    44,     0,   116,     0,   358,   175,     0,     0,     0,
     410,   516,     0,   333,     0,   539,    45,   201,   176,     0,
       0,   374,   373,     0,     0,   332,   359,   360,   411,   536,
       0,   537
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,    11,    93,    94,    95,   259,    12,   260,    13,
      18,    15,    96,    97,   147,    14,    98,   265,   693,   907,
     833,   834,    99,   255,   477,   478,   245,   208,   209,   426,
     481,   215,   293,   294,   100,   101,   272,   698,   102,   103,
     277,   273,   699,   700,   104,   105,   280,   106,   107,   108,
     109,   110,   432,   111,   112,   287,   288,   242,   275,   269,
     270,   113,   252,   114,   530,   295,   529,   115,   116,   302,
     539,   117,   118,   119,   853,   918,   305,   542,   299,   540,
     732,   296,   523,   524,   859,   525,   503,   710,   711,   712,
     355,   190,   356,   191,   192,   411,   653,   635,   193,   661,
     194,   195,   196,   197,   663,   811,   812,   813,   198,   199,
     200,   420,   415,   212,   206,   956,   957,   201,   121,   122,
     310,   738,   739,   927,   928,   123,   124,   125,   126,   127,
     202,   128,   551,   316,   222,   228,   129,   130,   131,   308,
     132,   133,   321,   134,   324,   559,   135,   136,   560,   561,
     137,   138,   580,   341,   585,   328,   235,   566,   567,   139,
     343,   140,   141,   344,   569,   346,   581,   582,   583,   757,
     386,   387,   388,   889,   225,   142,   441,   442,   443,   449,
     677,   451,   143,   552
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -711
static const yytype_int16 yypact[] =
{
    -711,   923,  -711,  -711,  -711,  -711,  -711,  -711,  -711,  -711,
    -711,  -711,   -98,  5724,  -711,  -711,  -711,  -711,  -711,  -711,
    -711,   -51,  -711,   -47,  -711,  -711,  4731,    54,  2666,    70,
     115,  -711,  4731,    34,    40,  2666,   117,  -711,   -63,  -711,
       7,  4731,  -711,  -711,   -39,    66,   -63,  -711,   137,  -711,
    -711,  -711,  -711,  -711,   184,   141,   170,  -711,  -711,   199,
     199,   155,  -711,   175,  -711,  -711,    37,  -711,  -711,    92,
     -63,   -63,  -711,   129,  -711,  -711,   116,  -711,   -63,  -711,
    -711,  -711,  -711,  -711,  -711,  -711,  -711,  -711,  -711,  -711,
    -711,  -711,  -711,   217,  -711,  -711,    31,  -711,    96,  -711,
     -64,   187,    97,   156,  -711,   149,  -711,   384,   390,  -711,
    -711,   395,   165,  -711,  -711,    91,   -15,    80,   125,   372,
     416,  -711,   253,   -98,   -63,  -711,   -63,   -63,  -711,   247,
    4731,  -711,    24,   434,  -711,   267,   278,  2666,  -711,   270,
    5575,   270,  -711,  2341,  -711,  -711,  -711,  -711,  4731,  4731,
    -711,  -711,  4731,   276,   279,   280,  4731,  4731,  4731,   285,
     291,   292,   296,   299,   301,   309,   310,   313,   316,   317,
     318,  4731,   319,   325,   328,   329,   330,  4859,  -711,  -711,
     331,  -711,  -711,  -711,  -711,  -711,  -711,  -711,  4731,  2530,
    -711,  -711,  4731,   304,  -711,   334,   342,  -711,   343,   354,
     -33,  -711,    43,  4731,  -711,  2530,  -711,   356,   528,  -711,
    4731,  2530,  -711,   534,  -711,   376,  -711,   376,  -711,  4731,
    -711,  -711,  -711,  -711,  4731,  -711,  1494,  -711,  -711,  -711,
    -711,   356,  -711,  -711,  4987,  -711,   356,   356,   376,  2588,
    -711,   377,   356,  -711,  4731,   544,  -711,  -711,  -711,  -711,
    -711,  -711,   546,  -711,  -711,   199,  -711,  -711,  5724,  -711,
     -98,   -51,   -51,  -711,  -711,  -711,  -711,   557,   394,   561,
    -711,    21,    33,   144,   397,   139,   388,  -711,   397,   356,
    -711,   139,  -711,   399,   402,   139,   388,   571,  -711,   407,
     733,   575,  -711,  -711,   -51,   572,  -711,  3067,  -711,   401,
     215,  3963,  -711,  5115,   -15,  -711,   425,  -711,   388,  -711,
     601,  -711,  -711,  -711,  -711,   384,   356,  2530,  4731,  4731,
     356,  -711,   429,  4091,  -711,  -711,  -711,  5430,  -711,  -711,
    -711,  -711,  4731,  4731,  4731,  4731,   610,  5430,  -711,   304,
     354,  -711,  1098,   616,   588,   618,  5621,  4859,  -711,   441,
     443,  -711,  -711,  1515,   670,  2530,    27,  4731,  4731,  4731,
      36,  1550,    45,  4731,  4731,  4731,  4731,  4731,  4731,  4731,
    4731,  4731,  4731,  4731,  4731,  1572,  4731,  4731,  4731,  4731,
    4731,  4859,  2530,  1594,   782,  -711,   500,   630,   631,  4731,
    1615,   633,  3451,  4731,  4731,  4731,  4731,  4731,  4731,  4731,
    4731,  4731,  4731,  4731,  4731,  4731,  4731,  3195,  4731,  3579,
    3707,  -711,   128,   356,  4731,  -711,  -711,  -711,  -711,  -711,
    -711,  -711,  4731,  4731,  -711,  1637,    48,   388,    70,  1697,
    -711,  -711,   641,   641,  1819,  1173,  -711,   304,   -24,    -1,
     457,   647,  -711,  -711,   471,   472,   151,    -1,  -711,   650,
    -711,    62,   641,  -711,  -711,  -711,  -711,  -711,  -711,  -711,
    -711,  -711,  -711,  -711,  -711,  -711,  -711,  -711,  -711,  -711,
    -711,  -711,  -711,  -711,  -711,  -711,  -711,  -711,   651,   634,
    -711,    71,  2530,   476,   488,   -63,  -711,  -711,   480,   480,
    -711,  4731,    72,   394,   314,  4731,  -711,   491,   495,   314,
    -711,   384,  2538,  -711,  -711,  -711,   384,   388,   388,   384,
    -711,   407,   571,  -711,  -711,   388,  -711,   486,  -711,  -711,
    -711,  -711,  -711,   358,  -711,  -711,  -711,   480,  -711,  -711,
     497,  4219,  -711,  -711,  2530,  -711,  -711,  3963,  3323,   487,
     665,  4347,  -711,  2530,   489,  -711,   388,    23,  -711,  -711,
    -711,  -711,   667,  2530,  2530,  -711,  -711,   498,  2530,   493,
     672,  -711,  -711,  -711,  5621,  3835,    75,  -711,   119,  -711,
    2530,  2530,  2530,  2530,  4731,   530,  5850,  2530,   304,  1840,
    -711,   674,  2530,  -711,  5850,  -711,  5621,  5621,  5621,  5621,
    5621,   654,  -711,   356,   356,  2666,  4731,  -711,    84,    85,
    1879,  -711,  -711,  -711,  1900,  1921,  1942,  1963,  2006,  2027,
    2145,  2209,  2230,  2251,  2272,  2293,  -711,    86,    88,    89,
      90,    98,  2327,   678,   687,   693,  4859,  -711,  4859,  4859,
      99,  -711,  4731,  4731,  2530,  -711,   710,   710,   569,   569,
     670,   814,   814,   814,   814,   814,   814,   133,   133,   128,
    4731,  4731,  2530,  -711,   128,  4731,   814,  4731,   814,   304,
     695,  2811,   517,   520,  2530,  2530,  -711,   356,  -711,  -711,
    -711,  -711,   532,   619,  4731,  2811,   -49,   526,  -711,  -711,
    -711,   -49,   527,   356,  -711,  2588,  -711,  4731,  -711,  4731,
    -711,  -711,    67,  -711,   681,  2530,  -711,  -711,   689,    50,
     716,  -711,  2348,   691,   538,  -711,   705,   397,  4731,  -711,
     105,  -711,  -711,  1384,   356,   707,   707,   407,  -711,  -711,
     287,   733,   732,  -711,   568,   388,  -711,   558,  4219,   665,
    -711,   570,  -711,   560,  -711,  -711,   738,   617,   741,  -711,
    4731,   388,  -711,  -711,  4091,   536,  -711,  -711,  2811,   519,
    5430,  -711,  -711,  2530,   727,   106,  -711,  2530,  2530,  5850,
    2530,   674,   574,   654,   654,   654,   406,   647,   650,  -711,
    2530,  -711,  -711,  -711,  -711,  -711,  -711,  -711,  -711,  -711,
    -711,  -711,  -711,  -711,  -711,  -711,  -711,  -711,  -711,  -711,
    -711,  -711,  5243,  5243,  5243,  2530,  -711,  2530,  -711,  2530,
    -711,  -711,   573,   814,   814,   128,   814,   814,  4731,  2939,
    1406,   745,  -711,  -711,   334,  -711,   388,  -711,  -711,  2475,
     107,   744,  -711,  -711,  -711,  -711,  -711,  -711,  2530,   109,
    -711,  -711,  -711,   110,  -711,   577,  -711,   314,   314,   314,
     314,   314,   354,  -711,   314,  -711,  -711,  2530,  2538,  -711,
    4475,  -711,  5371,  -711,  -711,   571,  -711,  -711,  -711,   579,
    -711,  -711,   388,   707,  -711,   665,  -711,  4219,  -711,   -50,
     581,   582,  1196,   388,  -711,  -711,  1362,   580,   356,  -711,
    -711,   588,  5891,  2530,  -711,  -711,   583,   584,   747,   585,
     590,   591,  -711,   592,  4731,  1428,  4731,  2811,  -711,  -711,
     754,  -711,   356,  -711,    26,  -711,    93,  -711,  -711,    29,
      29,   586,   113,   737,  -711,  -711,  2530,  4603,  -711,  2530,
    -711,   707,    64,  -711,  -711,   649,  -711,   770,  -711,  -711,
    -711,  4731,  -711,  -711,   457,   596,  2530,  -711,  -711,  4731,
    -711,  -711,  -711,  -711,  2530,  4731,  1471,  -711,  4731,  -711,
    -711,  -711,   111,  -711,   597,    64,  -711,   162,   609,   611,
    1282,  -711,  1305,  2530,  4731,  2530,  -711,  -711,  -711,  4731,
    4731,  -711,  -711,  4731,  4731,  2530,  2530,  2530,  2530,  1328,
    4731,  2530
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -711,  -711,  -711,  -711,   523,  -711,    30,  -711,   774,  -711,
     -66,   772,  -711,  -711,    65,     6,  -711,  -711,  -430,  -711,
    -118,  -110,  -711,  -711,   123,  -711,  -711,  -711,   373,  -711,
     120,     1,  -711,  -711,  -711,  -711,  -711,  -480,  -711,  -711,
    -264,   312,  -439,  -711,  -711,  -711,  -711,  -711,  -711,  -711,
    -711,  -711,   -68,  -711,  -711,  -276,   300,   -35,   -85,   548,
     324,  -711,  -711,  -711,   274,  -711,  -711,  -711,  -711,  -711,
     281,  -711,  -711,  -711,  -699,  -711,  -711,  -711,   515,  -290,
    -666,  -711,  -711,   100,  -711,   749,  -104,  -711,   -25,   -17,
     -26,  -136,    56,  -176,  -432,  -497,  -711,  -711,  -103,  -711,
    -711,  -711,  -204,  -711,   404,   159,   -57,    95,    -5,   -70,
    -711,  -711,  -197,  -398,   -30,  -108,  -711,  -166,  -711,  -711,
    -711,  -711,   -23,  -711,  -105,  -711,  -711,  -711,  -711,  -711,
    -711,  -711,  -711,  -711,   -28,  -711,  -711,  -711,   708,   302,
    -208,  -711,  -711,  -711,  -711,  -711,  -711,  -711,  -711,   118,
    -711,  -711,  -711,  -711,  -711,   277,  -711,  -711,   114,  -711,
    -711,  -711,  -711,  -306,   720,  -263,  -226,  -711,  -673,  -554,
     514,   482,  -326,  -710,   308,  -711,   273,  -202,  -711,   275,
    -416,  -711,  -711,  -711
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -511
static const yytype_int16 yytable[] =
{
     189,   384,   205,   421,   338,   218,   211,   533,   120,   205,
     500,   385,   391,   512,   504,   226,   660,   854,   230,   706,
     281,   568,   283,   440,   145,   758,   285,   438,   760,   318,
     596,   575,   438,   682,   448,   217,   282,   339,   213,   596,
     591,   383,   249,   250,   213,   494,   422,   423,   596,   830,
     257,   667,   840,   837,   736,   625,   243,   311,   445,   694,
     238,   239,   701,   866,   261,   683,   262,   701,   278,  -361,
     340,   838,   839,   840,   687,   493,   821,   289,   750,    17,
     756,   590,   756,   890,   891,   756,   885,   596,   596,   596,
     830,   596,   596,   596,   290,   291,   312,   723,   313,   314,
     274,   596,   596,   300,   317,   267,   220,   326,   848,   882,
     897,   205,   687,   904,   904,   268,   830,  -164,   146,   925,
      91,  -164,   353,   354,   221,   419,   292,   263,   437,   266,
     227,   361,   148,   437,   838,   839,   840,   926,   351,   586,
     329,   330,   587,   588,   589,   375,   414,   498,   303,   433,
     319,   382,   279,   408,   297,   416,   406,   407,   408,   240,
     246,   298,   390,   241,   922,  -315,   412,   969,   499,  -164,
     452,   384,   935,  -164,   247,   240,   240,   425,   675,   241,
     241,   385,   510,   223,   429,   240,   224,   497,  -314,   241,
    -361,   338,   737,   434,   487,   831,   253,   254,   435,   923,
     495,   338,   427,   213,   546,   623,   577,   597,   382,   885,
     338,   383,   360,   320,   362,   385,   601,   841,   482,   248,
     496,   214,   955,   424,   339,   603,   439,   216,   668,   446,
     549,   447,   450,   203,   339,   229,   831,   480,   841,   578,
     501,   726,   684,   339,   258,   383,   506,   832,   729,   207,
     509,   688,   696,   120,   264,   751,   485,   340,   745,   301,
     884,   884,   831,   884,   771,   772,   786,   340,   787,   788,
     789,   534,   579,   951,   505,   534,   340,   543,   790,   801,
     762,   763,   764,   765,   766,   849,   883,   901,   970,   903,
     905,   966,   553,   554,   210,   490,   219,   558,   251,   752,
     796,   841,   798,   800,   304,   590,   570,   571,   572,   573,
     659,   550,   590,   409,   410,   555,   231,   548,   409,   410,
     236,   382,   565,   669,    91,   276,   488,   489,   590,   936,
     417,   680,   565,   600,   286,   150,   151,   604,   605,   606,
     607,   608,   609,   610,   611,   612,   613,   614,   615,   237,
     755,   886,   887,   232,   244,   622,   271,   908,   761,   527,
     233,   721,   722,   234,   913,   391,   634,   636,   637,   638,
     639,   640,   641,   642,   643,   644,   645,   646,   647,   648,
     649,   652,   654,   656,   658,   440,   756,   282,   211,   438,
     438,   536,   448,   284,   537,   306,   664,   665,   282,   909,
     910,   911,   912,   715,   716,   701,   701,   701,   701,   701,
     893,   719,   701,   598,   599,  -164,   707,   342,   347,  -164,
     307,   714,   309,   315,   717,   856,   857,   858,   338,   587,
     588,   589,   617,   618,   619,   620,   621,   322,   865,   884,
     577,   855,   735,   846,   881,   630,   323,   325,   577,   327,
     338,   338,   338,   338,   338,   357,   802,   691,   358,   359,
     385,   339,   385,   385,   363,   695,   796,   798,   800,   702,
     364,   365,   438,   578,   822,   366,   713,   438,   367,   824,
     368,   578,   590,   339,   339,   339,   339,   339,   369,   370,
     437,   437,   371,   413,   340,   372,   373,   374,   376,   590,
     590,   590,   590,   590,   377,   534,   579,   378,   379,   380,
     389,   534,   534,   414,   579,   390,   340,   340,   340,   340,
     340,   416,   417,   418,   392,    91,   393,   394,   395,   396,
     397,   428,   398,   399,   400,   401,   402,   403,   430,   749,
     404,   405,   406,   407,   408,   431,   479,   483,   753,   484,
     586,   329,   330,   587,   588,   589,   586,   329,   330,   587,
     588,   589,   491,   267,   493,   769,   276,   502,   507,   205,
     770,   508,   391,   437,   511,   528,   286,   924,   437,   526,
     397,   535,   398,   399,   400,   401,   402,   403,   439,   447,
     404,   405,   406,   407,   408,   329,   330,   587,   588,   589,
     795,   545,   797,   799,   547,   556,   382,   803,   586,   329,
     330,   587,   588,   589,   338,   574,   802,   898,   590,   584,
     593,   863,   594,   577,   804,   805,   385,   385,   385,   806,
     842,   807,   627,   628,   629,   810,   632,   873,  -490,  -490,
    -490,  -490,  -490,  -490,   672,   879,   320,   339,   819,   810,
     676,   678,   679,   681,   685,   689,   578,   690,   686,   692,
     703,   828,   816,   482,   704,   720,   725,   730,   731,   734,
     934,   447,   740,   743,   223,   744,   447,   759,   826,   589,
     340,   792,   847,   398,   399,   400,   401,   402,   403,   579,
     793,   404,   405,   406,   407,   408,   794,   814,   502,   808,
     815,   817,   534,   818,   409,   410,   823,   825,   878,   851,
     754,   835,   899,   836,   872,   844,   875,   495,   558,   395,
     396,   397,   876,   398,   399,   400,   401,   402,   403,   845,
     852,   404,   405,   406,   407,   408,   861,   862,   864,   867,
     868,   391,   869,   870,   871,   565,   577,  -491,   897,   902,
     929,   737,   939,   892,   409,   410,   906,   948,   921,   920,
     933,   953,   513,   937,   938,   940,   795,   797,   799,   932,
     941,   942,   943,   959,   841,   958,   961,   967,   971,   578,
     925,   486,   211,   895,    16,   626,   144,  -247,   952,  -247,
    -247,  -247,  -247,  -247,   950,  -247,  -247,  -247,  -247,  -247,
    -247,   670,   724,  -247,  -247,  -247,  -247,  -247,   827,   829,
     705,   718,   579,   253,   254,   514,   492,   697,   727,   544,
     662,   860,   713,   914,   916,   256,   919,  -511,  -511,  -511,
    -511,  -511,  -511,   915,   820,   404,   405,   406,   407,   408,
     947,   534,   515,   877,   516,   517,   518,   968,   930,   519,
     520,   352,   521,   522,   972,   409,   410,   741,   178,   179,
     345,   592,   874,   624,   880,   742,   767,     0,   944,   768,
     946,   810,     0,   351,   181,   182,     0,     0,   183,   184,
     185,   186,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   390,   187,     0,     0,   409,   410,   949,     0,     0,
       0,     0,     0,     0,     0,   960,     0,     0,     0,     0,
       0,     0,     0,   962,     0,     0,     0,     0,     0,   963,
       0,     0,   965,     2,     3,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   975,     0,
       0,     0,     0,   976,   977,     0,     0,   978,   979,     0,
     -23,     0,   -23,     0,   981,   -23,   -23,   -23,   -23,     4,
       5,   -23,     0,     0,   -23,   -23,   -23,  -247,  -247,     0,
       0,   -23,   -23,     0,     0,     0,     6,     7,     0,     0,
       0,     0,     0,     0,     0,     0,   -23,     0,     0,     0,
     -23,   -23,     0,     0,     0,   -23,     0,     0,     0,   409,
     410,   -23,     0,   -23,   -23,     0,   -23,     0,   -23,   -23,
     -23,   -23,   -23,     0,   -23,   -23,   -23,   -23,   -23,   -23,
     -23,   -23,   -23,   -23,   -23,   -23,   -23,   -23,   -23,   -23,
       0,     0,   -23,   -23,   -23,     0,   -23,   -23,   -23,     0,
     -23,   -23,   -23,     0,     0,   -23,     0,   -23,     0,     0,
     -23,   -23,   -23,   -23,     0,     0,   -23,   -23,   -23,     0,
       0,     0,     0,     0,   -23,     0,     0,     8,   -23,   -23,
     -23,   -23,   -23,   -23,   -23,     0,   -23,   -23,   -23,   -23,
     -23,     0,     0,   -23,     0,     0,     0,     0,     0,     0,
     -23,   -23,   -23,     0,     0,     0,     0,     0,     0,   -23,
       9,  -510,     0,  -510,     0,  -510,  -510,  -510,  -510,  -510,
      10,  -510,  -510,  -510,  -510,  -510,  -510,     0,     0,  -510,
    -510,  -510,  -510,  -510,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   152,   153,     0,
       0,     0,   154,   155,   156,   157,     0,     0,     0,     0,
       0,     0,     0,     0,   158,   159,   160,   161,   162,   163,
     164,     0,   165,   166,   167,     0,     0,   168,   169,   170,
       0,   171,   172,   173,   174,   175,   674,   176,   392,     0,
     393,   394,   395,   396,   397,     0,   398,   399,   400,   401,
     402,   403,     0,     0,   404,   405,   406,   407,   408,   931,
       0,   392,     0,   393,   394,   395,   396,   397,     0,   398,
     399,   400,   401,   402,   403,     0,     0,   404,   405,   406,
     407,   408,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     178,   179,     0,     0,     0,     0,   180,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   181,   182,     0,     0,
     183,   184,   185,   186,     0,     0,     0,    91,     0,     0,
       0,     0,     0,     0,   187,     0,     0,   576,     0,     0,
       0,     0,     0,  -510,  -510,   973,     0,   392,     0,   393,
     394,   395,   396,   397,     0,   398,   399,   400,   401,   402,
     403,     0,     0,   404,   405,   406,   407,   408,   974,     0,
     392,     0,   393,   394,   395,   396,   397,     0,   398,   399,
     400,   401,   402,   403,     0,     0,   404,   405,   406,   407,
     408,   980,     0,   392,     0,   393,   394,   395,   396,   397,
       0,   398,   399,   400,   401,   402,   403,     0,     0,   404,
     405,   406,   407,   408,     0,     0,     0,     0,   409,   410,
       0,     0,     0,     0,     0,     0,   896,   392,     0,   393,
     394,   395,   396,   397,     0,   398,   399,   400,   401,   402,
     403,   409,   410,   404,   405,   406,   407,   408,   850,   392,
       0,   393,   394,   395,   396,   397,     0,   398,   399,   400,
     401,   402,   403,     0,     0,   404,   405,   406,   407,   408,
     896,   392,     0,   393,   394,   395,   396,   397,     0,   398,
     399,   400,   401,   402,   403,     0,     0,   404,   405,   406,
     407,   408,   945,   392,     0,   393,   394,   395,   396,   397,
       0,   398,   399,   400,   401,   402,   403,     0,     0,   404,
     405,   406,   407,   408,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   409,   410,     0,
       0,     0,     0,     0,     0,   964,   392,     0,   393,   394,
     395,   396,   397,     0,   398,   399,   400,   401,   402,   403,
     409,   410,   404,   405,   406,   407,   408,     0,     0,   392,
       0,   393,   394,   395,   396,   397,     0,   398,   399,   400,
     401,   402,   403,   409,   410,   404,   405,   406,   407,   408,
     392,     0,   393,   394,   395,   396,   397,     0,   398,   399,
     400,   401,   402,   403,     0,     0,   404,   405,   406,   407,
     408,     0,   631,     0,     0,     0,     0,   409,   410,     0,
       0,     0,     0,     0,     0,   392,     0,   393,   394,   395,
     396,   397,     0,   398,   399,   400,   401,   402,   403,   409,
     410,   404,   405,   406,   407,   408,     0,   392,     0,   393,
     394,   395,   396,   397,     0,   398,   399,   400,   401,   402,
     403,   409,   410,   404,   405,   406,   407,   408,     0,  -250,
       0,  -250,  -250,  -250,  -250,  -250,     0,  -250,  -250,  -250,
    -250,  -250,  -250,   409,   410,  -250,  -250,  -250,  -250,  -250,
     392,     0,   393,   394,   395,   396,   397,     0,   398,   399,
     400,   401,   402,   403,     0,     0,   404,   405,   406,   407,
     408,     0,   392,     0,   393,   394,   395,   396,   397,     0,
     398,   399,   400,   401,   402,   403,   409,   410,   404,   405,
     406,   407,   408,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   436,     0,     0,     0,     0,   409,
     410,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   595,     0,     0,     0,     0,
     409,   410,   392,     0,   393,   394,   395,   396,   397,     0,
     398,   399,   400,   401,   402,   403,     0,     0,   404,   405,
     406,   407,   408,     0,     0,     0,     0,     0,     0,     0,
     602,     0,     0,     0,     0,   409,   410,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   616,     0,     0,     0,     0,   409,   410,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -250,     0,     0,     0,     0,  -250,
    -250,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   631,     0,     0,     0,     0,
     409,   410,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   666,     0,     0,
       0,     0,   409,   410,   392,     0,   393,   394,   395,   396,
     397,     0,   398,   399,   400,   401,   402,   403,     0,     0,
     404,   405,   406,   407,   408,   392,     0,   393,   394,   395,
     396,   397,     0,   398,   399,   400,   401,   402,   403,     0,
       0,   404,   405,   406,   407,   408,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   671,     0,     0,
       0,     0,   409,   410,   392,     0,   393,   394,   395,   396,
     397,     0,   398,   399,   400,   401,   402,   403,     0,     0,
     404,   405,   406,   407,   408,   392,     0,   393,   394,   395,
     396,   397,     0,   398,   399,   400,   401,   402,   403,     0,
       0,   404,   405,   406,   407,   408,   392,     0,   393,   394,
     395,   396,   397,     0,   398,   399,   400,   401,   402,   403,
       0,     0,   404,   405,   406,   407,   408,   392,     0,   393,
     394,   395,   396,   397,     0,   398,   399,   400,   401,   402,
     403,     0,     0,   404,   405,   406,   407,   408,   392,     0,
     393,   394,   395,   396,   397,     0,   398,   399,   400,   401,
     402,   403,     0,     0,   404,   405,   406,   407,   408,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   673,
       0,     0,     0,     0,   409,   410,     0,     0,     0,   418,
       0,   392,     0,   393,   394,   395,   396,   397,     0,   398,
     399,   400,   401,   402,   403,   409,   410,   404,   405,   406,
     407,   408,   392,     0,   393,   394,   395,   396,   397,     0,
     398,   399,   400,   401,   402,   403,     0,     0,   404,   405,
     406,   407,   408,     0,     0,     0,     0,     0,     0,   773,
       0,     0,     0,     0,   409,   410,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     774,     0,     0,     0,     0,   409,   410,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   775,     0,     0,     0,     0,   409,   410,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   776,     0,     0,     0,     0,   409,   410,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   777,     0,     0,     0,     0,   409,   410,
     392,     0,   393,   394,   395,   396,   397,     0,   398,   399,
     400,   401,   402,   403,     0,     0,   404,   405,   406,   407,
     408,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   778,     0,     0,     0,
       0,   409,   410,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   779,     0,     0,
       0,     0,   409,   410,   392,     0,   393,   394,   395,   396,
     397,     0,   398,   399,   400,   401,   402,   403,     0,     0,
     404,   405,   406,   407,   408,   392,     0,   393,   394,   395,
     396,   397,     0,   398,   399,   400,   401,   402,   403,     0,
       0,   404,   405,   406,   407,   408,   392,     0,   393,   394,
     395,   396,   397,     0,   398,   399,   400,   401,   402,   403,
       0,     0,   404,   405,   406,   407,   408,   392,     0,   393,
     394,   395,   396,   397,     0,   398,   399,   400,   401,   402,
     403,     0,     0,   404,   405,   406,   407,   408,   392,     0,
     393,   394,   395,   396,   397,     0,   398,   399,   400,   401,
     402,   403,     0,     0,   404,   405,   406,   407,   408,     0,
       0,     0,     0,     0,     0,   780,     0,     0,     0,     0,
     409,   410,   392,     0,   393,   394,   395,   396,   397,     0,
     398,   399,   400,   401,   402,   403,     0,     0,   404,   405,
     406,   407,   408,   392,     0,   393,   394,   395,   396,   397,
       0,   398,   399,   400,   401,   402,   403,     0,     0,   404,
     405,   406,   407,   408,     0,     0,     0,     0,     0,    22,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   781,
       0,     0,     0,     0,   409,   410,     0,     0,     0,     0,
       0,     0,     0,     0,    28,     0,     0,     0,     0,     0,
     782,     0,     0,     0,     0,   409,   410,     0,     0,    32,
       0,     0,     0,     0,    35,   348,     0,     0,     0,    39,
      40,   783,     0,     0,     0,     0,   409,   410,     0,   349,
      49,    50,    51,    52,    53,    54,   350,     0,     0,     0,
       0,     0,   784,     0,     0,     0,     0,   409,   410,    64,
       0,     0,     0,     0,     0,    67,     0,     0,     0,     0,
       0,     0,     0,   785,    72,     0,    74,     0,   409,   410,
     392,     0,   393,   394,   395,   396,   397,     0,   398,   399,
     400,   401,   402,   403,     0,     0,   404,   405,   406,   407,
     408,     0,     0,     0,     0,     0,     0,   791,     0,     0,
      91,     0,   409,   410,     0,     0,     0,    92,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   843,     0,
       0,     0,     0,   409,   410,   392,     0,   393,   394,   395,
     396,   397,   708,   398,   399,   400,   401,   402,   403,     0,
     149,   404,   405,   406,   407,   408,     0,     0,     0,   150,
     151,   709,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   152,   153,     0,
       0,     0,   154,   155,   156,   157,     0,     0,     0,     0,
       0,     0,     0,     0,   158,   159,   160,   161,   162,   163,
     164,     0,   165,   166,   167,     0,     0,   168,   169,   170,
       0,   171,   172,   173,   174,   175,     0,   176,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   453,     0,
       0,     0,   454,   455,   456,   457,     0,     0,     0,     0,
       0,     0,     0,     0,   458,   459,   460,   461,   462,   463,
     464,     0,   465,   466,   467,   900,     0,   468,   469,   470,
     409,   410,   471,   472,   473,   474,     0,   475,     0,   177,
       0,     0,     0,     0,     0,     0,     0,     0,   149,     0,
     178,   179,     0,     0,     0,     0,   180,   150,   151,     0,
       0,     0,     0,     0,     0,     0,   181,   182,     0,     0,
     183,   184,   185,   186,     0,   152,   153,    91,     0,     0,
     154,   155,   156,   157,   187,   409,   410,   188,     0,     0,
       0,     0,   158,   159,   160,   161,   162,   163,   164,     0,
     165,   166,   167,     0,     0,   168,   169,   170,     0,   171,
     172,   173,   174,   175,     0,   176,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   476,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   177,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   178,   179,
       0,     0,     0,     0,   180,   809,     0,     0,     0,     0,
       0,     0,     0,   149,   181,   182,     0,     0,   183,   184,
     185,   186,   150,   151,     0,    91,     0,     0,     0,     0,
       0,     0,   187,     0,     0,   188,     0,     0,     0,     0,
     152,   153,     0,   204,     0,   154,   155,   156,   157,     0,
       0,     0,     0,     0,     0,     0,     0,   158,   159,   160,
     161,   162,   163,   164,     0,   165,   166,   167,     0,     0,
     168,   169,   170,     0,   171,   172,   173,   174,   175,     0,
     176,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   177,   894,     0,     0,     0,     0,     0,     0,
       0,   149,     0,   178,   179,     0,     0,     0,     0,   180,
     150,   151,     0,     0,     0,     0,     0,     0,     0,   181,
     182,     0,     0,   183,   184,   185,   186,     0,   152,   153,
      91,     0,     0,   154,   155,   156,   157,   187,     0,     0,
     188,     0,     0,     0,     0,   158,   159,   160,   161,   162,
     163,   164,     0,   165,   166,   167,     0,     0,   168,   169,
     170,     0,   171,   172,   173,   174,   175,     0,   176,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     177,     0,   531,     0,     0,     0,     0,     0,     0,   149,
       0,   178,   179,     0,     0,     0,     0,   180,   150,   151,
     532,     0,     0,     0,     0,     0,     0,   181,   182,     0,
       0,   183,   184,   185,   186,     0,   152,   153,    91,     0,
       0,   154,   155,   156,   157,   187,     0,     0,   188,     0,
       0,     0,     0,   158,   159,   160,   161,   162,   163,   164,
       0,   165,   166,   167,     0,     0,   168,   169,   170,     0,
     171,   172,   173,   174,   175,     0,   176,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   177,     0,
     650,     0,     0,     0,     0,     0,     0,   149,     0,   178,
     179,     0,     0,     0,     0,   180,   150,   151,     0,   651,
       0,     0,     0,     0,     0,   181,   182,     0,     0,   183,
     184,   185,   186,     0,   152,   153,    91,     0,     0,   154,
     155,   156,   157,   187,     0,     0,   188,     0,     0,     0,
       0,   158,   159,   160,   161,   162,   163,   164,     0,   165,
     166,   167,     0,     0,   168,   169,   170,     0,   171,   172,
     173,   174,   175,     0,   176,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   177,     0,   728,     0,
       0,     0,     0,     0,     0,   149,     0,   178,   179,     0,
       0,     0,     0,   180,   150,   151,   532,     0,     0,     0,
       0,     0,     0,   181,   182,     0,     0,   183,   184,   185,
     186,     0,   152,   153,    91,     0,     0,   154,   155,   156,
     157,   187,     0,     0,   188,     0,     0,     0,     0,   158,
     159,   160,   161,   162,   163,   164,     0,   165,   166,   167,
       0,     0,   168,   169,   170,     0,   171,   172,   173,   174,
     175,     0,   176,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   177,     0,   633,     0,     0,     0,
       0,     0,     0,   149,     0,   178,   179,     0,     0,     0,
       0,   180,   150,   151,     0,     0,     0,     0,     0,     0,
       0,   181,   182,     0,     0,   183,   184,   185,   186,     0,
     152,   153,    91,     0,     0,   154,   155,   156,   157,   187,
       0,     0,   188,     0,     0,     0,     0,   158,   159,   160,
     161,   162,   163,   164,     0,   165,   166,   167,     0,     0,
     168,   169,   170,     0,   171,   172,   173,   174,   175,     0,
     176,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   177,     0,   655,     0,     0,     0,     0,     0,
       0,   149,     0,   178,   179,     0,     0,     0,     0,   180,
     150,   151,     0,     0,     0,     0,     0,     0,     0,   181,
     182,     0,     0,   183,   184,   185,   186,     0,   152,   153,
      91,     0,     0,   154,   155,   156,   157,   187,     0,     0,
     188,     0,     0,     0,     0,   158,   159,   160,   161,   162,
     163,   164,     0,   165,   166,   167,     0,     0,   168,   169,
     170,     0,   171,   172,   173,   174,   175,     0,   176,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     177,     0,   657,     0,     0,     0,     0,     0,     0,   149,
       0,   178,   179,     0,     0,     0,     0,   180,   150,   151,
       0,     0,     0,     0,     0,     0,     0,   181,   182,     0,
       0,   183,   184,   185,   186,     0,   152,   153,    91,     0,
       0,   154,   155,   156,   157,   187,     0,     0,   188,     0,
       0,     0,     0,   158,   159,   160,   161,   162,   163,   164,
       0,   165,   166,   167,     0,     0,   168,   169,   170,     0,
     171,   172,   173,   174,   175,     0,   176,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   177,     0,
       0,     0,     0,     0,     0,     0,     0,   149,     0,   178,
     179,     0,     0,     0,     0,   180,   150,   151,   746,     0,
     747,     0,     0,     0,     0,   181,   182,     0,     0,   183,
     184,   185,   186,     0,   152,   153,    91,     0,     0,   154,
     155,   156,   157,   187,     0,     0,   188,     0,     0,     0,
       0,   158,   159,   160,   161,   162,   163,   164,     0,   165,
     166,   167,     0,     0,   168,   169,   170,     0,   171,   172,
     173,   174,   175,     0,   176,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   177,     0,     0,     0,
       0,     0,     0,     0,     0,   149,     0,   178,   179,     0,
       0,     0,     0,   180,   150,   151,   532,     0,     0,     0,
       0,     0,     0,   181,   182,     0,     0,   183,   184,   185,
     186,     0,   152,   153,    91,     0,     0,   154,   155,   156,
     157,   187,     0,     0,   748,     0,     0,     0,     0,   158,
     159,   160,   161,   162,   163,   164,     0,   165,   166,   167,
       0,     0,   168,   169,   170,     0,   171,   172,   173,   174,
     175,     0,   176,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   177,     0,     0,     0,     0,     0,
       0,     0,     0,   149,     0,   178,   179,     0,     0,     0,
       0,   180,   150,   151,   557,     0,     0,     0,     0,     0,
       0,   181,   182,     0,     0,   183,   184,   185,   186,     0,
     152,   153,   538,     0,     0,   154,   155,   156,   157,   187,
       0,     0,   188,     0,     0,     0,     0,   158,   159,   160,
     161,   162,   163,   164,     0,   165,   166,   167,     0,     0,
     168,   169,   170,     0,   171,   172,   173,   174,   175,     0,
     176,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   177,     0,     0,     0,     0,     0,     0,     0,
       0,   149,     0,   178,   179,     0,     0,     0,     0,   180,
     150,   151,   532,     0,     0,     0,     0,     0,     0,   181,
     182,     0,     0,   183,   184,   185,   186,     0,   152,   153,
      91,     0,     0,   154,   155,   156,   157,   187,     0,     0,
     188,     0,     0,     0,     0,   158,   159,   160,   161,   162,
     163,   164,     0,   165,   166,   167,     0,     0,   168,   169,
     170,     0,   171,   172,   173,   174,   175,     0,   176,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     177,     0,     0,     0,     0,     0,     0,     0,     0,   149,
       0,   178,   179,     0,     0,     0,     0,   180,   150,   151,
     733,     0,     0,     0,     0,     0,     0,   181,   182,     0,
       0,   183,   184,   185,   186,     0,   152,   153,    91,     0,
       0,   154,   155,   156,   157,   187,     0,     0,   188,     0,
       0,     0,     0,   158,   159,   160,   161,   162,   163,   164,
       0,   165,   166,   167,     0,     0,   168,   169,   170,     0,
     171,   172,   173,   174,   175,     0,   176,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   177,     0,
       0,     0,     0,     0,     0,     0,     0,   149,     0,   178,
     179,     0,     0,     0,     0,   180,   150,   151,   709,     0,
       0,     0,     0,     0,     0,   181,   182,     0,     0,   183,
     184,   185,   186,     0,   152,   153,    91,     0,     0,   154,
     155,   156,   157,   187,     0,     0,   188,     0,     0,     0,
       0,   158,   159,   160,   161,   162,   163,   164,     0,   165,
     166,   167,     0,     0,   168,   169,   170,     0,   171,   172,
     173,   174,   175,     0,   176,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   177,     0,     0,     0,
       0,     0,     0,     0,     0,   149,     0,   178,   179,     0,
       0,     0,     0,   180,   150,   151,   954,     0,     0,     0,
       0,     0,     0,   181,   182,     0,     0,   183,   184,   185,
     186,     0,   152,   153,    91,     0,     0,   154,   155,   156,
     157,   187,     0,     0,   188,     0,     0,     0,     0,   158,
     159,   160,   161,   162,   163,   164,     0,   165,   166,   167,
       0,     0,   168,   169,   170,     0,   171,   172,   173,   174,
     175,     0,   176,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   177,     0,     0,     0,     0,     0,
       0,     0,     0,   149,     0,   178,   179,     0,     0,     0,
       0,   180,   150,   151,     0,     0,     0,     0,     0,     0,
       0,   181,   182,     0,     0,   183,   184,   185,   186,     0,
     152,   153,    91,     0,     0,   154,   155,   156,   157,   187,
       0,     0,   188,     0,     0,     0,     0,   158,   159,   160,
     161,   162,   163,   164,     0,   165,   166,   167,     0,     0,
     168,   169,   170,     0,   171,   172,   173,   174,   175,     0,
     176,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   177,     0,     0,     0,     0,     0,     0,     0,
       0,   149,     0,   178,   179,     0,     0,     0,     0,   180,
     150,   151,     0,     0,     0,     0,     0,     0,     0,   181,
     182,     0,     0,   183,   184,   185,   186,     0,   152,   153,
      91,     0,     0,   154,   155,   156,   157,   187,     0,     0,
     188,     0,     0,     0,     0,   158,   159,   160,   161,   162,
     163,   164,     0,   165,   166,   167,     0,     0,   168,   169,
     170,     0,   171,   172,   173,   174,   175,     0,   176,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     177,     0,     0,     0,     0,     0,     0,     0,     0,   149,
       0,   178,   179,     0,     0,     0,     0,   180,   150,   151,
       0,     0,     0,     0,     0,     0,     0,   181,   182,     0,
       0,   183,   184,   185,   186,     0,   152,   153,    91,     0,
       0,   154,   155,   156,   157,   187,     0,     0,   381,     0,
       0,     0,     0,   158,   159,   160,   161,   162,   163,   164,
       0,   165,   166,   167,     0,     0,   168,   169,   170,     0,
     171,   172,   173,   174,   175,     0,   176,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   177,     0,
       0,     0,     0,     0,     0,     0,     0,   149,     0,   178,
     179,     0,     0,     0,     0,   180,   150,   151,     0,     0,
       0,     0,     0,     0,     0,   181,   182,     0,     0,   183,
     184,   185,   186,     0,   152,   153,    91,     0,     0,   154,
     155,   156,   157,   444,     0,     0,   188,     0,     0,     0,
       0,   158,   159,   160,   161,   162,   163,   164,     0,   165,
     166,   167,     0,     0,   168,   169,   170,     0,   171,   172,
     173,   174,   175,     0,   176,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   177,     0,     0,     0,
       0,     0,     0,     0,     0,   149,     0,   178,   179,     0,
       0,     0,     0,   180,   150,   151,     0,     0,     0,     0,
       0,     0,     0,   181,   182,     0,     0,   183,   184,   185,
     186,     0,   152,   153,    91,     0,     0,   154,   155,   156,
     157,   187,     0,     0,   541,     0,     0,     0,     0,   158,
     159,   160,   161,   162,   163,   164,     0,   165,   166,   167,
       0,     0,   168,   169,   170,     0,   171,   172,   173,   174,
     175,     0,   176,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   177,     0,     0,     0,     0,     0,
       0,     0,     0,   149,     0,   178,   179,     0,     0,     0,
       0,   180,   150,   151,     0,     0,     0,     0,     0,     0,
       0,   181,   182,     0,     0,   183,   184,   185,   186,     0,
     152,   153,   888,     0,     0,   154,   155,   156,   157,   187,
       0,     0,   381,     0,     0,     0,     0,   158,   159,   160,
     161,   162,   163,   164,     0,   165,   166,   167,     0,     0,
     168,   169,   170,     0,   171,   172,   173,   174,   175,     0,
     176,   329,   330,   562,     0,   563,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   152,
     153,     0,     0,     0,   154,   155,   156,   157,     0,     0,
     332,   333,   334,     0,     0,   335,   158,   159,   160,   161,
     162,   163,   164,     0,   165,   166,   167,     0,     0,   168,
     169,   170,   177,   171,   172,   173,   174,   175,     0,   176,
       0,     0,     0,   178,   179,     0,     0,     0,     0,   180,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   181,
     182,     0,     0,   183,   184,   185,   186,     0,     0,     0,
      91,     0,     0,     0,     0,     0,     0,   187,     0,     0,
     917,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   178,   179,     0,     0,     0,     0,   180,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   181,   182,
       0,     0,   183,   184,   185,   186,   329,   330,   331,   336,
       0,     0,     0,     0,     0,     0,   187,     0,     0,   564,
       0,     0,     0,     0,   152,   153,     0,     0,     0,   154,
     155,   156,   157,     0,     0,   332,   333,   334,     0,     0,
     335,   158,   159,   160,   161,   162,   163,   164,     0,   165,
     166,   167,   329,   330,   168,   169,   170,     0,   171,   172,
     173,   174,   175,     0,   176,     0,     0,     0,     0,     0,
     152,   153,     0,     0,     0,   154,   155,   156,   157,     0,
       0,   332,   333,   334,     0,     0,   335,   158,   159,   160,
     161,   162,   163,   164,     0,   165,   166,   167,     0,     0,
     168,   169,   170,     0,   171,   172,   173,   174,   175,     0,
     176,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   178,   179,     0,
       0,     0,     0,   180,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   181,   182,     0,     0,   183,   184,   185,
     186,     0,     0,     0,   336,     0,     0,     0,     0,     0,
       0,   187,     0,    19,   337,     0,    20,   -28,    21,   -28,
       0,     0,    22,   178,   179,    23,    24,    25,     0,   180,
       0,     0,    26,    27,     0,     0,     0,     0,     0,   181,
     182,     0,     0,   183,   184,   185,   186,    28,     0,     0,
     336,    29,    30,     0,     0,     0,    31,   187,     0,     0,
     564,     0,    32,     0,    33,    34,     0,    35,     0,    36,
      37,    38,    39,    40,     0,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,     0,     0,    57,    58,    59,     0,    60,    61,    62,
       0,    63,    64,    65,     0,     0,    66,     0,    67,     0,
       0,    68,    69,    70,    71,     0,     0,    72,    73,    74,
       0,     0,     0,     0,     0,    75,     0,     0,     0,    76,
      77,    78,    79,    80,    81,    82,     0,    83,    84,    85,
      86,    87,     0,     0,    88,     0,     0,     0,     0,   152,
     153,    89,    90,    91,   154,   155,   156,   157,     0,     0,
      92,     0,     0,     0,     0,     0,   158,   159,   160,   161,
     162,   163,   164,     0,   165,   166,   167,     0,     0,   168,
     169,   170,     0,   171,   172,   173,   174,   175,     0,   176,
     152,   153,     0,     0,     0,   154,   155,   156,   157,     0,
       0,     0,     0,     0,     0,     0,     0,   158,   159,   160,
     161,   162,   163,   164,     0,   165,   166,   167,     0,     0,
     168,   169,   170,     0,   171,   172,   173,   174,   175,     0,
     176,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   178,   179,     0,     0,     0,     0,   180,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   181,   182,
       0,     0,   183,   184,   185,   186,     0,     0,     0,    91,
       0,     0,     0,     0,     0,     0,   187,     0,     0,   576,
       0,     0,     0,   178,   179,     0,     0,     0,     0,   180,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   181,
     182,     0,     0,   183,   184,   185,   186,     0,     0,     0,
     888,     0,     0,     0,     0,     0,     0,   187,     0,     0,
     576
};

static const yytype_int16 yycheck[] =
{
      26,   177,    28,   200,   140,    35,    32,   297,    13,    35,
     274,   177,   188,   289,   278,    41,   414,   716,    46,   499,
     105,   327,   107,   231,    18,   579,   111,   231,   582,     5,
       3,   337,   236,   449,   236,    34,     3,   140,     4,     3,
     346,   177,    70,    71,     4,    24,     3,     4,     3,    23,
      78,     3,    23,     3,    31,   381,    61,   123,   234,   489,
      59,    60,   494,   729,    33,     3,    35,   499,   103,     5,
     140,    21,    22,    23,     3,     3,   125,   112,     3,   177,
     577,   344,   579,   793,   794,   582,   759,     3,     3,     3,
      23,     3,     3,     3,     3,     4,   124,   527,   126,   127,
       3,     3,     3,    23,   130,   169,   169,   137,     3,     3,
       3,   137,     3,     3,     3,   179,    23,    20,   169,   169,
     169,    24,   148,   149,   187,   158,    35,    97,   231,    99,
     169,   157,   179,   236,    21,    22,    23,   187,   143,    20,
      21,    22,    23,    24,    25,   171,   179,     3,    23,   217,
     126,   177,     3,    25,   169,   179,    23,    24,    25,    20,
     123,   176,   188,    24,   863,   189,   192,     5,    24,    20,
     238,   347,   882,    24,   137,    20,    20,   203,   179,    24,
      24,   347,   286,   176,   210,    20,   179,   272,   189,    24,
     126,   327,   169,   219,   260,   169,    80,    81,   224,   865,
     179,   337,   207,     4,   308,   381,   342,   180,   234,   882,
     346,   347,   156,   189,   158,   381,   180,   188,   244,   127,
     187,   187,   921,   180,   327,   180,   231,   187,   180,   234,
     315,   236,   237,   179,   337,   169,   169,   242,   188,   342,
     275,   531,   180,   346,    27,   381,   281,   180,   538,   179,
     285,   180,   180,   258,   158,   180,   255,   327,   564,   179,
     757,   758,   169,   760,   180,   180,   180,   337,   180,   180,
     180,   297,   342,   180,   279,   301,   346,   303,   180,   180,
     586,   587,   588,   589,   590,   180,   180,   180,   126,   180,
     180,   180,   318,   319,   179,   265,   179,   323,   169,   180,
     626,   188,   628,   629,   179,   568,   332,   333,   334,   335,
     413,   316,   575,   185,   186,   320,   179,   311,   185,   186,
     179,   347,   327,   427,   169,   169,   261,   262,   591,   883,
     179,   180,   337,   359,   169,    21,    22,   363,   364,   365,
     366,   367,   368,   369,   370,   371,   372,   373,   374,   179,
     576,   767,   768,   169,   179,   381,   169,   837,   584,   294,
     176,     3,     4,   179,   844,   541,   392,   393,   394,   395,
     396,   397,   398,   399,   400,   401,   402,   403,   404,   405,
     406,   407,   408,   409,   410,   593,   883,     3,   414,   593,
     594,   176,   594,     3,   179,    23,   422,   423,     3,   838,
     839,   840,   841,   507,   508,   837,   838,   839,   840,   841,
     808,   515,   844,   357,   358,    20,   501,   140,   141,    24,
       4,   506,   169,   176,   509,   138,   139,   140,   564,    23,
      24,    25,   376,   377,   378,   379,   380,     3,   728,   936,
     576,   717,   546,   707,   750,   389,   179,   169,   584,   179,
     586,   587,   588,   589,   590,   179,   632,   485,   179,   179,
     626,   564,   628,   629,   179,   491,   792,   793,   794,   495,
     179,   179,   676,   576,   676,   179,   502,   681,   179,   681,
     179,   584,   745,   586,   587,   588,   589,   590,   179,   179,
     593,   594,   179,   189,   564,   179,   179,   179,   179,   762,
     763,   764,   765,   766,   179,   531,   576,   179,   179,   179,
     179,   537,   538,   179,   584,   541,   586,   587,   588,   589,
     590,   179,   179,   169,     5,   169,     7,     8,     9,    10,
      11,     3,    13,    14,    15,    16,    17,    18,     4,   565,
      21,    22,    23,    24,    25,   169,   169,     3,   574,     3,
      20,    21,    22,    23,    24,    25,    20,    21,    22,    23,
      24,    25,     5,   169,     3,   595,   169,   179,   169,   595,
     596,   169,   748,   676,     3,     3,   169,   867,   681,     4,
      11,   180,    13,    14,    15,    16,    17,    18,   593,   594,
      21,    22,    23,    24,    25,    21,    22,    23,    24,    25,
     626,   176,   628,   629,     3,   176,   632,   633,    20,    21,
      22,    23,    24,    25,   750,     5,   792,   814,   881,     3,
     179,   725,   179,   759,   650,   651,   792,   793,   794,   655,
     700,   657,   132,     3,     3,   661,     3,   741,    20,    21,
      22,    23,    24,    25,     3,   749,   189,   750,   674,   675,
       3,   180,   180,     3,     3,   179,   759,   169,    24,   179,
     169,   687,   667,   689,   169,   179,   169,   180,     3,   180,
     878,   676,     5,   180,   176,     3,   681,     3,   683,    25,
     750,     3,   708,    13,    14,    15,    16,    17,    18,   759,
       3,    21,    22,    23,    24,    25,     3,   180,   179,     4,
     180,   169,   728,    84,   185,   186,   180,   180,   189,   714,
     180,    30,   816,    24,   740,    24,   180,   179,   744,     9,
      10,    11,   748,    13,    14,    15,    16,    17,    18,    24,
      23,    21,    22,    23,    24,    25,     4,   169,   180,   169,
     180,   917,     4,   126,     3,   750,   882,    20,     3,     5,
     169,   169,     5,   180,   185,   186,   179,     3,   862,   180,
     180,    24,    29,   180,   180,   180,   792,   793,   794,   873,
     180,   180,   180,     3,   188,   126,   180,   180,   169,   882,
     169,   258,   808,   809,    10,     3,    14,     5,   906,     7,
       8,     9,    10,    11,   904,    13,    14,    15,    16,    17,
      18,   428,   528,    21,    22,    23,    24,    25,   685,   689,
     498,   511,   882,    80,    81,    82,   268,   493,   537,   304,
     416,   721,   848,   848,   850,    76,   852,    13,    14,    15,
      16,    17,    18,   850,   675,    21,    22,    23,    24,    25,
     897,   867,   109,   748,   111,   112,   113,   955,   871,   116,
     117,   143,   119,   120,   959,   185,   186,   555,   142,   143,
     140,   347,   744,   381,   750,   557,   593,    -1,   894,   594,
     896,   897,    -1,   878,   158,   159,    -1,    -1,   162,   163,
     164,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   917,   176,    -1,    -1,   185,   186,   902,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   931,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   939,    -1,    -1,    -1,    -1,    -1,   945,
      -1,    -1,   948,     0,     1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   964,    -1,
      -1,    -1,    -1,   969,   970,    -1,    -1,   973,   974,    -1,
      27,    -1,    29,    -1,   980,    32,    33,    34,    35,    36,
      37,    38,    -1,    -1,    41,    42,    43,   185,   186,    -1,
      -1,    48,    49,    -1,    -1,    -1,    53,    54,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    63,    -1,    -1,    -1,
      67,    68,    -1,    -1,    -1,    72,    -1,    -1,    -1,   185,
     186,    78,    -1,    80,    81,    -1,    83,    -1,    85,    86,
      87,    88,    89,    -1,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,   104,   105,   106,
      -1,    -1,   109,   110,   111,    -1,   113,   114,   115,    -1,
     117,   118,   119,    -1,    -1,   122,    -1,   124,    -1,    -1,
     127,   128,   129,   130,    -1,    -1,   133,   134,   135,    -1,
      -1,    -1,    -1,    -1,   141,    -1,    -1,   144,   145,   146,
     147,   148,   149,   150,   151,    -1,   153,   154,   155,   156,
     157,    -1,    -1,   160,    -1,    -1,    -1,    -1,    -1,    -1,
     167,   168,   169,    -1,    -1,    -1,    -1,    -1,    -1,   176,
     177,     3,    -1,     5,    -1,     7,     8,     9,    10,    11,
     187,    13,    14,    15,    16,    17,    18,    -1,    -1,    21,
      22,    23,    24,    25,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    39,    40,    -1,
      -1,    -1,    44,    45,    46,    47,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    56,    57,    58,    59,    60,    61,
      62,    -1,    64,    65,    66,    -1,    -1,    69,    70,    71,
      -1,    73,    74,    75,    76,    77,     3,    79,     5,    -1,
       7,     8,     9,    10,    11,    -1,    13,    14,    15,    16,
      17,    18,    -1,    -1,    21,    22,    23,    24,    25,     3,
      -1,     5,    -1,     7,     8,     9,    10,    11,    -1,    13,
      14,    15,    16,    17,    18,    -1,    -1,    21,    22,    23,
      24,    25,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,   143,    -1,    -1,    -1,    -1,   148,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   158,   159,    -1,    -1,
     162,   163,   164,   165,    -1,    -1,    -1,   169,    -1,    -1,
      -1,    -1,    -1,    -1,   176,    -1,    -1,   179,    -1,    -1,
      -1,    -1,    -1,   185,   186,     3,    -1,     5,    -1,     7,
       8,     9,    10,    11,    -1,    13,    14,    15,    16,    17,
      18,    -1,    -1,    21,    22,    23,    24,    25,     3,    -1,
       5,    -1,     7,     8,     9,    10,    11,    -1,    13,    14,
      15,    16,    17,    18,    -1,    -1,    21,    22,    23,    24,
      25,     3,    -1,     5,    -1,     7,     8,     9,    10,    11,
      -1,    13,    14,    15,    16,    17,    18,    -1,    -1,    21,
      22,    23,    24,    25,    -1,    -1,    -1,    -1,   185,   186,
      -1,    -1,    -1,    -1,    -1,    -1,     4,     5,    -1,     7,
       8,     9,    10,    11,    -1,    13,    14,    15,    16,    17,
      18,   185,   186,    21,    22,    23,    24,    25,     4,     5,
      -1,     7,     8,     9,    10,    11,    -1,    13,    14,    15,
      16,    17,    18,    -1,    -1,    21,    22,    23,    24,    25,
       4,     5,    -1,     7,     8,     9,    10,    11,    -1,    13,
      14,    15,    16,    17,    18,    -1,    -1,    21,    22,    23,
      24,    25,     4,     5,    -1,     7,     8,     9,    10,    11,
      -1,    13,    14,    15,    16,    17,    18,    -1,    -1,    21,
      22,    23,    24,    25,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   185,   186,    -1,
      -1,    -1,    -1,    -1,    -1,     4,     5,    -1,     7,     8,
       9,    10,    11,    -1,    13,    14,    15,    16,    17,    18,
     185,   186,    21,    22,    23,    24,    25,    -1,    -1,     5,
      -1,     7,     8,     9,    10,    11,    -1,    13,    14,    15,
      16,    17,    18,   185,   186,    21,    22,    23,    24,    25,
       5,    -1,     7,     8,     9,    10,    11,    -1,    13,    14,
      15,    16,    17,    18,    -1,    -1,    21,    22,    23,    24,
      25,    -1,   180,    -1,    -1,    -1,    -1,   185,   186,    -1,
      -1,    -1,    -1,    -1,    -1,     5,    -1,     7,     8,     9,
      10,    11,    -1,    13,    14,    15,    16,    17,    18,   185,
     186,    21,    22,    23,    24,    25,    -1,     5,    -1,     7,
       8,     9,    10,    11,    -1,    13,    14,    15,    16,    17,
      18,   185,   186,    21,    22,    23,    24,    25,    -1,     5,
      -1,     7,     8,     9,    10,    11,    -1,    13,    14,    15,
      16,    17,    18,   185,   186,    21,    22,    23,    24,    25,
       5,    -1,     7,     8,     9,    10,    11,    -1,    13,    14,
      15,    16,    17,    18,    -1,    -1,    21,    22,    23,    24,
      25,    -1,     5,    -1,     7,     8,     9,    10,    11,    -1,
      13,    14,    15,    16,    17,    18,   185,   186,    21,    22,
      23,    24,    25,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   180,    -1,    -1,    -1,    -1,   185,
     186,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   180,    -1,    -1,    -1,    -1,
     185,   186,     5,    -1,     7,     8,     9,    10,    11,    -1,
      13,    14,    15,    16,    17,    18,    -1,    -1,    21,    22,
      23,    24,    25,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     180,    -1,    -1,    -1,    -1,   185,   186,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   180,    -1,    -1,    -1,    -1,   185,   186,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   180,    -1,    -1,    -1,    -1,   185,
     186,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   180,    -1,    -1,    -1,    -1,
     185,   186,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,    -1,    -1,
      -1,    -1,   185,   186,     5,    -1,     7,     8,     9,    10,
      11,    -1,    13,    14,    15,    16,    17,    18,    -1,    -1,
      21,    22,    23,    24,    25,     5,    -1,     7,     8,     9,
      10,    11,    -1,    13,    14,    15,    16,    17,    18,    -1,
      -1,    21,    22,    23,    24,    25,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,    -1,    -1,
      -1,    -1,   185,   186,     5,    -1,     7,     8,     9,    10,
      11,    -1,    13,    14,    15,    16,    17,    18,    -1,    -1,
      21,    22,    23,    24,    25,     5,    -1,     7,     8,     9,
      10,    11,    -1,    13,    14,    15,    16,    17,    18,    -1,
      -1,    21,    22,    23,    24,    25,     5,    -1,     7,     8,
       9,    10,    11,    -1,    13,    14,    15,    16,    17,    18,
      -1,    -1,    21,    22,    23,    24,    25,     5,    -1,     7,
       8,     9,    10,    11,    -1,    13,    14,    15,    16,    17,
      18,    -1,    -1,    21,    22,    23,    24,    25,     5,    -1,
       7,     8,     9,    10,    11,    -1,    13,    14,    15,    16,
      17,    18,    -1,    -1,    21,    22,    23,    24,    25,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,
      -1,    -1,    -1,    -1,   185,   186,    -1,    -1,    -1,   169,
      -1,     5,    -1,     7,     8,     9,    10,    11,    -1,    13,
      14,    15,    16,    17,    18,   185,   186,    21,    22,    23,
      24,    25,     5,    -1,     7,     8,     9,    10,    11,    -1,
      13,    14,    15,    16,    17,    18,    -1,    -1,    21,    22,
      23,    24,    25,    -1,    -1,    -1,    -1,    -1,    -1,   180,
      -1,    -1,    -1,    -1,   185,   186,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     180,    -1,    -1,    -1,    -1,   185,   186,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   180,    -1,    -1,    -1,    -1,   185,   186,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   180,    -1,    -1,    -1,    -1,   185,   186,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   180,    -1,    -1,    -1,    -1,   185,   186,
       5,    -1,     7,     8,     9,    10,    11,    -1,    13,    14,
      15,    16,    17,    18,    -1,    -1,    21,    22,    23,    24,
      25,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   180,    -1,    -1,    -1,
      -1,   185,   186,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,    -1,    -1,
      -1,    -1,   185,   186,     5,    -1,     7,     8,     9,    10,
      11,    -1,    13,    14,    15,    16,    17,    18,    -1,    -1,
      21,    22,    23,    24,    25,     5,    -1,     7,     8,     9,
      10,    11,    -1,    13,    14,    15,    16,    17,    18,    -1,
      -1,    21,    22,    23,    24,    25,     5,    -1,     7,     8,
       9,    10,    11,    -1,    13,    14,    15,    16,    17,    18,
      -1,    -1,    21,    22,    23,    24,    25,     5,    -1,     7,
       8,     9,    10,    11,    -1,    13,    14,    15,    16,    17,
      18,    -1,    -1,    21,    22,    23,    24,    25,     5,    -1,
       7,     8,     9,    10,    11,    -1,    13,    14,    15,    16,
      17,    18,    -1,    -1,    21,    22,    23,    24,    25,    -1,
      -1,    -1,    -1,    -1,    -1,   180,    -1,    -1,    -1,    -1,
     185,   186,     5,    -1,     7,     8,     9,    10,    11,    -1,
      13,    14,    15,    16,    17,    18,    -1,    -1,    21,    22,
      23,    24,    25,     5,    -1,     7,     8,     9,    10,    11,
      -1,    13,    14,    15,    16,    17,    18,    -1,    -1,    21,
      22,    23,    24,    25,    -1,    -1,    -1,    -1,    -1,    38,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,
      -1,    -1,    -1,    -1,   185,   186,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,
     180,    -1,    -1,    -1,    -1,   185,   186,    -1,    -1,    78,
      -1,    -1,    -1,    -1,    83,    84,    -1,    -1,    -1,    88,
      89,   180,    -1,    -1,    -1,    -1,   185,   186,    -1,    98,
      99,   100,   101,   102,   103,   104,   105,    -1,    -1,    -1,
      -1,    -1,   180,    -1,    -1,    -1,    -1,   185,   186,   118,
      -1,    -1,    -1,    -1,    -1,   124,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   180,   133,    -1,   135,    -1,   185,   186,
       5,    -1,     7,     8,     9,    10,    11,    -1,    13,    14,
      15,    16,    17,    18,    -1,    -1,    21,    22,    23,    24,
      25,    -1,    -1,    -1,    -1,    -1,    -1,   180,    -1,    -1,
     169,    -1,   185,   186,    -1,    -1,    -1,   176,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,    -1,
      -1,    -1,    -1,   185,   186,     5,    -1,     7,     8,     9,
      10,    11,     4,    13,    14,    15,    16,    17,    18,    -1,
      12,    21,    22,    23,    24,    25,    -1,    -1,    -1,    21,
      22,    23,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    39,    40,    -1,
      -1,    -1,    44,    45,    46,    47,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    56,    57,    58,    59,    60,    61,
      62,    -1,    64,    65,    66,    -1,    -1,    69,    70,    71,
      -1,    73,    74,    75,    76,    77,    -1,    79,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    40,    -1,
      -1,    -1,    44,    45,    46,    47,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    56,    57,    58,    59,    60,    61,
      62,    -1,    64,    65,    66,   180,    -1,    69,    70,    71,
     185,   186,    74,    75,    76,    77,    -1,    79,    -1,   131,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,
     142,   143,    -1,    -1,    -1,    -1,   148,    21,    22,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   158,   159,    -1,    -1,
     162,   163,   164,   165,    -1,    39,    40,   169,    -1,    -1,
      44,    45,    46,    47,   176,   185,   186,   179,    -1,    -1,
      -1,    -1,    56,    57,    58,    59,    60,    61,    62,    -1,
      64,    65,    66,    -1,    -1,    69,    70,    71,    -1,    73,
      74,    75,    76,    77,    -1,    79,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   169,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,   143,
      -1,    -1,    -1,    -1,   148,     4,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    12,   158,   159,    -1,    -1,   162,   163,
     164,   165,    21,    22,    -1,   169,    -1,    -1,    -1,    -1,
      -1,    -1,   176,    -1,    -1,   179,    -1,    -1,    -1,    -1,
      39,    40,    -1,   187,    -1,    44,    45,    46,    47,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    56,    57,    58,
      59,    60,    61,    62,    -1,    64,    65,    66,    -1,    -1,
      69,    70,    71,    -1,    73,    74,    75,    76,    77,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   131,     4,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    12,    -1,   142,   143,    -1,    -1,    -1,    -1,   148,
      21,    22,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,
     159,    -1,    -1,   162,   163,   164,   165,    -1,    39,    40,
     169,    -1,    -1,    44,    45,    46,    47,   176,    -1,    -1,
     179,    -1,    -1,    -1,    -1,    56,    57,    58,    59,    60,
      61,    62,    -1,    64,    65,    66,    -1,    -1,    69,    70,
      71,    -1,    73,    74,    75,    76,    77,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     131,    -1,     5,    -1,    -1,    -1,    -1,    -1,    -1,    12,
      -1,   142,   143,    -1,    -1,    -1,    -1,   148,    21,    22,
      23,    -1,    -1,    -1,    -1,    -1,    -1,   158,   159,    -1,
      -1,   162,   163,   164,   165,    -1,    39,    40,   169,    -1,
      -1,    44,    45,    46,    47,   176,    -1,    -1,   179,    -1,
      -1,    -1,    -1,    56,    57,    58,    59,    60,    61,    62,
      -1,    64,    65,    66,    -1,    -1,    69,    70,    71,    -1,
      73,    74,    75,    76,    77,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,
       5,    -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,   142,
     143,    -1,    -1,    -1,    -1,   148,    21,    22,    -1,    24,
      -1,    -1,    -1,    -1,    -1,   158,   159,    -1,    -1,   162,
     163,   164,   165,    -1,    39,    40,   169,    -1,    -1,    44,
      45,    46,    47,   176,    -1,    -1,   179,    -1,    -1,    -1,
      -1,    56,    57,    58,    59,    60,    61,    62,    -1,    64,
      65,    66,    -1,    -1,    69,    70,    71,    -1,    73,    74,
      75,    76,    77,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,     5,    -1,
      -1,    -1,    -1,    -1,    -1,    12,    -1,   142,   143,    -1,
      -1,    -1,    -1,   148,    21,    22,    23,    -1,    -1,    -1,
      -1,    -1,    -1,   158,   159,    -1,    -1,   162,   163,   164,
     165,    -1,    39,    40,   169,    -1,    -1,    44,    45,    46,
      47,   176,    -1,    -1,   179,    -1,    -1,    -1,    -1,    56,
      57,    58,    59,    60,    61,    62,    -1,    64,    65,    66,
      -1,    -1,    69,    70,    71,    -1,    73,    74,    75,    76,
      77,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   131,    -1,     5,    -1,    -1,    -1,
      -1,    -1,    -1,    12,    -1,   142,   143,    -1,    -1,    -1,
      -1,   148,    21,    22,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   158,   159,    -1,    -1,   162,   163,   164,   165,    -1,
      39,    40,   169,    -1,    -1,    44,    45,    46,    47,   176,
      -1,    -1,   179,    -1,    -1,    -1,    -1,    56,    57,    58,
      59,    60,    61,    62,    -1,    64,    65,    66,    -1,    -1,
      69,    70,    71,    -1,    73,    74,    75,    76,    77,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   131,    -1,     5,    -1,    -1,    -1,    -1,    -1,
      -1,    12,    -1,   142,   143,    -1,    -1,    -1,    -1,   148,
      21,    22,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,
     159,    -1,    -1,   162,   163,   164,   165,    -1,    39,    40,
     169,    -1,    -1,    44,    45,    46,    47,   176,    -1,    -1,
     179,    -1,    -1,    -1,    -1,    56,    57,    58,    59,    60,
      61,    62,    -1,    64,    65,    66,    -1,    -1,    69,    70,
      71,    -1,    73,    74,    75,    76,    77,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     131,    -1,     5,    -1,    -1,    -1,    -1,    -1,    -1,    12,
      -1,   142,   143,    -1,    -1,    -1,    -1,   148,    21,    22,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,   159,    -1,
      -1,   162,   163,   164,   165,    -1,    39,    40,   169,    -1,
      -1,    44,    45,    46,    47,   176,    -1,    -1,   179,    -1,
      -1,    -1,    -1,    56,    57,    58,    59,    60,    61,    62,
      -1,    64,    65,    66,    -1,    -1,    69,    70,    71,    -1,
      73,    74,    75,    76,    77,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,   142,
     143,    -1,    -1,    -1,    -1,   148,    21,    22,    23,    -1,
      25,    -1,    -1,    -1,    -1,   158,   159,    -1,    -1,   162,
     163,   164,   165,    -1,    39,    40,   169,    -1,    -1,    44,
      45,    46,    47,   176,    -1,    -1,   179,    -1,    -1,    -1,
      -1,    56,    57,    58,    59,    60,    61,    62,    -1,    64,
      65,    66,    -1,    -1,    69,    70,    71,    -1,    73,    74,
      75,    76,    77,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    12,    -1,   142,   143,    -1,
      -1,    -1,    -1,   148,    21,    22,    23,    -1,    -1,    -1,
      -1,    -1,    -1,   158,   159,    -1,    -1,   162,   163,   164,
     165,    -1,    39,    40,   169,    -1,    -1,    44,    45,    46,
      47,   176,    -1,    -1,   179,    -1,    -1,    -1,    -1,    56,
      57,    58,    59,    60,    61,    62,    -1,    64,    65,    66,
      -1,    -1,    69,    70,    71,    -1,    73,    74,    75,    76,
      77,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    12,    -1,   142,   143,    -1,    -1,    -1,
      -1,   148,    21,    22,    23,    -1,    -1,    -1,    -1,    -1,
      -1,   158,   159,    -1,    -1,   162,   163,   164,   165,    -1,
      39,    40,   169,    -1,    -1,    44,    45,    46,    47,   176,
      -1,    -1,   179,    -1,    -1,    -1,    -1,    56,    57,    58,
      59,    60,    61,    62,    -1,    64,    65,    66,    -1,    -1,
      69,    70,    71,    -1,    73,    74,    75,    76,    77,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    12,    -1,   142,   143,    -1,    -1,    -1,    -1,   148,
      21,    22,    23,    -1,    -1,    -1,    -1,    -1,    -1,   158,
     159,    -1,    -1,   162,   163,   164,   165,    -1,    39,    40,
     169,    -1,    -1,    44,    45,    46,    47,   176,    -1,    -1,
     179,    -1,    -1,    -1,    -1,    56,    57,    58,    59,    60,
      61,    62,    -1,    64,    65,    66,    -1,    -1,    69,    70,
      71,    -1,    73,    74,    75,    76,    77,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,
      -1,   142,   143,    -1,    -1,    -1,    -1,   148,    21,    22,
      23,    -1,    -1,    -1,    -1,    -1,    -1,   158,   159,    -1,
      -1,   162,   163,   164,   165,    -1,    39,    40,   169,    -1,
      -1,    44,    45,    46,    47,   176,    -1,    -1,   179,    -1,
      -1,    -1,    -1,    56,    57,    58,    59,    60,    61,    62,
      -1,    64,    65,    66,    -1,    -1,    69,    70,    71,    -1,
      73,    74,    75,    76,    77,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,   142,
     143,    -1,    -1,    -1,    -1,   148,    21,    22,    23,    -1,
      -1,    -1,    -1,    -1,    -1,   158,   159,    -1,    -1,   162,
     163,   164,   165,    -1,    39,    40,   169,    -1,    -1,    44,
      45,    46,    47,   176,    -1,    -1,   179,    -1,    -1,    -1,
      -1,    56,    57,    58,    59,    60,    61,    62,    -1,    64,
      65,    66,    -1,    -1,    69,    70,    71,    -1,    73,    74,
      75,    76,    77,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    12,    -1,   142,   143,    -1,
      -1,    -1,    -1,   148,    21,    22,    23,    -1,    -1,    -1,
      -1,    -1,    -1,   158,   159,    -1,    -1,   162,   163,   164,
     165,    -1,    39,    40,   169,    -1,    -1,    44,    45,    46,
      47,   176,    -1,    -1,   179,    -1,    -1,    -1,    -1,    56,
      57,    58,    59,    60,    61,    62,    -1,    64,    65,    66,
      -1,    -1,    69,    70,    71,    -1,    73,    74,    75,    76,
      77,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    12,    -1,   142,   143,    -1,    -1,    -1,
      -1,   148,    21,    22,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   158,   159,    -1,    -1,   162,   163,   164,   165,    -1,
      39,    40,   169,    -1,    -1,    44,    45,    46,    47,   176,
      -1,    -1,   179,    -1,    -1,    -1,    -1,    56,    57,    58,
      59,    60,    61,    62,    -1,    64,    65,    66,    -1,    -1,
      69,    70,    71,    -1,    73,    74,    75,    76,    77,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    12,    -1,   142,   143,    -1,    -1,    -1,    -1,   148,
      21,    22,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,
     159,    -1,    -1,   162,   163,   164,   165,    -1,    39,    40,
     169,    -1,    -1,    44,    45,    46,    47,   176,    -1,    -1,
     179,    -1,    -1,    -1,    -1,    56,    57,    58,    59,    60,
      61,    62,    -1,    64,    65,    66,    -1,    -1,    69,    70,
      71,    -1,    73,    74,    75,    76,    77,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,
      -1,   142,   143,    -1,    -1,    -1,    -1,   148,    21,    22,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,   159,    -1,
      -1,   162,   163,   164,   165,    -1,    39,    40,   169,    -1,
      -1,    44,    45,    46,    47,   176,    -1,    -1,   179,    -1,
      -1,    -1,    -1,    56,    57,    58,    59,    60,    61,    62,
      -1,    64,    65,    66,    -1,    -1,    69,    70,    71,    -1,
      73,    74,    75,    76,    77,    -1,    79,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    -1,   142,
     143,    -1,    -1,    -1,    -1,   148,    21,    22,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   158,   159,    -1,    -1,   162,
     163,   164,   165,    -1,    39,    40,   169,    -1,    -1,    44,
      45,    46,    47,   176,    -1,    -1,   179,    -1,    -1,    -1,
      -1,    56,    57,    58,    59,    60,    61,    62,    -1,    64,
      65,    66,    -1,    -1,    69,    70,    71,    -1,    73,    74,
      75,    76,    77,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    12,    -1,   142,   143,    -1,
      -1,    -1,    -1,   148,    21,    22,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   158,   159,    -1,    -1,   162,   163,   164,
     165,    -1,    39,    40,   169,    -1,    -1,    44,    45,    46,
      47,   176,    -1,    -1,   179,    -1,    -1,    -1,    -1,    56,
      57,    58,    59,    60,    61,    62,    -1,    64,    65,    66,
      -1,    -1,    69,    70,    71,    -1,    73,    74,    75,    76,
      77,    -1,    79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    12,    -1,   142,   143,    -1,    -1,    -1,
      -1,   148,    21,    22,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   158,   159,    -1,    -1,   162,   163,   164,   165,    -1,
      39,    40,   169,    -1,    -1,    44,    45,    46,    47,   176,
      -1,    -1,   179,    -1,    -1,    -1,    -1,    56,    57,    58,
      59,    60,    61,    62,    -1,    64,    65,    66,    -1,    -1,
      69,    70,    71,    -1,    73,    74,    75,    76,    77,    -1,
      79,    21,    22,    23,    -1,    25,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    39,
      40,    -1,    -1,    -1,    44,    45,    46,    47,    -1,    -1,
      50,    51,    52,    -1,    -1,    55,    56,    57,    58,    59,
      60,    61,    62,    -1,    64,    65,    66,    -1,    -1,    69,
      70,    71,   131,    73,    74,    75,    76,    77,    -1,    79,
      -1,    -1,    -1,   142,   143,    -1,    -1,    -1,    -1,   148,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,
     159,    -1,    -1,   162,   163,   164,   165,    -1,    -1,    -1,
     169,    -1,    -1,    -1,    -1,    -1,    -1,   176,    -1,    -1,
     179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,   143,    -1,    -1,    -1,    -1,   148,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,   159,
      -1,    -1,   162,   163,   164,   165,    21,    22,    23,   169,
      -1,    -1,    -1,    -1,    -1,    -1,   176,    -1,    -1,   179,
      -1,    -1,    -1,    -1,    39,    40,    -1,    -1,    -1,    44,
      45,    46,    47,    -1,    -1,    50,    51,    52,    -1,    -1,
      55,    56,    57,    58,    59,    60,    61,    62,    -1,    64,
      65,    66,    21,    22,    69,    70,    71,    -1,    73,    74,
      75,    76,    77,    -1,    79,    -1,    -1,    -1,    -1,    -1,
      39,    40,    -1,    -1,    -1,    44,    45,    46,    47,    -1,
      -1,    50,    51,    52,    -1,    -1,    55,    56,    57,    58,
      59,    60,    61,    62,    -1,    64,    65,    66,    -1,    -1,
      69,    70,    71,    -1,    73,    74,    75,    76,    77,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,   143,    -1,
      -1,    -1,    -1,   148,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   158,   159,    -1,    -1,   162,   163,   164,
     165,    -1,    -1,    -1,   169,    -1,    -1,    -1,    -1,    -1,
      -1,   176,    -1,    29,   179,    -1,    32,    33,    34,    35,
      -1,    -1,    38,   142,   143,    41,    42,    43,    -1,   148,
      -1,    -1,    48,    49,    -1,    -1,    -1,    -1,    -1,   158,
     159,    -1,    -1,   162,   163,   164,   165,    63,    -1,    -1,
     169,    67,    68,    -1,    -1,    -1,    72,   176,    -1,    -1,
     179,    -1,    78,    -1,    80,    81,    -1,    83,    -1,    85,
      86,    87,    88,    89,    -1,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,   104,   105,
     106,    -1,    -1,   109,   110,   111,    -1,   113,   114,   115,
      -1,   117,   118,   119,    -1,    -1,   122,    -1,   124,    -1,
      -1,   127,   128,   129,   130,    -1,    -1,   133,   134,   135,
      -1,    -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,   145,
     146,   147,   148,   149,   150,   151,    -1,   153,   154,   155,
     156,   157,    -1,    -1,   160,    -1,    -1,    -1,    -1,    39,
      40,   167,   168,   169,    44,    45,    46,    47,    -1,    -1,
     176,    -1,    -1,    -1,    -1,    -1,    56,    57,    58,    59,
      60,    61,    62,    -1,    64,    65,    66,    -1,    -1,    69,
      70,    71,    -1,    73,    74,    75,    76,    77,    -1,    79,
      39,    40,    -1,    -1,    -1,    44,    45,    46,    47,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    56,    57,    58,
      59,    60,    61,    62,    -1,    64,    65,    66,    -1,    -1,
      69,    70,    71,    -1,    73,    74,    75,    76,    77,    -1,
      79,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,   143,    -1,    -1,    -1,    -1,   148,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,   159,
      -1,    -1,   162,   163,   164,   165,    -1,    -1,    -1,   169,
      -1,    -1,    -1,    -1,    -1,    -1,   176,    -1,    -1,   179,
      -1,    -1,    -1,   142,   143,    -1,    -1,    -1,    -1,   148,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,
     159,    -1,    -1,   162,   163,   164,   165,    -1,    -1,    -1,
     169,    -1,    -1,    -1,    -1,    -1,    -1,   176,    -1,    -1,
     179
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   191,     0,     1,    36,    37,    53,    54,   144,   177,
     187,   192,   197,   199,   205,   201,   198,   177,   200,    29,
      32,    34,    38,    41,    42,    43,    48,    49,    63,    67,
      68,    72,    78,    80,    81,    83,    85,    86,    87,    88,
      89,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,   103,   104,   105,   106,   109,   110,   111,
     113,   114,   115,   117,   118,   119,   122,   124,   127,   128,
     129,   130,   133,   134,   135,   141,   145,   146,   147,   148,
     149,   150,   151,   153,   154,   155,   156,   157,   160,   167,
     168,   169,   176,   193,   194,   195,   202,   203,   206,   212,
     224,   225,   228,   229,   234,   235,   237,   238,   239,   240,
     241,   243,   244,   251,   253,   257,   258,   261,   262,   263,
     298,   308,   309,   315,   316,   317,   318,   319,   321,   326,
     327,   328,   330,   331,   333,   336,   337,   340,   341,   349,
     351,   352,   365,   372,   201,   205,   169,   204,   179,    12,
      21,    22,    39,    40,    44,    45,    46,    47,    56,    57,
      58,    59,    60,    61,    62,    64,    65,    66,    69,    70,
      71,    73,    74,    75,    76,    77,    79,   131,   142,   143,
     148,   158,   159,   162,   163,   164,   165,   176,   179,   280,
     281,   283,   284,   288,   290,   291,   292,   293,   298,   299,
     300,   307,   320,   179,   187,   280,   304,   179,   217,   218,
     179,   280,   303,     4,   187,   221,   187,   221,   304,   179,
     169,   187,   324,   176,   179,   364,   280,   169,   325,   169,
     324,   179,   169,   176,   179,   346,   179,   179,   221,   221,
      20,    24,   247,   298,   179,   216,   123,   137,   127,   324,
     324,   169,   252,    80,    81,   213,   275,   324,    27,   196,
     198,    33,    35,   196,   158,   207,   196,   169,   179,   249,
     250,   169,   226,   231,     3,   248,   169,   230,   247,     3,
     236,   248,     3,   248,     3,   248,   169,   245,   246,   247,
       3,     4,    35,   222,   223,   255,   271,   169,   176,   268,
      23,   179,   259,    23,   179,   266,    23,     4,   329,   169,
     310,   200,   324,   324,   324,   176,   323,   280,     5,   126,
     189,   332,     3,   179,   334,   169,   304,   179,   345,    21,
      22,    23,    50,    51,    52,    55,   169,   179,   281,   288,
     299,   343,   345,   350,   353,   354,   355,   345,    84,    98,
     105,   298,   328,   280,   280,   280,   282,   179,   179,   179,
     282,   280,   282,   179,   179,   179,   179,   179,   179,   179,
     179,   179,   179,   179,   179,   280,   179,   179,   179,   179,
     179,   179,   280,   281,   283,   307,   360,   361,   362,   179,
     280,   283,     5,     7,     8,     9,    10,    11,    13,    14,
      15,    16,    17,    18,    21,    22,    23,    24,    25,   185,
     186,   285,   280,   189,   179,   302,   179,   179,   169,   158,
     301,   302,     3,     4,   180,   280,   219,   298,     3,   280,
       4,   169,   242,   242,   280,   280,   180,   288,   292,   298,
     330,   366,   367,   368,   176,   283,   298,   298,   367,   369,
     298,   371,   242,    40,    44,    45,    46,    47,    56,    57,
      58,    59,    60,    61,    62,    64,    65,    66,    69,    70,
      71,    74,    75,    76,    77,    79,   169,   214,   215,   169,
     298,   220,   280,     3,     3,   221,   194,   200,   204,   204,
     196,     5,   249,     3,    24,   179,   187,   248,     3,    24,
     230,   247,   179,   276,   230,   298,   247,   169,   169,   247,
     276,     3,   245,    29,    82,   109,   111,   112,   113,   116,
     117,   119,   120,   272,   273,   275,     4,   204,     3,   256,
     254,     5,    23,   269,   280,   180,   176,   179,   169,   260,
     269,   179,   267,   280,   268,   176,   276,     3,   205,   248,
     298,   322,   373,   280,   280,   298,   176,    23,   280,   335,
     338,   339,    23,    25,   179,   298,   347,   348,   353,   354,
     280,   280,   280,   280,     5,   353,   179,   281,   288,   299,
     342,   356,   357,   358,     3,   344,    20,    23,    24,    25,
     355,   353,   360,   179,   179,   180,     3,   180,   282,   282,
     280,   180,   180,   180,   280,   280,   280,   280,   280,   280,
     280,   280,   280,   280,   280,   280,   180,   282,   282,   282,
     282,   282,   280,   283,   361,   362,     3,   132,     3,     3,
     282,   180,     3,     5,   280,   287,   280,   280,   280,   280,
     280,   280,   280,   280,   280,   280,   280,   280,   280,   280,
       5,    24,   280,   286,   280,     5,   280,     5,   280,   288,
     303,   289,   294,   294,   280,   280,   180,     3,   180,   276,
     218,   180,     3,   180,     3,   179,     3,   370,   180,   180,
     180,     3,   370,     3,   180,     3,    24,     3,   180,   179,
     169,   324,   179,   208,   208,   280,   180,   250,   227,   232,
     233,   284,   280,   169,   169,   231,   227,   248,     4,    23,
     277,   278,   279,   280,   248,   276,   276,   248,   246,   276,
     179,     3,     4,   208,   254,   169,   269,   260,     5,   269,
     180,     3,   270,    23,   180,   276,    31,   169,   311,   312,
       5,   329,   364,   180,     3,   353,    23,    25,   179,   280,
       3,   180,   180,   280,   180,   356,   285,   359,   359,     3,
     359,   356,   353,   353,   353,   353,   353,   366,   369,   304,
     280,   180,   180,   180,   180,   180,   180,   180,   180,   180,
     180,   180,   180,   180,   180,   180,   180,   180,   180,   180,
     180,   180,     3,     3,     3,   280,   362,   280,   362,   280,
     362,   180,   283,   280,   280,   280,   280,   280,     4,     4,
     280,   295,   296,   297,   180,   180,   298,   169,    84,   280,
     295,   125,   367,   180,   367,   180,   298,   214,   280,   220,
      23,   169,   180,   210,   211,    30,    24,     3,    21,    22,
      23,   188,   299,   180,    24,    24,   230,   280,     3,   180,
       4,   298,    23,   264,   264,   245,   138,   139,   140,   274,
     273,     4,   169,   276,   180,   269,   270,   169,   180,     4,
     126,     3,   280,   276,   339,   180,   280,   297,   189,   276,
     348,   353,     3,   180,   285,   358,   370,   370,   169,   363,
     363,   363,   180,   303,     4,   280,     4,     3,   302,   276,
     180,   180,     5,   180,     3,   180,   179,   209,   227,   232,
     232,   232,   232,   227,   278,   279,   280,   179,   265,   280,
     180,   276,   264,   270,   269,   169,   187,   313,   314,   169,
     312,     3,   276,   180,   330,   363,   359,   180,   180,     5,
     180,   180,   180,   180,   280,     4,   280,   296,     3,   298,
     211,   180,   210,    24,    23,   264,   305,   306,   126,     3,
     280,   180,   280,   280,     4,   280,   180,   180,   305,     5,
     126,   169,   314,     3,     3,   280,   280,   280,   280,   280,
       3,   280
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 8:
#line 344 "fortran.y"
    {yyerrok;yyclearin;;}
    break;

  case 13:
#line 352 "fortran.y"
    {
                     if (inmoduledeclare == 0 )
                     {
                        pos_end = setposcur();
                        RemoveWordSET_0(fortranout,pos_curinclude,
                                              pos_end-pos_curinclude);
                     }
                  ;}
    break;

  case 18:
#line 368 "fortran.y"
    {
            /* we should ignore the declaration until the keyword             */
            /*    TOK_ENDDONOTTREAT                                           */
            couldaddvariable = 0 ;
            RemoveWordCUR_0(fortranout,-20,20);
         ;}
    break;

  case 19:
#line 375 "fortran.y"
    {
             couldaddvariable = 1 ;
             RemoveWordCUR_0(fortranout,-24,24);
          ;}
    break;

  case 22:
#line 382 "fortran.y"
    {pos_cur = setposcur();;}
    break;

  case 27:
#line 394 "fortran.y"
    {if (incom !=1) {strcpy(curbuf,"");incom=0;};}
    break;

  case 28:
#line 397 "fortran.y"
    {isrecursive = 0;;}
    break;

  case 29:
#line 399 "fortran.y"
    {isrecursive = 1;;}
    break;

  case 31:
#line 403 "fortran.y"
    {
                      if ( couldaddvariable == 1)
                      {
                      /* open param file                                      */
                      if ( firstpass == 0 )
                      {
                         sprintf(ligne,"%s/ParamFile%s.h",nomdir,(yyvsp[(3) - (4)].nac));
                         paramout=fopen(ligne,"w");
                         if ( retour77 == 0 ) fprintf(paramout,"!\n");
                         else fprintf(paramout,"C\n");

                      }
                      Add_SubroutineArgument_Var_1((yyvsp[(4) - (4)].l));
                      if ( inmodulemeet == 1 )
                      {
                         insubroutinedeclare = 1;
                         /* in the second step we should write the head of    */
                         /*    the subroutine sub_loop_<subroutinename>       */
                         writeheadnewsub_0(1);
                      }
                      else
                      {
                            insubroutinedeclare = 1;
                            writeheadnewsub_0(1);
                      }
                      }
                   ;}
    break;

  case 32:
#line 431 "fortran.y"
    {
                      /* open param file                                      */
                      if ( firstpass == 0 )
                      {
                         sprintf(ligne,"%s/ParamFile%s.h",nomdir,(yyvsp[(2) - (2)].nac));
                         paramout=fopen(ligne,"w");
                         if ( retour77 == 0 ) fprintf(paramout,"!\n");
                         else fprintf(paramout,"C\n");

                      }
                      strcpy(subroutinename,(yyvsp[(2) - (2)].nac));
                      /* Common case                                          */
                      insubroutinedeclare = 1;
                      /* in the second step we should write the head of       */
                      /*    the subroutine sub_loop_<subroutinename>          */
                      writeheadnewsub_0(1);
                   ;}
    break;

  case 33:
#line 449 "fortran.y"
    {
                      /* open param file                                      */
                      if ( firstpass == 0 )
                      {
                         sprintf(ligne,"%s/ParamFile%s.h",nomdir,(yyvsp[(3) - (6)].nac));
                         paramout=fopen(ligne,"w");
                         if ( retour77 == 0 ) fprintf(paramout,"!\n");
                         else fprintf(paramout,"C\n");
                      }
                      strcpy(subroutinename,(yyvsp[(3) - (6)].nac));
                      if ( inmodulemeet == 1 )
                      {
                         insubroutinedeclare = 1;
                         /* we should to list of the subroutine argument the  */
                         /*    name of the function which has to be defined   */
                         Add_SubroutineArgument_Var_1((yyvsp[(4) - (6)].l));
                         strcpy(DeclType,"");
                         /* in the second step we should write the head of    */
                         /*    the subroutine sub_loop_<subroutinename>       */
                         writeheadnewsub_0(2);
                      }
                      else
                      {
                            insubroutinedeclare = 1;
                            /* we should to list of the subroutine argument   */
                            /* name of the function which has to be defined   */
                            Add_SubroutineArgument_Var_1((yyvsp[(4) - (6)].l));
                            strcpy(DeclType,"");
                            Add_FunctionType_Var_1((yyvsp[(3) - (6)].nac));
                            writeheadnewsub_0(2);
                      }
                   ;}
    break;

  case 34:
#line 482 "fortran.y"
    {
                      /* open param file                                      */
                      if ( firstpass == 0 )
                      {
                         sprintf(ligne,"%s/ParamFile%s.h",nomdir,(yyvsp[(3) - (4)].nac));
                         paramout=fopen(ligne,"w");
                         if ( retour77 == 0 ) fprintf(paramout,"!\n");
                         else fprintf(paramout,"C\n");
                      }
                      strcpy(subroutinename,(yyvsp[(3) - (4)].nac));
                      if ( inmodulemeet == 1 )
                      {
                         insubroutinedeclare = 1;
                         /* we should to list of the subroutine argument the  */
                         /*    name of the function which has to be defined   */
                         Add_SubroutineArgument_Var_1((yyvsp[(4) - (4)].l));
                         strcpy(DeclType,"");
                         Add_FunctionType_Var_1((yyvsp[(3) - (4)].nac));
                         /* in the second step we should write the head of    */
                         /*    the subroutine sub_loop_<subroutinename>       */
                         writeheadnewsub_0(2);
                      }
                      else
                      {
                            insubroutinedeclare = 1;
                            /* we should to list of the subroutine argument   */
                            /* name of the function which has to be defined   */
                            Add_SubroutineArgument_Var_1((yyvsp[(4) - (4)].l));
                            strcpy(DeclType,"");
                            Add_FunctionType_Var_1((yyvsp[(3) - (4)].nac));
                            writeheadnewsub_0(2);
                      }
                   ;}
    break;

  case 35:
#line 516 "fortran.y"
    {
                      GlobalDeclaration = 0;
                      strcpy(curmodulename,(yyvsp[(2) - (2)].nac));
                      strcpy(subroutinename,"");
                      Add_NameOfModule_1((yyvsp[(2) - (2)].nac));
                      if ( inmoduledeclare == 0 )
                      {
                         /* To know if there are in the module declaration    */
                         inmoduledeclare = 1;
                         /* to know if a module has been met                  */
                         inmodulemeet = 1;
                         /* to know if we are after the keyword contains      */
                         aftercontainsdeclare = 0 ;
                      }
                   ;}
    break;

  case 36:
#line 533 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                         strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));strcpy(subroutinename,(yyvsp[(1) - (1)].nac));
                      }
                   ;}
    break;

  case 38:
#line 542 "fortran.y"
    {
                      pos_curinclude = setposcur()-9;
                   ;}
    break;

  case 39:
#line 546 "fortran.y"
    {
                      if ( couldaddvariable == 1 ) Add_Include_1((yyvsp[(1) - (1)].nac));
                   ;}
    break;

  case 40:
#line 550 "fortran.y"
    {
                      if ( firstpass == 1 && couldaddvariable == 1) (yyval.l)=NULL;
                   ;}
    break;

  case 41:
#line 553 "fortran.y"
    {
                      if ( firstpass == 1 && couldaddvariable == 1 ) (yyval.l)=NULL;
                   ;}
    break;

  case 42:
#line 557 "fortran.y"
    {
                       if ( firstpass == 1 && couldaddvariable == 1 ) (yyval.l)=(yyvsp[(2) - (3)].l);
                   ;}
    break;

  case 45:
#line 564 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                         Add_SubroutineArgument_Var_1((yyvsp[(2) - (3)].l));
                      }
                   ;}
    break;

  case 46:
#line 571 "fortran.y"
    {
                      if ( firstpass == 1  && couldaddvariable == 1)
                      {
                         strcpy(nameinttypenameback,nameinttypename);
                         strcpy(nameinttypename,"");
                         curvar=createvar((yyvsp[(1) - (1)].na),NULL);
                        strcpy(nameinttypename,nameinttypenameback);
                         curlistvar=insertvar(NULL,curvar);
                         (yyval.l)=settype("",curlistvar);
                      }
                   ;}
    break;

  case 47:
#line 583 "fortran.y"
    {
                      if ( firstpass == 1  && couldaddvariable == 1)
                      {
                         strcpy(nameinttypenameback,nameinttypename);
                         strcpy(nameinttypename,"");                      
                         curvar=createvar((yyvsp[(3) - (3)].na),NULL);
                         strcpy(nameinttypename,nameinttypenameback);                         
                         (yyval.l)=insertvar((yyvsp[(1) - (3)].l),curvar);
                      }
                   ;}
    break;

  case 48:
#line 594 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].nac));;}
    break;

  case 49:
#line 595 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na),"*");;}
    break;

  case 50:
#line 598 "fortran.y"
    {
                      if ( VarTypepar == 1 )
                      {
                         couldaddvariable = 1 ;
                         VarTypepar = 0;
                      }
                   ;}
    break;

  case 51:
#line 606 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                         VarType = 1;
                         couldaddvariable = 0 ;
                      }
                   ;}
    break;

  case 52:
#line 614 "fortran.y"
    {
                      if ( VarType == 1 ) couldaddvariable = 1 ;
                      VarType = 0;
                      VarTypepar = 0;
                   ;}
    break;

  case 54:
#line 621 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                         if ( insubroutinedeclare == 0 )
                         {
                                                  Add_GlobalParameter_Var_1((yyvsp[(3) - (4)].l));
                                                  }
                         else Add_Parameter_Var_1((yyvsp[(3) - (4)].l));
                         pos_end = setposcur();
                        RemoveWordSET_0(fortranout,pos_cur_decl,
                                                    pos_end-pos_cur_decl);
                      }
                      VariableIsParameter =  0 ;
                   ;}
    break;

  case 55:
#line 636 "fortran.y"
    {
                     if ( couldaddvariable == 1 )
                     {
                        if ( insubroutinedeclare == 0 )
                                                  Add_GlobalParameter_Var_1((yyvsp[(2) - (2)].l));
                         else Add_Parameter_Var_1((yyvsp[(2) - (2)].l));
                         pos_end = setposcur();
                         RemoveWordSET_0(fortranout,pos_cur_decl,
                                                    pos_end-pos_cur_decl);
                      }
                      VariableIsParameter =  0 ;
                   ;}
    break;

  case 57:
#line 650 "fortran.y"
    {
                     pos_end = setposcur();
                     RemoveWordSET_0(fortranout,pos_cursave,
                                                pos_end-pos_cursave);
                  ;}
    break;

  case 59:
#line 657 "fortran.y"
    {
                   /* if the variable is a parameter we can suppose that is   */
                   /*    value is the same on each grid. It is not useless to */
                   /*    create a copy of it on each grid                     */
                      if ( couldaddvariable == 1 )
                      {
                         Add_Globliste_1((yyvsp[(1) - (1)].l));
                         /* if variableparamlists has been declared in a      */
                         /*    subroutine                                     */
                         if ( insubroutinedeclare == 1 )
                         {
                            Add_Dimension_Var_1((yyvsp[(1) - (1)].l));
                         }
                         pos_end = setposcur();
                         RemoveWordSET_0(fortranout,pos_curdimension,
                                                pos_end-pos_curdimension);
                      }
                      /*                                                      */
                      PublicDeclare = 0;
                      PrivateDeclare = 0;
                      ExternalDeclare = 0;
                      strcpy(NamePrecision,"");
                      c_star = 0;
                      strcpy(InitialValueGiven," ");
                      strcpy(IntentSpec,"");
                      VariableIsParameter =  0 ;
                      Allocatabledeclare = 0 ;
                      Targetdeclare = 0 ;
                      SaveDeclare = 0;
                      pointerdeclare = 0;
                      optionaldeclare = 0 ;
                      dimsgiven=0;
                      c_selectorgiven=0;
                      strcpy(nameinttypename,"");
                      strcpy(c_selectorname,"");
                   ;}
    break;

  case 60:
#line 694 "fortran.y"
    {
      if (firstpass == 0)
      {
      if ((yyvsp[(1) - (1)].lnn))
      {
      removeglobfromlist(&((yyvsp[(1) - (1)].lnn)));
      pos_end = setposcur();
           RemoveWordSET_0(fortranout,pos_cur,pos_end-pos_cur);
      writelistpublic((yyvsp[(1) - (1)].lnn));
      }
      }
      ;}
    break;

  case 70:
#line 716 "fortran.y"
    {
                      /* we should remove the data declaration                */
                      if ( couldaddvariable == 1 && aftercontainsdeclare != 2 )
                      {
                        pos_end = setposcur();
                        RemoveWordSET_0(fortranout,pos_curdata,
                                              pos_end-pos_curdata);
                      }
                      if ( couldaddvariable == 1 && aftercontainsdeclare == 1 )
                      {
                      if (firstpass == 0)
                      {
                        ReWriteDataStatement_0(fortranout);
                        pos_end = setposcur();
                  }
                      }
                  ;}
    break;

  case 72:
#line 736 "fortran.y"
    {
         PublicDeclare = 0 ;
         PrivateDeclare = 0 ;
      ;}
    break;

  case 110:
#line 787 "fortran.y"
    {
                      /* if the variable is a parameter we can suppose that is*/
                      /*    value is the same on each grid. It is not useless */
                      /*    to create a copy of it on each grid               */
                      if ( couldaddvariable == 1 )
                      {
                      pos_end = setposcur();
                      /*if (insubroutinedeclare == 0)
                        {   */
                         RemoveWordSET_0(fortranout,pos_cur_decl,
                                                 pos_end-pos_cur_decl);
                                         
                       /* }
                      else
                       {*/
                        ReWriteDeclarationAndAddTosubroutine_01((yyvsp[(1) - (2)].l));
                        pos_cur_decl = setposcur();
                        
                       /*}*/
                      if ( firstpass == 0 &&
                           GlobalDeclaration == 0 &&
                           insubroutinedeclare == 0 )
                      {

                         sprintf(ligne,"\n#include \"Module_Declar_%s.h\"\n"
                                                                ,curmodulename);
                         tofich(fortranout,ligne,1);
                         sprintf (ligne, "Module_Declar_%s.h",curmodulename);
                         module_declar = associate(ligne);
                         sprintf (ligne, " ");
                         tofich (module_declar, ligne,1);
                         GlobalDeclaration = 1 ;
                         pos_cur_decl = setposcur();

                      }
                         (yyval.l) = (yyvsp[(1) - (2)].l);
                         Add_Globliste_1((yyvsp[(1) - (2)].l));
                                                  
                         if ( insubroutinedeclare == 0 )
                                                  Add_GlobalParameter_Var_1((yyvsp[(1) - (2)].l));
                         else
                         {
                            if ( pointerdeclare == 1 )
                                                Add_Pointer_Var_From_List_1((yyvsp[(1) - (2)].l));
                            Add_Parameter_Var_1((yyvsp[(1) - (2)].l));
                         }

                         /* if variables has been declared in a subroutine    */
                         if ( insubroutinedeclare == 1 )
                         { 
                       /*    Add_SubroutineDeclaration_Var_1($1);*/
                         }
                         /* If there are a SAVE declarations in module's      */
                         /*    subroutines we should remove it from the       */
                         /*    subroutines declaration and add it in the      */
                         /*    global declarations                            */
                         if ( aftercontainsdeclare == 1 &&
                              SaveDeclare == 1 && firstpass == 1 )
                         {
                              if ( inmodulemeet == 0 ) Add_Save_Var_dcl_1((yyvsp[(1) - (2)].l));
                              else  Add_SubroutineDeclarationSave_Var_1((yyvsp[(1) - (2)].l));
                         }
                      }
                      /*                                                      */
                      PublicDeclare = 0;
                      PrivateDeclare = 0;
                      ExternalDeclare = 0;
                      strcpy(NamePrecision,"");
                      c_star = 0;
                      strcpy(InitialValueGiven," ");
                      strcpy(IntentSpec,"");
                      VariableIsParameter =  0 ;
                      Allocatabledeclare = 0 ;
                      Targetdeclare = 0 ;
                      SaveDeclare = 0;
                      pointerdeclare = 0;
                      optionaldeclare = 0 ;
                      dimsgiven=0;
                      c_selectorgiven=0;
                      strcpy(nameinttypename,"");
                      strcpy(c_selectorname,"");
                      GlobalDeclarationType = 0; 
                   ;}
    break;

  case 111:
#line 871 "fortran.y"
    {
                      /* open param file                                      */
                      if ( firstpass == 0 )
                      {
                         sprintf(ligne,"%s/ParamFile%s.h",nomdir,(yyvsp[(2) - (3)].nac));
                         paramout=fopen(ligne,"w");
                         if ( retour77 == 0 ) fprintf(paramout,"!\n");
                         else fprintf(paramout,"C\n");
                      }
                      strcpy(subroutinename,(yyvsp[(2) - (3)].nac));
                      if ( inmodulemeet == 1 )
                      {
                         insubroutinedeclare = 1;
                         /* we should to list of the subroutine argument the  */
                         /*    name of the function which has to be defined   */
                         Add_SubroutineArgument_Var_1((yyvsp[(3) - (3)].l));
                         Add_FunctionType_Var_1((yyvsp[(2) - (3)].nac));
                         /* in the second step we should write the head of    */
                         /*    the subroutine sub_loop_<subroutinename>       */
                         writeheadnewsub_0(2);
                      }
                      else
                      {
                         insubroutinedeclare = 1;
                         /* we should to list of the subroutine argument the  */
                         /*    name of the function which has to be defined   */
                         Add_SubroutineArgument_Var_1((yyvsp[(3) - (3)].l));
                         Add_FunctionType_Var_1((yyvsp[(2) - (3)].nac));
                         /* in the second step we should write the head of    */
                         /*    the subroutine sub_loop_<subroutinename>       */
                         writeheadnewsub_0(2);
                      }
                      strcpy(nameinttypename,"");

                   ;}
    break;

  case 112:
#line 908 "fortran.y"
    {
                       functiondeclarationisdone = 1;
                   ;}
    break;

  case 113:
#line 914 "fortran.y"
    {
                      VariableIsParameter = 1;
                      pos_curparameter = setposcur()-9;
                   ;}
    break;

  case 114:
#line 919 "fortran.y"
    {
                      pos_curdata = setposcur()-strlen((yyvsp[(1) - (1)].nac));
                      Init_List_Data_Var();
                   ;}
    break;

  case 115:
#line 924 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
/*                      if ( aftercontainsdeclare == 1 ) strcpy(ligne,"");
                      else */
/*                      sprintf(ligne,"%s",$3);*/
                      createstringfromlistname(ligne,(yyvsp[(3) - (4)].lnn));
                      if (firstpass == 1)
                      Add_Data_Var_1(&List_Data_Var,(yyvsp[(1) - (4)].nac),ligne);
                      else
                      Add_Data_Var_1(&List_Data_Var_Cur,(yyvsp[(1) - (4)].nac),ligne);
                      }
                   ;}
    break;

  case 116:
#line 938 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                      /*if ( aftercontainsdeclare == 1 ) strcpy(ligne,"");
                      else */
                      /*sprintf(ligne,"%s",$5);   */
                      createstringfromlistname(ligne,(yyvsp[(5) - (6)].lnn));                      
                      if (firstpass == 1)                      
                      Add_Data_Var_1(&List_Data_Var,(yyvsp[(3) - (6)].nac),ligne);
                      else
                      Add_Data_Var_1(&List_Data_Var_Cur,(yyvsp[(3) - (6)].nac),ligne);                      
                      }
                   ;}
    break;

  case 117:
#line 952 "fortran.y"
    {
                       /*******************************************************/
                       /*******************************************************/
                       /*******************************************************/
                       /*******************************************************/
                       /*******************************************************/
                       /*******************************************************/
                       /*******************************************************/
                       if (firstpass == 1)
                       Add_Data_Var_Names_01(&List_Data_Var,(yyvsp[(1) - (4)].lnn),(yyvsp[(3) - (4)].lnn));
                       else
                       Add_Data_Var_Names_01(&List_Data_Var_Cur,(yyvsp[(1) - (4)].lnn),(yyvsp[(3) - (4)].lnn));
                   ;}
    break;

  case 118:
#line 967 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                         (yyval.lnn) = Insertname(NULL,(yyvsp[(1) - (1)].na),0);
                      }
                   ;}
    break;

  case 119:
#line 974 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                         (yyval.lnn) = Insertname((yyvsp[(3) - (3)].lnn),(yyvsp[(1) - (3)].na),1);
                      }
                   ;}
    break;

  case 124:
#line 988 "fortran.y"
    {
                     pos_cursave = setposcur()-4;
                  ;}
    break;

  case 126:
#line 994 "fortran.y"
    {
                     if ( couldaddvariable == 1 ) Add_Save_Var_1((yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].d));
                  ;}
    break;

  case 127:
#line 999 "fortran.y"
    {
		(yyval.lnn)=Insertname(NULL,(yyvsp[(1) - (1)].nac),0);
		;}
    break;

  case 128:
#line 1003 "fortran.y"
    {
      printf("INSTRUCTION NON TRAITEE : INITIALISATION DE DATA AVEC EXPRESSION\n");
      exit(0);
      ;}
    break;

  case 129:
#line 1008 "fortran.y"
    {
      (yyval.lnn) = concat_listname((yyvsp[(1) - (3)].lnn),(yyvsp[(3) - (3)].lnn));
      ;}
    break;

  case 130:
#line 1013 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].nac));;}
    break;

  case 131:
#line 1015 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s+%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na));;}
    break;

  case 132:
#line 1017 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s-%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na));;}
    break;

  case 133:
#line 1019 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s*%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na));;}
    break;

  case 134:
#line 1021 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s/%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na));;}
    break;

  case 135:
#line 1024 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.nac),"");;}
    break;

  case 136:
#line 1026 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));;}
    break;

  case 143:
#line 1038 "fortran.y"
    {
	ininterfacedeclare = 1 ;
	printf("INTEFACE entree\n");
	;}
    break;

  case 144:
#line 1043 "fortran.y"
    {
      ininterfacedeclare = 0;
      ;}
    break;

  case 145:
#line 1048 "fortran.y"
    {
                      positioninblock=0;
                      pos_curdimension = setposcur()-9;
                   ;}
    break;

  case 146:
#line 1054 "fortran.y"
    {
         if ( couldaddvariable == 1 )
         {
            /*                                                                */
            curvar=createvar((yyvsp[(3) - (5)].nac),(yyvsp[(4) - (5)].d));
            /*                                                                */
            CreateAndFillin_Curvar("",curvar);
            /*                                                                */
            curlistvar=insertvar(NULL,curvar);
            /*                                                                */
            (yyval.l)=settype("",curlistvar);
            /*                                                                */
            strcpy(vallengspec,"");
         }
      ;}
    break;

  case 147:
#line 1070 "fortran.y"
    {
         if ( couldaddvariable == 1 )
         {
            /*                                                                */
            curvar=createvar((yyvsp[(3) - (5)].nac),(yyvsp[(4) - (5)].d));
            /*                                                                */
            CreateAndFillin_Curvar("",curvar);
            /*                                                                */
            curlistvar=insertvar((yyvsp[(1) - (5)].l),curvar);
            /*                                                                */
            (yyval.l)=curlistvar;
            /*                                                                */
            strcpy(vallengspec,"");
         }
      ;}
    break;

  case 150:
#line 1090 "fortran.y"
    {
        (yyval.lnn)=(listname *)NULL;
        ;}
    break;

  case 151:
#line 1094 "fortran.y"
    {
          (yyval.lnn)=(yyvsp[(3) - (3)].lnn);
         ;}
    break;

  case 152:
#line 1099 "fortran.y"
    {
           (yyval.lnn) = Insertname(NULL,(yyvsp[(1) - (1)].nac),0);
           ;}
    break;

  case 153:
#line 1103 "fortran.y"
    {
          (yyval.lnn) = Insertname((yyvsp[(1) - (3)].lnn),(yyvsp[(3) - (3)].nac),0);
          ;}
    break;

  case 154:
#line 1108 "fortran.y"
    {
                         pos_end = setposcur();
                         RemoveWordSET_0(fortranout,pos_curcommon,
                                                  pos_end-pos_curcommon);
                   ;}
    break;

  case 155:
#line 1114 "fortran.y"
    {
                         if ( couldaddvariable == 1 )
                         {
                            sprintf(charusemodule,"%s",(yyvsp[(2) - (3)].nac));
                            Add_NameOfCommon_1((yyvsp[(2) - (3)].nac),subroutinename);
                            pos_end = setposcur();
                            RemoveWordSET_0(fortranout,pos_curcommon,
                                                       pos_end-pos_curcommon);
                         }
                   ;}
    break;

  case 156:
#line 1125 "fortran.y"
    {
                         if ( couldaddvariable == 1 )
                         {
                            sprintf(charusemodule,"%s",(yyvsp[(3) - (5)].nac));
                            Add_NameOfCommon_1((yyvsp[(3) - (5)].nac),subroutinename);
                            pos_end = setposcur();
                            RemoveWordSET_0(fortranout,pos_curcommon,
                                                       pos_end-pos_curcommon);
                         }
                   ;}
    break;

  case 157:
#line 1137 "fortran.y"
    {
                      positioninblock=0;
                      pos_curcommon = setposcur()-6;
                   ;}
    break;

  case 158:
#line 1142 "fortran.y"
    {
                      positioninblock=0;
                      pos_curcommon = setposcur()-6-7;
                   ;}
    break;

  case 159:
#line 1148 "fortran.y"
    {
                      if ( couldaddvariable == 1 ) Add_Common_var_1();
                   ;}
    break;

  case 160:
#line 1153 "fortran.y"
    {
                      if ( couldaddvariable == 1 ) Add_Common_var_1();
                   ;}
    break;

  case 161:
#line 1157 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                         positioninblock = positioninblock + 1 ;
                         strcpy(commonvar,(yyvsp[(1) - (2)].nac));
                         commondim = (yyvsp[(2) - (2)].d);
                      }
                   ;}
    break;

  case 162:
#line 1167 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                         strcpy((yyval.nac),"");
                         positioninblock=0;
                         strcpy(commonblockname,"");
                      }
                   ;}
    break;

  case 163:
#line 1176 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                         strcpy((yyval.nac),(yyvsp[(2) - (3)].nac));
                         positioninblock=0;
                         strcpy(commonblockname,(yyvsp[(2) - (3)].nac));
                      }
                   ;}
    break;

  case 166:
#line 1189 "fortran.y"
    {
                      if ( couldaddvariable == 1 ) (yyval.l)=insertvar(NULL,(yyvsp[(1) - (1)].v));
                   ;}
    break;

  case 167:
#line 1193 "fortran.y"
    {
                      if ( couldaddvariable == 1 ) (yyval.l)=insertvar((yyvsp[(1) - (3)].l),(yyvsp[(3) - (3)].v));
                   ;}
    break;

  case 168:
#line 1198 "fortran.y"
    {
                     if ( couldaddvariable == 1 )
                     {
                         curvar=(variable *) malloc(sizeof(variable));
                         /*                                                   */
                         Init_Variable(curvar);
                         /*                                                   */
                         curvar->v_VariableIsParameter=1;
                         strcpy(curvar->v_nomvar,(yyvsp[(1) - (3)].nac));
                         Save_Length((yyvsp[(1) - (3)].nac),4);
                         strcpy(curvar->v_subroutinename,subroutinename);
                         Save_Length(subroutinename,11);
                         strcpy(curvar->v_modulename,curmodulename);
                         Save_Length(curmodulename,6);
                         strcpy(curvar->v_initialvalue,(yyvsp[(3) - (3)].na));
                         Save_Length((yyvsp[(3) - (3)].na),14);
                         strcpy(curvar->v_commoninfile,mainfile);
                         Save_Length(mainfile,10);
                         (yyval.v)=curvar;
                      }
                   ;}
    break;

  case 172:
#line 1226 "fortran.y"
    {
                       if ( insubroutinedeclare == 1 )
                       {
                          Add_ImplicitNoneSubroutine_1();
                          pos_end = setposcur();
                          RemoveWordSET_0(fortranout,pos_end-13,
                                                             13);
                       }
                    ;}
    break;

  case 175:
#line 1240 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                         /*                                                   */
                         if (dimsgiven == 1)
                         {
                            curvar=createvar((yyvsp[(3) - (6)].nac),curdim);
                            GlobalDeclarationType == 0;
                         }
                         else
                         {
                            curvar=createvar((yyvsp[(3) - (6)].nac),(yyvsp[(4) - (6)].d));
                         }
                         /*                                                   */
                         CreateAndFillin_Curvar(DeclType,curvar);
                         /*                                                   */
                         curlistvar=insertvar(NULL,curvar);
                         if (!strcasecmp(DeclType,"character"))
                         {
                            if (c_selectorgiven == 1)
                            {
                               strcpy(c_selectordim.first,"1");
                               strcpy(c_selectordim.last,c_selectorname);
                               Save_Length(c_selectorname,1);
                               change_dim_char
                                     (insertdim(NULL,c_selectordim),curlistvar);
                            }
                         }
                         (yyval.l)=settype(DeclType,curlistvar);
                      }
                      strcpy(vallengspec,"");
                   ;}
    break;

  case 176:
#line 1273 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                         if (dimsgiven == 1)
                         {
                            curvar=createvar((yyvsp[(4) - (7)].nac),curdim);
                         }
                         else
                         {
                            curvar=createvar((yyvsp[(4) - (7)].nac),(yyvsp[(5) - (7)].d));
                         }
                         /*                                                   */
                         CreateAndFillin_Curvar((yyvsp[(1) - (7)].l)->var->v_typevar,curvar);
                         /*                                                   */
                         strcpy(curvar->v_typevar,((yyvsp[(1) - (7)].l)->var->v_typevar));
                         Save_Length((yyvsp[(1) - (7)].l)->var->v_typevar,3);
                         /*                                                   */
                         curlistvar=insertvar((yyvsp[(1) - (7)].l),curvar);
                         if (!strcasecmp(DeclType,"character"))
                         {
                            if (c_selectorgiven == 1)
                            {
                               strcpy(c_selectordim.first,"1");
                               strcpy(c_selectordim.last,c_selectorname);
                               Save_Length(c_selectorname,1);
                               change_dim_char
                                     (insertdim(NULL,c_selectordim),curlistvar);
                            }
                         }
                         (yyval.l)=curlistvar;
                      }
                      strcpy(vallengspec,"");
                   ;}
    break;

  case 177:
#line 1307 "fortran.y"
    {dimsgiven=0;;}
    break;

  case 178:
#line 1310 "fortran.y"
    {strcpy(DeclType,(yyvsp[(1) - (2)].nac));;}
    break;

  case 179:
#line 1312 "fortran.y"
    {
                      strcpy(DeclType,"CHARACTER");
                   ;}
    break;

  case 180:
#line 1316 "fortran.y"
    {
                      strcpy(DeclType,(yyvsp[(1) - (3)].nac));
                      strcpy(nameinttypename,(yyvsp[(3) - (3)].nac));
                   ;}
    break;

  case 181:
#line 1321 "fortran.y"
    {
                      strcpy(DeclType,"TYPE");
                      GlobalDeclarationType = 1
                   ;}
    break;

  case 182:
#line 1327 "fortran.y"
    {
                 /*     if ( couldaddvariable == 1 ) VarTypepar = 1 ;
                      couldaddvariable = 0 ;
                      pos_cur_decl = setposcur()-5;*/
                   pos_cur_decl = setposcur()-5;
                   ;}
    break;

  case 184:
#line 1336 "fortran.y"
    {c_selectorgiven=1;strcpy(c_selectorname,(yyvsp[(2) - (2)].nac));;}
    break;

  case 185:
#line 1337 "fortran.y"
    {c_star = 1;;}
    break;

  case 190:
#line 1345 "fortran.y"
    {
                      pos_cur_decl = setposcur()-9;
                   ;}
    break;

  case 191:
#line 1349 "fortran.y"
    {strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));;}
    break;

  case 192:
#line 1352 "fortran.y"
    {
                      strcpy((yyval.nac),"INTEGER");
                      pos_cur_decl = setposcur()-7;
                   ;}
    break;

  case 193:
#line 1356 "fortran.y"
    {
                      strcpy((yyval.nac),"REAL");
                      pos_cur_decl = setposcur()-4;
                   ;}
    break;

  case 194:
#line 1361 "fortran.y"
    {strcpy((yyval.nac),"COMPLEX");
                   pos_cur_decl = setposcur()-7;;}
    break;

  case 195:
#line 1364 "fortran.y"
    {
                      pos_cur_decl = setposcur()-16;
                      strcpy((yyval.nac),"REAL");
                      strcpy(nameinttypename,"8");
                   ;}
    break;

  case 196:
#line 1370 "fortran.y"
    {strcpy((yyval.nac),"DOUBLE COMPLEX");;}
    break;

  case 197:
#line 1372 "fortran.y"
    {
                      strcpy((yyval.nac),"LOGICAL");
                      pos_cur_decl = setposcur()-7;
                   ;}
    break;

  case 199:
#line 1378 "fortran.y"
    {strcpy(vallengspec,(yyvsp[(2) - (2)].na));;}
    break;

  case 200:
#line 1380 "fortran.y"
    {sprintf((yyval.na),"*%s",(yyvsp[(1) - (1)].na));;}
    break;

  case 201:
#line 1381 "fortran.y"
    {strcpy((yyval.na),"*(*)");;}
    break;

  case 208:
#line 1392 "fortran.y"
    {
                      if ( strstr((yyvsp[(3) - (3)].na),"0.d0") )
                      {
                         strcpy(nameinttypename,"8");
                         sprintf(NamePrecision,"");
                      }
                      else sprintf(NamePrecision,"%s = %s",(yyvsp[(1) - (3)].nac),(yyvsp[(3) - (3)].na));
                   ;}
    break;

  case 209:
#line 1401 "fortran.y"
    {
                      strcpy(NamePrecision,(yyvsp[(1) - (1)].nac));
                   ;}
    break;

  case 210:
#line 1405 "fortran.y"
    {
                      strcpy(NamePrecision,(yyvsp[(1) - (1)].nac));
                   ;}
    break;

  case 211:
#line 1409 "fortran.y"
    {strcpy(CharacterSize,(yyvsp[(1) - (1)].na));
                    strcpy((yyval.na),(yyvsp[(1) - (1)].na));;}
    break;

  case 212:
#line 1411 "fortran.y"
    {strcpy(CharacterSize,"*");
                    strcpy((yyval.na),"*");;}
    break;

  case 220:
#line 1425 "fortran.y"
    {
                      VariableIsParameter = 1;
                   ;}
    break;

  case 222:
#line 1430 "fortran.y"
    {Allocatabledeclare = 1;;}
    break;

  case 223:
#line 1432 "fortran.y"
    {
                      dimsgiven=1;
                      curdim=(yyvsp[(2) - (2)].d);
                   ;}
    break;

  case 224:
#line 1437 "fortran.y"
    {ExternalDeclare = 1;;}
    break;

  case 225:
#line 1439 "fortran.y"
    {strcpy(IntentSpec,(yyvsp[(3) - (4)].nac));;}
    break;

  case 227:
#line 1441 "fortran.y"
    {optionaldeclare = 1 ;;}
    break;

  case 228:
#line 1442 "fortran.y"
    {pointerdeclare = 1 ;;}
    break;

  case 229:
#line 1443 "fortran.y"
    {
/*                       if ( inmodulemeet == 1 )
                       {*/
                          SaveDeclare = 1 ;
                     /*  }*/
                    ;}
    break;

  case 230:
#line 1450 "fortran.y"
    {Targetdeclare = 1;;}
    break;

  case 231:
#line 1452 "fortran.y"
    {strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));;}
    break;

  case 232:
#line 1453 "fortran.y"
    {strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));;}
    break;

  case 233:
#line 1454 "fortran.y"
    {strcpy((yyval.nac),(yyvsp[(1) - (1)].nac)); ;}
    break;

  case 234:
#line 1457 "fortran.y"
    {PublicDeclare = 1;;}
    break;

  case 235:
#line 1459 "fortran.y"
    {PrivateDeclare = 1;;}
    break;

  case 236:
#line 1461 "fortran.y"
    {if ( created_dimensionlist == 1 )
                       {
                           (yyval.d)=(listdim *)NULL;
                       }
                   ;}
    break;

  case 237:
#line 1467 "fortran.y"
    {if ( created_dimensionlist == 1 ||
                         agrif_parentcall      == 1 ) (yyval.d)=(yyvsp[(2) - (3)].d);;}
    break;

  case 238:
#line 1470 "fortran.y"
    {if ( created_dimensionlist == 1 ||
                         agrif_parentcall      == 1 ) (yyval.d)=insertdim(NULL,(yyvsp[(1) - (1)].dim1));;}
    break;

  case 239:
#line 1473 "fortran.y"
    {if ( couldaddvariable == 1 )
                         if ( created_dimensionlist == 1 ) (yyval.d)=insertdim((yyvsp[(1) - (3)].d),(yyvsp[(3) - (3)].dim1));;}
    break;

  case 240:
#line 1476 "fortran.y"
    {
                      strcpy((yyval.dim1).first,"1");
                      strcpy((yyval.dim1).last,(yyvsp[(1) - (1)].na));
                      Save_Length((yyvsp[(1) - (1)].na),1);
                   ;}
    break;

  case 241:
#line 1481 "fortran.y"
    {
                      strcpy((yyval.dim1).first,"");
                      strcpy((yyval.dim1).last,"");
                   ;}
    break;

  case 242:
#line 1485 "fortran.y"
    {
                      strcpy((yyval.dim1).first,(yyvsp[(1) - (2)].na));
                      Save_Length((yyvsp[(1) - (2)].na),2);
                      strcpy((yyval.dim1).last,"");
                   ;}
    break;

  case 243:
#line 1490 "fortran.y"
    {
                      strcpy((yyval.dim1).first,"");
                      strcpy((yyval.dim1).last,(yyvsp[(2) - (2)].na));
                      Save_Length((yyvsp[(2) - (2)].na),1);
                   ;}
    break;

  case 244:
#line 1496 "fortran.y"
    {
                      strcpy((yyval.dim1).first,(yyvsp[(1) - (3)].na));
                      Save_Length((yyvsp[(1) - (3)].na),2);
                      strcpy((yyval.dim1).last,(yyvsp[(3) - (3)].na));
                      Save_Length((yyvsp[(3) - (3)].na),1);
                   ;}
    break;

  case 245:
#line 1503 "fortran.y"
    {strcpy((yyval.na),"*");;}
    break;

  case 246:
#line 1504 "fortran.y"
    {strcpy((yyval.na),(yyvsp[(1) - (1)].na));;}
    break;

  case 247:
#line 1506 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na));;}
    break;

  case 248:
#line 1508 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"(%s)",(yyvsp[(2) - (3)].na));;}
    break;

  case 249:
#line 1510 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na));;}
    break;

  case 250:
#line 1512 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na));;}
    break;

  case 251:
#line 1516 "fortran.y"
    {sprintf((yyval.na),"SUM(%s)",(yyvsp[(2) - (3)].na));;}
    break;

  case 252:
#line 1518 "fortran.y"
    {sprintf((yyval.na),"MAX(%s)",(yyvsp[(2) - (3)].na));;}
    break;

  case 253:
#line 1520 "fortran.y"
    {sprintf((yyval.na),"TANH(%s)",(yyvsp[(3) - (4)].na));;}
    break;

  case 254:
#line 1522 "fortran.y"
    {sprintf((yyval.na),"MAXVAL(%s)",(yyvsp[(3) - (4)].na));;}
    break;

  case 255:
#line 1524 "fortran.y"
    {sprintf((yyval.na),"MIN(%s)",(yyvsp[(2) - (3)].na));;}
    break;

  case 256:
#line 1526 "fortran.y"
    {sprintf((yyval.na),"MINVAL(%s)",(yyvsp[(3) - (4)].na));;}
    break;

  case 257:
#line 1528 "fortran.y"
    {sprintf((yyval.na),"TRIM(%s)",(yyvsp[(3) - (4)].na));;}
    break;

  case 258:
#line 1530 "fortran.y"
    {sprintf((yyval.na),"SQRT(%s)",(yyvsp[(2) - (3)].na));;}
    break;

  case 259:
#line 1532 "fortran.y"
    {sprintf((yyval.na),"REAL(%s)",(yyvsp[(3) - (4)].na));;}
    break;

  case 260:
#line 1534 "fortran.y"
    {sprintf((yyval.na),"NINT(%s)",(yyvsp[(3) - (4)].na));;}
    break;

  case 261:
#line 1536 "fortran.y"
    {sprintf((yyval.na),"FLOAT(%s)",(yyvsp[(3) - (4)].na));;}
    break;

  case 262:
#line 1538 "fortran.y"
    {sprintf((yyval.na),"EXP(%s)",(yyvsp[(3) - (4)].na));;}
    break;

  case 263:
#line 1540 "fortran.y"
    {sprintf((yyval.na),"COS(%s)",(yyvsp[(3) - (4)].na));;}
    break;

  case 264:
#line 1542 "fortran.y"
    {sprintf((yyval.na),"COSH(%s)",(yyvsp[(3) - (4)].na));;}
    break;

  case 265:
#line 1544 "fortran.y"
    {sprintf((yyval.na),"ACOS(%s)",(yyvsp[(3) - (4)].na));;}
    break;

  case 266:
#line 1546 "fortran.y"
    {sprintf((yyval.na),"SIN(%s)",(yyvsp[(3) - (4)].na));;}
    break;

  case 267:
#line 1548 "fortran.y"
    {sprintf((yyval.na),"SINH(%s)",(yyvsp[(3) - (4)].na));;}
    break;

  case 268:
#line 1550 "fortran.y"
    {sprintf((yyval.na),"ASIN(%s)",(yyvsp[(3) - (4)].na));;}
    break;

  case 269:
#line 1552 "fortran.y"
    {sprintf((yyval.na),"LOG(%s)",(yyvsp[(3) - (4)].na));;}
    break;

  case 270:
#line 1554 "fortran.y"
    {sprintf((yyval.na),"TAN(%s)",(yyvsp[(3) - (4)].na));;}
    break;

  case 271:
#line 1556 "fortran.y"
    {sprintf((yyval.na),"ATAN(%s)",(yyvsp[(3) - (4)].na));;}
    break;

  case 272:
#line 1558 "fortran.y"
    {sprintf((yyval.na),"ABS(%s)",(yyvsp[(2) - (3)].na));;}
    break;

  case 273:
#line 1560 "fortran.y"
    {sprintf((yyval.na),"MOD(%s)",(yyvsp[(3) - (4)].na));;}
    break;

  case 274:
#line 1562 "fortran.y"
    {sprintf((yyval.na),"SIGN(%s)",(yyvsp[(3) - (4)].na));;}
    break;

  case 275:
#line 1564 "fortran.y"
    {sprintf((yyval.na),"MINLOC(%s)",(yyvsp[(3) - (4)].na));;}
    break;

  case 276:
#line 1566 "fortran.y"
    {sprintf((yyval.na),"MAXLOC(%s)",(yyvsp[(3) - (4)].na));;}
    break;

  case 277:
#line 1568 "fortran.y"
    {strcpy((yyval.na),(yyvsp[(1) - (1)].na));;}
    break;

  case 278:
#line 1570 "fortran.y"
    {if ( couldaddvariable == 1 )
                   { strcpy((yyval.na),(yyvsp[(1) - (3)].na));strcat((yyval.na),",");strcat((yyval.na),(yyvsp[(3) - (3)].na));};}
    break;

  case 279:
#line 1573 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na));;}
    break;

  case 280:
#line 1575 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].nac));;}
    break;

  case 281:
#line 1577 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na));;}
    break;

  case 282:
#line 1579 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].na),(yyvsp[(2) - (2)].na));;}
    break;

  case 283:
#line 1581 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));;}
    break;

  case 284:
#line 1583 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));;}
    break;

  case 285:
#line 1585 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.nac),"+");;}
    break;

  case 286:
#line 1586 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.nac),"-");;}
    break;

  case 287:
#line 1589 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"+%s",(yyvsp[(2) - (2)].na));;}
    break;

  case 288:
#line 1591 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"-%s",(yyvsp[(2) - (2)].na));;}
    break;

  case 289:
#line 1593 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"*%s",(yyvsp[(2) - (2)].na));;}
    break;

  case 290:
#line 1595 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));;}
    break;

  case 291:
#line 1597 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));;}
    break;

  case 292:
#line 1599 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));;}
    break;

  case 293:
#line 1601 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));;}
    break;

  case 294:
#line 1603 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na)," > %s",(yyvsp[(2) - (2)].na));;}
    break;

  case 295:
#line 1605 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));;}
    break;

  case 296:
#line 1607 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na)," < %s",(yyvsp[(2) - (2)].na));;}
    break;

  case 297:
#line 1609 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));;}
    break;

  case 298:
#line 1611 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na)," >= %s",(yyvsp[(3) - (3)].na));;}
    break;

  case 299:
#line 1613 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));;}
    break;

  case 300:
#line 1615 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na)," <= %s",(yyvsp[(3) - (3)].na));;}
    break;

  case 301:
#line 1617 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));;}
    break;

  case 302:
#line 1619 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));;}
    break;

  case 303:
#line 1621 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));;}
    break;

  case 304:
#line 1623 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));;}
    break;

  case 305:
#line 1625 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));;}
    break;

  case 306:
#line 1627 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s",(yyvsp[(2) - (2)].na));;}
    break;

  case 307:
#line 1629 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s",(yyvsp[(2) - (2)].na));;}
    break;

  case 308:
#line 1631 "fortran.y"
    {strcpy((yyval.na),"");;}
    break;

  case 309:
#line 1633 "fortran.y"
    {sprintf((yyval.na),"/%s",(yyvsp[(1) - (1)].na));;}
    break;

  case 310:
#line 1635 "fortran.y"
    {sprintf((yyval.na),"/= %s",(yyvsp[(2) - (2)].na));;}
    break;

  case 311:
#line 1637 "fortran.y"
    {sprintf((yyval.na),"//%s",(yyvsp[(2) - (2)].na));;}
    break;

  case 312:
#line 1640 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"==%s",(yyvsp[(2) - (2)].na));;}
    break;

  case 313:
#line 1642 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"= %s",(yyvsp[(1) - (1)].na));;}
    break;

  case 314:
#line 1645 "fortran.y"
    {if ( couldaddvariable == 1 )
						{
						printf("ident = %s\n",(yyvsp[(1) - (1)].nac));
						strcpy((yyval.na),(yyvsp[(1) - (1)].nac));}
						;}
    break;

  case 315:
#line 1651 "fortran.y"
    {if ( couldaddvariable == 1 ) {
                   printf("struct = %s\n",(yyvsp[(1) - (1)].na));
                   strcpy((yyval.na),(yyvsp[(1) - (1)].na));}
                   ;}
    break;

  case 316:
#line 1656 "fortran.y"
    {if ( couldaddvariable == 1 ) {
                   printf("arrayref = %s\n",(yyvsp[(1) - (1)].na));
                   strcpy((yyval.na),(yyvsp[(1) - (1)].na));
                   };}
    break;

  case 317:
#line 1661 "fortran.y"
    {
                      agrif_parentcall =0;
                      if (!strcasecmp(identcopy,"Agrif_Parent") )
                                                            agrif_parentcall =1;
                      if ( Agrif_in_Tok_NAME(identcopy) == 1 )
                      {
                         inagrifcallargument = 1;
                         Add_SubroutineWhereAgrifUsed_1(subroutinename,
                                                        curmodulename);
                      }
                   ;}
    break;

  case 318:
#line 1674 "fortran.y"
    {
                     strcpy((yyval.na),(yyvsp[(1) - (1)].na));
                     if ( incalldeclare == 0 ) inagrifcallargument = 0;
                   ;}
    break;

  case 319:
#line 1679 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na)," %s %s ",(yyvsp[(1) - (2)].na),(yyvsp[(2) - (2)].na));;}
    break;

  case 320:
#line 1681 "fortran.y"
    {if ( couldaddvariable == 1 )
                                                sprintf((yyval.na)," %s ( %s )",(yyvsp[(1) - (4)].na),(yyvsp[(3) - (4)].na));;}
    break;

  case 321:
#line 1684 "fortran.y"
    {if ( couldaddvariable == 1 )
                                         sprintf((yyval.na)," %s ( %s ) %s ",(yyvsp[(1) - (5)].na),(yyvsp[(3) - (5)].na),(yyvsp[(5) - (5)].na));;}
    break;

  case 322:
#line 1688 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                         sprintf((yyval.na)," %s ( %s )",(yyvsp[(1) - (4)].nac),(yyvsp[(3) - (4)].na));
                         ModifyTheAgrifFunction_0((yyvsp[(3) - (4)].na));
                         agrif_parentcall =0;
                      }
                   ;}
    break;

  case 323:
#line 1698 "fortran.y"
    {
                      sprintf((yyval.na)," %s %% %s ",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na));
                      if ( incalldeclare == 0 ) inagrifcallargument = 0;
                   ;}
    break;

  case 324:
#line 1704 "fortran.y"
    {sprintf((yyval.na),"(/%s/)",(yyvsp[(2) - (3)].na));;}
    break;

  case 325:
#line 1706 "fortran.y"
    {strcpy((yyval.na)," ");;}
    break;

  case 326:
#line 1708 "fortran.y"
    {strcpy((yyval.na),(yyvsp[(2) - (2)].na));;}
    break;

  case 327:
#line 1710 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na));;}
    break;

  case 328:
#line 1712 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s,%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na));;}
    break;

  case 329:
#line 1714 "fortran.y"
    {strcpy((yyval.na),(yyvsp[(1) - (1)].na));;}
    break;

  case 330:
#line 1715 "fortran.y"
    {strcpy((yyval.na),(yyvsp[(1) - (1)].na));;}
    break;

  case 331:
#line 1718 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s :%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na));;}
    break;

  case 332:
#line 1720 "fortran.y"
    {if ( couldaddvariable == 1 )
                                               sprintf((yyval.na),"%s :%s :%s",(yyvsp[(1) - (5)].na),(yyvsp[(3) - (5)].na),(yyvsp[(5) - (5)].na));;}
    break;

  case 333:
#line 1723 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),":%s :%s",(yyvsp[(2) - (4)].na),(yyvsp[(4) - (4)].na));;}
    break;

  case 334:
#line 1724 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),": : %s",(yyvsp[(3) - (3)].na));;}
    break;

  case 335:
#line 1725 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),":%s",(yyvsp[(2) - (2)].na));;}
    break;

  case 336:
#line 1726 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s :",(yyvsp[(1) - (2)].na));;}
    break;

  case 337:
#line 1727 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),":");;}
    break;

  case 338:
#line 1729 "fortran.y"
    {
                       if ( couldaddvariable == 1 && afterpercent == 0)
                       {
                       if ( Vartonumber((yyvsp[(1) - (1)].nac)) == 1 )
                       {
                          Add_SubroutineWhereAgrifUsed_1(subroutinename,
                                                        curmodulename);
                       }
                       if (!strcasecmp((yyvsp[(1) - (1)].nac),"Agrif_Parent") )
                                                            agrif_parentcall =1;
                       if ( VariableIsNotFunction((yyvsp[(1) - (1)].nac)) == 0 )
                       {
                       printf("var = %s\n",(yyvsp[(1) - (1)].nac));
                          if ( inagrifcallargument == 1 )
                          {
                             if ( !strcasecmp((yyvsp[(1) - (1)].nac),identcopy) )
                             {
                                strcpy(sameagrifname,identcopy);
                                sameagrifargument = 1;
                             }
                          }
                          strcpy(identcopy,(yyvsp[(1) - (1)].nac));
                          pointedvar=0;
                          strcpy(truename,(yyvsp[(1) - (1)].nac));
                          if (variscoupled_0((yyvsp[(1) - (1)].nac))) strcpy(truename,getcoupledname_0((yyvsp[(1) - (1)].nac)));

                          if ( VarIsNonGridDepend(truename) == 0 &&
                               Variableshouldberemove(truename) == 0 )
                          {                      
                             if ( inagrifcallargument == 1 ||
                                  varispointer_0(truename) == 1 )
                             {
                             printf("var2 = %s\n",(yyvsp[(1) - (1)].nac));
                            if ((IsinListe(List_UsedInSubroutine_Var,(yyvsp[(1) - (1)].nac)) == 1) || (inagrifcallargument == 1))
                             {
                              if (varistyped_0(truename) == 0)
                                 {
                                 ModifyTheVariableName_0(truename,strlen((yyvsp[(1) - (1)].nac)));
                                 }
                                 }
                             }
                             printf("ici3\n");
                             if ( inagrifcallargument != 1 ||
                                  sameagrifargument ==1 )
                                  {
                                  printf("ici5 %s\n",truename);
                                  Add_UsedInSubroutine_Var_1(truename);
                          }
                          }
                          NotifyAgrifFunction_0(truename);
                       }
                       }
                       else
                       {
                       afterpercent = 0;
                    }
                    ;}
    break;

  case 339:
#line 1788 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.nac),".TRUE.");;}
    break;

  case 340:
#line 1789 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.nac),".FALSE.");;}
    break;

  case 341:
#line 1790 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));;}
    break;

  case 342:
#line 1791 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));;}
    break;

  case 343:
#line 1792 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));;}
    break;

  case 344:
#line 1793 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));;}
    break;

  case 345:
#line 1795 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.nac),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].nac));;}
    break;

  case 347:
#line 1799 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));;}
    break;

  case 349:
#line 1802 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));;}
    break;

  case 350:
#line 1804 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));;}
    break;

  case 351:
#line 1806 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na)," ");;}
    break;

  case 352:
#line 1807 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na));;}
    break;

  case 353:
#line 1810 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"(%s :%s)",(yyvsp[(2) - (5)].na),(yyvsp[(4) - (5)].na));;}
    break;

  case 354:
#line 1812 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na)," ");;}
    break;

  case 355:
#line 1813 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na));;}
    break;

  case 356:
#line 1815 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na)," ");;}
    break;

  case 357:
#line 1816 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na));;}
    break;

  case 358:
#line 1818 "fortran.y"
    {   strcpy(InitialValueGiven," ");;}
    break;

  case 359:
#line 1820 "fortran.y"
    {
                       if ( couldaddvariable == 1 )
                       {
                          strcpy(InitValue,(yyvsp[(3) - (3)].na));
                          strcpy(InitialValueGiven,"=");
                       }
                    ;}
    break;

  case 360:
#line 1828 "fortran.y"
    {
                       if ( couldaddvariable == 1 )
                       {
                          strcpy(InitValue,(yyvsp[(3) - (3)].na));
                          strcpy(InitialValueGiven,"=>");
                       }
                    ;}
    break;

  case 361:
#line 1836 "fortran.y"
    {pos_curinit = setposcur();;}
    break;

  case 362:
#line 1839 "fortran.y"
    {sprintf((yyval.na),"(%s,%s)",(yyvsp[(2) - (5)].na),(yyvsp[(4) - (5)].na));;}
    break;

  case 363:
#line 1842 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                      /* if variables has been declared in a subroutine       */
                      if (insubroutinedeclare == 1)
                      {
                         copyuse_0((yyvsp[(2) - (2)].nac));
                      }
                      sprintf(charusemodule,"%s",(yyvsp[(2) - (2)].nac));
                      Add_NameOfModuleUsed_1((yyvsp[(2) - (2)].nac));

                      if ( inmoduledeclare == 0 )
                      {
                         pos_end = setposcur();
                         RemoveWordSET_0(fortranout,pos_curuse,
                                               pos_end-pos_curuse);
                      }
                      }
                    ;}
    break;

  case 364:
#line 1862 "fortran.y"
    {
                       if ( couldaddvariable == 1 )
                       {
                      if (insubroutinedeclare == 1)
                      {
                         Add_CouplePointed_Var_1((yyvsp[(2) - (4)].nac),(yyvsp[(4) - (4)].lc));
                      }
                      if ( firstpass == 1 )
                      {
                         if ( insubroutinedeclare == 1 )
                         {
                            coupletmp = (yyvsp[(4) - (4)].lc);
                            strcpy(ligne,"");
                            while ( coupletmp )
                            {
                               strcat(ligne,coupletmp->c_namevar);
                               strcat(ligne," => ");
                               strcat(ligne,coupletmp->c_namepointedvar);
                               coupletmp = coupletmp->suiv;
                               if ( coupletmp ) strcat(ligne,",");
                            }
                            sprintf(charusemodule,"%s",(yyvsp[(2) - (4)].nac));
                         }
                         Add_NameOfModuleUsed_1((yyvsp[(2) - (4)].nac));
                      }
                      if ( inmoduledeclare == 0 )
                      {
                         pos_end = setposcur();
                         RemoveWordSET_0(fortranout,pos_curuse,
                                               pos_end-pos_curuse);
                      }
                      }
                    ;}
    break;

  case 365:
#line 1896 "fortran.y"
    {
                       if ( couldaddvariable == 1 )
                       {
                      /* if variables has been declared in a subroutine       */
                      if (insubroutinedeclare == 1)
                      {
                         copyuseonly_0((yyvsp[(2) - (6)].nac));
                      }
                      sprintf(charusemodule,"%s",(yyvsp[(2) - (6)].nac));
                      Add_NameOfModuleUsed_1((yyvsp[(2) - (6)].nac));

                       if ( inmoduledeclare == 0 )
                       {
                          pos_end = setposcur();
                          RemoveWordSET_0(fortranout,pos_curuse,
                                                pos_end-pos_curuse);
                       }
                       }
                    ;}
    break;

  case 366:
#line 1916 "fortran.y"
    {
                       if ( couldaddvariable == 1 )
                       {
                       /* if variables has been declared in a subroutine      */
                       if (insubroutinedeclare == 1)
                       {
                          Add_CouplePointed_Var_1((yyvsp[(2) - (6)].nac),(yyvsp[(6) - (6)].lc));
                       }
                       if ( firstpass == 1 )
                       {
                         if ( insubroutinedeclare == 1 )
                         {
                             coupletmp = (yyvsp[(6) - (6)].lc);
                             strcpy(ligne,"");
                             while ( coupletmp )
                             {
                                strcat(ligne,coupletmp->c_namevar);
                               if ( strcasecmp(coupletmp->c_namepointedvar,"") )
                                                           strcat(ligne," => ");
                                strcat(ligne,coupletmp->c_namepointedvar);
                                coupletmp = coupletmp->suiv;
                                if ( coupletmp ) strcat(ligne,",");
                             }
                             sprintf(charusemodule,"%s",(yyvsp[(2) - (6)].nac));
                          }
                          Add_NameOfModuleUsed_1((yyvsp[(2) - (6)].nac));
                       }
                       if ( firstpass == 0 )
                       {
                          if ( inmoduledeclare == 0 )
                          {

                            pos_end = setposcur();
                             RemoveWordSET_0(fortranout,pos_curuse,
                                                   pos_end-pos_curuse);
                       if (oldfortranout) 
                         variableisglobalinmodule((yyvsp[(6) - (6)].lc),(yyvsp[(2) - (6)].nac),oldfortranout,pos_curuseold);
                        
                          }
                          else
                          {

                             /* if we are in the module declare and if the    */
                             /* onlylist is a list of global variable         */
                             variableisglobalinmodule((yyvsp[(6) - (6)].lc), (yyvsp[(2) - (6)].nac), fortranout,pos_curuse);
                          }
                       }
                       }
                    ;}
    break;

  case 367:
#line 1967 "fortran.y"
    {
                      pos_curuse = setposcur()-strlen((yyvsp[(1) - (1)].nac));
                     if (firstpass == 0 && oldfortranout) {
                     pos_curuseold = setposcurname(oldfortranout);
                     }
                   ;}
    break;

  case 368:
#line 1975 "fortran.y"
    {strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));;}
    break;

  case 369:
#line 1978 "fortran.y"
    {
                       if ( couldaddvariable == 1 ) (yyval.lc) = (yyvsp[(1) - (1)].lc);
                    ;}
    break;

  case 370:
#line 1982 "fortran.y"
    {
                        if ( couldaddvariable == 1 )
                        {
                        /* insert the variable in the list $1                 */
                        (yyvsp[(3) - (3)].lc)->suiv = (yyvsp[(1) - (3)].lc);
                        (yyval.lc) = (yyvsp[(3) - (3)].lc);
                        }
                    ;}
    break;

  case 371:
#line 1992 "fortran.y"
    {
                       coupletmp =(listcouple *)malloc(sizeof(listcouple));
                       strcpy(coupletmp->c_namevar,(yyvsp[(1) - (3)].nac));
                       Save_Length((yyvsp[(1) - (3)].nac),21);
                       strcpy(coupletmp->c_namepointedvar,(yyvsp[(3) - (3)].nac));
                       Save_Length((yyvsp[(3) - (3)].nac),22);
                       coupletmp->suiv = NULL;
                       (yyval.lc) = coupletmp;
                     ;}
    break;

  case 372:
#line 2003 "fortran.y"
    {
                       if ( couldaddvariable == 1 ) (yyval.lc) = (yyvsp[(1) - (1)].lc);
                    ;}
    break;

  case 373:
#line 2007 "fortran.y"
    {
                        if ( couldaddvariable == 1 )
                        {
                        /* insert the variable in the list $1                 */
                        (yyvsp[(3) - (3)].lc)->suiv = (yyvsp[(1) - (3)].lc);
                        (yyval.lc) = (yyvsp[(3) - (3)].lc);
                        }
                    ;}
    break;

  case 374:
#line 2017 "fortran.y"
    {
                       coupletmp =(listcouple *)malloc(sizeof(listcouple));
                       strcpy(coupletmp->c_namevar,(yyvsp[(1) - (3)].nac));
                       Save_Length((yyvsp[(1) - (3)].nac),21);
                       strcpy(coupletmp->c_namepointedvar,(yyvsp[(3) - (3)].nac));
                       Save_Length((yyvsp[(3) - (3)].nac),22);
                       coupletmp->suiv = NULL;
                       (yyval.lc) = coupletmp;
                       pointedvar=1;
                       Add_UsedInSubroutine_Var_1((yyvsp[(1) - (3)].nac));
                    ;}
    break;

  case 375:
#line 2028 "fortran.y"
    {
                       coupletmp =(listcouple *)malloc(sizeof(listcouple));
                       strcpy(coupletmp->c_namevar,(yyvsp[(1) - (1)].nac));
                       Save_Length((yyvsp[(1) - (1)].nac),21);
                       strcpy(coupletmp->c_namepointedvar,"");
                       coupletmp->suiv = NULL;
                       (yyval.lc) = coupletmp;
                     ;}
    break;

  case 377:
#line 2039 "fortran.y"
    {
                         Add_SubroutineWhereAgrifUsed_1(subroutinename,
                                                        curmodulename);
                                                        inallocate = 0;
                     ;}
    break;

  case 378:
#line 2045 "fortran.y"
    {
                          Add_SubroutineWhereAgrifUsed_1(subroutinename,
                                                         curmodulename);
                                                         inallocate = 0;
                     ;}
    break;

  case 380:
#line 2052 "fortran.y"
    {
                       GlobalDeclaration = 0 ;
                       if ( firstpass == 0 &&
                            strcasecmp(subroutinename,"") )
                       {
                          if ( module_declar && insubroutinedeclare == 0 )
                          {
                              fclose(module_declar);
                          }
                       }
                       if ( couldaddvariable == 1 &&
                            strcasecmp(subroutinename,"") )
                       {
                       if ( inmodulemeet == 1 )
                       {
                         /* we are in a module                                */
                         if ( insubroutinedeclare == 1 )
                         {
                            /* it is like an end subroutine <name>            */
                            insubroutinedeclare = 0 ;
                            /*                                                */
                            pos_cur = setposcur();
                            closeandcallsubloopandincludeit_0(1);
                            functiondeclarationisdone = 0;
                         }
                         else
                         {
                            /* it is like an end module <name>                */
                            inmoduledeclare = 0 ;
                            inmodulemeet = 0 ;
                         }
                       }
                       else
                       {
                          insubroutinedeclare = 0;
                          /*                                                  */
                          pos_cur = setposcur();                        
                          closeandcallsubloopandincludeit_0(2);
                            functiondeclarationisdone = 0;
                          if ( firstpass == 0 )
                          {
                             if ( retour77 == 0 ) fprintf(paramout,"!\n");
                             else fprintf(paramout,"C\n");
                             fclose(paramout);
                           }
                        }
                      }
                      strcpy(subroutinename,"");
                    ;}
    break;

  case 381:
#line 2102 "fortran.y"
    {
                       if ( couldaddvariable == 1 )
                       {
                       insubroutinedeclare = 0;
                       /*                                                     */
                       pos_cur = setposcur();                      
                       closeandcallsubloopandincludeit_0(3);
                            functiondeclarationisdone = 0;
                      if ( firstpass == 0 )
                      {
                         if ( retour77 == 0 ) fprintf(paramout,"!\n");
                         else fprintf(paramout,"C\n");
                         fclose(paramout);
                      }
                      strcpy(subroutinename,"");
                      }
                    ;}
    break;

  case 382:
#line 2120 "fortran.y"
    {
                       if ( couldaddvariable == 1 &&
                            strcasecmp(subroutinename,"") )
                       {
                       insubroutinedeclare = 0;
                       /*                                                     */
                       pos_cur = setposcur();
                                             
                       closeandcallsubloopandincludeit_0(1);
                            functiondeclarationisdone = 0;
                      if ( firstpass == 0 )
                      {
                         if ( retour77 == 0 ) fprintf(paramout,"!\n");
                         else fprintf(paramout,"C\n");
                         fclose(paramout);
                      }
                      strcpy(subroutinename,"");
                      }
                    ;}
    break;

  case 383:
#line 2140 "fortran.y"
    {
                       if ( couldaddvariable == 1 )
                       {
                       insubroutinedeclare = 0;
                       /*                                                     */
                       pos_cur = setposcur();

                       closeandcallsubloopandincludeit_0(0);
                            functiondeclarationisdone = 0;
                      if ( firstpass == 0 )
                      {
                         if ( retour77 == 0 ) fprintf(paramout,"!\n");
                         else fprintf(paramout,"C\n");
                         fclose(paramout);
                      }
                      strcpy(subroutinename,"");
                      }
                    ;}
    break;

  case 384:
#line 2159 "fortran.y"
    {
                       if ( couldaddvariable == 1 )
                       {
                       /* if we never meet the contains keyword               */
                      Remove_Word_end_module_0(strlen((yyvsp[(2) - (2)].nac)));
                       if ( inmoduledeclare == 1 )
                       {
                          if ( aftercontainsdeclare == 0 )
                          {
                             Write_GlobalParameter_Declaration_0();
                             Write_NotGridDepend_Declaration_0();
                             Write_GlobalType_Declaration_0();
                             if ( module_declar_type )
                             {
                             strcpy (ligne, "\n#include \"Module_DeclarType_");
                             strcat (ligne, curmodulename);
                             strcat (ligne, ".h\"\n");
                             tofich(fortranout,ligne,1); 
                             }
                             Write_Alloc_Subroutine_For_End_0();
                          }
                       }
                                           
                       inmoduledeclare = 0 ;
                       inmodulemeet = 0 ;

                      Write_Word_end_module_0();
                      strcpy(curmodulename,"");
                      aftercontainsdeclare = 1;
                      if ( firstpass == 0 )
                      {
                         if ( module_declar && insubroutinedeclare == 0)
                         {
                           fclose(module_declar);
                         }
                         if ( module_declar_type && insubroutinedeclare == 0)
                         {
                          fclose(module_declar_type);
                          module_declar_type = 0;
                         }
                      }
                      GlobalDeclaration = 0 ;
                      }
                  ;}
    break;

  case 398:
#line 2217 "fortran.y"
    {
                      if (inmoduledeclare == 1 )
                      {
                         Remove_Word_Contains_0();
                         Write_GlobalParameter_Declaration_0();
                         Write_GlobalType_Declaration_0();
                             if ( module_declar_type)
                             {                         
                             strcpy (ligne, "\n#include \"Module_DeclarType_");
                             strcat (ligne, curmodulename);
                             strcat (ligne, ".h\"\n");
                             tofich(fortranout,ligne,1); 
                             }
                         Write_NotGridDepend_Declaration_0();
                         Write_Alloc_Subroutine_0();
                         inmoduledeclare = 0 ;
                         aftercontainsdeclare = 1;
                      }
                      else
                      {
                      incontainssubroutine = 1;
                      strcpy(previoussubroutinename,subroutinename);
                       if ( couldaddvariable == 1 )
                       {
                          if ( firstpass == 1 ) List_ContainsSubroutine =
                                                Addtolistnom(subroutinename,
                                                     List_ContainsSubroutine,0);
                          insubroutinedeclare = 0;
                          /*                                                  */

                          closeandcallsubloop_contains_0();
                            functiondeclarationisdone = 0;
                         if ( firstpass == 0 )
                         {
                            if ( retour77 == 0 ) fprintf(paramout,"!\n");
                            else fprintf(paramout,"C\n");
                            fclose(paramout);
                         }
                         }
                         strcpy(subroutinename,"");
                      }
                   ;}
    break;

  case 399:
#line 2261 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                       strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));
                       pos_endsubroutine = setposcur()-strlen((yyvsp[(1) - (1)].nac));
                       functiondeclarationisdone = 0;
                       }
                    ;}
    break;

  case 400:
#line 2271 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                       strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));
                       pos_endsubroutine = setposcur()-strlen((yyvsp[(1) - (1)].nac));
                       }
                    ;}
    break;

  case 401:
#line 2280 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                       strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));
                       pos_endsubroutine = setposcur()-strlen((yyvsp[(1) - (1)].nac));
                       }
                    ;}
    break;

  case 402:
#line 2289 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                       strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));
                       pos_endsubroutine = setposcur()-strlen((yyvsp[(1) - (1)].nac));
                       }
                    ;}
    break;

  case 414:
#line 2311 "fortran.y"
    {strcpy((yyval.nac),"");;}
    break;

  case 415:
#line 2312 "fortran.y"
    {strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));;}
    break;

  case 425:
#line 2332 "fortran.y"
    {
                          Add_SubroutineWhereAgrifUsed_1(subroutinename,
                                                        curmodulename);
                                                        inallocate = 0;
                     ;}
    break;

  case 426:
#line 2338 "fortran.y"
    {
                          Add_SubroutineWhereAgrifUsed_1(subroutinename,
                                                        curmodulename);
                                                        inallocate = 0;
                     ;}
    break;

  case 432:
#line 2349 "fortran.y"
    {if ( couldaddvariable == 1 ) created_dimensionlist = 0;;}
    break;

  case 433:
#line 2351 "fortran.y"
    {
                  created_dimensionlist = 1;
                  if  ( agrif_parentcall == 1 )
                  {
                      ModifyTheAgrifFunction_0((yyvsp[(3) - (4)].d)->dim.last);
                      agrif_parentcall =0;
                      fprintf(fortranout," = ");
                  }
              ;}
    break;

  case 434:
#line 2361 "fortran.y"
    {created_dimensionlist = 1;;}
    break;

  case 439:
#line 2369 "fortran.y"
    {
                      inagrifcallargument = 0 ;
                      incalldeclare=0;
                      if ( oldfortranout &&
                           !strcasecmp(meetagrifinitgrids,subroutinename) &&
                           firstpass == 0 &&
                           callmpiinit == 1)
                      {
                      /*   pos_end = setposcur();
                         RemoveWordSET_0(fortranout,pos_curcall,
                                               pos_end-pos_curcall);
                         fprintf(oldfortranout,"      Call MPI_Init (%s) \n"
                                                                   ,mpiinitvar);*/
                      }
                      if ( oldfortranout           &&
                           callagrifinitgrids == 1 &&
                           firstpass == 0 )
                      {
                         pos_end = setposcur();
                         RemoveWordSET_0(fortranout,pos_curcall,
                                               pos_end-pos_curcall);

                         strcpy(subofagrifinitgrids,subroutinename);
                      }
                      Instanciation_0(sameagrifname);
                   ;}
    break;

  case 444:
#line 2403 "fortran.y"
    {
                       if (!strcasecmp((yyvsp[(2) - (2)].nac),"MPI_Init") )
                       {
                          callmpiinit = 1;
                       }
                       else
                       {
                          callmpiinit = 0;
                       }
                       if (!strcasecmp((yyvsp[(2) - (2)].nac),"Agrif_Init_Grids") )
                       {
                          callagrifinitgrids = 1;
                          strcpy(meetagrifinitgrids,subroutinename);
                       }
                       else callagrifinitgrids = 0;
                       if ( !strcasecmp((yyvsp[(2) - (2)].nac),"Agrif_Open_File") )
                       {
                          Add_SubroutineWhereAgrifUsed_1(subroutinename,
                                                        curmodulename);
                       }
                       if ( Vartonumber((yyvsp[(2) - (2)].nac)) == 1 )
                       {
                          incalldeclare=1;
                          inagrifcallargument = 1 ;
                          Add_SubroutineWhereAgrifUsed_1(subroutinename,
                                                        curmodulename);
                       }
                    ;}
    break;

  case 445:
#line 2433 "fortran.y"
    {pos_curcall=setposcur()-4;;}
    break;

  case 448:
#line 2438 "fortran.y"
    {
                  if ( callmpiinit == 1 )
                  {
                     strcpy(mpiinitvar,(yyvsp[(1) - (1)].na));
                     if ( firstpass == 1 )
                     {
                        Add_UsedInSubroutine_Var_1 (mpiinitvar);
/*                        curvar=createvar($1,NULL);
                        curlistvar=insertvar(NULL,curvar);
                        List_Subr outineArgument_Var = AddListvarToListvar
                         (curlistvar,List_SubroutineAr gument_Var,1);*/
                     }
                  }
               ;}
    break;

  case 520:
#line 2562 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na));;}
    break;

  case 521:
#line 2563 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na));;}
    break;

  case 522:
#line 2564 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na));;}
    break;

  case 523:
#line 2567 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s,%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na));;}
    break;

  case 524:
#line 2569 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s,%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na));;}
    break;

  case 525:
#line 2571 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s,%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na));;}
    break;

  case 526:
#line 2573 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s,%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na));;}
    break;

  case 527:
#line 2575 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s,%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na));;}
    break;

  case 528:
#line 2577 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s,%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na));;}
    break;

  case 529:
#line 2578 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na));;}
    break;

  case 530:
#line 2579 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na));;}
    break;

  case 531:
#line 2582 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na));;}
    break;

  case 532:
#line 2584 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na)," (%s)",(yyvsp[(2) - (3)].na));;}
    break;

  case 533:
#line 2586 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"(%s,%s)",(yyvsp[(2) - (5)].na),(yyvsp[(4) - (5)].na));;}
    break;

  case 534:
#line 2588 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"(%s,%s)",(yyvsp[(2) - (5)].na),(yyvsp[(4) - (5)].na));;}
    break;

  case 535:
#line 2590 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"(%s,%s)",(yyvsp[(2) - (5)].na),(yyvsp[(4) - (5)].na));;}
    break;

  case 536:
#line 2594 "fortran.y"
    {if ( couldaddvariable == 1 )
                                              sprintf((yyval.na),"%s=%s,%s)",(yyvsp[(1) - (5)].nac),(yyvsp[(3) - (5)].na),(yyvsp[(5) - (5)].na));;}
    break;

  case 537:
#line 2597 "fortran.y"
    {if ( couldaddvariable == 1 )
                                        sprintf((yyval.na),"%s=%s,%s,%s)",(yyvsp[(1) - (7)].nac),(yyvsp[(3) - (7)].na),(yyvsp[(5) - (7)].na),(yyvsp[(7) - (7)].na));;}
    break;

  case 544:
#line 2613 "fortran.y"
    {Add_Allocate_Var_1((yyvsp[(1) - (1)].nac),curmodulename);;}
    break;

  case 547:
#line 2618 "fortran.y"
    {Add_Allocate_Var_1((yyvsp[(1) - (4)].nac),curmodulename);;}
    break;

  case 555:
#line 2641 "fortran.y"
    {strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));;}
    break;


/* Line 1267 of yacc.c.  */
#line 6522 "fortran.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 2643 "fortran.y"


void processfortran(char *fichier_entree)
{
   extern FILE *fortranin;
   extern FILE *fortranout;
   char nomfile[LONG_C];
   int c;
   int confirmyes;

/*   fortrandebug = 1;*/
   if ( mark == 1 ) printf("Firstpass == %d \n",firstpass);
/******************************************************************************/
/*  1-  Open input and output files                                           */
/******************************************************************************/
   strcpy(nomfile,commondirin);
   strcat(nomfile,"/");
   strcat(nomfile,fichier_entree);
   fortranin=fopen( nomfile,"r");
   if (! fortranin)
   {
      printf("Error : File %s does not exist\n",nomfile);
      exit(1);
   }

   strcpy(curfile,nomfile);
   strcpy(nomfile,commondirout);
   strcat(nomfile,"/");
   strcat(nomfile,fichier_entree);
   strcpy(nomfileoutput,nomfile);
   Save_Length(nomfileoutput,31);
   if (firstpass == 1)
   {
      if (checkexistcommon == 1)
      {
         if (fopen(nomfile,"r"))
         {
            printf("Warning : file %s already exist\n",nomfile);
            confirmyes = 0;
            while (confirmyes==0)
            {
               printf("Override file %s ? [Y/N]\n",nomfile);
               c=getchar();
               getchar();
               if (c==79 || c==110)
               {
                  printf("We stop\n");
                  exit(1);
               }
               if (c==89 || c==121)
               {
                  confirmyes=1;
               }
            }
         }
      }
   }

/******************************************************************************/
/*  2-  Variables initialization                                              */
/******************************************************************************/

   line_num_fortran_common=1;
   line_num_fortran=1;
   PublicDeclare = 0;
   PrivateDeclare = 0;
   ExternalDeclare = 0;
   SaveDeclare = 0;
   pointerdeclare = 0;
   optionaldeclare = 0;
   incalldeclare = 0;
   VarType = 0;
   VarTypepar = 0;
   Allocatabledeclare = 0 ;
   Targetdeclare = 0 ;
   strcpy(NamePrecision," ");
   VariableIsParameter =  0 ;
   strcpy(NamePrecision,"");
   c_star = 0 ;
   functiondeclarationisdone = 0;
   insubroutinedeclare = 0 ;
   ininterfacedeclare = 0 ;   
   strcpy(subroutinename," ");
   isrecursive = 0;
   strcpy(InitialValueGiven," ");
   strcpy(EmptyChar," ");
   inmoduledeclare = 0;
   incontainssubroutine = 0;
   module_declar_type = 0;
   GlobalDeclarationType = 0;
   colnum=0;
   incom=0;
   couldaddvariable=1;
   afterpercent = 0;
   aftercontainsdeclare = 1;
   strcpy(nameinttypename,"");
   /* Name of the file without format                                         */
   tmp = strchr(fichier_entree, '.');
   strncpy(curfilename,fichier_entree,strlen(fichier_entree)-strlen(tmp));
   Save_Length(curfilename,30);
/******************************************************************************/
/*  3-  Parsing of the input file (1 time)                                    */
/******************************************************************************/
   if (firstpass == 0 )
   {
      fortranout=fopen(nomfileoutput,"w");

/*      NewModule_Creation_0();*/
   }

   fortranparse();

   if (firstpass == 0 )
   {
   NewModule_Creation_0();
   }
   
   strcpy(curfile,mainfile);

   if (firstpass == 0 )
   {
   fclose(fortranout);
   }
}

#line 2 "fortran.yy.c"

#line 4 "fortran.yy.c"

#define  YY_INT_ALIGNED short int

/* A lexical scanner generated by flex */

#define yy_create_buffer fortran_create_buffer
#define yy_delete_buffer fortran_delete_buffer
#define yy_flex_debug fortran_flex_debug
#define yy_init_buffer fortran_init_buffer
#define yy_flush_buffer fortran_flush_buffer
#define yy_load_buffer_state fortran_load_buffer_state
#define yy_switch_to_buffer fortran_switch_to_buffer
#define yyin fortranin
#define yyleng fortranleng
#define yylex fortranlex
#define yylineno fortranlineno
#define yyout fortranout
#define yyrestart fortranrestart
#define yytext fortrantext
#define yywrap fortranwrap
#define yyalloc fortranalloc
#define yyrealloc fortranrealloc
#define yyfree fortranfree

#define FLEX_SCANNER
#define YY_FLEX_MAJOR_VERSION 2
#define YY_FLEX_MINOR_VERSION 5
#define YY_FLEX_SUBMINOR_VERSION 35
#if YY_FLEX_SUBMINOR_VERSION > 0
#define FLEX_BETA
#endif

/* First, we deal with  platform-specific or compiler-specific issues. */

/* begin standard C headers. */
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>

/* end standard C headers. */

/* flex integer type definitions */

#ifndef FLEXINT_H
#define FLEXINT_H

/* C99 systems have <inttypes.h>. Non-C99 systems may or may not. */

#if defined (__STDC_VERSION__) && __STDC_VERSION__ >= 199901L

/* C99 says to define __STDC_LIMIT_MACROS before including stdint.h,
 * if you want the limit (max/min) macros for int types. 
 */
#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS 1
#endif

#include <inttypes.h>
typedef int8_t flex_int8_t;
typedef uint8_t flex_uint8_t;
typedef int16_t flex_int16_t;
typedef uint16_t flex_uint16_t;
typedef int32_t flex_int32_t;
typedef uint32_t flex_uint32_t;
#else
typedef signed char flex_int8_t;
typedef short int flex_int16_t;
typedef int flex_int32_t;
typedef unsigned char flex_uint8_t; 
typedef unsigned short int flex_uint16_t;
typedef unsigned int flex_uint32_t;
#endif /* ! C99 */

/* Limits of integral types. */
#ifndef INT8_MIN
#define INT8_MIN               (-128)
#endif
#ifndef INT16_MIN
#define INT16_MIN              (-32767-1)
#endif
#ifndef INT32_MIN
#define INT32_MIN              (-2147483647-1)
#endif
#ifndef INT8_MAX
#define INT8_MAX               (127)
#endif
#ifndef INT16_MAX
#define INT16_MAX              (32767)
#endif
#ifndef INT32_MAX
#define INT32_MAX              (2147483647)
#endif
#ifndef UINT8_MAX
#define UINT8_MAX              (255U)
#endif
#ifndef UINT16_MAX
#define UINT16_MAX             (65535U)
#endif
#ifndef UINT32_MAX
#define UINT32_MAX             (4294967295U)
#endif

#endif /* ! FLEXINT_H */

#ifdef __cplusplus

/* The "const" storage-class-modifier is valid. */
#define YY_USE_CONST

#else	/* ! __cplusplus */

/* C99 requires __STDC__ to be defined as 1. */
#if defined (__STDC__)

#define YY_USE_CONST

#endif	/* defined (__STDC__) */
#endif	/* ! __cplusplus */

#ifdef YY_USE_CONST
#define yyconst const
#else
#define yyconst
#endif

/* Returned upon end-of-file. */
#define YY_NULL 0

/* Promotes a possibly negative, possibly signed char to an unsigned
 * integer for use as an array index.  If the signed char is negative,
 * we want to instead treat it as an 8-bit unsigned char, hence the
 * double cast.
 */
#define YY_SC_TO_UI(c) ((unsigned int) (unsigned char) c)

/* Enter a start condition.  This macro really ought to take a parameter,
 * but we do it the disgusting crufty way forced on us by the ()-less
 * definition of BEGIN.
 */
#define BEGIN (yy_start) = 1 + 2 *

/* Translate the current start state into a value that can be later handed
 * to BEGIN to return to the state.  The YYSTATE alias is for lex
 * compatibility.
 */
#define YY_START (((yy_start) - 1) / 2)
#define YYSTATE YY_START

/* Action number for EOF rule of a given start state. */
#define YY_STATE_EOF(state) (YY_END_OF_BUFFER + state + 1)

/* Special action meaning "start processing a new file". */
#define YY_NEW_FILE fortranrestart(fortranin  )

#define YY_END_OF_BUFFER_CHAR 0

/* Size of default input buffer. */
#ifndef YY_BUF_SIZE
#define YY_BUF_SIZE 16384
#endif

/* The state buf must be large enough to hold one state per character in the main buffer.
 */
#define YY_STATE_BUF_SIZE   ((YY_BUF_SIZE + 2) * sizeof(yy_state_type))

#ifndef YY_TYPEDEF_YY_BUFFER_STATE
#define YY_TYPEDEF_YY_BUFFER_STATE
typedef struct yy_buffer_state *YY_BUFFER_STATE;
#endif

#ifndef YY_TYPEDEF_YY_SIZE_T
#define YY_TYPEDEF_YY_SIZE_T
typedef size_t yy_size_t;
#endif

extern yy_size_t fortranleng;

extern FILE *fortranin, *fortranout;

#define EOB_ACT_CONTINUE_SCAN 0
#define EOB_ACT_END_OF_FILE 1
#define EOB_ACT_LAST_MATCH 2

    #define YY_LESS_LINENO(n)
    
/* Return all but the first "n" matched characters back to the input stream. */
#define yyless(n) \
	do \
		{ \
		/* Undo effects of setting up fortrantext. */ \
        int yyless_macro_arg = (n); \
        YY_LESS_LINENO(yyless_macro_arg);\
		*yy_cp = (yy_hold_char); \
		YY_RESTORE_YY_MORE_OFFSET \
		(yy_c_buf_p) = yy_cp = yy_bp + yyless_macro_arg - YY_MORE_ADJ; \
		YY_DO_BEFORE_ACTION; /* set up fortrantext again */ \
		} \
	while ( 0 )

#define unput(c) yyunput( c, (yytext_ptr)  )

#ifndef YY_STRUCT_YY_BUFFER_STATE
#define YY_STRUCT_YY_BUFFER_STATE
struct yy_buffer_state
	{
	FILE *yy_input_file;

	char *yy_ch_buf;		/* input buffer */
	char *yy_buf_pos;		/* current position in input buffer */

	/* Size of input buffer in bytes, not including room for EOB
	 * characters.
	 */
	yy_size_t yy_buf_size;

	/* Number of characters read into yy_ch_buf, not including EOB
	 * characters.
	 */
	yy_size_t yy_n_chars;

	/* Whether we "own" the buffer - i.e., we know we created it,
	 * and can realloc() it to grow it, and should free() it to
	 * delete it.
	 */
	int yy_is_our_buffer;

	/* Whether this is an "interactive" input source; if so, and
	 * if we're using stdio for input, then we want to use getc()
	 * instead of fread(), to make sure we stop fetching input after
	 * each newline.
	 */
	int yy_is_interactive;

	/* Whether we're considered to be at the beginning of a line.
	 * If so, '^' rules will be active on the next match, otherwise
	 * not.
	 */
	int yy_at_bol;

    int yy_bs_lineno; /**< The line count. */
    int yy_bs_column; /**< The column count. */
    
	/* Whether to try to fill the input buffer when we reach the
	 * end of it.
	 */
	int yy_fill_buffer;

	int yy_buffer_status;

#define YY_BUFFER_NEW 0
#define YY_BUFFER_NORMAL 1
	/* When an EOF's been seen but there's still some text to process
	 * then we mark the buffer as YY_EOF_PENDING, to indicate that we
	 * shouldn't try reading from the input source any more.  We might
	 * still have a bunch of tokens to match, though, because of
	 * possible backing-up.
	 *
	 * When we actually see the EOF, we change the status to "new"
	 * (via fortranrestart()), so that the user can continue scanning by
	 * just pointing fortranin at a new input file.
	 */
#define YY_BUFFER_EOF_PENDING 2

	};
#endif /* !YY_STRUCT_YY_BUFFER_STATE */

/* Stack of input buffers. */
static size_t yy_buffer_stack_top = 0; /**< index of top of stack. */
static size_t yy_buffer_stack_max = 0; /**< capacity of stack. */
static YY_BUFFER_STATE * yy_buffer_stack = 0; /**< Stack as an array. */

/* We provide macros for accessing buffer states in case in the
 * future we want to put the buffer states in a more general
 * "scanner state".
 *
 * Returns the top of the stack, or NULL.
 */
#define YY_CURRENT_BUFFER ( (yy_buffer_stack) \
                          ? (yy_buffer_stack)[(yy_buffer_stack_top)] \
                          : NULL)

/* Same as previous macro, but useful when we know that the buffer stack is not
 * NULL or when we need an lvalue. For internal use only.
 */
#define YY_CURRENT_BUFFER_LVALUE (yy_buffer_stack)[(yy_buffer_stack_top)]

/* yy_hold_char holds the character lost when fortrantext is formed. */
static char yy_hold_char;
static yy_size_t yy_n_chars;		/* number of characters read into yy_ch_buf */
yy_size_t fortranleng;

/* Points to current character in buffer. */
static char *yy_c_buf_p = (char *) 0;
static int yy_init = 0;		/* whether we need to initialize */
static int yy_start = 0;	/* start state number */

/* Flag which is used to allow fortranwrap()'s to do buffer switches
 * instead of setting up a fresh fortranin.  A bit of a hack ...
 */
static int yy_did_buffer_switch_on_eof;

void fortranrestart (FILE *input_file  );
void fortran_switch_to_buffer (YY_BUFFER_STATE new_buffer  );
YY_BUFFER_STATE fortran_create_buffer (FILE *file,int size  );
void fortran_delete_buffer (YY_BUFFER_STATE b  );
void fortran_flush_buffer (YY_BUFFER_STATE b  );
void fortranpush_buffer_state (YY_BUFFER_STATE new_buffer  );
void fortranpop_buffer_state (void );

static void fortranensure_buffer_stack (void );
static void fortran_load_buffer_state (void );
static void fortran_init_buffer (YY_BUFFER_STATE b,FILE *file  );

#define YY_FLUSH_BUFFER fortran_flush_buffer(YY_CURRENT_BUFFER )

YY_BUFFER_STATE fortran_scan_buffer (char *base,yy_size_t size  );
YY_BUFFER_STATE fortran_scan_string (yyconst char *yy_str  );
YY_BUFFER_STATE fortran_scan_bytes (yyconst char *bytes,yy_size_t len  );

void *fortranalloc (yy_size_t  );
void *fortranrealloc (void *,yy_size_t  );
void fortranfree (void *  );

#define yy_new_buffer fortran_create_buffer

#define yy_set_interactive(is_interactive) \
	{ \
	if ( ! YY_CURRENT_BUFFER ){ \
        fortranensure_buffer_stack (); \
		YY_CURRENT_BUFFER_LVALUE =    \
            fortran_create_buffer(fortranin,YY_BUF_SIZE ); \
	} \
	YY_CURRENT_BUFFER_LVALUE->yy_is_interactive = is_interactive; \
	}

#define yy_set_bol(at_bol) \
	{ \
	if ( ! YY_CURRENT_BUFFER ){\
        fortranensure_buffer_stack (); \
		YY_CURRENT_BUFFER_LVALUE =    \
            fortran_create_buffer(fortranin,YY_BUF_SIZE ); \
	} \
	YY_CURRENT_BUFFER_LVALUE->yy_at_bol = at_bol; \
	}

#define YY_AT_BOL() (YY_CURRENT_BUFFER_LVALUE->yy_at_bol)

/* Begin user sect3 */

typedef unsigned char YY_CHAR;

FILE *fortranin = (FILE *) 0, *fortranout = (FILE *) 0;

typedef int yy_state_type;

extern int fortranlineno;

int fortranlineno = 1;

extern char *fortrantext;
#define yytext_ptr fortrantext

static yy_state_type yy_get_previous_state (void );
static yy_state_type yy_try_NUL_trans (yy_state_type current_state  );
static int yy_get_next_buffer (void );
static void yy_fatal_error (yyconst char msg[]  );

/* Done after the current pattern has been matched and before the
 * corresponding action - sets up fortrantext.
 */
#define YY_DO_BEFORE_ACTION \
	(yytext_ptr) = yy_bp; \
	fortranleng = (size_t) (yy_cp - yy_bp); \
	(yy_hold_char) = *yy_cp; \
	*yy_cp = '\0'; \
	(yy_c_buf_p) = yy_cp;

#define YY_NUM_RULES 176
#define YY_END_OF_BUFFER 177
/* This struct is not used in this scanner,
   but its presence is necessary. */
struct yy_trans_info
	{
	flex_int32_t yy_verify;
	flex_int32_t yy_nxt;
	};
static yyconst flex_int16_t yy_accept[1162] =
    {   0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
        0,    0,  177,  176,  165,  161,  164,  175,  152,  151,
      155,  167,  152,  154,  154,  154,  157,  153,  137,  150,
      156,  159,  158,  160,  145,  145,  145,  145,  145,  145,
      145,  145,  145,  145,  145,  145,  145,  145,  145,  145,
      145,  145,  165,  161,  164,  175,  150,  145,  145,  145,
      145,  145,  145,  176,  176,  173,  176,  176,  176,  161,
      161,  154,  145,    0,    0,  165,    0,  166,    0,  164,
      175,  175,  175,    0,  141,    0,    0,  167,  167,  167,
      167,    0,    0,    0,  140,    0,    0,  132,   26,    0,

      146,    0,    0,    0,    0,    0,    0,    0,  133,    0,
      150,   25,    0,  145,  145,  145,  145,  145,  145,  145,
      145,  145,  145,  145,  145,  145,  145,  145,  145,  145,
       43,  145,  145,  145,  145,  145,  145,  145,  145,  145,
      145,  145,  145,  145,   85,  145,  145,  145,  145,  145,
      145,  145,  145,  145,  145,  145,  145,  145,  145,  145,
      145,  145,  145,  145,  145,  145,  145,  145,  145,  145,
      145,  145,  165,  163,    0,  163,    0,    0,    0,    0,
        0,    0,  166,  162,  163,    0,  175,  174,  175,  175,
      175,  163,  150,    4,  145,  145,  145,  145,   85,  145,

      145,    0,  173,    0,    0,    0,    0,    0,    0,    0,
      169,   26,    0,    0,    4,    0,  145,  145,  145,  145,
      145,  145,    0,    0,    0,  175,  175,    0,    0,  167,
      167,    0,    0,    0,    0,  139,    0,    0,    0,    0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
        0,  149,  146,    0,    0,    0,  142,    0,  145,  145,
      145,  145,  145,  145,  145,  145,  145,  145,  145,  145,
      145,  115,  145,  145,  145,    0,  145,  145,  145,  145,
      145,   16,  145,  145,  145,  114,  145,  145,  145,  145,
      145,    0,  145,    0,   96,  145,  145,  145,  145,  145,

      121,  145,  145,  126,  145,  145,  145,  145,  145,  145,
      145,   89,  145,  145,  145,  145,  145,  145,  145,  145,
      145,  145,  145,  145,  145,  145,  118,  145,  145,  145,
      145,  145,  122,  145,  145,  145,  145,  145,  165,  163,
        0,  166,    0,    0,    0,    0,    0,    0,    0,    0,
      163,    0,  163,  175,  175,  175,  150,    4,    4,    4,
        4,  145,  145,  145,  145,  145,  145,  145,    0,    0,
        0,    0,  170,    0,    0,  169,    0,    0,    4,    4,
        4,    4,  142,    0,  145,  145,    0,  145,  145,  145,
      145,    0,    0,    0,  175,  175,    0,    0,  167,  167,

        0,    0,    0,    0,  147,    0,  146,    0,  148,    0,
       28,    0,   30,   29,   32,   31,   34,    0,    0,   36,
        0,  146,    0,  147,    0,  146,    0,  148,    0,  142,
        0,  125,  117,  145,  145,  120,  123,  145,  145,   22,
      145,  145,  145,  145,  145,  116,  145,  145,  145,    0,
      145,  145,  145,   94,    0,  108,  145,  145,  145,  145,
      145,  145,  145,  145,  145,    0,  109,  145,  145,  145,
      145,  145,  145,  145,    0,   88,  145,  145,  145,  145,
      145,  145,  145,    0,   98,  145,  145,    0,  111,  145,
      145,  145,  145,  112,   21,  145,   60,   74,  145,  145,

      145,  145,  145,  145,  145,  145,   79,   44,  145,  145,
      145,  145,   69,  145,  127,  119,  145,   72,   54,  145,
        0,   97,   99,  145,   92,  101,  145,  145,  165,  163,
      166,    0,    0,    0,    0,    0,    0,    0,  163,    0,
      163,  175,  175,  175,  150,    4,    4,  145,  145,  145,
      145,  145,  145,   18,    0,    0,    0,    0,    0,  170,
        0,    0,    4,    4,    0,  142,    0,  145,  145,  145,
      145,    0,    0,    0,  175,  175,    0,    0,  167,  167,
        0,    0,   38,   27,    0,   35,   37,    0,  142,    0,
      142,  145,  145,  145,  145,  145,   49,  145,  145,  145,

      124,  145,  145,    0,  145,  145,  145,    0,  145,  145,
        0,    0,    0,    0,    0,    0,    0,    0,   42,  145,
       95,  145,  145,  145,  145,  145,  145,  145,  145,   76,
       76,   76,   76,  145,    0,  107,  113,  145,  145,   88,
      145,  145,   90,  145,  145,  145,  145,  145,  145,  145,
      145,  145,  145,  145,  145,  145,  145,   52,  145,   77,
      145,  145,  145,    0,  145,  145,  145,  145,  145,  102,
      145,  145,   55,   81,  165,  163,  166,    0,    0,    0,
        0,    0,    0,  163,    0,  163,  175,  175,  175,  150,
        4,    3,    0,  104,  145,  145,   86,  145,  145,   70,

       71,   70,    0,    0,    0,    0,    4,    3,    0,  142,
        0,  142,  145,   49,  145,  124,    0,   23,    0,  175,
       23,    0,   23,   23,  167,   23,    0,   23,   23,   23,
       33,  145,  145,   23,   23,   23,  145,  145,   63,  145,
      145,  145,  145,    0,  145,  145,  138,    0,    0,   93,
      145,   42,    0,   95,    0,    0,    0,    0,    0,    0,
      145,  145,  145,  145,  145,  145,  145,  145,    0,  110,
      145,  145,  145,  145,  145,  145,  145,   66,  145,  145,
      129,  100,  128,  130,   39,  145,  145,  145,  145,  145,
      145,  145,   83,    0,  145,   11,   75,   19,  145,  145,

       82,  165,  163,  166,    0,    0,    0,    0,  163,  175,
      175,   23,    4,    3,    3,  145,  145,  145,  145,    0,
        0,    0,   23,    4,    3,    3,  145,   23,   23,   23,
       24,    0,  168,   24,   24,   24,   24,   24,   24,   24,
       24,   24,   24,  145,  145,  145,   58,  145,  145,  145,
        0,    0,  145,  145,   40,   93,    0,  145,    0,    0,
        0,    0,    0,    0,    0,  145,  145,  145,  145,  145,
       73,  145,  145,  145,    0,    0,  145,  145,   17,   50,
       45,  145,   46,    0,  145,  145,    9,  145,  145,   67,
       84,    7,    0,    0,  145,    0,  145,  145,    0,    0,

        0,    0,  175,   24,    4,  145,  145,   64,  145,    0,
        0,   24,    4,   24,   24,   24,  145,    8,  145,  145,
       87,  145,  145,   40,    0,    0,  145,  145,    0,  145,
        0,    0,    0,    0,    0,   73,    0,  145,  145,  145,
      145,  145,   56,  145,   65,    0,    0,    0,    0,  134,
       12,   20,  145,    0,  145,   80,   68,  145,    0,  145,
        0,  145,  145,    0,    0,    0,  175,    4,  145,   59,
      145,    0,    0,    4,  145,  131,   47,  145,   51,    0,
        0,  145,  145,    0,   57,    0,    0,    0,    0,    0,
       56,  145,   41,  145,  106,  145,  145,    0,    0,    0,

        0,    0,  134,   91,    0,  145,   61,    0,   62,    0,
      145,  145,    0,   59,    0,  175,    4,    4,  145,  143,
        0,    0,    4,    4,  145,   10,    0,    0,  145,  145,
       57,    0,   41,    0,  106,    0,  145,   14,  145,  145,
        0,    0,    0,  145,    0,    0,  103,    6,    0,  143,
      175,    4,    4,  145,    0,    0,    4,    4,   48,    0,
        0,  145,  145,    0,   14,    0,   15,  145,   53,    0,
        0,    0,  145,    0,  103,    0,  175,    4,    2,  105,
        0,    0,    4,    2,    0,    0,  145,  145,   15,    0,
      145,    0,    0,    0,  145,    0,  105,  175,    4,    0,

        0,    4,    0,    0,  136,  145,    0,   13,    0,    0,
        0,  145,    0,  175,    1,    0,    0,  144,    1,  136,
        0,  145,   13,    0,    0,    0,    0,  145,    0,  175,
        0,    0,  135,    0,    0,    0,   78,    5,  175,    0,
      135,   78,  175,    0,  175,    0,  175,    0,  175,    0,
      175,    0,  175,  171,    0,    0,    0,    0,    0,  172,
        0
    } ;

static yyconst flex_int32_t yy_ec[256] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    2,    3,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    4,    5,    6,    7,    8,    9,   10,   11,   12,
       13,   14,   15,   16,   17,   18,   19,   20,   21,   22,
       23,   24,   25,   26,   27,   28,   29,   30,   31,   32,
       33,   34,    1,    1,   35,   36,   37,   38,   39,   40,
       41,   42,   43,   44,   45,   46,   47,   48,   49,   50,
       51,   52,   53,   54,   55,   56,   57,   58,   59,   60,
       61,    1,   62,    1,   63,    1,   64,   65,   66,   67,

       68,   69,   70,   71,   72,   44,   73,   74,   75,   76,
       77,   78,   79,   80,   81,   82,   83,   84,   85,   86,
       87,   88,   89,   89,   89,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,

        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1
    } ;

static yyconst flex_int32_t yy_meta[90] =
    {   0,
        1,    2,    3,    2,    1,    4,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    5,    1,    6,
        6,    6,    6,    6,    6,    6,    6,    6,    6,    1,
        1,    1,    1,    1,    7,    8,    8,    9,    9,    5,
       10,   11,    8,    8,    8,   10,   10,   10,    7,    8,
        9,   10,   10,    5,   10,    8,   12,    8,    8,    8,
        1,    1,    8,    7,    8,    8,    9,    9,    5,   10,
       11,    8,    8,   10,   10,   10,    7,    8,    9,   10,
       10,    5,   10,    8,   12,    8,    8,    8,    4
    } ;

static yyconst flex_int16_t yy_base[1215] =
    {   0,
        0,   88,    0,    0,    0,    0, 1215,   93, 1196,   91,
        0,    0, 1179,   64,   97,  117,  132,   87,  124,   98,
       99,  142,  173,  154,  108,  135,  134,  254,  172,  179,
      175,  178,  219,  180,  323,  388,  384,  318,  386,  319,
      186,  128,  250,  256,  392,  404,  438,  428,  204,  455,
      432,  465,  544,  120,  537,  548,  627,  624,  622,  544,
      644,  658,  251, 9159, 1175, 9159,  267,   99,  162,  569,
      572,  620,  745,  164,  107,  262,  307,  335,  376,  522,
        0,  167,  157, 1162, 9159,  278,  321, 1160, 1118,  372,
      304,  532,  593,  567, 9159,  633,  832, 9159, 9159,  911,

      798,  318,  319,  450,  799,  552,  320,  354, 9159,  978,
      834, 9159,  865,  860,  862,  866,  914,  863,  973,  980,
      974, 1013, 1024, 1045,  869, 1049, 1075, 1095, 1098, 1101,
     1182, 1177, 1161, 1181, 1214, 1212, 1237, 1239, 1235, 1270,
     1290, 1316, 1325, 1293, 1346, 1323, 1366, 1368, 1391, 1399,
     1403, 1429, 1430, 1455, 1452, 1478, 1498, 1482, 1523, 1521,
     1554, 1548, 1579, 1586, 1606, 1629, 1649, 1675, 1681, 1698,
     1707, 1729,  871, 1114, 1109,  140, 1023,  187,  372,  403,
      553,  401, 1095,  565, 1792, 1818, 1083, 9159,  643,  669,
      589, 1844, 1870, 1580, 1865, 1867, 1868, 1905, 1926, 1922,

     1925, 1078, 9159,  837,  437,  677,  249,  660, 1072,  712,
     9159, 1062,  844,  625, 1874, 1993, 2022, 1042,  864,  473,
      904,  621,  506,  651,  721,  798,  791,  890,  912,  936,
      907,  725, 1029, 1143,  729, 9159, 1114, 1179, 2097, 2112,
     2127,  229,  937, 1033, 1012, 1006,  994,  943,  883,  974,
      891, 9159, 1476, 2142, 2157, 2172, 2182, 2202, 2178, 2180,
     2200, 2203, 2223, 2243, 2246, 2244, 2276, 2269, 2299, 2307,
     2301, 2324, 2344, 2351, 2377,  901, 1018, 2374, 2394, 2397,
     2414, 2495, 2434, 2440, 2465, 2472, 2518, 2521, 2515, 2548,
     2556, 1138, 2564,  968, 9159, 2581, 2597, 2604, 2627, 2649,

     2650, 2690, 2693, 2686, 2726, 2738, 2746, 2747, 2763, 2783,
     2799, 2800, 2803, 2820, 2840, 2843, 2870, 2891, 2895, 2903,
     2926, 2928, 2935, 2961, 2981, 2958, 2984, 2978, 3004, 3011,
     3027, 3052, 3048, 3059, 3085, 3068, 3105, 3111,  196,  203,
      976,  383,  974,  988, 1018, 1014, 1025, 1029, 1042,  253,
     3174, 3200, 3226, 1067, 1182, 1172, 3252,    0,  418,  288,
     1096, 3228, 3229, 3231, 3251, 3267, 3287, 3290, 1188, 1566,
      973,  740, 9159,  938,  866, 9159, 1268, 1178,  931, 1170,
     1241, 1299, 3354, 3381,    0,  903, 3408, 1148, 1116, 1177,
     1322, 1145,  410, 1415, 1235,  460, 1292, 1218, 1323, 1022,

      963, 1520, 1662, 3418, 3430, 3440, 3450, 3460, 3470,  889,
     9159,  865, 9159, 9159, 9159, 9159, 9159, 1239,  853, 9159,
      846, 2472, 3480, 3490, 3500, 3510, 3520, 3530, 3545, 3555,
     3575, 9159, 3289, 3553, 3573, 3551, 3574, 3590, 3596, 3612,
     3628, 3634, 3635, 3655, 3671, 3651, 3687, 3688, 3696,  459,
     3728, 3736, 3740, 3784, 3868, 9159, 3780, 3776, 3807, 3811,
     3850, 3912, 3872, 3920, 3921, 1351, 9159, 3828, 4005, 4060,
     4095, 3904, 4063, 4069, 1258, 4096, 4104, 4126, 4134, 4156,
     4165, 4196, 4205, 1409, 9159, 4208, 4197, 1476, 9159, 4235,
     4241, 4257, 4261, 4273, 4284, 4293, 4305, 4309, 4332, 4336,

     4353, 4316, 4359, 4362, 4384, 4407, 4385, 4416, 4423, 4448,
     4455, 4456, 4486, 4494, 4488, 4511, 4527, 4519, 4531, 4554,
     1498, 9159, 4552, 4575, 4584, 4587, 4607, 4610,  493,  529,
      551,  476, 1287,  816,  953, 1119, 1293,  841, 4678, 4704,
     4730, 1607, 1346, 1458, 4756, 1362, 1397, 4733, 4732, 4759,
     4627, 4768, 4817,  733, 1619, 1594, 1406,  728, 1537, 9159,
     1571, 1651, 1625, 1613, 4839, 4867, 4894, 1627, 1658, 1686,
     1714, 1394,    0, 1756, 1574,    0, 1668,  724, 1709,  724,
     1763, 1775, 9159, 9159,  708, 9159, 9159, 4904, 4916, 4926,
     4936, 4771, 4931, 5013, 4934, 5068, 5069, 5071, 5075, 5072,

     5120, 5121, 5123, 1725, 5127, 5153, 5176, 1832, 5175, 5179,
     1458, 1493, 1739, 1516, 1519, 1873, 1592, 1739, 5201, 5224,
     5202, 5241, 5240, 5257, 5278, 5280, 5301, 5284, 5352, 9159,
     5351, 5346, 5349, 5353, 1802, 9159, 5355, 5385, 5401, 9159,
     5405, 5408, 5412, 5428, 5449, 5451, 5458, 5460, 5495, 5501,
     5531, 5527, 5539, 5571, 5564, 5575, 5591, 5602, 5623, 5614,
     5634, 5635, 5660,  693, 5666, 5667, 5692, 5703, 5715, 9159,
     5723, 5719, 5740, 5746,  846, 1103, 1234, 1788, 1768, 1386,
     1788, 1615, 1847, 1854,  236,  590, 1781, 1913,  715, 5809,
     1790, 1645, 2264, 9159, 5785, 5805, 1690, 5806, 5807, 2344,

     9159, 1951, 1858, 1924, 2256,  679, 1930, 1879, 5870, 5880,
     5907, 5917, 1811,  658, 5946,  624,    0, 9159, 6034,    0,
        0,  613,  609, 9159,  604,  586, 2245, 2364, 2389, 9159,
     9159, 6063, 6118, 6139, 6138, 6134, 6135, 6141, 6161, 6173,
     6184, 6209, 6217, 1654, 6260, 6248, 6216, 1844, 1864, 6239,
     6280, 9159, 1689, 9159, 1930, 1750, 1763, 1934, 1933, 2077,
     6300, 6292, 6317, 6337, 6338, 6344, 6369, 6376, 2212, 9159,
     6360, 6413, 6392, 6412, 6424, 6433, 6456, 6435, 6465, 6476,
     6488, 6497, 6508, 6509, 6552, 6529, 6540, 6549, 6560, 6585,
     6593, 6605, 6628, 2392, 6636, 6637, 6648, 6659, 6694, 6685,

     6686, 6775, 2286,  518, 2092, 2103, 1938, 2148, 2420, 2303,
      472,  442, 2196,    0, 1803, 6692, 6717, 6768, 6756, 2424,
     2334,  426,  422, 2432,  378, 1883, 6858, 6945, 6974,  337,
     9159, 1922, 9159,    0,  328, 9159,  314, 2481, 2510, 9159,
     6929, 6757, 7029, 7031, 7037, 7040, 7033, 7077, 7070, 7086,
     2264, 2842, 7093, 7097, 7118, 9159, 2372, 7129, 2384, 2060,
     2411, 2418, 2078, 2421, 2139, 7138, 7149, 7170, 7179, 7195,
     7201, 7227, 7234, 7236, 2483, 3160, 7259, 7257, 7279, 7280,
     7282, 7302, 7318, 2564, 7325, 7319, 7345, 7356, 7377, 7357,
     7368, 7388, 2513, 2467, 7400, 2603, 7389, 7425, 2468, 2371,

     2235, 2479,  361,  296,  182, 7409, 7432, 2249, 7455,  622,
     2529,  227, 1008, 7505,    0,  207, 7482, 7503, 7505, 7514,
     7535, 7536, 7544, 9159, 2413, 2526, 7567, 7571, 2546, 7593,
     2547, 2562, 2567, 2526, 2575, 9159, 2526, 7587, 7625, 7594,
     7631, 7632, 7648, 7657, 7668, 3179, 3203, 2672, 3229, 3335,
     7684, 7688, 7709, 2597, 7705, 7725, 7732, 7736,  178, 7758,
     2596, 7775, 7769, 2619, 2617, 2641, 2696, 2668, 7795, 7801,
     7807, 2677, 2720, 3150, 7827, 7818, 7834, 7867, 7850, 2625,
     2685, 7870, 7890, 2688, 7859, 2699, 2731, 2741, 2661, 2665,
     9159, 7911, 7907, 7910, 7927, 7933, 7963, 2822, 3360, 3384,

     3411, 4103, 4220, 7959, 2702, 7970, 7966, 2741, 7986, 2738,
     8003, 8019, 2758, 9159, 2765, 2781, 2809, 2818, 8030, 8026,
     2900, 2883, 2903, 2951, 8042, 8051, 2820, 2829, 8063, 8086,
     9159, 2838, 9159, 2846, 9159, 2846, 8088, 8090, 8113, 8134,
     3067, 4430, 2880, 8136,  175, 2911, 8125, 8156, 2877, 9159,
     1190, 2938, 2908, 8157, 3034, 3081, 3049, 3028, 8173, 2959,
     2985, 8189, 8182, 3029, 9159, 3039, 8205, 8212, 8214, 3241,
     4684, 3056, 8235, 3049, 9159, 3056, 3212, 3072,    0, 8237,
     1210, 3169, 3157,  144, 3103, 3103, 8257, 8260, 9159, 3110,
     8283, 3255, 4708, 3110, 8290,  123, 9159, 3235, 3207, 3329,

     3387, 3558, 3232, 3256, 8306, 8313, 3278, 8315, 8396, 4870,
     3259, 8391, 3280, 3664,    0, 3349, 3414, 9159,  103, 9159,
     3346, 8392, 9159, 3745, 4900, 8473, 3347, 8450,   90, 1237,
     1328, 3570, 8470, 4973, 4977, 3380, 8471, 9159, 3676, 3645,
     9159, 9159, 3693, 3608, 3764, 3684, 3616, 1433, 3705, 3782,
     3810, 3774, 3875, 9159, 3852, 3697, 3910, 3892, 3896, 9159,
     9159, 8553, 8565, 8577, 8589, 8601, 8613, 8621, 8623, 8635,
     8647, 8659, 8671, 8678, 8687, 8699, 8711, 8723, 8735, 8747,
     8754, 8762, 8774, 8786, 8798, 8810, 8822, 8834, 8846, 8858,
     8870, 8882, 8894, 8906, 8918, 8930, 8942, 8954, 8966, 8978,

     8990, 9002, 9014, 9026, 9038, 9050, 9062, 9074, 9086, 9098,
     9110, 9122, 9134, 9146
    } ;

static yyconst flex_int16_t yy_def[1215] =
    {   0,
     1161,    1, 1162, 1162,    1,    2, 1163, 1163,    1,    2,
        1,    2, 1161, 1161, 1161, 1161, 1161, 1164, 1165, 1161,
     1161, 1166, 1167, 1161, 1161, 1161, 1161, 1161, 1161, 1168,
     1161, 1161, 1161, 1161, 1169, 1169,   36,   36,   36,   36,
       36,   36,   36,   36,   36,   36,   36,   36,   36,   36,
       36,   36, 1161, 1161,   53, 1170, 1161,   36,   36,   36,
       36,   36,   36, 1161, 1171, 1161, 1171, 1171, 1171, 1161,
     1161, 1172, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1164, 1164, 1164, 1165, 1161, 1165, 1165, 1166, 1161, 1166,
     1166, 1167, 1173, 1167, 1161, 1167, 1167, 1161, 1161, 1161,

     1174, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1175,
     1161, 1161, 1161,   36,   36,   36,   36,   36,   36,   36,
       36,   36,   36,   36,   36,   36,   36,   36,   36,   36,
     1169,   36,   36,   36,   36,   36,   36,   36,   36,   36,
       36,   36,   36,   36,   36,   36,   36,   36,   36,   36,
       36,   36,   36,   36,   36,   36,   36,   36,   36,   36,
       36,   36,   36,   36,   36,   36,   36,   36,   36,   36,
       36,   36,   53,  173, 1176, 1161, 1161, 1161, 1161, 1161,
     1161, 1161,  173, 1161,  173, 1161, 1170, 1161, 1170, 1170,
     1170, 1161, 1161, 1177,   36,   36,   36,   36,   36,   36,

       36, 1171, 1161, 1171, 1171, 1171, 1171, 1178, 1179, 1179,
     1161, 1179, 1179, 1179, 1180, 1179, 1161,  217,  217,  217,
      217,  217, 1161, 1161, 1161, 1164, 1164, 1165, 1165, 1166,
     1166, 1173, 1173, 1173, 1173, 1161, 1167, 1167, 1161, 1161,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161, 1161, 1181, 1161, 1161, 1161, 1161, 1161,   36,   36,
       36,   36,   36,   36,   36,   36,   36,   36,   36,   36,
       36,   36,   36,   36,   36, 1182,  131,   36,   36,   36,
       36,   36,   36,   36,   36,   36,   36,   36,   36,   36,
       36, 1161,   36, 1161, 1161,   36,   36,   36,   36,   36,

       36,   36,   36,   36,   36,   36,   36,   36,   36,   36,
       36,   36,   36,   36,   36,   36,   36,   36,   36,   36,
       36,   36,   36,   36,   36,   36,   36,   36,   36,   36,
       36,   36,   36,   36,   36,   36,   36,   36,  173,  173,
     1161,  173, 1176, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
      185, 1161, 1161, 1170, 1170, 1170, 1161, 1177, 1177, 1177,
     1177,   36,   36,   36,   36,   36,   36,   36, 1171, 1171,
     1183, 1183, 1161, 1179, 1179, 1161, 1179, 1179, 1180, 1180,
     1180, 1180, 1179, 1179,  217,  217, 1179,  217,  217,  217,
      217, 1161, 1161, 1161, 1164, 1164, 1165, 1165, 1166, 1166,

     1173, 1167, 1167, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161, 1181, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161, 1161,   36,   36,   36,   36,   36,   36,   36,   36,
       36,   36,   36,   36,   36,   36,   36,   36,   36, 1161,
       36,   36,   36,   36, 1161, 1161,   36,   36,   36,   36,
       36,   36,   36,   36,   36, 1161, 1161,   36, 1161,   36,
       36,   36,   36,   36, 1161,   36,   36,   36,   36,   36,
       36,   36,   36, 1161, 1161,   36,   36, 1161, 1161,   36,
       36,   36,   36,   36,   36,   36,   36,   36,   36,   36,

       36,   36,   36,   36,   36,   36,   36,   36,   36,   36,
       36,   36,   36,   36,   36,   36,   36,   36,   36,   36,
     1161, 1161,   36,   36,   36,   36,   36,   36,  173,  173,
      173, 1161, 1161, 1161, 1161, 1161, 1161, 1161,  185, 1161,
     1161, 1170, 1170, 1170, 1161, 1177, 1177,   36,   36,   36,
       36,   36,   36, 1161, 1171, 1171, 1171, 1183, 1183, 1161,
     1179, 1179, 1180, 1180, 1179, 1179, 1179,  217,  217,  217,
      217, 1161, 1184, 1161, 1164, 1185, 1165, 1186, 1187, 1188,
     1189, 1190, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161,   36,   36, 1161,   36,   36,   36,   36,   36,   36,

       36,   36,   36, 1161,   36,   36,   36, 1161,   36,   36,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,   36,   36,
       36,   36,   36,   36,   36,   36,   36,   36,   36, 1161,
     1161,   36,   36,   36, 1161, 1161,   36,   36,   36, 1161,
       36,   36,   36,   36,   36,   36,   36,   36,   36,   36,
       36,   36,   36,   36,   36,   36,   36,   36,   36,   36,
       36,   36,   36, 1161,   36,   36,   36,   36,   36, 1161,
       36,   36,   36,   36,  173,  173,  173, 1161, 1161, 1161,
     1161, 1161, 1161,  173, 1161, 1161, 1170, 1170, 1191, 1161,
     1177, 1192, 1161, 1161,   36,   36, 1161,   36,   36, 1161,

     1161, 1161, 1171, 1171, 1179, 1193, 1180, 1194, 1179, 1179,
     1179, 1179,  217,  217, 1161,  217, 1195, 1161, 1161, 1196,
     1197, 1198, 1199, 1161, 1200, 1201, 1202, 1203, 1203, 1161,
     1161, 1161,   36, 1161,   36,   36,   36,   36,   36,   36,
       36,   36,   36, 1161,   36,   36,   36, 1161, 1161,   36,
       36, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
       36,   36,   36,   36,   36,   36,   36,   36, 1161, 1161,
       36,   36,   36,   36,   36,   36,   36,   36,   36,   36,
       36,   36,   36,   36,   36,   36,   36,   36,   36,   36,
       36,   36,   36, 1161,   36,   36,   36,   36,   36,   36,

       36, 1161,  802,  802, 1161, 1161, 1161, 1161,  802, 1204,
     1205, 1204, 1206, 1207, 1207,   36,   36,   36,   36, 1208,
     1208, 1209, 1210, 1211, 1212, 1212, 1161, 1210, 1161,  829,
     1161, 1161, 1161, 1197, 1199, 1161, 1201, 1203, 1203, 1161,
     1161,   36,   36,   36,   36,   36,   36,   36,   36,   36,
     1161, 1161,   36,   36,   36, 1161, 1161,   36, 1161, 1161,
     1161, 1161, 1161, 1161, 1161,   36,   36,   36,   36,   36,
       36,   36,   36,   36, 1161, 1213,   36,   36,   36,   36,
       36,   36,   36, 1161,   36,   36,   36,   36,   36,   36,
       36,   36, 1161, 1161,   36, 1161,   36,   36, 1161, 1161,

     1161, 1161, 1204, 1204, 1206,   36,   36, 1161,   36, 1208,
     1208, 1210, 1211, 1210,  829,  829,   36,   36,   36,   36,
       36,   36,   36, 1161, 1161, 1161,   36,   36, 1161,   36,
     1161, 1161, 1161, 1161, 1161, 1161, 1161,   36,   36,   36,
       36,   36,   36,   36,   36, 1213, 1213, 1161, 1214, 1213,
       36,   36,   36, 1161,   36,   36,   36,   36, 1161,   36,
     1161,   36,   36, 1161, 1161, 1161, 1204, 1206,   36,   36,
       36, 1208, 1208, 1211,   36,   36,   36,   36,   36, 1161,
     1161,   36,   36, 1161,   36, 1161, 1161, 1161, 1161, 1161,
     1161,   36,   36,   36,   36,   36,   36, 1161, 1214, 1214,

     1213, 1214, 1214,   36, 1161,   36,   36, 1161,   36, 1161,
       36,   36, 1161, 1161, 1161, 1204, 1206, 1206,   36,   36,
     1208, 1208, 1211, 1211,   36,   36, 1161, 1161,   36,   36,
     1161, 1161, 1161, 1161, 1161, 1161,   36,   36,   36,   36,
     1161, 1213, 1161,   36, 1161, 1161,   36,   36, 1161, 1161,
     1204, 1206, 1206,   36, 1208, 1208, 1211, 1211,   36, 1161,
     1161,   36,   36, 1161, 1161, 1161,   36,   36,   36, 1161,
     1213, 1161,   36, 1161, 1161, 1161, 1204, 1206, 1206,   36,
     1208, 1208, 1211, 1211, 1161, 1161,   36,   36, 1161, 1161,
       36, 1161, 1213, 1161,   36, 1161, 1161, 1204, 1206, 1208,

     1208, 1211, 1161, 1161,   36,   36, 1161,   36, 1161, 1213,
     1161,   36, 1161, 1204, 1206, 1208, 1208, 1161, 1211, 1161,
     1161,   36, 1161, 1161, 1213, 1213, 1161,   36, 1161, 1204,
     1208, 1161,   36, 1213, 1214, 1161,   36, 1161, 1204, 1208,
     1161, 1161, 1204, 1208, 1204, 1208, 1204, 1208, 1204, 1208,
     1204, 1208, 1204, 1161, 1208, 1208, 1208, 1208, 1208, 1161,
        0, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,

     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161, 1161, 1161, 1161
    } ;

static yyconst flex_int16_t yy_nxt[9249] =
    {   0,
       14,   15,   16,   17,   18,   19,   14,   20,   21,   22,
       23,   24,   25,   26,   25,   27,   25,   28,   29,   30,
       30,   30,   30,   30,   30,   30,   30,   30,   30,   25,
       31,   32,   33,   34,   35,   36,   37,   38,   39,   40,
       41,   42,   43,   42,   42,   44,   45,   46,   47,   48,
       42,   49,   50,   51,   42,   42,   52,   42,   42,   42,
       25,   25,   42,   35,   36,   37,   38,   39,   40,   41,
       42,   43,   42,   44,   45,   46,   47,   48,   42,   49,
       50,   51,   42,   42,   52,   42,   42,   42,   14,   53,
       54,   55,   56,   71,   67,   66,   67,   68,   76,   77,

       78,  203, 1138,   74,   72,  211,  206,   57,   57,   57,
       57,   57,   57,   57,   57,   57,   57,   75,   79,   77,
       79,   79,   77,   79,   58,   59,   82,   73,   60,   85,
       61,   69,   74,   78,   77,   80,   74,   74,   74, 1113,
       83,   62,   63,  176,   89,   75,  211,   74,   99,  176,
       75,   75,   75,   58,   59,   82,   73,   60,  224,   61,
       69,   75,  115,   86,  203,   74,   74,   74,   83,   62,
       63,   74,   98,   74,   74,   93,   74,   87,   75,   75,
       75,   90,   94,   95,  109,   75,  224,   75,   75,   75,
     1074,  115,   86,   74, 1008,   91,  110,  529,  223,  530,

       74,  226,   74,   74,  531,   87,  530,   75,  227,  207,
       90,   74,   96,   75,   74,   75,   75,   74,   74,   74,
      115,  346,   74,   91,  387,   75,   97,  223,   75,  211,
      226,   75,   75,   75,  142,   75,  227,  207,  115,  176,
       74,   96,  161,   74,  968,  176,   74,   74,   74,  115,
      346,  203,  112,   75,   97,  100,   75,  100,   74,   75,
       75,   75,  142,   76,   77,   78,  410,  115,  204,  203,
      204,  161,   75,  101,  101,  101,  101,  101,  101,  101,
      101,  101,  101,   85,  115,  115,  370,   74,  102,  143,
      115,  538,  103,   74,  104,  410,  144,  145,  188,  105,

       75,  106,  107,  201,  146,  205,   89,   75,   79,   77,
       79,  108,  228,  115,  115,  370,   89,  102,  143,  115,
      538,  103,   74,  104,  144,  145,   85,  105,  546,  106,
      107,  201,  146,   85,  205,   75,   78,   77,   78,  108,
      113,  228,  114,  114,  114,  114,  114,  114,  114,  114,
      114,  114,  115,  115,  387,  231,  129,  546,  116,  117,
      130,  138,  118,  188,  139,  242,  131,  140,  119,  243,
      132,  250,  229,  141,   89,  120,  121,  225,   77,  225,
      211,  115,  115,  231,  531,  129,  530,  116,  117,  130,
      138,  118,  139,  242,  131,  140,  119,  243,  132,  250,

      229,  141,  251,  120,  121,  113,  230,  114,  114,  114,
      114,  114,  114,  114,  114,  114,  114,  347,  124,  359,
      115,  359,  122,  967,  211,  125,  147,  118,  211,  126,
      251,  133,  127,  134,  148,  230,  135,  136,  150,  203,
      149,  123,  128,  137,  188,  347,  151,  124,  573,  115,
      348,  122,  152,  350,  125,  147,  118,  126,  153,  133,
      127,  134,  157,  148,  135,  136,  168,  150,  149,  123,
      128,  137,  115,  169,  188,  151,  158,  573,  348,  159,
      152,  350,  160,  170,  207,  154,  153,  155,  244,  162,
      387,  157,  156,  163,  675,  168,  676,  164,  576,  115,

      604,  115,  169,  245,  158,  165,  171,  159,  166,  167,
      160,  170,  207,  154,  678,  155,  172,  244,  162,  804,
      156,  389,  163,   78,   77,   80,  164,  576,  115,  604,
      677,  245,  676,  165,   93,  171,  166,  167,  183,  184,
      185,   94,   95,  678,  172,  173,   77,  174,  175,  389,
      188,  392,  677,  176,  676,  189,  186,  186,  186,  186,
      186,  186,  186,  186,  186,  186,   79,   77,   79,   93,
       79,   77,   79,   79,   77,   79,   94,   95,  115,  392,
      177,  178,  208,   74,  179,  208,  180,  190,   89,  198,
      248,  188,  142,  176,  233,  234,  233,  181,  182,  176,

      249,  191,  235,  236,  224,  208,   89,  115,  208,  177,
      178,  349,   74,  179,   85,  180,  190,  198,  836,  248,
      142,  210,  211,  210,  203,  181,  182,  211,  249,  191,
      192,  194,  224,  212,  208,   93,  176,  208,  387,  349,
      356,  387,   94,   95,  110,  188,  193,  193,  193,  193,
      193,  193,  193,  193,  193,  193,  197,  391,  195,  213,
      129,  372,  373,  372,  130,  125,   74,  237,  356,  126,
      131,  188,  196,  214,  132,  387,  378,  354,  115,  203,
       75,  211,  128,  143,  972,  197,  391,  195,  213,  129,
      144,  199,  168,  130,  125,   74,  237,  126,  131,  169,

      196,  214,  132,  355,  378,  393,  354,  115,   75,  170,
      128,  369,  143,  375,  376,  375,  200,  188,  144,  199,
      794,  168,  394,   77,  394,  731,   89,  234,  169,  724,
      373,  234,  355,  393,  401,  236,  554,  170,  401,  236,
      369,  559,  560,  559,  200,  209,  210,  211,  210,  209,
      209,  209,  215,  209,  209,  209,  209,  209,  209,  209,
      209,  209,  216,  209,  217,  217,  217,  217,  217,  217,
      217,  217,  217,  217,  209,  209,  209,  209,  209,  195,
      218,  218,  218,  218,  219,  218,  125,  218,  218,  218,
      220,  218,  218,  196,  218,  218,  218,  218,  221,  218,

      218,  218,  218,  222,  218,  209,  209,  218,  195,  218,
      218,  218,  218,  219,  218,  125,  218,  218,  220,  218,
      218,  196,  218,  218,  218,  218,  221,  218,  218,  218,
      218,  222,  218,  209,   93,  239,  240,  246,  204,  203,
      204,   94,   95,  395,  554,  396,  211,  802,  241,  803,
      680,  110,  247,  111,  111,  111,  111,  111,  111,  111,
      111,  111,  111,  587,  239,  240,  246,  375,  376,  375,
      586,  395,  339,  396,  340,  205,  241,  258,  377,  680,
      247,  387,  584,  238,  257,  257,  257,  257,  257,  257,
      257,  257,  257,  257,  115,   85,  115,  261,  388,  115,

      115,  115,  115,  268,  205,  115,  583,  377,  115,   89,
     1161,  238,  100,  115,  100,  115,  115,   85,  259,  115,
      387,  387,  115,  115,  341,  115,  261,  388,  115,  115,
      115,  115,  268,  211,  115,  397,  419,  115,   89, 1161,
      211,  115,  421,  115,  115,  102,  259,  115,  115,  103,
      115,  104,  341,  115,  411,  390,  105,  450,  106,  107,
      417,  400,  260,  397,  419,  234,  398,  115,  108,  294,
      421,  294,  401,  236,  102,  373,  188,  115,  103,  295,
      104,  399,  115,  390,  105,  450,  106,  107,  681,  400,
      260,  420,  412,  418,  398,  115,  108,  253,  253,  253,

      253,  253,  253,  253,  253,  253,  253,  115,  264,  399,
      211,  416,  115,  115,  115,  254,  255,  681,  262,  115,
      412,  418,  263,  415,   89,  265,  115,  115,  256,  414,
      233,  234,  233,  115,  349,  258,  115,  264,  235,  236,
      532,  115,  115,  115,  254,  255,  262,  115,  115,  266,
      413,  263,  115,  265,  115,  115,  256,  344,  115,  387,
      580,  115,  349,  115,  211,  533,  115,  534,  532,  188,
      974,  345,  115,  535,  211,  265,  115,  115,  266,  115,
      203,  115,  536,  115,  115,  188,  344,  115,  115,  580,
      267,  537,  115,  533,  115,  534,  342,  269,  115,  345,

      115,  535,  115,  265,  804,  115,  803,  542,  115,  115,
      536,  188,  115,  115,  115,  342,   93,  115,  267,  537,
       89,  270,  271,   94,   95,  269,  115,  272,  115,  115,
      115,  273,  274,  387,  115,  115,  542,  115,  115,  292,
      115,  292,  547,  115,  233,  234,  233,  275,  115,  270,
      271,  115,  235,  236,  115,  272,  115,  682,  115,  402,
      273,  274,   89,  115,  115,  387,  115,   85,  569,  115,
      547,  380,  211,  380,  188,  275,  115,  203, 1161,  115,
      211,   93,  115,  276,  188,  276,  682,  402,   94,   95,
      203,  475,  188,  568,  387,  115,  569,  572,   70,  113,

      115,  277,  277,  277,  277,  277,  277,  277,  277,  277,
      277,  115,  203,  281,  115,  115,  115,   66,  282,  475,
      115,  568,  280,   85,  115,  572,  544,  543,  555,  115,
      115,  570,  562,  403,  115,  804,  278,  803,  279,  188,
      115,  281,  115,  211,  115,  115,  115,  282,  115,  115,
      280,  115, 1077,  115,  544,  543,  578,  555,  115,  570,
      562,  403,  115,  284,  278,  115,  279,  115,  283,  115,
      211,  115, 1100,  115,  115,  115,  115,  115,  115,  285,
      115,  563,  115,  289,  288,  578,  286,  575,  115, 1161,
      287,  284,  115,  115,  585,  115,  283,   85,  115, 1139,

      115,  211,  115,  115,  115,  115,  640,  115,  285,  115,
      563,  289,  288,  561,  286,  575,  115,  292,  287,  292,
      115,  290,  585,  115,  115,   89,  294,  115,  294,  115,
      203,  683,  115,  115,  640, 1161,  295,  291,  115,  387,
      679,  561,  296,  115,  577,  564,  115, 1161,  188,  290,
      115,  115,  466,  115,  466,  115,  115,  115,  115,  261,
      683,  115,  115,  301,  115,  291, 1161,  571,  679,  293,
      296,  115,  577,  564,  115,  579,  115, 1161,  115,  115,
      115, 1161,  297,  467,  115,  115,  115,  697,  261,  697,
     1140,  115,  301,  115,  298,  571,  299,  293,  688,  300,

      115, 1161,  115,  579,  115,  115,  115,  115,  203,  115,
      484,  297,  484,  691,  115,  303,  574,   77,  574,  115,
      485,  115,  298,  302,  299,  115,  688,  300,  304,  115,
      115,  115,  717,  115,  115,  203,  115,  115,  115, 1161,
     1161,  691,  115,  303,  115,  305,  692,  115, 1161,  115,
      306,  302,  115,  704,  115, 1161,  115,  304, 1161,  115,
      188,  717,  115,  115,  115, 1161,  115,  115,  115,  115,
     1161,  115,  115,  305,  692,  308,  307,  488,  306,  488,
      115,  704,  115,  115,  115, 1161,  115,  489, 1161,  115,
      310,  115,  115,  115,  115, 1150,  689,  115,  115,  521,

      309,  521, 1161,  308,  307,  311,  752, 1161,  115,  522,
      115,  115,  115,  239,  240,  115,  115,  115,  115,  310,
      115,  115,   93,  115,  315,  689,  241, 1161,  309,   94,
       95,  312,  115,  311,  752,  115,  115,  115,  559,  560,
      559,  115,  239,  240, 1161,  115,  115,  753, 1161,  313,
      115,  115,  314,  315,  241,  115,  318,  115, 1161,  312,
      115,  115,  115,  115,  755,  316,  115,  556,  203,  556,
      756,  317,  581,  211,  115,  753,  115,  313, 1161,  115,
      314,  359,  115,  359,  115,  318,  115,  115,  319,  115,
      320,  115,  755,  115,  316,  556,  203,  556,  756,  317,

      581,  115,  115,  324,  115, 1161,  321,  322,  557,  188,
      323,  115,  720,  115,  360,  211,  115,  319,  115,  320,
      115,  203,  115,  705,  325,  115,  326,  211,  361,  115,
     1161,  324,  115,  327,  321,  322,  557,  557,  323,  115,
      115,  720,  115,  360,  387,  115,  815,  115,  815,  115,
      759,  705,  325,  211,  115,  326,  361,  328,  687,  115,
      115,  327,  708,  329,   93,  557,  808,  115,  115,  115,
      703,   94,   95,   85,  115,  387,  707,  330,  759,  713,
      265, 1161,  115,  115,  331,  328,  687,  115,  115,  706,
      708,  697,  329,  697,  808,  332,  714,  115,  703,  851,

      582, 1161,  115,  387,  707,  330,  722,  713,  265,  115,
      115,   89,  115,  331,  115,  115, 1161,  115,  706,  335,
      115, 1161,  333,  332,  715,  714,  334,  851,  115,  582,
      115,  387,  115, 1161,  115,  722,  859,  115,  115, 1161,
      336,  115, 1161,  115,  115,  337,  115,  725,  335,  115,
      333,  115,  716,  715,  334, 1161,  115,  719,   77,  719,
      115,  115,  115,  115,  859,   93,  115,  744,  115,  336,
      115,  338,   94,   95,  337,  115,  725,   93,  754,  115,
      760,  716,  115,  188,  729,  730, 1161, 1161,  115,  693,
     1161,  693,  115,  342,  184,  351,  744,  115,  861,  694,

      338,  727,  806,  635,  815,  635,  815,  754,  862,  760,
      115,  352,  352,  352,  352,  352,  352,  352,  352,  352,
      352,  353,  807,  810, 1161,  805,  861,  176,  387, 1161,
      727,  806,  813,  608,  636,  608,  862,  352,  352,  352,
      352,  352,  352,  352,  352,  352,  352,  353,  700,  827,
      700,  807,  810,  176,  805,  804,  184,  809,  701, 1161,
      203,  813,  702,  352,  352,  352,  352,  352,  352,  352,
      352,  352,  352,  353,  748,  380,  211,  380,  827,  176,
      826,  211,  826,  856,  826,  211,  826,  110,  749,  357,
      357,  357,  357,  357,  357,  357,  357,  357,  357,  115,

      820,  115,  115,  748,  115,  857,  115,  115,  381, 1161,
      267,  757,  856,  270,  363,  188,  749,  362,  115,  272,
      115,  364,  382,  832,   77,  832,  203,  758,  115,  820,
      115,  115,  211,  115,  857,  115,  115,  381,  267,  115,
      757,  270,  363, 1161,  115,  362,  115,  272,  115,  364,
      382,  811,  702,  365,  702,  758,  115, 1161,  115,  115,
      115,  115,  297,  368,  115,  115,  702,  860,  115,  863,
     1161,  367,  824,  115,  298,  115,  299,  821,  115,  366,
      811,  365,  864,  901, 1161,  115,  115, 1161,  115,  115,
      115,  297,  368,  115,  115,  211,  860, 1161,  863,  367,

     1161,  824,  298,  115,  299,  821,  115,  366, 1161, 1161,
      864,  901,  383,  383,  383,  383,  383,  383,  383,  383,
      383,  383,  374,  374,  211,  374,  374,  374,  374,  374,
      374,  374,  374,  374,  374,  374,  374,  374,  374,  384,
      374,  385,  385,  385,  385,  385,  385,  385,  385,  385,
      385,  374,  374,  374,  374,  374,  386,  386,  386,  386,
      386,  386,  386,  386,  386,  386,  386,  386,  386,  386,
      386,  386,  386,  386,  386,  386,  386,  386,  386,  386,
      386,  386,  374,  374,  386,  386,  386,  386,  386,  386,
      386,  386,  386,  386,  386,  386,  386,  386,  386,  386,

      386,  386,  386,  386,  386,  386,  386,  386,  386,  386,
      374,  404, 1161,  404,  932,  865,  405,  405,  405,  405,
      405,  405,  405,  405,  405,  405,  406, 1161,  406,  935,
      899,  407,  407,  407,  407,  407,  407,  407,  407,  407,
      407,  408,  932,  408,  865,  900,  409,  409,  409,  409,
      409,  409,  409,  409,  409,  409,  423,  935,  423,  899,
     1161,  424,  424,  424,  424,  424,  424,  424,  424,  424,
      424,  425, 1161,  425,  900, 1161,  426,  426,  426,  426,
      426,  426,  426,  426,  426,  426,  427,  902,  427,  432,
      937,  428,  428,  428,  428,  428,  428,  428,  428,  428,

      428,  257,  257,  257,  257,  257,  257,  257,  257,  257,
      257, 1161,  115,  769,  115,  769,  902,  115,  937,  115,
      429,  430,  430,  430,  430,  430,  430,  430,  430,  430,
      430,  115,  433,  115,  115,  905,  908,  115,  908,  115,
      431,  115,  115,  115,  770,  434,  115,   93,  115,  429,
      908,  435,  908,  115,  839,  840,  115,  115,  211,  115,
      433,  115,  115,  115,  905,  693,  115,  693,  115,  431,
      436,  115, 1161,  434, 1161,  694,  115,  115,  115,  435,
      115,  115,  115,  115,  115,  115,  115,  804,  439,  803,
      437,  115, 1161, 1161,  822,  176,  115,  115,  436,  115,

      438,  805,  924,  115,  115,  188,  115,  115,  115,  115,
      115,  115,  115, 1161,  115,  115,  439, 1161,  437, 1161,
      441,  440,  115,  822,  115,  115, 1161,  115,  438,  115,
      805,  924,  115,  115, 1161,  115,  203,  115,  115,  115,
      115,  115,  903, 1161,  115,  700,  115,  700,  441,  440,
      115,  442,  115,  443,  445,  701,  444,  115,  115,  702,
      115, 1161,  115,  115,  115,  446,   93,  115, 1161,  115,
      115,  903,  911,   94,   95,  115, 1161,  115,  115,  442,
      115,  443,  445,  115,  444,  115, 1161,  115,  115,  447,
      115,   93,  115,  893,  446,  893,  448,  115,   94,   95,

     1161,  911, 1161,  894,  115,  115, 1161,  115,  115,  451,
      929,  115,  115,  115,  115,  449,  115,  447,  965,  115,
      931,  804,  184,  809,  448,  115,  203,  115,  115,  176,
      115,  453,  115,  115,  211,  452,  115,  115,  451,  929,
      115,  466,  115,  466,  449,  115,  965,  115,  115,  931,
      115,  933,  454,  115, 1161,  115,  934,  115,  115,  936,
      453,  980,  115,  910,  452,  115, 1161,  115,  115, 1161,
     1161,  913,  467,  115,  115,  115,  465,  115,  115,  115,
      933,  454,  115,   93,  875,  934,  875,  115,  936,  980,
       94,   95,  910,  115,  876,  115,  455,  115,  455,  115,

      913,  959,  115,  115,  115,  465,  115,  964,  115,  239,
      240,  115,   93,  966,  893,  115,  893,  468,  469,   94,
       95,  115,  241, 1161,  894,  115, 1161,  456,  115,  115,
      959,  203,  457,  115,  458,  115,  964,  459,  239,  240,
      115,  460,  966, 1161,  461,  468,  469,  462,  463,  472,
      241,  464,  115,  115,  115,  115,  470,  115,  115,  471,
      115,  457,  989,  458,  991,  884,  459,  884,  115,  460,
     1161,  115,  461, 1161,  115,  462,  463,  981,  472,  464,
      973,  115,  115,  115,  115,  470,  115,  115,  471,  115,
      115,  989,  474,  991,  473,  115,  115,  984,  115,  115,

      986,  115,  115,  115,  896,  981,  896,  987,  973,  115,
     1161,  115,  476,  954, 1161,  115,  115,  115,  988,  115,
      115,  474,  473,  990,  115,  984,  477,  115,  986,  115,
     1010,  115,  115, 1161,  115,  987,  115,  115,  115,  961,
      476,  954,  478,  115,  115,  115,  988, 1161, 1005,  115,
      115,  990, 1161, 1013,  477, 1161, 1161,  115,  479, 1010,
      115,  115,  115, 1161, 1161,  115,  115,  115,  961, 1014,
      478, 1027,  115,  998,  948,  998, 1005, 1015,  115,  203,
      115,  480, 1013,  115,  115,  115,  479,  481,  115,  115,
      115,  484,  483,  484,  488,  115,  488, 1014,  188, 1027,

      482,  485,  115,  115,  489, 1017, 1015, 1018,  115,  480,
     1161, 1161,  115,  115, 1035, 1021,  481,  115,  115, 1036,
      115,  483,  203, 1028,  115,  115, 1031,  115,  482,  115,
      115,  115,  115, 1016, 1017,  486, 1018, 1161,  490,  115,
      492, 1032, 1035,  115, 1021,  487,  115, 1036,  491,  115,
     1043, 1161, 1028,  115,  115, 1031,  115, 1161,  115, 1022,
      115,  115, 1016,  486,  493,  115,  490,  115,  492, 1033,
     1032,  115,  115,  487,  115, 1034,  491,  115, 1043,  115,
      115,  115, 1045,  188,  495,  115,  115, 1161, 1022,  115,
     1046,  494,  496,  493,  115, 1161, 1161,  115, 1033,  115,

      115,  115,  115, 1050, 1034, 1161,  115,  115, 1161,  115,
      115, 1045, 1049,  495,  115,  115,  115,  115, 1046,  494,
      496,  497,  115, 1041,  948, 1041,  115,  115,  115, 1051,
      498,  115, 1050,  115,  115, 1161,  115,  500,  115,  115,
     1049,  499,  115,  852,  115,  852,  115, 1052, 1161,  497,
     1161,  115,  115,  115,  115, 1161,  115, 1051,  498,  115,
     1053, 1161,  115,  115,  115, 1061,  500,  115,  115, 1060,
      499,  115,  501,  115,  115, 1161, 1052,  115,  925,  115,
      115,  115,  115,  115,  115,  203, 1064,  502,  115, 1053,
      503,  926, 1065,  115, 1061, 1161,  115, 1060,  504, 1066,

      501,  115,  203,  115,  115,  211,  115,  925,  115,  115,
      505,  115, 1161, 1161, 1064,  502, 1072, 1056,  503,  926,
     1065,  115, 1076,  115,  115,  115,  504, 1066, 1161,  115,
      115, 1161,  507,  115,  115, 1161,  506,  115,  115,  505,
      508, 1057,  115, 1161,  115, 1072, 1056, 1055,  115, 1075,
     1076,  115, 1161,  211,  115, 1079,  115,  509,  115,  115,
      115,  507,  115,  115,  506,  115,  115,  115,  508,  115,
     1057,  115,  115, 1078,  115, 1055,  115,  512, 1075,  115,
      510,  115,  511, 1079,  115,  509, 1161, 1161,  115,  115,
     1161,  115,  115, 1058,  115,  115,  115,  115,  115,  513,

      115, 1161, 1078,  115, 1085,  515,  512,  115,  510,  115,
      511,  115,  115, 1161,  115,  115,  115,  115,  115,  514,
      115,  115, 1058,  115,  115,  516,  115, 1086,  513,  115,
      211,  517, 1085,  515,  115, 1161,  203,  115,  115,  115,
     1161,  115,  115,  115,  115,  115,  115,  115,  514,  115,
      115,  211,  115,  521,  516,  521, 1086,  518, 1161,  517,
      519,  115,  115,  522,  115,  115,  115,  115, 1070,  948,
     1070, 1081,  115, 1161,  115, 1084, 1089, 1161,  520,  115,
      115, 1090,  115,  203, 1083,  518,  115,  115,  519,  523,
      115,  115,  115,  115, 1094,  115, 1161, 1096,  115,  524,

     1081,  115,  115, 1084, 1089,  115,  520,  115,  115, 1097,
     1090,  115,  115, 1083,  526,  115,  115, 1082,  523,  115,
      115,  115,  115, 1094,  115, 1096, 1099,  115,  524,  115,
     1161,  115,  525,  115, 1161, 1161,  115, 1097,  115,  115,
      115, 1103,  526, 1161,  115,  115, 1082, 1111,  115,  115,
      115, 1161,  211,  115, 1099, 1104,  527, 1107,  115,  211,
      525,  947,  948,  947,  528, 1161,  115, 1161,  115,  949,
     1103,  203,  950,  115,  115,  531, 1111,  539, 1161,  115,
      947,  948,  947, 1104,  527, 1107,  115, 1023,  949, 1024,
     1161,  950,  528,  540,  540,  540,  540,  540,  540,  540,

      540,  540,  540,  541,  947,  948,  947, 1101, 1161,  176,
     1161, 1102,  949, 1161,  188,  950, 1023, 1161, 1024,  540,
      540,  540,  540,  540,  540,  540,  540,  540,  540,  541,
     1000, 1001, 1000, 1161, 1161,  176, 1101,  188, 1002, 1102,
     1161, 1003, 1092,  948, 1092,  540,  540,  540,  540,  540,
      540,  540,  540,  540,  540,  541, 1109,  948, 1109, 1098,
     1115,  176,  115,  115, 1161,  550,  548,  115,  115,  110,
      115,  545,  545,  545,  545,  545,  545,  545,  545,  545,
      545,  115,  549, 1114,  115,  115,  551, 1098, 1115, 1120,
      115,  115,  115,  554,  550,  548,  115,  115, 1121,  115,

     1161,  115, 1161, 1161,  115,  552,  115, 1161, 1161,  115,
      549, 1114,  115, 1127,  115,  551, 1123, 1120,  482,  115,
      115,  115, 1161,  115,  115,  553,  115, 1121,  115,  115,
      115,  203,  115, 1161,  552,  115,  947,  948,  947, 1129,
      115, 1127,  115,  115,  949, 1123,  482,  950,  115, 1161,
      115,  203,  115,  115,  553,  115,  211,  115,  115, 1161,
     1161, 1000, 1001, 1000, 1161, 1161, 1116, 1129,  115, 1002,
      115,  115, 1003,  383,  383,  383,  383,  383,  383,  383,
      383,  383,  383,  211, 1161, 1000, 1001, 1000, 1117, 1118,
     1117, 1161,  565, 1002, 1132, 1116, 1003, 1131, 1136, 1161,

      566,  566,  566,  566,  566,  566,  566,  566,  566,  566,
      211, 1161, 1042, 1001, 1042, 1117, 1118, 1117, 1142,  567,
      949,  565, 1132,  950, 1161, 1131, 1136,  383,  383,  383,
      383,  383,  383,  383,  383,  383,  383,  405,  405,  405,
      405,  405,  405,  405,  405,  405,  405, 1142,  567,  405,
      405,  405,  405,  405,  405,  405,  405,  405,  405,  407,
      407,  407,  407,  407,  407,  407,  407,  407,  407,  407,
      407,  407,  407,  407,  407,  407,  407,  407,  407,  409,
      409,  409,  409,  409,  409,  409,  409,  409,  409,  409,
      409,  409,  409,  409,  409,  409,  409,  409,  409,  424,

      424,  424,  424,  424,  424,  424,  424,  424,  424,  424,
      424,  424,  424,  424,  424,  424,  424,  424,  424,  426,
      426,  426,  426,  426,  426,  426,  426,  426,  426,  426,
      426,  426,  426,  426,  426,  426,  426,  426,  426,  428,
      428,  428,  428,  428,  428,  428,  428,  428,  428,  428,
      428,  428,  428,  428,  428,  428,  428,  428,  428,  588,
      211,  588, 1161, 1161,  589,  589,  589,  589,  589,  589,
      589,  589,  589,  589,  430,  430,  430,  430,  430,  430,
      430,  430,  430,  430, 1161,  115, 1161,  115, 1161,  590,
      115,  590,  115,  429,  591,  591,  591,  591,  591,  591,

      591,  591,  591,  591,  115,  592,  115,  115,  115,  593,
      203, 1119,  115,  115,  115, 1161,  115, 1141,  188,  115,
     1161,  115,  429, 1161,  115, 1161,  115,  115,  594,  115,
      115, 1161,  115,  592,  115,  115,  115,  115,  593, 1119,
     1161,  115,  115,  115, 1161, 1141,  115,  203,  595,  115,
     1149,  115, 1161,  115,  115,  115, 1146,  594,  115,  115,
     1161, 1161,  596, 1161,  115,  115,  188,  115,  115,  115,
     1161,  115,  597,  115,  115,  115,  595,  115,  188, 1149,
      115,  115, 1161,  598, 1146,  115,  203,  115,  115,  115,
      115,  596, 1144,  115,  115,  188,  115,  115,  115,  203,

      599,  597,  115,  115,  115,  115, 1161,  188,  115,  115,
      115,  598, 1161,  600,  115,  115,  115, 1130,  115,  115,
     1144,  115,  115,  115,  115,  601,  115,  115,  599, 1143,
      115, 1157,  115,  602,  115,  115,  115, 1148, 1161,  115,
      115,  115,  600,  603, 1145, 1130, 1124,  948, 1124,  115,
      115,  115,  115, 1161,  601,  115,  115, 1143, 1151,  115,
     1157,  602,  115, 1161,  115, 1148,  188,  115,  115,  115,
      115,  603, 1145,  605,  115,  115,  203,  115,  606,  115,
     1161,  115, 1161, 1161,  203,  608, 1151,  608, 1161,  115,
     1161,  115,  607,  115, 1161, 1161,  115, 1161, 1161,  115,

     1161,  605, 1147,  115,  115, 1161, 1161,  606,  115,  115,
      115, 1153, 1154, 1153,  115,  115, 1161,  115,  115,  115,
      607,  115, 1161,  115, 1161, 1155,  609, 1161,  619,  115,
      620, 1147, 1161,  115, 1161, 1152, 1161,  115, 1161,  115,
      610,  115, 1161,  115,  115,  115,  621,  115,  115, 1161,
      115, 1161,  115, 1155,  203,  609,  619,  115,  620,  622,
      115,  115,  115, 1152,  115,  115, 1161,  115,  610,  455,
      115,  455, 1161, 1161,  115,  621, 1153, 1154, 1153,  115,
     1161,  629, 1161, 1161,  115, 1161, 1161,  622,  115,  115,
     1156,  115,  115, 1159, 1160, 1159,  115, 1159, 1160, 1159,

      456,  623, 1161,  115, 1161,  611,  115,  612, 1161,  629,
      613,  115,  203,  115,  614, 1161, 1161,  615,  115, 1156,
      616,  617, 1161, 1161,  618,  115, 1161, 1161, 1161,  623,
      626,  115, 1161, 1161,  611,  115,  612, 1161,  115,  613,
      115, 1161,  614,  115, 1161,  615,  115, 1161,  616,  617,
      624,  115,  618,  115,  115,  115, 1161,  637,  626,  115,
      115,  627, 1161, 1158, 1161,  115,  625,  115, 1161, 1161,
     1161, 1161,  115,  115,  115,  115,  628, 1161, 1161,  624,
      115, 1161, 1161,  115,  115,  637, 1161, 1161,  115,  115,
      627, 1158, 1161,  115,  625, 1161, 1161, 1161, 1161, 1161,

     1161,  115,  115, 1161,  628,  630,  630,  630,  630,  630,
      630,  630,  630,  630,  630,  630, 1161,  630,  630,  630,
      630,  630,  631,  630,  632,  632,  632,  632,  632,  632,
      632,  632,  632,  632,  630,  630,  630,  630,  630,  633,
      633,  633,  633,  633,  633,  633,  633,  633,  633,  633,
      633,  633,  633,  633,  633,  633,  633,  633,  633,  633,
      633,  633,  633,  633,  633,  630,  630,  633,  633,  633,
      633,  633,  633,  633,  633,  633,  633,  633,  633,  633,
      633,  633,  633,  633,  633,  633,  633,  633,  633,  633,
      633,  633,  633,  630,  115, 1161,  635,  638,  635,  115,

     1161, 1161,  115,  115, 1000, 1001, 1000, 1161,  115, 1161,
     1161,  634, 1002,  115, 1161, 1003,  115, 1161, 1161, 1161,
     1161, 1161,  639,  115, 1161, 1161,  638,  636,  115,  115,
      115,  115,  115, 1161,  115,  115, 1161,  115,  115,  634,
     1161,  115, 1161,  115,  115, 1161,  641, 1161,  115,  115,
      639, 1161, 1161, 1161, 1161, 1161, 1161,  115,  115,  115,
      115, 1161, 1161,  115,  115,  115, 1161,  115,  115, 1161,
     1161, 1161,  115,  115, 1161,  641,  115,  115, 1161,  115,
      642, 1161, 1161, 1161, 1161,  115, 1161,  643, 1161,  115,
      115, 1161, 1161, 1161,  115,  115, 1161,  115,  644,  115,

     1161, 1161,  115, 1161,  115,  645, 1161,  115,  642,  115,
     1161, 1161,  646, 1161, 1161,  643, 1161, 1161,  115,  115,
     1161, 1000, 1001, 1000,  115, 1161, 1161,  644,  115, 1002,
      115,  650, 1003,  115,  645,  115,  115,  115,  647,  115,
      646,  648,  115, 1161,  115, 1161,  115,  115, 1161,  115,
      115, 1161, 1161, 1161, 1161, 1161,  649, 1161,  115,  115,
      650,  115, 1161, 1161,  115,  115, 1161,  647,  115,  115,
      648,  115, 1161,  115,  115,  652,  115,  115,  115, 1161,
      115, 1161, 1161,  651,  649, 1161,  115, 1161,  115,  115,
     1161,  115, 1161, 1161,  115,  115,  115, 1161,  115, 1161,

      115, 1161,  653,  115,  652, 1161,  654,  115, 1161,  115,
      115,  651,  115, 1161,  115, 1161,  115, 1161,  115, 1161,
      115, 1161,  115,  115,  115,  115,  115,  115, 1161,  115,
      653, 1161,  115, 1161,  654,  655,  115,  115,  115,  115,
     1161,  115,  115,  115,  115, 1161,  115,  115,  115, 1161,
      115, 1161,  115, 1161,  115,  115,  115, 1161,  115, 1161,
     1161,  115,  115, 1161,  655,  115,  115, 1161,  115,  659,
      115,  115,  115,  115,  115,  115, 1161,  115, 1161,  115,
      656, 1161,  657, 1161,  115,  115,  115,  115, 1161,  115,
      115,  658,  115,  115, 1161,  115,  661,  659,  115,  115,

      115,  115, 1161, 1161,  115, 1161,  115, 1161,  656, 1161,
      657, 1161,  660,  115, 1161,  115,  115,  115,  115,  115,
      658,  115,  115,  115,  115,  661, 1161,  115, 1161,  664,
      115, 1071,  948, 1071,  115,  662, 1161,  115,  115,  949,
      660,  115,  950,  115, 1161, 1161,  115,  115,  115,  663,
      115, 1161,  115,  115, 1161,  115, 1161,  115, 1161, 1161,
      115, 1161,  115,  662, 1161,  115,  115, 1161, 1161,  115,
      115, 1161, 1161, 1161,  665,  115,  115, 1161,  663,  115,
     1161, 1161,  115, 1161,  115, 1161,  115,  115,  115,  115,
      115,  115, 1161,  666,  115,  115, 1161,  115, 1161, 1161,

     1161,  115,  665,  668,  115, 1161,  667, 1161,  115,  115,
     1161,  115, 1161, 1161, 1161, 1161,  115, 1161,  115,  115,
      115,  666,  115,  115,  115,  115, 1161,  115,  115,  115,
      669,  668, 1161,  115,  667, 1161,  115,  115,  670,  115,
     1161,  115, 1161, 1161, 1161,  115, 1161,  115, 1161,  115,
      115,  115, 1161,  115,  115, 1161,  115,  115,  115,  669,
     1161,  115,  115, 1161,  115,  115,  115,  115, 1161,  115,
      115, 1161,  115, 1161,  115,  115, 1161, 1161, 1161,  115,
      115, 1161,  115, 1161,  115, 1161,  115,  115,  115, 1161,
      115,  115,  115,  115,  115,  115, 1161, 1161, 1161,  115,

      115, 1161,  671, 1161, 1161,  115, 1161,  115,  115,  115,
     1161, 1161,  115,  672,  115,  115, 1161,  115,  115, 1161,
      115,  115,  115,  115, 1161, 1161,  115, 1161,  115, 1161,
      671, 1161, 1161,  115, 1161,  115, 1161,  115,  115, 1161,
      115,  115,  672,  115,  115,  673,  115,  115,  674,  115,
      115, 1161,  115, 1161, 1161,  115,  115, 1161, 1161, 1161,
      115,  698, 1161,  115, 1161,  115,  115, 1161,  115, 1161,
      115, 1161, 1161,  115,  673,  115, 1161,  674,  115,  677,
      115,  684, 1161, 1161, 1161, 1093,  948, 1093,  115, 1161,
      698,  115, 1161,  949, 1161,  115,  950,  685,  685,  685,

      685,  685,  685,  685,  685,  685,  685,  686,  115, 1110,
      948, 1110, 1161,  176, 1161, 1161, 1161,  949, 1161, 1161,
      950, 1161, 1161,  685,  685,  685,  685,  685,  685,  685,
      685,  685,  685,  686,  693, 1161,  693, 1161, 1161,  176,
     1161, 1161, 1161, 1161,  694, 1161, 1161, 1161, 1161,  685,
      685,  685,  685,  685,  685,  685,  685,  685,  685,  686,
      697, 1161,  697, 1161, 1161,  176,  696,  115, 1161, 1161,
      695,  115,  115,  110,  600,  690,  690,  690,  690,  690,
      690,  690,  690,  690,  690,  115,  115, 1161, 1161, 1161,
     1161, 1161, 1161,  115, 1161,  696,  115, 1161,  115,  695,

      115,  115,  115,  600, 1161,  115, 1161,  115,  645,  732,
      115, 1161,  115,  115,  115,  646, 1161, 1161,  700,  699,
      700,  115,  115, 1161,  115, 1161, 1161,  115,  701, 1161,
     1161,  115,  702, 1161,  115, 1161,  115,  645,  732,  115,
      115,  211, 1161,  646, 1161, 1161, 1161,  699, 1161,  115,
     1161,  115,  115,  709, 1161,  709,  115, 1161,  710,  710,
      710,  710,  710,  710,  710,  710,  710,  710, 1161,  211,
      115, 1126,  948, 1126, 1161, 1161, 1161, 1161, 1161,  949,
      115, 1161,  950, 1161, 1161,  115,  566,  566,  566,  566,
      566,  566,  566,  566,  566,  566,  211, 1161,  115, 1161,

     1161,  947,  948,  947, 1161,  565, 1161, 1161,  711,  949,
      711, 1161,  950,  712,  712,  712,  712,  712,  712,  712,
      712,  712,  712,  589,  589,  589,  589,  589,  589,  589,
      589,  589,  589, 1161,  565,  589,  589,  589,  589,  589,
      589,  589,  589,  589,  589,  591,  591,  591,  591,  591,
      591,  591,  591,  591,  591,  591,  591,  591,  591,  591,
      591,  591,  591,  591,  591,  733, 1161, 1161,  115, 1161,
      115, 1161, 1161,  115,  947,  948,  947, 1161, 1000, 1001,
     1000, 1161,  949,  737,  115,  950, 1002,  115, 1161, 1003,
     1161, 1161, 1161, 1161,  733, 1161, 1161,  115, 1161,  115,

     1161, 1161,  115, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161,  737,  115,  718,  718,  115,  718,  718,  718,  718,
      718,  718,  718,  718,  718,  718,  718,  718,  718,  718,
      734,  718,  735,  735,  735,  735,  735,  735,  735,  735,
      735,  735,  718,  718,  718,  718,  718,  736,  736,  736,
      736,  736,  736,  736,  736,  736,  736,  736,  736,  736,
      736,  736,  736,  736,  736,  736,  736,  736,  736,  736,
      736,  736,  736,  718,  718,  736,  736,  736,  736,  736,
      736,  736,  736,  736,  736,  736,  736,  736,  736,  736,
      736,  736,  736,  736,  736,  736,  736,  736,  736,  736,

      736,  718,  115,  115,  738,  115,  115,  115,  115,  115,
      115,  115, 1161,  740,  115, 1161, 1161, 1161,  739,  741,
     1161,  115,  115, 1161,  115,  115, 1161, 1161,  115, 1161,
     1161,  115,  115,  738,  115,  115,  115,  115,  115,  115,
      115, 1161,  740,  115, 1161, 1161,  739,  741, 1161,  115,
      115, 1161,  115,  115,  115,  115,  115,  115, 1161,  115,
      115,  115,  115, 1161, 1161,  745,  115, 1161, 1161,  742,
     1161, 1161, 1161,  115,  115,  743,  115, 1161, 1161, 1161,
      115, 1161, 1161,  115,  115, 1161,  115,  115,  115,  115,
      115,  115,  115, 1161,  745,  115, 1161,  742,  746, 1161,

     1161,  115,  115,  743,  115, 1161,  115, 1161,  115,  115,
      115, 1161, 1161,  115,  750,  115,  115,  747,  115, 1161,
      751,  115, 1161, 1161, 1161, 1161,  746, 1161,  115,  115,
     1161, 1161,  115, 1161,  115,  115,  115, 1161,  115,  115,
      115,  115,  115,  750,  115, 1161,  747,  115, 1161,  751,
     1161, 1161, 1161, 1161,  115,  115,  115,  115,  115, 1161,
      115, 1161, 1161,  115,  115,  115, 1161, 1161, 1161,  115,
      115,  761, 1161, 1161,  115,  115, 1161,  115,  762,  115,
      115, 1161,  115,  115, 1161, 1161, 1161,  115,  763, 1161,
     1161,  115,  115,  115,  115, 1161,  115, 1161, 1161,  761,

     1161, 1161,  764,  115,  115,  115, 1161,  762,  115,  115,
      115, 1161,  115,  765,  115, 1161,  763,  115,  768,  115,
      115,  115,  115,  115, 1161,  115, 1161, 1161, 1161,  766,
      764,  115, 1161,  115, 1161,  115, 1161,  115,  115,  767,
      115,  115,  765,  115, 1161, 1161,  115,  768,  115, 1161,
     1161, 1161,  115,  769,  115,  769, 1161,  766, 1161,  115,
     1161,  115, 1161,  258,  115,  115, 1161, 1161,  767,  115,
      257,  257,  257,  257,  257,  257,  257,  257,  257,  257,
      115, 1161,  115,  115,  770,  115,  115,  115,  115,  115,
     1161,  115,  115, 1161,  115, 1161, 1161, 1161, 1161,  115,

      771, 1161,  115, 1161, 1161,  115,  115, 1161,  115,  115,
     1161, 1161,  115, 1161,  115,  115,  115,  115,  115,  115,
      115,  115, 1161,  115,  115, 1161, 1161,  115,  771, 1161,
      115, 1161, 1161,  115,  115,  115,  115, 1161,  772,  115,
      115,  774,  115,  773,  115,  775,  115,  115,  115, 1161,
     1161,  115, 1161,  115,  115, 1161, 1161, 1161,  115, 1161,
     1161,  115,  115, 1161,  115,  115,  772,  115,  115,  115,
      774,  115,  773,  115,  775,  115,  115, 1161, 1161,  776,
      115,  115,  115,  115, 1161,  115,  115,  777,  115,  115,
      115,  115,  115,  115,  780, 1161,  115,  115, 1161,  115,

     1161, 1161,  115, 1161,  778,  779, 1161,  776, 1161,  115,
     1161,  115,  115,  115,  115, 1161,  777,  115, 1161,  115,
     1161,  115, 1161,  780, 1161, 1161,  115, 1161,  115,  115,
      115,  781,  778,  779,  115,  115, 1161, 1161, 1161,  115,
      115,  115, 1161, 1161, 1161, 1161,  782, 1161,  115, 1161,
     1161, 1161, 1161, 1161,  115, 1161, 1161, 1161,  115, 1161,
      781,  115, 1161,  115,  115,  115,  115,  783, 1161,  115,
      115, 1161,  784,  115,  782, 1161,  115,  785,  115, 1161,
      115, 1161,  115, 1161,  115, 1161, 1161, 1161, 1161, 1161,
      115, 1161,  115, 1161,  115,  115,  783, 1161,  115,  115,

      784, 1161,  115,  787, 1161,  115,  785,  115,  115,  115,
      115, 1161,  115,  786,  115, 1161, 1161,  115, 1161, 1161,
      115, 1161,  788, 1161,  115,  115, 1161,  115,  115,  789,
      115, 1161,  787, 1161,  115, 1161,  115, 1161,  115,  115,
     1161,  115,  786,  115,  115,  115, 1161, 1161,  115, 1161,
      788, 1161,  115,  115,  115,  115,  115,  115,  789,  115,
     1161,  790,  115, 1161, 1161,  115, 1161,  115,  115,  792,
      115, 1161,  115,  115,  115, 1161,  115,  115, 1161, 1161,
     1161, 1161,  115,  115, 1161, 1161,  115,  791,  115, 1161,
      790,  115, 1161, 1161,  115,  115,  793,  115,  792,  115,

      115,  115,  115,  115,  115,  115,  115, 1161, 1161, 1161,
     1161, 1161, 1161,  115, 1161,  791,  115, 1161,  795,  115,
      796, 1161, 1161,  115, 1161,  793,  115, 1161,  115,  115,
      115,  115, 1161, 1161,  115,  115, 1161,  115, 1161,  797,
      798,  115,  115, 1161, 1161,  115,  795,  115,  796,  115,
     1161, 1161, 1161,  115,  115,  115,  115,  115,  115, 1161,
      115, 1161,  115, 1161, 1161, 1161,  115,  797,  799,  798,
     1161,  115,  801,  115,  115, 1161,  115,  800,  115,  115,
      115, 1161,  115,  115,  115,  115,  115,  115, 1161, 1161,
     1161,  115, 1161,  115, 1161, 1161,  799, 1161, 1161,  115,

      801, 1161, 1161,  115,  115,  800, 1161, 1161,  115,  115,
     1161, 1161,  176, 1161,  115, 1161, 1161, 1161,  176,  115,
     1161,  115, 1161,  816,  115, 1161,  110,  115,  111,  111,
      111,  111,  111,  111,  111,  111,  111,  111,  115,  115,
      115,  115, 1161, 1161,  115,  115,  819,  817,  115, 1161,
     1161,  818,  816,  115, 1161, 1161, 1161, 1161,  115,  115,
      115, 1161, 1161, 1161, 1161, 1161,  115, 1161,  115,  115,
      115, 1161,  211,  115,  115,  819,  817, 1161, 1161,  818,
     1161, 1161,  211, 1161, 1161, 1161,  115,  115,  115,  710,
      710,  710,  710,  710,  710,  710,  710,  710,  710,  710,

      710,  710,  710,  710,  710,  710,  710,  710,  710,  211,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,  211,
     1161, 1161, 1161, 1161, 1161, 1161,  712,  712,  712,  712,
      712,  712,  712,  712,  712,  712,  712,  712,  712,  712,
      712,  712,  712,  712,  712,  712,  823,  823,  211,  823,
      823,  823,  823,  823,  823,  823,  823,  823,  823,  823,
      823,  823,  823,  828,  823,  829,  829,  829,  829,  829,
      829,  829,  829,  829,  829,  823,  823,  823,  823,  823,
      830,  830,  830,  830,  830,  830,  830,  830,  830,  830,
      830,  830,  830,  830,  830,  830,  830,  830,  830,  830,

      830,  830,  830,  830,  830,  830,  823,  823,  830,  830,
      830,  830,  830,  830,  830,  830,  830,  830,  830,  830,
      830,  830,  830,  830,  830,  830,  830,  830,  830,  830,
      830,  830,  830,  830,  823,  832,   77,  832, 1161, 1161,
      833,  833, 1161,  833, 1161, 1161, 1161,  833,  833, 1161,
     1161,  833, 1161, 1161,  833,  833,  833,  833,  833,  833,
      833,  833,  833,  831,  831, 1161,  831,  831,  831,  831,
      831,  831,  831,  831,  831,  831,  831,  831,  831,  831,
      841,  831,  842,  842,  842,  842,  842,  842,  842,  842,
      842,  842,  831,  831,  831,  831,  831,  843,  843,  843,

      843,  843,  843,  843,  843,  843,  843,  843,  843,  843,
      843,  843,  843,  843,  843,  843,  843,  843,  843,  843,
      843,  843,  843,  831,  831,  843,  843,  843,  843,  843,
      843,  843,  843,  843,  843,  843,  843,  843,  843,  843,
      843,  843,  843,  843,  843,  843,  843,  843,  843,  843,
      843,  831,  115, 1161, 1161,  258, 1161,  115,  257,  257,
      257,  257,  257,  257,  257,  257,  257,  257,  115,  845,
     1161,  844,  115,  115,  115,  115, 1161,  115, 1161, 1161,
      115,  115, 1161, 1161, 1161, 1161,  115,  115,  115, 1161,
     1161,  115, 1161, 1161,  846,  115, 1161,  115,  845,  844,

      115,  115,  115,  115,  115, 1161,  115,  115, 1161,  115,
     1161, 1161,  115, 1161,  115,  115,  115, 1161,  115,  115,
     1161, 1161,  846,  115,  115, 1161,  115, 1161, 1161,  115,
      847, 1161, 1161, 1161, 1161, 1161,  115,  115,  848, 1161,
     1161,  115,  115,  115, 1161,  849, 1161,  115,  115, 1161,
      115,  115,  115, 1161,  115,  115,  115, 1161,  847,  850,
     1161,  852,  115,  852, 1161,  115,  848, 1161, 1161,  115,
      115, 1161,  115,  115,  849, 1161, 1161,  115,  115,  115,
      115, 1161,  115, 1161,  115,  115,  855,  115,  850, 1161,
      115, 1161,  115, 1161,  115, 1161,  853,  115,  115,  115,

     1161,  115,  115, 1161, 1161, 1161, 1161,  115, 1161,  854,
     1161,  115, 1161,  115,  115,  855,  115, 1161,  858,  115,
      115, 1161, 1161,  115, 1161,  853,  115, 1161,  115,  115,
     1161,  115, 1161,  115,  115, 1161,  866,  854, 1161,  115,
     1161,  115, 1161,  115, 1161,  115,  867,  858,  115, 1161,
     1161,  115, 1161,  115, 1161,  115,  115,  868, 1161, 1161,
      115,  115, 1161,  115, 1161,  866, 1161, 1161,  115, 1161,
      115,  115,  115,  115,  867,  869,  115,  115,  115, 1161,
      115,  115,  871,  115, 1161,  115,  868, 1161, 1161,  870,
      115,  115, 1161, 1161,  874, 1161, 1161,  115,  115,  115,

      115,  115, 1161,  115,  869,  115,  115,  115,  115, 1161,
      115,  871,  115,  115,  875,  115,  875,  870,  115,  115,
      872,  873,  115,  874,  876,  115,  115, 1161,  115,  115,
     1161,  115,  115, 1161, 1161, 1161, 1161,  115, 1161,  115,
      877,  115, 1161, 1161,  115,  115,  115,  115,  872,  873,
      115,  115,  115, 1161,  878,  115, 1161,  115,  115, 1161,
      115, 1161,  879,  115, 1161,  115,  115,  115,  877,  115,
     1161,  880,  115,  115,  115,  115,  115,  115, 1161, 1161,
      115,  115, 1161,  878, 1161, 1161,  115,  115,  115, 1161,
      115,  879,  115,  115,  115,  115,  115, 1161,  115,  115,

      880,  115, 1161,  115,  115,  115, 1161,  881, 1161,  115,
      115, 1161, 1161, 1161,  115,  115,  115,  882,  115,  115,
     1161,  883,  115, 1161,  115, 1161, 1161,  115,  115,  115,
     1161,  115, 1161,  115, 1161,  881,  115,  115, 1161,  115,
     1161,  115,  115,  115,  115,  882,  115,  115,  115,  883,
      115,  115, 1161,  884, 1161,  884,  115,  115, 1161, 1161,
      115,  115,  115,  115, 1161,  115, 1161, 1161,  115,  115,
     1161,  115,  115, 1161,  115, 1161,  115,  115,  115,  115,
     1161,  886,  115,  888, 1161, 1161,  115, 1161,  115,  115,
      115,  115,  115,  115,  115, 1161, 1161,  115,  887,  115,

     1161,  885,  115,  115, 1161,  115, 1161, 1161,  115,  886,
      115, 1161,  888,  889, 1161,  115, 1161,  115, 1161,  115,
      115,  115, 1161,  115,  115, 1161,  887,  115,  115,  885,
      115,  891,  115,  115, 1161, 1161,  890, 1161,  115,  115,
     1161,  889, 1161, 1161,  115, 1161,  115, 1161,  115, 1161,
     1161,  892, 1161,  115, 1161, 1161,  115, 1161,  115, 1161,
      891,  115,  115, 1161,  890, 1161,  115,  115,  115, 1161,
      115,  115, 1161,  115,  115,  115,  115, 1161,  895,  892,
     1161,  115,  115, 1161, 1161, 1161,  115,  115, 1161,  115,
      115,  115, 1161,  115, 1161,  896,  115,  896,  115,  115,

      115,  115, 1161, 1161,  115,  115, 1161,  895, 1161,  115,
     1161,  115,  115, 1161, 1161, 1161,  115,  115,  115,  115,
      115, 1161,  115, 1161,  115,  115,  115,  115,  115,  115,
      897,  906, 1161,  115, 1161, 1161, 1161, 1161,  898,  115,
      115, 1161, 1161, 1161, 1161,  115, 1161,  115,  115,  115,
     1161,  115, 1161,  115,  115,  115,  115,  115, 1161,  897,
      906, 1161,  115, 1161,  907, 1161,  898,  115, 1161,  908,
      115,  908, 1161,  115,  258,  115,  802,   77,  804,  175,
      115, 1161, 1161, 1161, 1161,  115, 1161, 1161, 1161, 1161,
      909,  115,  907, 1161, 1161,  115,  115, 1161,  115, 1161,

     1161, 1161,  115, 1161, 1161, 1161, 1161,  115, 1161,  115,
      115,  177,  178, 1161, 1161,  179, 1161,  180, 1161,  909,
      115,  115, 1161, 1161,  115,  115, 1161, 1161,  341,  182,
     1161,  115, 1161, 1161, 1161, 1161,  115,  115,  115, 1161,
      177,  178, 1161, 1161,  179, 1161,  180, 1161, 1161,  115,
     1161, 1161, 1161, 1161, 1161, 1161,  341,  182,  912,  912,
      211,  912,  912,  912,  912,  912,  912,  912,  912,  912,
      912,  912,  912,  912,  912,  914,  912,  915,  915,  915,
      915,  915,  915,  915,  915,  915,  915,  912,  912,  912,
      912,  912,  916,  916,  916,  916,  916,  916,  916,  916,

      916,  916,  916,  916,  916,  916,  916,  916,  916,  916,
      916,  916,  916,  916,  916,  916,  916,  916,  912,  912,
      916,  916,  916,  916,  916,  916,  916,  916,  916,  916,
      916,  916,  916,  916,  916,  916,  916,  916,  916,  916,
      916,  916,  916,  916,  916,  916,  912,  211,  257,  257,
      257,  257,  257,  257,  257,  257,  257,  257, 1161, 1161,
     1161, 1161, 1161, 1161,  383,  383,  383,  383,  383,  383,
      383,  383,  383,  383,  374,  374,  211,  374,  374,  374,
      374,  374,  374,  374,  374,  374,  374,  374,  374,  374,
      374,  384,  374,  385,  385,  385,  385,  385,  385,  385,

      385,  385,  385,  374,  374,  374,  374,  374,  386,  386,
      386,  386,  386,  386,  386,  386,  386,  386,  386,  386,
      386,  386,  386,  386,  386,  386,  386,  386,  386,  386,
      386,  386,  386,  386,  374,  374,  386,  386,  386,  386,
      386,  386,  386,  386,  386,  386,  386,  386,  386,  386,
      386,  386,  386,  386,  386,  386,  386,  386,  386,  386,
      386,  386,  374,  115, 1161,  917, 1161,  115,  115,  918,
      115,  115,  115,  919,  115, 1161,  115, 1161,  920,  115,
     1161, 1161,  115, 1161,  115, 1161,  115, 1161, 1161, 1161,
      115, 1161,  115,  115,  917, 1161,  115,  115,  918,  115,

      115,  115,  919,  115,  922,  115, 1161,  920,  115,  115,
      115,  115,  115, 1161,  115,  921,  115, 1161,  115, 1161,
      115,  115, 1161,  115, 1161,  115, 1161,  115, 1161, 1161,
      115,  115,  115,  922,  923, 1161,  115, 1161,  115,  115,
      115,  927, 1161, 1161,  921,  115,  115, 1161,  928,  115,
      115,  115,  115, 1161,  115, 1161,  115,  115,  115, 1161,
      115,  115,  923,  115, 1161,  115, 1161,  115,  115,  927,
     1161,  115,  115, 1161,  115, 1161,  928,  115,  115, 1161,
      930,  115,  115,  115, 1161, 1161,  115, 1161,  115, 1161,
     1161,  938,  115, 1161,  939, 1161, 1161,  115, 1161,  115,

     1161,  115,  115, 1161,  115, 1161,  115, 1161,  930,  115,
      115, 1161,  115,  115, 1161,  941, 1161,  115,  115,  938,
     1161,  940,  939,  115, 1161, 1161, 1161, 1161, 1161,  115,
      115, 1161,  115,  115,  115,  115, 1161, 1161,  115, 1161,
      115, 1161,  115,  942,  941, 1161, 1161,  115,  115,  940,
     1161,  115, 1161, 1161,  115, 1161, 1161, 1161,  115, 1161,
      115,  115, 1161,  115,  115,  943,  115, 1161,  115,  115,
      115,  942,  944,  115, 1161,  115,  115, 1161, 1161, 1161,
      115,  945,  115, 1161, 1161, 1161, 1161,  115, 1161,  115,
      115,  115, 1161,  115,  943,  115,  115,  115,  115,  115,

     1161,  944,  115, 1161,  115, 1161,  951, 1161,  115,  945,
      952, 1161,  115,  115,  115,  115,  115,  115,  115,  115,
      115,  115,  115, 1161, 1161,  115, 1161,  115, 1161, 1161,
     1161, 1161,  115,  115,  951,  115,  115, 1161,  952, 1161,
      115,  115,  115,  115,  953,  115, 1161,  115,  115, 1161,
      115, 1161,  115,  115, 1161,  115, 1161,  115,  115,  115,
      115,  115, 1161,  115,  115,  115, 1161, 1161, 1161, 1161,
      115,  115,  956,  953, 1161, 1161,  955, 1161,  115,  115,
     1161,  115,  115,  115,  115, 1161,  115,  115,  115, 1161,
      115,  115, 1161,  115, 1161,  115,  115, 1161,  115,  115,

      956,  957,  115, 1161,  955, 1161,  115,  115,  115,  115,
      115,  115, 1161,  115, 1161,  958,  115, 1161, 1161,  115,
      115,  115,  115,  962,  115,  115,  115,  115,  115,  957,
      115,  115, 1161, 1161,  115, 1161,  115,  115,  115,  115,
      115,  115,  115,  969,  958,  115, 1161, 1161,  115,  115,
     1161,  115,  962,  115, 1161,  960,  115,  115,  115,  115,
     1161, 1161,  115,  115,  115, 1161,  115,  963,  115,  115,
      115,  115,  969, 1161, 1161, 1161, 1161,  115,  115, 1161,
     1161,  115, 1161,  960,  970,  115, 1161, 1161,  115,  115,
      115,  971, 1161,  115,  115,  115,  963, 1161, 1161, 1161,

      115, 1161, 1161, 1161, 1161, 1161,  115,  211,  115, 1161,
     1161, 1161,  970,  115, 1161, 1161,  115,  975,  115, 1161,
      971,  115, 1161,  115,  383,  383,  383,  383,  383,  383,
      383,  383,  383,  383, 1161,  115,  115,  115, 1161,  115,
     1161, 1161,  115,  976,  115,  115,  975, 1161,  115, 1161,
      115, 1161, 1161,  115, 1161, 1161,  115, 1161,  115, 1161,
     1161, 1161, 1161,  115, 1161,  977,  115,  115,  115,  115,
      115,  115,  976,  115,  115,  115, 1161,  115,  115, 1161,
     1161, 1161,  115,  115,  115, 1161,  115, 1161,  115,  978,
     1161,  979, 1161,  977, 1161,  115, 1161,  115,  115,  115,

     1161,  115, 1161,  115,  115,  115,  115,  115, 1161,  983,
      115, 1161,  115,  982, 1161, 1161,  115,  978, 1161,  979,
      115,  115, 1161, 1161,  115,  115,  115,  115,  994,  992,
      115,  985,  115,  115,  115,  115, 1161, 1161,  983,  115,
      115,  982, 1161, 1161, 1161, 1161,  115,  115,  115, 1161,
      115, 1161,  115, 1161, 1161,  115,  115,  994,  992,  115,
      985,  115,  115,  993,  115,  115,  115, 1161,  115, 1161,
      115,  115, 1161, 1161,  115,  115, 1161, 1161,  115, 1161,
     1161, 1161,  115, 1161,  995,  115,  996,  115,  115, 1161,
     1161,  115,  993,  115,  115,  115,  115, 1161, 1161,  115,

      115,  115,  115, 1161,  997, 1161,  115,  115, 1161, 1161,
      115,  115,  995,  115,  996, 1161,  115, 1161,  115, 1161,
      115,  115,  115,  115, 1161,  115, 1161,  115, 1161,  115,
     1161,  115,  997, 1161, 1161, 1161,  115,  115,  115,  115,
     1161,  115, 1161,  115,  115, 1004, 1161,  115,  115,  115,
     1161,  115,  115, 1006, 1161, 1161,  115, 1161,  115,  115,
     1161, 1161,  115, 1161,  115,  115,  115, 1161,  115,  115,
      115,  115,  115,  115, 1004,  115, 1161,  115,  115, 1161,
     1161, 1006, 1161, 1161, 1161,  115,  115, 1007,  115,  115,
      115, 1161,  115,  115, 1161,  115, 1009,  115, 1161,  115,

      115, 1161, 1161,  115,  115, 1161,  115, 1161,  115,  115,
     1161,  115, 1161,  115,  115, 1007, 1012,  115, 1161, 1161,
     1161,  115,  115, 1161, 1161, 1009,  115, 1011,  115,  115,
     1161, 1161,  115, 1161,  115,  115, 1161,  115,  115,  115,
      115,  115, 1161,  115, 1012, 1020,  115, 1161,  115, 1019,
      115, 1161,  115, 1161,  115, 1011,  115,  115,  115, 1161,
      115,  115, 1161,  115,  115, 1161,  115, 1161,  115,  115,
      115,  115, 1025,  115, 1020,  115,  115, 1019, 1161, 1161,
      115,  115,  115, 1161,  115, 1161,  115,  115,  115,  115,
      115, 1161, 1161,  115, 1161,  115, 1161,  115,  115,  115,

     1025,  115,  115,  115,  115, 1026,  115, 1161,  115,  115,
     1161, 1161,  115,  115, 1161,  115, 1161, 1161,  115, 1029,
      115, 1161,  115,  115,  115, 1161, 1030,  115, 1161,  115,
      115,  115, 1161,  115, 1026,  115, 1161, 1161,  115, 1161,
      115,  115, 1161,  115,  115,  115,  115, 1029,  115,  115,
      115,  115, 1161,  115, 1161, 1030, 1038, 1161,  115, 1037,
      115,  115, 1161,  115,  115, 1161,  115,  115, 1161, 1161,
      115,  115,  115,  115,  115,  115, 1161, 1161,  115,  115,
      115, 1161, 1161, 1161, 1038, 1161, 1039, 1037,  115, 1161,
      115,  115,  115,  115, 1161,  115,  115,  115,  115, 1040,

      115,  115,  115, 1161,  115,  115, 1044, 1161,  115,  115,
     1161, 1161,  115, 1161, 1039, 1161,  115, 1161, 1161,  115,
      115, 1161,  115,  115, 1161,  115,  115,  115, 1040,  115,
     1161,  115, 1161,  115,  115, 1044, 1161,  115,  115,  115,
      115, 1047,  115, 1161,  115, 1161, 1161,  115, 1161,  115,
     1161,  115, 1161,  115,  115, 1161,  115, 1048,  115, 1161,
      115, 1161, 1161, 1161,  115,  115,  115,  115, 1161,  115,
     1047,  115,  115, 1161, 1161, 1054,  115, 1161, 1161,  115,
     1059,  115,  115,  115,  115,  115, 1048,  115, 1161,  115,
      115, 1161, 1161,  115,  115,  115, 1161,  115,  115, 1161,

      115, 1161,  115, 1054,  115,  115, 1161,  115, 1062, 1059,
      115,  115, 1161, 1161,  115, 1161,  115, 1161, 1161,  115,
      115, 1161,  115,  115,  115,  115,  115,  115, 1063,  115,
     1161,  115,  115, 1161, 1161, 1067, 1062, 1161, 1161,  115,
     1161,  115, 1161,  115,  115, 1161, 1161,  115, 1161,  115,
     1161,  115,  115,  115,  115, 1068,  115, 1063,  115,  115,
     1161, 1161, 1161, 1067,  115, 1161,  115,  115,  115,  115,
      115,  115, 1069,  115, 1073,  115,  115, 1161,  115, 1161,
     1161,  115, 1161, 1161, 1068, 1161, 1161,  115,  115,  115,
      115,  115, 1161,  115,  115,  115,  115,  115, 1161,  115,

     1161, 1069,  115, 1073,  115, 1161,  115,  115, 1161,  115,
     1080, 1161,  115, 1161, 1161,  115,  115,  115, 1161,  115,
      115,  115, 1161,  115,  115,  115,  115, 1087,  115, 1161,
     1161, 1161, 1161, 1161, 1088,  115,  115,  115, 1080,  115,
     1161,  115,  115, 1161,  115,  115,  115, 1161,  115, 1161,
      115,  115,  115,  115,  115, 1161, 1087,  115,  115, 1091,
     1161, 1161, 1088,  115, 1161,  115, 1161,  115,  115,  115,
      115,  115, 1095,  115,  115,  115,  115,  115, 1161, 1161,
      115, 1161,  115, 1161, 1161, 1161,  115, 1091,  115, 1161,
      115,  115, 1161,  115,  115,  115,  115, 1161,  115,  115,

      115, 1095, 1106,  115, 1161,  115, 1161, 1161, 1161, 1161,
      115, 1161, 1161,  115, 1105, 1161,  115,  115,  115, 1161,
      115, 1108,  115,  115,  115,  115, 1161, 1161,  115,  115,
     1161, 1106, 1161, 1161, 1161, 1161,  115, 1161,  115, 1161,
      115,  115, 1105,  115, 1112,  115,  115,  115, 1161,  115,
     1108,  115,  115,  115,  115, 1161, 1161, 1161,  115,  115,
     1161, 1122, 1161, 1161,  115, 1161,  115, 1161,  115,  115,
     1161,  115, 1112, 1161,  115, 1161,  115, 1161,  115, 1161,
     1161,  115, 1161,  115, 1161, 1161, 1161,  115, 1161, 1122,
     1161, 1161, 1161, 1161,  115, 1161,  115, 1124,  948, 1124,

     1161, 1161, 1125, 1125, 1161, 1125, 1161, 1161, 1161, 1125,
     1125, 1161, 1161, 1125, 1161, 1161, 1125, 1125, 1125, 1125,
     1125, 1125, 1125, 1125, 1125,  115,  115, 1161, 1161, 1161,
      115,  115, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1133,
     1161, 1161, 1128, 1161,  115,  115, 1161, 1161, 1161, 1161,
     1161, 1161, 1161, 1161,  115,  115, 1161, 1161, 1161,  115,
      115, 1161, 1161, 1161, 1161, 1161, 1161, 1133, 1161, 1161,
     1128, 1161,  115,  115,  947,  948,  947, 1161, 1161, 1134,
     1134, 1161, 1135, 1161,  115,  950, 1134, 1134, 1137,  115,
     1134, 1161, 1161, 1134, 1134, 1134, 1134, 1134, 1134, 1134,

     1134, 1134, 1161,  115,  115,  115, 1161, 1161, 1161,  115,
      115, 1161, 1161,  115, 1161, 1161, 1161, 1137,  115, 1161,
     1161, 1161, 1161,  115,  115, 1161, 1161, 1161, 1161, 1161,
     1161,  115, 1161,  115,  115, 1161, 1161, 1161,  115,  115,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161,  115,  115,   64,   64,   64,   64,   64,   64,   64,
       64,   64,   64,   64,   64,   65,   65,   65,   65,   65,
       65,   65,   65,   65,   65,   65,   65,   81,   81, 1161,
       81,   81,   81,   81,   81,   81,   81,   81,   81,   84,
       84, 1161,   84,   84,   84,   84,   84,   84,   84,   84,

       84,   88,   88,   88,   88,   88,   88,   88,   88,   88,
       88,   88,   88,   92,   92,   92,   92,   92,   92,   92,
       92,   92,   92,   92,   92,  111,  111,  115,  115,  115,
      115,  115,  115,  115,  115,  187,  187,  187,  187,  187,
      187,  187,  187,  187,  187,  187,  187,  202,  202,  202,
      202,  202,  202,  202,  202,  202,  202,  202,  202,  209,
      209,  209,  209,  209,  209, 1161,  209,  209,  209, 1161,
      209,  232,  232,  232,  232,  232,  232,  232,  232,  232,
      232,  232,  232,  101, 1161, 1161,  101,  252,  252,  252,
     1161, 1161,  252, 1161,  252,  252, 1161,  252,  252,  343,

      343,  343,  343,  343,  343,  343,  343,  343,  343,  343,
      343,  358,  358, 1161,  358,  358,  358,  358,  358,  358,
      358,  358,  358,  371,  371,  371,  371,  371,  371, 1161,
      371,  371,  371, 1161,  371,  374,  374,  374,  374,  374,
      374,  374,  374,  374,  374,  374,  374,  379,  379,  379,
      379,  379,  379,  379,  379,  379,  379,  379,  379,  422,
     1161, 1161,  422,  276, 1161, 1161, 1161,  276, 1161, 1161,
     1161, 1161, 1161,  276,  558,  558,  558,  558,  558,  558,
      558,  558,  558,  558,  558,  558,  718,  718, 1161,  718,
      718,  718,  718,  718,  718,  718,  718,  718,  721,  721,

     1161,  721,  721,  721,  721,  721,  721,  721,  721,  721,
      723,  723, 1161,  723,  723,  723,  723,  723,  723,  723,
      723,  723,   88,   88,   88,   88,   88,   88,   88,   88,
       88,   88,   88,   88,  726,  726,  726,  726,  726,  726,
      726,  726,  726,  726,  726,  726,   92,   92,   92,   92,
       92,   92,   92,   92,   92,   92,   92,   92,  728,  728,
      728,  728,  728,  728,  728,  728,  728,  728,  728,  728,
      812,  812,  812,  812,  812,  812,  812,  812,  812,  812,
      812,  812,  814,  814, 1161,  814,  814,  814,  814,  814,
      814,  814,  814,  814,  823,  823,  823,  823,  823,  823,

      823,  823,  823,  823,  823,  823,  825,  825,  825,  825,
      825,  825,  825,  825,  825,  825,  825,  825,  831,  831,
     1161,  831,  831,  831,  831,  831,  831,  831,  831,  831,
      834,  834, 1161,  834,  834,  834,  834,  834,  834,  834,
      834,  834,   81,   81, 1161,   81,   81,   81,   81,   81,
       81,   81,   81,   81,  835,  835, 1161,  835,  835,  835,
      835,  835,  835,  835,  835,  835,   84,   84, 1161,   84,
       84,   84,   84,   84,   84,   84,   84,   84,  837,  837,
      837,  837,  837,  837,  837,  837,  837,  837,  837,  837,
       88,   88,   88,   88,   88,   88,   88,   88,   88,   88,

       88,   88,  838,  838,  838,  838,  838,  838,  838,  838,
      838,  838,  838,  838,   92,   92,   92,   92,   92,   92,
       92,   92,   92,   92,   92,   92,  187,  187,  187,  187,
      187,  187,  187,  187,  187,  187,  187,  187,  904,  904,
      904,  904,  904,  904,  904,  904,  904,  904,  904,  904,
      358,  358, 1161,  358,  358,  358,  358,  358,  358,  358,
      358,  358,  814,  814, 1161,  814,  814,  814,  814,  814,
      814,  814,  814,  814,  202,  202,  202,  202,  202,  202,
      202,  202,  202,  202,  202,  202,  912,  912,  912,  912,
      912,  912,  912,  912,  912,  912,  912,  912,  374,  374,

      374,  374,  374,  374,  374,  374,  374,  374,  374,  374,
      379,  379,  379,  379,  379,  379,  379,  379,  379,  379,
      379,  379,  825,  825,  825,  825,  825,  825,  825,  825,
      825,  825,  825,  825,  946,  946,  946,  946,  946,  946,
      946,  946,  946,  946,  946,  946,  999,  999,  999,  999,
      999,  999,  999,  999,  999,  999,  999,  999,   13, 1161,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,

     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161
    } ;

static yyconst flex_int16_t yy_chk[9249] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    2,
        2,    2,    2,   10,    8,    8,    8,    8,   15,   15,

       15,   68, 1129,   14,   10, 1119,   68,    2,    2,    2,
        2,    2,    2,    2,    2,    2,    2,   14,   16,   16,
       16,   54,   54,   54,    2,    2,   18,   10,    2,   19,
        2,    8,   14,   17,   17,   17,   15,   20,   21, 1096,
       18,    2,    2,  176,   22,   14, 1084,   25,   26,  176,
       15,   20,   21,    2,    2,   18,   10,    2,   75,    2,
        8,   25,   42,   19,   69,   15,   20,   21,   18,    2,
        2,   17,   24,   27,   26,   23,   25,   19,   15,   20,
       21,   22,   23,   23,   29,   17,   75,   27,   26,   25,
     1045,   42,   19,   24,  959,   22,   30,  339,   74,  339,

       17,   82,   27,   26,  340,   19,  340,   24,   83,   69,
       22,   29,   23,   17,   31,   27,   26,   32,   30,   34,
       41,  178,   24,   22,  916,   29,   23,   74,   31,  912,
       82,   32,   30,   34,   41,   24,   83,   69,   49,  685,
       29,   23,   49,   31,  905,  685,   32,   30,   34,   41,
      178,  207,   33,   29,   23,   28,   31,   28,   33,   32,
       30,   34,   41,   76,   76,   76,  242,   49,   67,   67,
       67,   49,   33,   28,   28,   28,   28,   28,   28,   28,
       28,   28,   28,   86,   43,   63,  207,   33,   28,   43,
       44,  350,   28,   28,   28,  242,   43,   43,  904,   28,

       33,   28,   28,   63,   44,   67,   91,   28,   77,   77,
       77,   28,   86,   43,   63,  207,  837,   28,   43,   44,
      350,   28,   28,   28,   43,   43,   87,   28,  360,   28,
       28,   63,   44,  835,   67,   28,   78,   78,   78,   28,
       35,   86,   35,   35,   35,   35,   35,   35,   35,   35,
       35,   35,   38,   40,  830,   91,   38,  360,   35,   35,
       38,   40,   35,  903,   40,  102,   38,   40,   35,  103,
       38,  107,   87,   40,   90,   35,   35,   79,   79,   79,
      825,   38,   40,   91,  342,   38,  342,   35,   35,   38,
       40,   35,   40,  102,   38,   40,   35,  103,   38,  107,

       87,   40,  108,   35,   35,   36,   90,   36,   36,   36,
       36,   36,   36,   36,   36,   36,   36,  179,   37,  359,
       39,  359,   36,  903,  823,   37,   45,   36,  822,   37,
      108,   39,   37,   39,   45,   90,   39,   39,   46,  205,
       45,   36,   37,   39,  812,  179,   46,   37,  393,   39,
      180,   36,   46,  182,   37,   45,   36,   37,   46,   39,
       37,   39,   48,   45,   39,   39,   51,   46,   45,   36,
       37,   39,   47,   51,  811,   46,   48,  393,  180,   48,
       46,  182,   48,   51,  205,   47,   46,   47,  104,   50,
      220,   48,   47,   50,  529,   51,  529,   50,  396,   52,

      450,   47,   51,  104,   48,   50,   52,   48,   50,   50,
       48,   51,  205,   47,  532,   47,   52,  104,   50,  804,
       47,  220,   50,   80,   80,   80,   50,  396,   52,  450,
      530,  104,  530,   50,   92,   52,   50,   50,   55,   55,
       55,   92,   92,  532,   52,   53,   53,   53,   53,  220,
       56,  223,  531,   53,  531,   56,   55,   55,   55,   55,
       55,   55,   55,   55,   55,   55,  184,  184,  184,   94,
       70,   70,   70,   71,   71,   71,   94,   94,   60,  223,
       53,   53,   70,   53,   53,   71,   53,   56,  726,   60,
      106,  191,   60,  686,   93,   93,   93,   53,   53,  686,

      106,   56,   93,   93,  181,   70,  725,   60,   71,   53,
       53,  181,   53,   53,  723,   53,   56,   60,  722,  106,
       60,   72,   72,   72,  910,   53,   53,  214,  106,   56,
       57,   58,  181,   72,   70,   96,   57,   71,  222,  181,
      191,  716,   96,   96,   57,  189,   57,   57,   57,   57,
       57,   57,   57,   57,   57,   57,   59,  222,   58,   72,
       59,  208,  208,  208,   59,   58,   57,   96,  191,   58,
       59,  190,   58,   72,   59,  714,  214,  189,   61,  206,
       57,  706,   58,   61,  910,   59,  222,   58,   72,   59,
       61,   61,   62,   59,   58,   57,   96,   58,   59,   62,

       58,   72,   59,  190,  214,  224,  189,   61,   57,   62,
       58,  206,   61,  210,  210,  210,   62,  689,   61,   61,
      664,   62,  225,  225,  225,  585,  580,  232,   62,  578,
      558,  235,  190,  224,  232,  232,  554,   62,  235,  235,
      206,  372,  372,  372,   62,   73,   73,   73,   73,   73,
       73,   73,   73,   73,   73,   73,   73,   73,   73,   73,
       73,   73,   73,   73,   73,   73,   73,   73,   73,   73,
       73,   73,   73,   73,   73,   73,   73,   73,   73,   73,
       73,   73,   73,   73,   73,   73,   73,   73,   73,   73,
       73,   73,   73,   73,   73,   73,   73,   73,   73,   73,

       73,   73,   73,   73,   73,   73,   73,   73,   73,   73,
       73,   73,   73,   73,   73,   73,   73,   73,   73,   73,
       73,   73,   73,   73,   73,   73,   73,   73,   73,   73,
       73,   73,   73,   73,   97,  101,  101,  105,  204,  204,
      204,   97,   97,  226,  538,  227,  213,  675,  101,  675,
      534,  111,  105,  111,  111,  111,  111,  111,  111,  111,
      111,  111,  111,  421,  101,  101,  105,  375,  375,  375,
      419,  226,  173,  227,  173,  204,  101,  114,  213,  534,
      105,  219,  412,   97,  113,  113,  113,  113,  113,  113,
      113,  113,  113,  113,  114,  228,  115,  118,  219,  114,

      116,  115,  118,  125,  204,  116,  410,  213,  125,  231,
      173,   97,  100,  114,  100,  115,  118,  229,  116,  116,
      386,  221,  125,  114,  173,  115,  118,  219,  114,  116,
      115,  118,  125,  379,  116,  228,  249,  125,  230,  173,
      374,  114,  251,  115,  118,  100,  116,  116,  117,  100,
      125,  100,  173,  117,  243,  221,  100,  276,  100,  100,
      248,  231,  117,  228,  249,  401,  229,  117,  100,  294,
      251,  294,  401,  401,  100,  371,  343,  117,  100,  294,
      100,  230,  117,  221,  100,  276,  100,  100,  535,  231,
      117,  250,  243,  248,  229,  117,  100,  110,  110,  110,

      110,  110,  110,  110,  110,  110,  110,  119,  121,  230,
      913,  247,  119,  121,  120,  110,  110,  535,  119,  120,
      243,  248,  120,  246,  400,  121,  119,  121,  110,  245,
      233,  233,  233,  120,  341,  277,  119,  121,  233,  233,
      344,  119,  121,  120,  110,  110,  119,  122,  120,  122,
      244,  120,  122,  121,  119,  121,  110,  177,  123,  218,
      400,  120,  341,  123,  212,  345,  122,  346,  344,  354,
      913,  177,  277,  347,  209,  123,  122,  123,  122,  124,
      202,  122,  348,  126,  124,  187,  177,  123,  126,  400,
      124,  349,  123,  345,  122,  346,  183,  126,  124,  177,

      277,  347,  126,  123,  676,  123,  676,  354,  124,  127,
      348,  175,  126,  124,  127,  174,  237,  126,  124,  349,
       89,  127,  127,  237,  237,  126,  124,  127,  127,  128,
      126,  128,  129,  389,  128,  130,  354,  129,  127,  292,
      130,  292,  361,  127,  234,  234,  234,  130,  128,  127,
      127,  129,  234,  234,  130,  127,  127,  536,  128,  237,
      128,  129,   88,  128,  130,  388,  129,   84,  389,  130,
      361,  380,  380,  380,  356,  130,  128,   65,   13,  129,
      378,  238,  130,  131,  355,  131,  536,  237,  238,  238,
      369,  292, 1051,  388,  390,  133,  389,  392,    9,  131,

      133,  131,  131,  131,  131,  131,  131,  131,  131,  131,
      131,  132, 1081,  133,  133,  134,  132,    7,  134,  292,
      134,  388,  132,  398,  133,  392,  356,  355,  369,  133,
      132,  390,  378,  238,  134,  677,  131,  677,  131, 1130,
      132,  133,  133,  381,  134,  132,  136,  134,  135,  134,
      132,  136, 1051,  135,  356,  355,  398,  369,  132,  390,
      378,  238,  134,  136,  131,  136,  131,  135,  135,  139,
      377,  137, 1081,  138,  139,  136,  137,  135,  138,  137,
      136,  381,  135,  139,  138,  398,  137,  395,  139,    0,
      137,  136,  138,  136,  418,  135,  135,  397,  139, 1130,

      137,  382,  138,  139,  140,  137,  475,  138,  137,  140,
      381,  139,  138,  377,  137,  395,  139,  142,  137,  142,
      138,  140,  418,  140,  141,  399,  143,  144,  143,  141,
     1131,  537,  144,  140,  475,    0,  143,  141,  140,  391,
      533,  377,  144,  141,  397,  382,  144,    0,  543,  140,
      142,  140,  466,  141,  466,  142,  144,  146,  141,  143,
      537,  144,  146,  146,  143,  141,    0,  391,  533,  142,
      144,  141,  397,  382,  144,  399,  146,    0,  143,  142,
      145,    0,  145,  466,  142,  145,  146,  680,  143,  680,
     1131,  146,  146,  143,  145,  391,  145,  142,  543,  145,

      147,    0,  148,  399,  146,  147,  143,  148,  557,  145,
      484,  145,  484,  546,  145,  148,  394,  394,  394,  147,
      484,  148,  145,  147,  145,  149,  543,  145,  149,  147,
      149,  148,  572,  150,  147, 1148,  148,  151,  150,    0,
        0,  546,  151,  148,  149,  150,  547,  147,    0,  148,
      151,  147,  150,  557,  149,    0,  151,  149,    0,  149,
      544,  572,  150,  152,  153,    0,  151,  150,  152,  153,
        0,  151,  149,  150,  547,  153,  152,  488,  151,  488,
      150,  557,  152,  153,  151,    0,  155,  488,    0,  154,
      155,  155,  152,  153,  154, 1148,  544,  152,  153,  521,

      154,  521,    0,  153,  152,  155,  611,    0,  154,  521,
      152,  153,  156,  253,  253,  155,  158,  156,  154,  155,
      155,  158,  402,  154,  158,  544,  253,    0,  154,  402,
      402,  156,  157,  155,  611,  158,  154,  157,  559,  559,
      559,  156,  253,  253,    0,  158,  156,  612,    0,  157,
      158,  157,  157,  158,  253,  160,  160,  159,    0,  156,
      160,  157,  159,  158,  614,  159,  157,  370,  370,  370,
      615,  159,  402,  561,  160,  612,  159,  157,    0,  157,
      157,  194,  162,  194,  160,  160,  159,  162,  161,  160,
      161,  159,  614,  161,  159,  556,  556,  556,  615,  159,

      402,  162,  160,  162,  159,    0,  161,  161,  370,  542,
      161,  162,  575,  163,  194,  564,  162,  161,  163,  161,
      164,  555,  161,  561,  163,  164,  164,  563,  194,  162,
        0,  162,  163,  164,  161,  161,  556,  370,  161,  164,
      165,  575,  163,  194,  568,  165,  692,  163,  692,  164,
      617,  561,  163,  562,  164,  164,  194,  165,  542,  165,
      163,  164,  564,  166,  403,  556,  682,  164,  166,  165,
      555,  403,  403,  577,  165,  569,  563,  166,  617,  568,
      166,    0,  166,  167,  167,  165,  542,  165,  167,  562,
      564,  697,  166,  697,  682,  167,  569,  166,  555,  744,

      403,    0,  167,  570,  563,  166,  577,  568,  166,  168,
      166,  579,  167,  167,  168,  169,    0,  167,  562,  169,
      169,    0,  168,  167,  570,  569,  168,  744,  168,  403,
      167,  571,  170,    0,  169,  577,  753,  170,  168,    0,
      170,  171,    0,  168,  169,  171,  171,  579,  169,  169,
      168,  170,  571,  570,  168,    0,  168,  574,  574,  574,
      171,  170,  169,  172,  753,  581,  170,  604,  172,  170,
      171,  172,  581,  581,  171,  171,  579,  582,  613,  170,
      618,  571,  172,  687,  582,  582,    0,    0,  171,  678,
        0,  678,  172,  185,  185,  185,  604,  172,  756,  678,

      172,  581,  679,  635,  815,  635,  815,  613,  757,  618,
      172,  185,  185,  185,  185,  185,  185,  185,  185,  185,
      185,  186,  681,  687,    0,  678,  756,  186,  713,    0,
      581,  679,  691,  608,  635,  608,  757,  186,  186,  186,
      186,  186,  186,  186,  186,  186,  186,  192,  683,  713,
      683,  681,  687,  192,  678,  684,  684,  684,  683,    0,
      703,  691,  683,  192,  192,  192,  192,  192,  192,  192,
      192,  192,  192,  193,  608,  215,  215,  215,  713,  193,
      708,  708,  708,  748,  826,  826,  826,  193,  608,  193,
      193,  193,  193,  193,  193,  193,  193,  193,  193,  195,

      703,  196,  197,  608,  195,  749,  196,  197,  215,    0,
      195,  616,  748,  196,  196,  688,  608,  195,  195,  196,
      196,  197,  215,  832,  832,  832,  704,  616,  195,  703,
      196,  197,  707,  195,  749,  196,  197,  215,  195,  198,
      616,  196,  196,    0,  198,  195,  195,  196,  196,  197,
      215,  688,  702,  198,  702,  616,  200,    0,  198,  201,
      199,  200,  199,  201,  201,  199,  702,  755,  198,  758,
        0,  200,  707,  198,  199,  200,  199,  704,  201,  199,
      688,  198,  759,  807,    0,  200,  198,    0,  201,  199,
      200,  199,  201,  201,  199,  216,  755,    0,  758,  200,

        0,  707,  199,  200,  199,  704,  201,  199,    0,    0,
      759,  807,  216,  216,  216,  216,  216,  216,  216,  216,
      216,  216,  217,  217,  217,  217,  217,  217,  217,  217,
      217,  217,  217,  217,  217,  217,  217,  217,  217,  217,
      217,  217,  217,  217,  217,  217,  217,  217,  217,  217,
      217,  217,  217,  217,  217,  217,  217,  217,  217,  217,
      217,  217,  217,  217,  217,  217,  217,  217,  217,  217,
      217,  217,  217,  217,  217,  217,  217,  217,  217,  217,
      217,  217,  217,  217,  217,  217,  217,  217,  217,  217,
      217,  217,  217,  217,  217,  217,  217,  217,  217,  217,

      217,  217,  217,  217,  217,  217,  217,  217,  217,  217,
      217,  239,    0,  239,  860,  760,  239,  239,  239,  239,
      239,  239,  239,  239,  239,  239,  240,    0,  240,  863,
      805,  240,  240,  240,  240,  240,  240,  240,  240,  240,
      240,  241,  860,  241,  760,  806,  241,  241,  241,  241,
      241,  241,  241,  241,  241,  241,  254,  863,  254,  805,
        0,  254,  254,  254,  254,  254,  254,  254,  254,  254,
      254,  255,    0,  255,  806,    0,  255,  255,  255,  255,
      255,  255,  255,  255,  255,  255,  256,  808,  256,  259,
      865,  256,  256,  256,  256,  256,  256,  256,  256,  256,

      256,  257,  257,  257,  257,  257,  257,  257,  257,  257,
      257,    0,  259,  769,  260,  769,  808,  259,  865,  260,
      257,  258,  258,  258,  258,  258,  258,  258,  258,  258,
      258,  259,  260,  260,  261,  813,  901,  262,  901,  261,
      258,  259,  262,  260,  769,  261,  259,  727,  260,  257,
      908,  262,  908,  261,  727,  727,  262,  263,  705,  259,
      260,  260,  263,  261,  813,  693,  262,  693,  261,  258,
      263,  262,    0,  261,    0,  693,  263,  264,  266,  262,
      265,  261,  264,  266,  262,  265,  263,  803,  266,  803,
      264,  263,    0,    0,  705,  803,  264,  266,  263,  265,

      265,  693,  851,  268,  263,  810,  264,  266,  268,  265,
      267,  264,  266,    0,  265,  267,  266,    0,  264,    0,
      268,  267,  268,  705,  264,  266,    0,  265,  265,  267,
      693,  851,  268,  269,    0,  271,  821,  268,  269,  267,
      271,  270,  810,    0,  267,  700,  270,  700,  268,  267,
      268,  269,  269,  270,  271,  700,  270,  267,  272,  700,
      270,    0,  269,  272,  271,  272,  728,  269,    0,  271,
      270,  810,  821,  728,  728,  270,    0,  272,  273,  269,
      269,  270,  271,  273,  270,  274,    0,  272,  270,  273,
      274,  729,  272,  794,  272,  794,  274,  273,  729,  729,

        0,  821,    0,  794,  274,  272,    0,  273,  278,  278,
      857,  275,  273,  278,  274,  275,  275,  273,  900,  274,
      859,  809,  809,  809,  274,  273,  820,  278,  279,  809,
      275,  280,  274,  279,  824,  279,  280,  278,  278,  857,
      275,  284,  278,  284,  275,  275,  900,  279,  281,  859,
      280,  861,  281,  281,    0,  278,  862,  279,  275,  864,
      280,  925,  279,  820,  279,  280,    0,  281,  283,    0,
        0,  824,  284,  283,  284,  279,  283,  281,  280,  284,
      861,  281,  281,  838,  875,  862,  875,  283,  864,  925,
      838,  838,  820,  284,  875,  281,  282,  283,  282,  285,

      824,  894,  283,  284,  285,  283,  286,  899,  284,  422,
      422,  286,  839,  902,  893,  283,  893,  285,  285,  839,
      839,  284,  422,    0,  893,  286,    0,  282,  285,  282,
      894,  911,  282,  285,  282,  286,  899,  282,  422,  422,
      286,  282,  902,    0,  282,  285,  285,  282,  282,  289,
      422,  282,  287,  286,  289,  288,  287,  287,  282,  288,
      288,  282,  934,  282,  937,  884,  282,  884,  289,  282,
        0,  287,  282,    0,  288,  282,  282,  926,  289,  282,
      911,  287,  290,  289,  288,  287,  287,  290,  288,  288,
      291,  934,  291,  937,  290,  291,  289,  929,  293,  287,

      931,  290,  288,  293,  896,  926,  896,  932,  911,  291,
        0,  290,  293,  884,    0,  296,  290,  293,  933,  291,
      296,  291,  290,  935,  291,  929,  296,  293,  931,  290,
      961,  297,  293,    0,  296,  932,  297,  291,  298,  896,
      293,  884,  297,  298,  296,  293,  933,    0,  954,  296,
      297,  935,    0,  964,  296,    0,    0,  298,  298,  961,
      297,  299,  296,    0,    0,  297,  299,  298,  896,  965,
      297,  980,  298,  948,  948,  948,  954,  966,  297,  972,
      299,  299,  964,  300,  301,  298,  298,  300,  300,  301,
      299,  302,  301,  302,  303,  299,  303,  965,  967,  980,

      300,  302,  300,  301,  303,  968,  966,  968,  299,  299,
        0,    0,  300,  301,  989,  972,  300,  300,  301,  990,
      304,  301,  973,  981,  302,  304,  984,  303,  300,  302,
      300,  301,  303,  967,  968,  302,  968,    0,  303,  304,
      304,  986,  989,  302,  972,  302,  303,  990,  303,  304,
     1005,    0,  981,  302,  304,  984,  303,    0,  302,  973,
      305,  303,  967,  302,  305,  305,  303,  304,  304,  987,
      986,  302,  306,  302,  303,  988,  303,  306, 1005,  305,
      307,  308, 1008, 1016,  307,  307,  308,    0,  973,  305,
     1010,  306,  308,  305,  305,    0,    0,  309,  987,  307,

      308,  306,  309, 1015,  988,    0,  306,  305,    0,  307,
      308, 1008, 1013,  307,  307,  308,  309,  310, 1010,  306,
      308,  309,  310,  998,  998,  998,  309,  307,  308, 1016,
      310,  309, 1015,  311,  312,    0,  310,  313,  311,  312,
     1013,  311,  313,  852,  309,  852,  310, 1017,    0,  309,
        0,  310,  311,  312,  314,    0,  313, 1016,  310,  314,
     1018,    0,  311,  312,  310, 1028,  313,  311,  312, 1027,
      311,  313,  314,  314,  315,    0, 1017,  316,  852,  315,
      311,  312,  316,  314,  313, 1022, 1032,  315,  314, 1018,
      316,  852, 1034,  315, 1028,    0,  316, 1027,  316, 1036,

      314,  314, 1021,  315,  317, 1023,  316,  852,  315,  317,
      317,  316,    0,    0, 1032,  315, 1043, 1022,  316,  852,
     1034,  315, 1049,  317,  316,  318,  316, 1036,    0,  319,
      318,    0,  319,  317,  319,    0,  318,  320,  317,  317,
      319, 1023,  320,    0,  318, 1043, 1022, 1021,  319, 1046,
     1049,  317,    0, 1024,  318, 1053,  320,  320,  319,  318,
      321,  319,  322,  319,  318,  321,  320,  322,  319,  323,
     1023,  320,  318, 1052,  323, 1021,  319,  323, 1046,  321,
      321,  322,  322, 1053,  320,  320,    0,    0,  323,  321,
        0,  322,  326, 1024,  321,  324,  322,  326,  323,  324,

      324,    0, 1052,  323, 1060,  326,  323,  321,  321,  322,
      322,  326,  328,    0,  324,  325,  323,  328,  327,  325,
      325,  326, 1024,  327,  324,  327,  326, 1061,  324,  324,
     1058,  328, 1060,  326,  325,    0, 1055,  327,  329,  326,
        0,  328,  324,  329,  325,  330,  328,  327,  325,  325,
      330, 1057,  327,  332,  327,  332, 1061,  329,    0,  328,
      330,  331,  325,  332,  330,  327,  331,  329, 1041, 1041,
     1041, 1055,  329,    0,  330, 1058, 1064,    0,  331,  330,
      331, 1066,  333, 1056, 1057,  329,  332,  333,  330,  333,
      331,  332,  330,  334, 1072,  331,    0, 1074,  334,  334,

     1055,  333,  336, 1058, 1064,  332,  331,  336,  331, 1076,
     1066,  333,  334, 1057,  336,  332,  333, 1056,  333,  335,
      332,  336,  334, 1072,  335, 1074, 1078,  334,  334,  333,
        0,  336,  335,  332,    0,    0,  336, 1076,  335,  337,
      334, 1085,  336,    0,  337,  338, 1056, 1094,  335,  336,
      338,    0,  974,  335, 1078, 1086,  337, 1090,  337, 1083,
      335,  876,  876,  876,  338,    0,  335,    0,  337,  876,
     1085, 1082,  876,  337,  338,  351, 1094,  351,    0,  338,
      946,  946,  946, 1086,  337, 1090,  337,  974,  946,  974,
        0,  946,  338,  351,  351,  351,  351,  351,  351,  351,

      351,  351,  351,  352,  947,  947,  947, 1082,    0,  352,
        0, 1083,  947,    0, 1077,  947,  974,    0,  974,  352,
      352,  352,  352,  352,  352,  352,  352,  352,  352,  353,
      949,  949,  949,    0,    0,  353, 1082, 1098,  949, 1083,
        0,  949, 1070, 1070, 1070,  353,  353,  353,  353,  353,
      353,  353,  353,  353,  353,  357, 1092, 1092, 1092, 1077,
     1099,  357,  362,  363,    0,  364,  362,  362,  363,  357,
      364,  357,  357,  357,  357,  357,  357,  357,  357,  357,
      357,  362,  363, 1098,  364,  365,  365, 1077, 1099, 1103,
      365,  362,  363,  368,  364,  362,  362,  363, 1104,  364,

        0,  366,    0,    0,  365,  366,  366,    0,    0,  362,
      363, 1098,  364, 1111,  365,  365, 1107, 1103,  366,  365,
      366,  367,    0,  433,  368,  367,  367, 1104,  433,  368,
      366, 1100,  365,    0,  366,  366,  950,  950,  950, 1113,
      367, 1111,  433,  368,  950, 1107,  366,  950,  366,    0,
      367, 1116,  433,  368,  367,  367,  383,  433,  368,    0,
        0,  999,  999,  999,    0,    0, 1100, 1113,  367,  999,
      433,  368,  999,  383,  383,  383,  383,  383,  383,  383,
      383,  383,  383,  384,    0, 1000, 1000, 1000, 1101, 1101,
     1101,    0,  383, 1000, 1121, 1100, 1000, 1116, 1127,    0,

      384,  384,  384,  384,  384,  384,  384,  384,  384,  384,
      387,    0, 1001, 1001, 1001, 1117, 1117, 1117, 1136,  384,
     1001,  383, 1121, 1001,    0, 1116, 1127,  387,  387,  387,
      387,  387,  387,  387,  387,  387,  387,  404,  404,  404,
      404,  404,  404,  404,  404,  404,  404, 1136,  384,  405,
      405,  405,  405,  405,  405,  405,  405,  405,  405,  406,
      406,  406,  406,  406,  406,  406,  406,  406,  406,  407,
      407,  407,  407,  407,  407,  407,  407,  407,  407,  408,
      408,  408,  408,  408,  408,  408,  408,  408,  408,  409,
      409,  409,  409,  409,  409,  409,  409,  409,  409,  423,

      423,  423,  423,  423,  423,  423,  423,  423,  423,  424,
      424,  424,  424,  424,  424,  424,  424,  424,  424,  425,
      425,  425,  425,  425,  425,  425,  425,  425,  425,  426,
      426,  426,  426,  426,  426,  426,  426,  426,  426,  427,
      427,  427,  427,  427,  427,  427,  427,  427,  427,  428,
      428,  428,  428,  428,  428,  428,  428,  428,  428,  429,
     1102,  429,    0,    0,  429,  429,  429,  429,  429,  429,
      429,  429,  429,  429,  430,  430,  430,  430,  430,  430,
      430,  430,  430,  430,    0,  436,    0,  434,    0,  431,
      436,  431,  434,  430,  431,  431,  431,  431,  431,  431,

      431,  431,  431,  431,  436,  434,  434,  435,  437,  435,
     1144, 1102,  435,  437,  436,    0,  434, 1132, 1147,  436,
        0,  434,  430,    0,  438,    0,  435,  437,  438,  438,
      439,    0,  436,  434,  434,  439,  435,  437,  435, 1102,
        0,  435,  437,  438,    0, 1132,  440, 1140,  439,  439,
     1147,  440,    0,  438,  435,  437, 1144,  438,  438,  439,
        0,    0,  441,    0,  439,  440, 1114,  441,  442,  443,
        0,  438,  442,  442,  443,  440,  439,  439, 1139, 1147,
      440,  441,    0,  443, 1144,  446, 1146,  442,  443,  444,
      446,  441, 1140,  440,  444, 1143,  441,  442,  443, 1156,

      444,  442,  442,  443,  446,  445,    0, 1149,  444,  441,
      445,  443,    0,  445,  446,  442,  443, 1114,  444,  446,
     1140,  447,  448,  444,  445,  447,  447,  448,  444, 1139,
      449, 1156,  446,  448,  445,  449,  444, 1146,    0,  445,
      447,  448,  445,  449, 1143, 1114, 1124, 1124, 1124,  449,
      447,  448,  445,    0,  447,  447,  448, 1139, 1149,  449,
     1156,  448,  451,    0,  449, 1146, 1145,  451,  447,  448,
      452,  449, 1143,  451,  453,  452, 1152,  449,  452,  453,
        0,  451,    0,    0, 1150,  454, 1149,  454,    0,  452,
        0,  451,  453,  453,    0,    0,  451,    0,    0,  452,

        0,  451, 1145,  453,  452,    0,    0,  452,  453,  451,
      458, 1151, 1151, 1151,  457,  458,    0,  452,  454,  457,
      453,  453,    0,  454,    0, 1152,  454,    0,  457,  458,
      458, 1145,    0,  457,    0, 1150,    0,  454,    0,  458,
      454,  459,    0,  457,  458,  460,  459,  454,  457,    0,
      460,    0,  454, 1152, 1155,  454,  457,  458,  458,  460,
      459,  457,  468, 1150,  460,  454,    0,  468,  454,  455,
      459,  455,    0,    0,  460,  459, 1153, 1153, 1153,  460,
        0,  468,    0,    0,  461,    0,    0,  460,  459,  461,
     1155,  468,  460, 1158, 1158, 1158,  468, 1159, 1159, 1159,

      455,  461,    0,  461,    0,  455,  463,  455,    0,  468,
      455,  463, 1157,  461,  455,    0,    0,  455,  461, 1155,
      455,  455,    0,    0,  455,  463,    0,    0,    0,  461,
      463,  461,    0,    0,  455,  463,  455,    0,  472,  455,
      463,    0,  455,  472,    0,  455,  462,    0,  455,  455,
      462,  462,  455,  463,  464,  465,    0,  472,  463,  464,
      465,  464,    0, 1157,    0,  462,  462,  472,    0,    0,
        0,    0,  472,  464,  465,  462,  465,    0,    0,  462,
      462,    0,    0,  464,  465,  472,    0,    0,  464,  465,
      464, 1157,    0,  462,  462,    0,    0,    0,    0,    0,

        0,  464,  465,    0,  465,  469,  469,  469,  469,  469,
      469,  469,  469,  469,  469,  469,    0,  469,  469,  469,
      469,  469,  469,  469,  469,  469,  469,  469,  469,  469,
      469,  469,  469,  469,  469,  469,  469,  469,  469,  469,
      469,  469,  469,  469,  469,  469,  469,  469,  469,  469,
      469,  469,  469,  469,  469,  469,  469,  469,  469,  469,
      469,  469,  469,  469,  469,  469,  469,  469,  469,  469,
      469,  469,  469,  469,  469,  469,  469,  469,  469,  469,
      469,  469,  469,  469,  469,  469,  469,  469,  469,  469,
      469,  469,  469,  469,  470,    0,  471,  473,  471,  470,

        0,    0,  473,  474, 1002, 1002, 1002,    0,  474,    0,
        0,  470, 1002,  470,    0, 1002,  473,    0,    0,    0,
        0,    0,  474,  470,    0,    0,  473,  471,  470,  471,
      476,  473,  474,    0,  471,  476,    0,  474,  477,  470,
        0,  470,    0,  477,  473,    0,  477,    0,  471,  476,
      474,    0,    0,    0,    0,    0,    0,  477,  471,  476,
      478,    0,    0,  471,  476,  478,    0,  477,  479,    0,
        0,    0,  477,  479,    0,  477,  471,  476,    0,  478,
      478,    0,    0,    0,    0,  477,    0,  479,    0,  478,
      480,    0,    0,    0,  478,  480,    0,  479,  480,  481,

        0,    0,  479,    0,  481,  481,    0,  478,  478,  480,
        0,    0,  481,    0,    0,  479,    0,    0,  481,  480,
        0, 1003, 1003, 1003,  480,    0,    0,  480,  481, 1003,
      482,  487, 1003,  481,  481,  482,  487,  480,  482,  483,
      481,  483,  486,    0,  483,    0,  481,  486,    0,  482,
      487,    0,    0,    0,    0,    0,  486,    0,  483,  482,
      487,  486,    0,    0,  482,  487,    0,  482,  483,  490,
      483,  486,    0,  483,  490,  491,  486,  482,  487,    0,
      491,    0,    0,  490,  486,    0,  483,    0,  490,  486,
        0,  492,    0,    0,  491,  493,  492,    0,  490,    0,

      493,    0,  492,  490,  491,    0,  493,  494,    0,  491,
      492,  490,  494,    0,  493,    0,  490,    0,  495,    0,
      492,    0,  491,  495,  493,  492,  494,  496,    0,  493,
      492,    0,  496,    0,  493,  496,  494,  495,  492,  497,
        0,  494,  493,  498,  497,    0,  496,  495,  498,    0,
      502,    0,  495,    0,  494,  502,  496,    0,  497,    0,
        0,  496,  498,    0,  496,  495,  499,    0,  497,  502,
      500,  499,  498,  497,  496,  500,    0,  498,    0,  502,
      499,    0,  500,    0,  502,  499,  497,  501,    0,  500,
      498,  501,  501,  503,    0,  499,  504,  502,  503,  500,

      499,  504,    0,    0,  500,    0,  501,    0,  499,    0,
      500,    0,  503,  499,    0,  504,  501,  500,  505,  507,
      501,  501,  503,  505,  507,  504,    0,  503,    0,  508,
      504, 1042, 1042, 1042,  501,  505,    0,  505,  507, 1042,
      503,  506, 1042,  504,    0,    0,  506,  505,  507,  506,
      508,    0,  505,  507,    0,  508,    0,  509,    0,    0,
      506,    0,  509,  505,    0,  505,  507,    0,    0,  508,
      506,    0,    0,    0,  509,  506,  509,    0,  506,  508,
        0,    0,  510,    0,  508,    0,  509,  510,  506,  511,
      512,  509,    0,  510,  511,  512,    0,  508,    0,    0,

        0,  510,  509,  512,  509,    0,  511,    0,  511,  512,
        0,  510,    0,    0,    0,    0,  510,    0,  511,  512,
      513,  510,  515,  511,  512,  513,    0,  515,  514,  510,
      514,  512,    0,  514,  511,    0,  511,  512,  517,  513,
        0,  515,    0,    0,    0,  516,    0,  514,    0,  513,
      516,  515,    0,  518,  513,    0,  515,  514,  518,  514,
        0,  517,  514,    0,  516,  519,  517,  513,    0,  515,
      519,    0,  518,    0,  516,  514,    0,    0,    0,  516,
      517,    0,  518,    0,  519,    0,  523,  518,  520,    0,
      517,  523,  516,  520,  519,  517,    0,    0,    0,  519,

      518,    0,  520,    0,    0,  523,    0,  520,  517,  524,
        0,    0,  519,  524,  524,  523,    0,  520,  525,    0,
      523,  526,  520,  525,    0,    0,  526,    0,  524,    0,
      520,    0,    0,  523,    0,  520,    0,  525,  524,    0,
      526,  527,  524,  524,  528,  527,  527,  525,  528,  528,
      526,    0,  525,    0,    0,  526,  524,    0,    0,    0,
      527,  551,    0,  528,    0,  525,  551,    0,  526,    0,
      527,    0,    0,  528,  527,  527,    0,  528,  528,  539,
      551,  539,    0,    0,    0, 1071, 1071, 1071,  527,    0,
      551,  528,    0, 1071,    0,  551, 1071,  539,  539,  539,

      539,  539,  539,  539,  539,  539,  539,  540,  551, 1093,
     1093, 1093,    0,  540,    0,    0,    0, 1093,    0,    0,
     1093,    0,    0,  540,  540,  540,  540,  540,  540,  540,
      540,  540,  540,  541,  548,    0,  548,    0,    0,  541,
        0,    0,    0,    0,  548,    0,    0,    0,    0,  541,
      541,  541,  541,  541,  541,  541,  541,  541,  541,  545,
      550,    0,  550,    0,    0,  545,  549,  548,    0,    0,
      548,  549,  548,  545,  549,  545,  545,  545,  545,  545,
      545,  545,  545,  545,  545,  549,  548,    0,    0,    0,
        0,    0,    0,  550,    0,  549,  548,    0,  550,  548,

      549,  548,  552,  549,    0,  592,    0,  552,  552,  592,
      592,    0,  550,  549,  548,  552,    0,    0,  553,  552,
      553,  552,  550,    0,  592,    0,    0,  550,  553,    0,
        0,  552,  553,    0,  592,    0,  552,  552,  592,  592,
      550,  565,    0,  552,    0,    0,    0,  552,    0,  552,
        0,  553,  592,  565,    0,  565,  553,    0,  565,  565,
      565,  565,  565,  565,  565,  565,  565,  565,    0,  566,
      553, 1110, 1110, 1110,    0,    0,    0,    0,    0, 1110,
      553,    0, 1110,    0,    0,  553,  566,  566,  566,  566,
      566,  566,  566,  566,  566,  566,  567,    0,  553,    0,

        0, 1125, 1125, 1125,    0,  566,    0,    0,  567, 1125,
      567,    0, 1125,  567,  567,  567,  567,  567,  567,  567,
      567,  567,  567,  588,  588,  588,  588,  588,  588,  588,
      588,  588,  588,    0,  566,  589,  589,  589,  589,  589,
      589,  589,  589,  589,  589,  590,  590,  590,  590,  590,
      590,  590,  590,  590,  590,  591,  591,  591,  591,  591,
      591,  591,  591,  591,  591,  593,    0,    0,  595,    0,
      593,    0,    0,  595, 1134, 1134, 1134,    0, 1135, 1135,
     1135,    0, 1134,  595,  593, 1134, 1135,  595,    0, 1135,
        0,    0,    0,    0,  593,    0,    0,  595,    0,  593,

        0,    0,  595,    0,    0,    0,    0,    0,    0,    0,
        0,  595,  593,  594,  594,  595,  594,  594,  594,  594,
      594,  594,  594,  594,  594,  594,  594,  594,  594,  594,
      594,  594,  594,  594,  594,  594,  594,  594,  594,  594,
      594,  594,  594,  594,  594,  594,  594,  594,  594,  594,
      594,  594,  594,  594,  594,  594,  594,  594,  594,  594,
      594,  594,  594,  594,  594,  594,  594,  594,  594,  594,
      594,  594,  594,  594,  594,  594,  594,  594,  594,  594,
      594,  594,  594,  594,  594,  594,  594,  594,  594,  594,
      594,  594,  594,  594,  594,  594,  594,  594,  594,  594,

      594,  594,  596,  597,  596,  598,  600,  596,  597,  599,
      598,  600,    0,  599,  599,    0,    0,    0,  598,  600,
        0,  596,  597,    0,  598,  600,    0,    0,  599,    0,
        0,  596,  597,  596,  598,  600,  596,  597,  599,  598,
      600,    0,  599,  599,    0,    0,  598,  600,    0,  596,
      597,    0,  598,  600,  601,  602,  599,  603,    0,  601,
      602,  605,  603,    0,    0,  605,  605,    0,    0,  602,
        0,    0,    0,  601,  602,  603,  603,    0,    0,    0,
      605,    0,    0,  601,  602,    0,  603,  606,  601,  602,
      605,  603,  606,    0,  605,  605,    0,  602,  606,    0,

        0,  601,  602,  603,  603,    0,  606,    0,  605,  609,
      607,    0,    0,  610,  609,  607,  606,  607,  610,    0,
      610,  606,    0,    0,    0,    0,  606,    0,  609,  607,
        0,    0,  610,    0,  606,  619,  621,    0,  609,  607,
      619,  621,  610,  609,  607,    0,  607,  610,    0,  610,
        0,    0,    0,    0,  619,  621,  609,  607,  620,    0,
      610,    0,    0,  620,  619,  621,    0,    0,    0,  619,
      621,  620,    0,    0,  623,  622,    0,  620,  622,  623,
      622,    0,  619,  621,    0,    0,    0,  620,  623,    0,
        0,  624,  620,  623,  622,    0,  624,    0,    0,  620,

        0,    0,  624,  623,  622,  620,    0,  622,  623,  622,
      624,    0,  625,  625,  626,    0,  623,  625,  628,  626,
      624,  623,  622,  628,    0,  624,    0,    0,    0,  626,
      624,  625,    0,  626,    0,  627,    0,  628,  624,  627,
      627,  625,  625,  626,    0,    0,  625,  628,  626,    0,
        0,    0,  628,  629,  627,  629,    0,  626,    0,  625,
        0,  626,    0,  632,  627,  628,    0,    0,  627,  627,
      631,  631,  631,  631,  631,  631,  631,  631,  631,  631,
      632,    0,  627,  633,  629,  632,  629,  634,  633,  637,
        0,  629,  634,    0,  637,    0,    0,    0,    0,  632,

      634,    0,  633,    0,    0,  629,  634,    0,  637,  632,
        0,    0,  633,    0,  632,  629,  634,  633,  637,  638,
      629,  634,    0,  637,  638,    0,    0,  632,  634,    0,
      633,    0,    0,  629,  634,  639,  637,    0,  638,  641,
      639,  641,  642,  639,  641,  642,  643,  642,  638,    0,
        0,  643,    0,  638,  639,    0,    0,    0,  641,    0,
        0,  642,  644,    0,  639,  643,  638,  644,  641,  639,
      641,  642,  639,  641,  642,  643,  642,    0,    0,  644,
      643,  644,  639,  645,    0,  646,  641,  645,  645,  642,
      646,  644,  647,  643,  648,    0,  644,  647,    0,  648,

        0,    0,  645,    0,  646,  647,    0,  644,    0,  644,
        0,  647,  645,  648,  646,    0,  645,  645,    0,  646,
        0,  647,    0,  648,    0,    0,  647,    0,  648,  649,
      645,  649,  646,  647,  649,  650,    0,    0,    0,  647,
      650,  648,    0,    0,    0,    0,  650,    0,  649,    0,
        0,    0,    0,    0,  650,    0,    0,    0,  649,    0,
      649,  652,    0,  649,  650,  651,  652,  651,    0,  650,
      651,    0,  652,  653,  650,    0,  649,  653,  653,    0,
      652,    0,  650,    0,  651,    0,    0,    0,    0,    0,
      652,    0,  653,    0,  651,  652,  651,    0,  655,  651,

      652,    0,  653,  655,    0,  654,  653,  653,  652,  656,
      654,    0,  651,  654,  656,    0,    0,  655,    0,    0,
      653,    0,  656,    0,  654,  657,    0,  655,  656,  657,
      657,    0,  655,    0,  654,    0,  658,    0,  656,  654,
        0,  658,  654,  656,  657,  655,    0,    0,  660,    0,
      656,    0,  654,  660,  657,  658,  656,  659,  657,  657,
        0,  659,  659,    0,    0,  658,    0,  660,  661,  662,
      658,    0,  657,  661,  662,    0,  659,  660,    0,    0,
        0,    0,  660,  658,    0,    0,  659,  661,  662,    0,
      659,  659,    0,    0,  663,  660,  663,  661,  662,  663,

      665,  666,  661,  662,  659,  665,  666,    0,    0,    0,
        0,    0,    0,  663,    0,  661,  662,    0,  665,  665,
      666,    0,    0,  663,    0,  663,  667,    0,  663,  665,
      666,  667,    0,    0,  665,  666,    0,  668,    0,  667,
      668,  663,  668,    0,    0,  667,  665,  665,  666,  669,
        0,    0,    0,  672,  669,  667,  668,  671,  672,    0,
      667,    0,  671,    0,    0,    0,  668,  667,  669,  668,
        0,  668,  672,  667,  673,    0,  671,  671,  669,  673,
      674,    0,  672,  669,  668,  674,  671,  672,    0,    0,
        0,  671,    0,  673,    0,    0,  669,    0,    0,  674,

      672,    0,    0,  673,  671,  671,    0,    0,  673,  674,
        0,    0,  690,    0,  674,    0,    0,    0,  690,  695,
        0,  673,    0,  695,  695,    0,  690,  674,  690,  690,
      690,  690,  690,  690,  690,  690,  690,  690,  695,  696,
      698,  699,    0,    0,  696,  698,  699,  696,  695,    0,
        0,  698,  695,  695,    0,    0,    0,    0,  696,  698,
      699,    0,    0,    0,    0,    0,  695,    0,  696,  698,
      699,    0,  709,  696,  698,  699,  696,    0,    0,  698,
        0,    0,  710,    0,    0,    0,  696,  698,  699,  709,
      709,  709,  709,  709,  709,  709,  709,  709,  709,  710,

      710,  710,  710,  710,  710,  710,  710,  710,  710,  711,
        0,    0,    0,    0,    0,    0,    0,    0,    0,  712,
        0,    0,    0,    0,    0,    0,  711,  711,  711,  711,
      711,  711,  711,  711,  711,  711,  712,  712,  712,  712,
      712,  712,  712,  712,  712,  712,  715,  715,  715,  715,
      715,  715,  715,  715,  715,  715,  715,  715,  715,  715,
      715,  715,  715,  715,  715,  715,  715,  715,  715,  715,
      715,  715,  715,  715,  715,  715,  715,  715,  715,  715,
      715,  715,  715,  715,  715,  715,  715,  715,  715,  715,
      715,  715,  715,  715,  715,  715,  715,  715,  715,  715,

      715,  715,  715,  715,  715,  715,  715,  715,  715,  715,
      715,  715,  715,  715,  715,  715,  715,  715,  715,  715,
      715,  715,  715,  715,  715,  715,  715,  715,  715,  715,
      715,  715,  715,  715,  715,  719,  719,  719,    0,    0,
      719,  719,    0,  719,    0,    0,    0,  719,  719,    0,
        0,  719,    0,    0,  719,  719,  719,  719,  719,  719,
      719,  719,  719,  732,  732,    0,  732,  732,  732,  732,
      732,  732,  732,  732,  732,  732,  732,  732,  732,  732,
      732,  732,  732,  732,  732,  732,  732,  732,  732,  732,
      732,  732,  732,  732,  732,  732,  732,  732,  732,  732,

      732,  732,  732,  732,  732,  732,  732,  732,  732,  732,
      732,  732,  732,  732,  732,  732,  732,  732,  732,  732,
      732,  732,  732,  732,  732,  732,  732,  732,  732,  732,
      732,  732,  732,  732,  732,  732,  732,  732,  732,  732,
      732,  732,  732,  732,  732,  732,  732,  732,  732,  732,
      732,  732,  733,    0,    0,  735,    0,  733,  734,  734,
      734,  734,  734,  734,  734,  734,  734,  734,  736,  737,
        0,  733,  735,  736,  737,  738,    0,  735,    0,    0,
      738,  733,    0,    0,    0,    0,  733,  736,  737,    0,
        0,  735,    0,    0,  738,  739,    0,  736,  737,  733,

      739,  735,  736,  737,  738,    0,  735,  740,    0,  738,
        0,    0,  740,    0,  739,  736,  737,    0,  741,  735,
        0,    0,  738,  741,  739,    0,  740,    0,    0,  739,
      740,    0,    0,    0,    0,    0,  740,  741,  741,    0,
        0,  740,  739,  742,    0,  742,    0,  741,  742,    0,
      747,  743,  741,    0,  740,  747,  743,    0,  740,  743,
        0,  745,  742,  745,    0,  741,  741,    0,    0,  747,
      743,    0,  742,  750,  742,    0,    0,  742,  750,  747,
      743,    0,  746,    0,  747,  743,  746,  746,  743,    0,
      742,    0,  750,    0,  745,    0,  745,  747,  743,  745,

        0,  746,  750,    0,    0,    0,    0,  750,    0,  745,
        0,  746,    0,  745,  751,  746,  746,    0,  751,  751,
      750,    0,    0,  745,    0,  745,  762,    0,  745,  746,
        0,  762,    0,  751,  761,    0,  761,  745,    0,  761,
        0,  745,    0,  751,    0,  762,  762,  751,  751,    0,
        0,  763,    0,  761,    0,  762,  763,  763,    0,    0,
      762,  751,    0,  761,    0,  761,    0,    0,  761,    0,
      763,  764,  765,  762,  762,  764,  764,  765,  766,    0,
      763,  761,  766,  766,    0,  763,  763,    0,    0,  765,
      764,  765,    0,    0,  771,    0,    0,  766,  763,  771,

      764,  765,    0,  767,  764,  764,  765,  766,  767,    0,
      768,  766,  766,  771,  772,  768,  772,  765,  764,  765,
      767,  768,  767,  771,  772,  766,  773,    0,  771,  768,
        0,  773,  767,    0,    0,    0,    0,  767,    0,  768,
      773,  771,    0,    0,  768,  773,  774,  772,  767,  768,
      767,  774,  772,    0,  774,  773,    0,  768,  775,    0,
      773,    0,  775,  775,    0,  774,  772,  776,  773,  778,
        0,  776,  776,  773,  778,  774,  772,  775,    0,    0,
      774,  772,    0,  774,    0,    0,  776,  775,  778,    0,
      777,  775,  775,  774,  772,  777,  776,    0,  778,  779,

      776,  776,    0,  778,  779,  775,    0,  777,    0,  777,
      780,    0,    0,    0,  776,  780,  778,  779,  779,  777,
        0,  780,  781,    0,  777,    0,    0,  781,  779,  780,
        0,  782,    0,  779,    0,  777,  782,  777,    0,  780,
        0,  781,  783,  784,  780,  779,  779,  783,  784,  780,
      782,  781,    0,  785,    0,  785,  781,  780,    0,    0,
      782,  783,  784,  786,    0,  782,    0,    0,  786,  781,
        0,  783,  784,    0,  787,    0,  783,  784,  782,  787,
        0,  786,  786,  788,    0,    0,  785,    0,  788,  783,
      784,  785,  786,  787,  789,    0,    0,  786,  787,  789,

        0,  785,  788,  787,    0,  785,    0,    0,  787,  786,
      786,    0,  788,  789,    0,  785,    0,  788,    0,  790,
      785,  787,    0,  789,  790,    0,  787,  791,  789,  785,
      788,  791,  791,  785,    0,    0,  790,    0,  790,  792,
        0,  789,    0,    0,  792,    0,  791,    0,  790,    0,
        0,  792,    0,  790,    0,    0,  791,    0,  792,    0,
      791,  791,  793,    0,  790,    0,  790,  793,  792,    0,
      795,  796,    0,  792,  791,  795,  796,    0,  795,  792,
        0,  793,  797,    0,    0,    0,  792,  797,    0,  795,
      796,  793,    0,  798,    0,  799,  793,  799,  798,  795,

      796,  797,    0,    0,  795,  796,    0,  795,    0,  793,
        0,  797,  798,    0,    0,    0,  797,  795,  796,  800,
      801,    0,  798,    0,  800,  801,  816,  798,  799,  797,
      799,  816,    0,  799,    0,    0,    0,    0,  800,  801,
      798,    0,    0,    0,    0,  816,    0,  799,  800,  801,
        0,  817,    0,  800,  801,  816,  817,  799,    0,  799,
      816,    0,  799,    0,  817,    0,  800,  801,    0,  818,
      817,  818,    0,  816,  842,  799,  802,  802,  802,  802,
      817,    0,    0,    0,    0,  817,    0,    0,    0,    0,
      819,  842,  817,    0,    0,  819,  842,    0,  817,    0,

        0,    0,  818,    0,    0,    0,    0,  818,    0,  819,
      842,  802,  802,    0,    0,  802,    0,  802,    0,  819,
      842,  818,    0,    0,  819,  842,    0,    0,  802,  802,
        0,  818,    0,    0,    0,    0,  818,  819,  842,    0,
      802,  802,    0,    0,  802,    0,  802,    0,    0,  818,
        0,    0,    0,    0,    0,    0,  802,  802,  827,  827,
      827,  827,  827,  827,  827,  827,  827,  827,  827,  827,
      827,  827,  827,  827,  827,  827,  827,  827,  827,  827,
      827,  827,  827,  827,  827,  827,  827,  827,  827,  827,
      827,  827,  827,  827,  827,  827,  827,  827,  827,  827,

      827,  827,  827,  827,  827,  827,  827,  827,  827,  827,
      827,  827,  827,  827,  827,  827,  827,  827,  827,  827,
      827,  827,  827,  827,  827,  827,  827,  827,  827,  827,
      827,  827,  827,  827,  827,  827,  827,  827,  827,  827,
      827,  827,  827,  827,  827,  827,  827,  828,  841,  841,
      841,  841,  841,  841,  841,  841,  841,  841,    0,    0,
        0,    0,    0,    0,  828,  828,  828,  828,  828,  828,
      828,  828,  828,  828,  829,  829,  829,  829,  829,  829,
      829,  829,  829,  829,  829,  829,  829,  829,  829,  829,
      829,  829,  829,  829,  829,  829,  829,  829,  829,  829,

      829,  829,  829,  829,  829,  829,  829,  829,  829,  829,
      829,  829,  829,  829,  829,  829,  829,  829,  829,  829,
      829,  829,  829,  829,  829,  829,  829,  829,  829,  829,
      829,  829,  829,  829,  829,  829,  829,  829,  829,  829,
      829,  829,  829,  829,  829,  829,  829,  829,  829,  829,
      829,  829,  829,  829,  829,  829,  829,  829,  829,  829,
      829,  829,  829,  843,    0,  844,    0,  847,  843,  844,
      844,  845,  847,  845,  846,    0,  845,    0,  846,  846,
        0,    0,  843,    0,  844,    0,  847,    0,    0,    0,
      845,    0,  843,  846,  844,    0,  847,  843,  844,  844,

      845,  847,  845,  846,  849,  845,    0,  846,  846,  849,
      843,  848,  844,    0,  847,  848,  848,    0,  845,    0,
      850,  846,    0,  849,    0,  850,    0,  853,    0,    0,
      848,  854,  853,  849,  850,    0,  854,    0,  849,  850,
      848,  853,    0,    0,  848,  848,  853,    0,  854,  850,
      854,  849,  855,    0,  850,    0,  853,  855,  848,    0,
      854,  853,  850,  858,    0,  854,    0,  850,  858,  853,
        0,  855,  866,    0,  853,    0,  854,  866,  854,    0,
      858,  855,  858,  867,    0,    0,  855,    0,  867,    0,
        0,  866,  858,    0,  867,    0,    0,  858,    0,  855,

        0,  866,  867,    0,  868,    0,  866,    0,  858,  868,
      858,    0,  867,  869,    0,  869,    0,  867,  869,  866,
        0,  868,  867,  868,    0,    0,    0,    0,    0,  870,
      867,    0,  869,  868,  870,  871,    0,    0,  868,    0,
      871,    0,  869,  870,  869,    0,    0,  869,  870,  868,
        0,  868,    0,    0,  871,    0,    0,    0,  870,    0,
      869,  872,    0,  870,  871,  872,  872,    0,  873,  871,
      874,  870,  873,  873,    0,  874,  870,    0,    0,    0,
      872,  874,  871,    0,    0,    0,    0,  873,    0,  874,
      872,  878,    0,  877,  872,  872,  878,  873,  877,  874,

        0,  873,  873,    0,  874,    0,  877,    0,  872,  874,
      878,    0,  877,  879,  880,  873,  881,  874,  879,  880,
      878,  881,  877,    0,    0,  878,    0,  877,    0,    0,
        0,    0,  879,  880,  877,  881,  882,    0,  878,    0,
      877,  882,  879,  880,  882,  881,    0,  879,  880,    0,
      881,    0,  883,  886,    0,  882,    0,  883,  886,  885,
      879,  880,    0,  881,  885,  882,    0,    0,    0,    0,
      882,  883,  886,  882,    0,    0,  885,    0,  885,  887,
        0,  883,  886,  882,  887,    0,  883,  886,  885,    0,
      888,  890,    0,  885,    0,  888,  890,    0,  887,  883,

      886,  888,  891,    0,  885,    0,  885,  891,  887,  888,
      890,  889,    0,  887,    0,  889,  889,    0,    0,  888,
      890,  891,  892,  897,  888,  890,  887,  892,  897,  888,
      889,  891,    0,    0,  895,    0,  891,  888,  890,  895,
      889,  892,  897,  906,  889,  889,    0,    0,  906,  891,
        0,  892,  897,  895,    0,  895,  892,  897,  889,  898,
        0,    0,  906,  895,  898,    0,  907,  898,  895,  892,
      897,  907,  906,    0,    0,    0,    0,  906,  898,    0,
        0,  895,    0,  895,  907,  907,    0,    0,  898,  909,
      906,  909,    0,  898,  909,  907,  898,    0,    0,    0,

      907,    0,    0,    0,    0,    0,  898,  914,  909,    0,
        0,    0,  907,  907,    0,    0,  917,  917,  909,    0,
      909,  917,    0,  909,  914,  914,  914,  914,  914,  914,
      914,  914,  914,  914,    0,  917,  909,  918,    0,  919,
        0,    0,  918,  919,  919,  917,  917,    0,  920,    0,
      917,    0,    0,  920,    0,    0,  918,    0,  919,    0,
        0,    0,    0,  917,    0,  920,  918,  920,  919,  921,
      922,  918,  919,  919,  921,  922,    0,  920,  923,    0,
        0,    0,  920,  923,  918,    0,  919,    0,  921,  922,
        0,  923,    0,  920,    0,  920,    0,  923,  921,  922,

        0,  927,    0,  921,  922,  928,  927,  923,    0,  928,
      928,    0,  923,  927,    0,    0,  921,  922,    0,  923,
      927,  938,    0,    0,  928,  923,  938,  930,  940,  938,
      927,  930,  930,  940,  928,  927,    0,    0,  928,  928,
      938,  927,    0,    0,    0,    0,  930,  940,  927,    0,
      938,    0,  928,    0,    0,  938,  930,  940,  938,  939,
      930,  930,  940,  939,  939,  941,  942,    0,  938,    0,
      941,  942,    0,    0,  930,  940,    0,    0,  939,    0,
        0,    0,  943,    0,  941,  942,  942,  943,  939,    0,
        0,  944,  939,  939,  941,  942,  944,    0,    0,  941,

      942,  943,  945,    0,  944,    0,  939,  945,    0,    0,
      944,  943,  941,  942,  942,    0,  943,    0,  951,    0,
      944,  945,  952,  951,    0,  944,    0,  952,    0,  943,
        0,  945,  944,    0,    0,    0,  945,  951,  944,  955,
        0,  952,    0,  953,  955,  953,    0,  951,  953,  945,
        0,  952,  951,  955,    0,    0,  952,    0,  955,  956,
        0,    0,  953,    0,  956,  951,  957,    0,  955,  952,
      958,  957,  953,  955,  953,  958,    0,  953,  956,    0,
        0,  955,    0,    0,    0,  957,  955,  958,  956,  958,
      953,    0,  960,  956,    0,  957,  960,  960,    0,  958,

      957,    0,    0,  963,  958,    0,  956,    0,  963,  962,
        0,  960,    0,  957,  962,  958,  963,  958,    0,    0,
        0,  960,  963,    0,    0,  960,  960,  962,  962,  969,
        0,    0,  963,    0,  969,  970,    0,  963,  962,  960,
      970,  971,    0,  962,  963,  971,  971,    0,  969,  969,
      963,    0,  976,    0,  970,  962,  962,  976,  969,    0,
      971,  975,    0,  969,  970,    0,  975,    0,  977,  970,
      971,  976,  975,  977,  971,  971,  969,  969,    0,    0,
      975,  976,  970,    0,  979,    0,  976,  977,  971,  979,
      975,    0,    0,  985,    0,  975,    0,  977,  985,  976,

      975,  978,  977,  979,  982,  978,  978,    0,  975,  982,
        0,    0,  985,  979,    0,  977,    0,    0,  979,  982,
      978,    0,  985,  982,  983,    0,  983,  985,    0,  983,
      978,  979,    0,  982,  978,  978,    0,    0,  982,    0,
      985,  993,    0,  983,  994,  992,  993,  982,  978,  994,
      992,  982,    0,  983,    0,  983,  994,    0,  983,  992,
      993,  995,    0,  994,  992,    0,  995,  996,    0,    0,
      993,  983,  996,  994,  992,  993,    0,    0,  994,  992,
      995,    0,    0,    0,  994,    0,  996,  992,  993,    0,
      995,  994,  992, 1004,    0,  995,  996,  997, 1004,  997,

     1007,  996,  997,    0, 1006, 1007, 1006,    0,  995, 1006,
        0,    0, 1004,    0,  996,    0,  997,    0,    0, 1007,
     1009,    0, 1004, 1006,    0, 1009,  997, 1004,  997, 1007,
        0,  997,    0, 1006, 1007, 1006,    0, 1011, 1006, 1009,
     1004, 1011, 1011,    0,  997,    0,    0, 1007,    0, 1009,
        0, 1006,    0, 1012, 1009,    0, 1011, 1012, 1012,    0,
     1020,    0,    0,    0, 1019, 1020, 1011, 1009,    0, 1019,
     1011, 1011, 1012,    0,    0, 1019, 1025,    0,    0, 1020,
     1025, 1025, 1012, 1019, 1011, 1026, 1012, 1012,    0, 1020,
     1026,    0,    0, 1019, 1020, 1025,    0, 1029, 1019,    0,

     1012,    0, 1029, 1019, 1026, 1025,    0, 1020, 1029, 1025,
     1025, 1019,    0,    0, 1026,    0, 1029,    0,    0, 1026,
     1030,    0, 1037, 1025, 1038, 1030, 1029, 1037, 1030, 1038,
        0, 1029, 1026,    0,    0, 1037, 1029,    0,    0, 1030,
        0, 1037,    0, 1038, 1029,    0,    0, 1039,    0, 1030,
        0, 1037, 1039, 1038, 1030, 1039, 1037, 1030, 1038, 1047,
        0,    0,    0, 1037, 1047,    0, 1039, 1030, 1040, 1037,
     1044, 1038, 1040, 1040, 1044, 1044, 1039,    0, 1047,    0,
        0, 1039,    0,    0, 1039,    0,    0, 1040, 1047, 1044,
     1048, 1054,    0, 1047, 1039, 1048, 1054, 1040,    0, 1044,

        0, 1040, 1040, 1044, 1044,    0, 1047, 1059,    0, 1048,
     1054,    0, 1059,    0,    0, 1040, 1063, 1044,    0, 1048,
     1054, 1063,    0, 1062, 1048, 1054, 1059, 1062, 1062,    0,
        0,    0,    0,    0, 1063, 1063, 1059, 1048, 1054, 1067,
        0, 1059, 1062,    0, 1067, 1063, 1068,    0, 1069,    0,
     1063, 1068, 1062, 1069, 1059,    0, 1062, 1062, 1067, 1068,
        0,    0, 1063, 1063,    0, 1068,    0, 1069, 1067, 1073,
     1062, 1080, 1073, 1067, 1073, 1068, 1080, 1069,    0,    0,
     1068,    0, 1069,    0,    0,    0, 1067, 1068, 1073,    0,
     1080, 1087,    0, 1068, 1088, 1069, 1087,    0, 1073, 1088,

     1080, 1073, 1088, 1073,    0, 1080,    0,    0,    0,    0,
     1087,    0,    0, 1088, 1087,    0, 1073, 1091, 1080,    0,
     1087, 1091, 1091, 1088, 1095, 1087,    0,    0, 1088, 1095,
        0, 1088,    0,    0,    0,    0, 1091,    0, 1087,    0,
     1105, 1088, 1087, 1095, 1095, 1105, 1091, 1106,    0, 1108,
     1091, 1091, 1106, 1095, 1108,    0,    0,    0, 1095, 1105,
        0, 1106,    0,    0, 1091,    0, 1106,    0, 1108, 1105,
        0, 1095, 1095,    0, 1105,    0, 1106,    0, 1108,    0,
        0, 1106,    0, 1108,    0,    0,    0, 1105,    0, 1106,
        0,    0,    0,    0, 1106,    0, 1108, 1109, 1109, 1109,

        0,    0, 1109, 1109,    0, 1109,    0,    0,    0, 1109,
     1109,    0,    0, 1109,    0,    0, 1109, 1109, 1109, 1109,
     1109, 1109, 1109, 1109, 1109, 1112, 1122,    0,    0,    0,
     1112, 1122,    0,    0,    0,    0,    0,    0,    0, 1122,
        0,    0, 1112,    0, 1112, 1122,    0,    0,    0,    0,
        0,    0,    0,    0, 1112, 1122,    0,    0,    0, 1112,
     1122,    0,    0,    0,    0,    0,    0, 1122,    0,    0,
     1112,    0, 1112, 1122, 1126, 1126, 1126,    0,    0, 1126,
     1126,    0, 1126,    0, 1128, 1126, 1126, 1126, 1128, 1128,
     1126,    0,    0, 1126, 1126, 1126, 1126, 1126, 1126, 1126,

     1126, 1126,    0, 1128, 1133, 1137,    0,    0,    0, 1133,
     1137,    0,    0, 1128,    0,    0,    0, 1128, 1128,    0,
        0,    0,    0, 1133, 1137,    0,    0,    0,    0,    0,
        0, 1128,    0, 1133, 1137,    0,    0,    0, 1133, 1137,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
        0, 1133, 1137, 1162, 1162, 1162, 1162, 1162, 1162, 1162,
     1162, 1162, 1162, 1162, 1162, 1163, 1163, 1163, 1163, 1163,
     1163, 1163, 1163, 1163, 1163, 1163, 1163, 1164, 1164,    0,
     1164, 1164, 1164, 1164, 1164, 1164, 1164, 1164, 1164, 1165,
     1165,    0, 1165, 1165, 1165, 1165, 1165, 1165, 1165, 1165,

     1165, 1166, 1166, 1166, 1166, 1166, 1166, 1166, 1166, 1166,
     1166, 1166, 1166, 1167, 1167, 1167, 1167, 1167, 1167, 1167,
     1167, 1167, 1167, 1167, 1167, 1168, 1168, 1169, 1169, 1169,
     1169, 1169, 1169, 1169, 1169, 1170, 1170, 1170, 1170, 1170,
     1170, 1170, 1170, 1170, 1170, 1170, 1170, 1171, 1171, 1171,
     1171, 1171, 1171, 1171, 1171, 1171, 1171, 1171, 1171, 1172,
     1172, 1172, 1172, 1172, 1172,    0, 1172, 1172, 1172,    0,
     1172, 1173, 1173, 1173, 1173, 1173, 1173, 1173, 1173, 1173,
     1173, 1173, 1173, 1174,    0,    0, 1174, 1175, 1175, 1175,
        0,    0, 1175,    0, 1175, 1175,    0, 1175, 1175, 1176,

     1176, 1176, 1176, 1176, 1176, 1176, 1176, 1176, 1176, 1176,
     1176, 1177, 1177,    0, 1177, 1177, 1177, 1177, 1177, 1177,
     1177, 1177, 1177, 1178, 1178, 1178, 1178, 1178, 1178,    0,
     1178, 1178, 1178,    0, 1178, 1179, 1179, 1179, 1179, 1179,
     1179, 1179, 1179, 1179, 1179, 1179, 1179, 1180, 1180, 1180,
     1180, 1180, 1180, 1180, 1180, 1180, 1180, 1180, 1180, 1181,
        0,    0, 1181, 1182,    0,    0,    0, 1182,    0,    0,
        0,    0,    0, 1182, 1183, 1183, 1183, 1183, 1183, 1183,
     1183, 1183, 1183, 1183, 1183, 1183, 1184, 1184,    0, 1184,
     1184, 1184, 1184, 1184, 1184, 1184, 1184, 1184, 1185, 1185,

        0, 1185, 1185, 1185, 1185, 1185, 1185, 1185, 1185, 1185,
     1186, 1186,    0, 1186, 1186, 1186, 1186, 1186, 1186, 1186,
     1186, 1186, 1187, 1187, 1187, 1187, 1187, 1187, 1187, 1187,
     1187, 1187, 1187, 1187, 1188, 1188, 1188, 1188, 1188, 1188,
     1188, 1188, 1188, 1188, 1188, 1188, 1189, 1189, 1189, 1189,
     1189, 1189, 1189, 1189, 1189, 1189, 1189, 1189, 1190, 1190,
     1190, 1190, 1190, 1190, 1190, 1190, 1190, 1190, 1190, 1190,
     1191, 1191, 1191, 1191, 1191, 1191, 1191, 1191, 1191, 1191,
     1191, 1191, 1192, 1192,    0, 1192, 1192, 1192, 1192, 1192,
     1192, 1192, 1192, 1192, 1193, 1193, 1193, 1193, 1193, 1193,

     1193, 1193, 1193, 1193, 1193, 1193, 1194, 1194, 1194, 1194,
     1194, 1194, 1194, 1194, 1194, 1194, 1194, 1194, 1195, 1195,
        0, 1195, 1195, 1195, 1195, 1195, 1195, 1195, 1195, 1195,
     1196, 1196,    0, 1196, 1196, 1196, 1196, 1196, 1196, 1196,
     1196, 1196, 1197, 1197,    0, 1197, 1197, 1197, 1197, 1197,
     1197, 1197, 1197, 1197, 1198, 1198,    0, 1198, 1198, 1198,
     1198, 1198, 1198, 1198, 1198, 1198, 1199, 1199,    0, 1199,
     1199, 1199, 1199, 1199, 1199, 1199, 1199, 1199, 1200, 1200,
     1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200, 1200,
     1201, 1201, 1201, 1201, 1201, 1201, 1201, 1201, 1201, 1201,

     1201, 1201, 1202, 1202, 1202, 1202, 1202, 1202, 1202, 1202,
     1202, 1202, 1202, 1202, 1203, 1203, 1203, 1203, 1203, 1203,
     1203, 1203, 1203, 1203, 1203, 1203, 1204, 1204, 1204, 1204,
     1204, 1204, 1204, 1204, 1204, 1204, 1204, 1204, 1205, 1205,
     1205, 1205, 1205, 1205, 1205, 1205, 1205, 1205, 1205, 1205,
     1206, 1206,    0, 1206, 1206, 1206, 1206, 1206, 1206, 1206,
     1206, 1206, 1207, 1207,    0, 1207, 1207, 1207, 1207, 1207,
     1207, 1207, 1207, 1207, 1208, 1208, 1208, 1208, 1208, 1208,
     1208, 1208, 1208, 1208, 1208, 1208, 1209, 1209, 1209, 1209,
     1209, 1209, 1209, 1209, 1209, 1209, 1209, 1209, 1210, 1210,

     1210, 1210, 1210, 1210, 1210, 1210, 1210, 1210, 1210, 1210,
     1211, 1211, 1211, 1211, 1211, 1211, 1211, 1211, 1211, 1211,
     1211, 1211, 1212, 1212, 1212, 1212, 1212, 1212, 1212, 1212,
     1212, 1212, 1212, 1212, 1213, 1213, 1213, 1213, 1213, 1213,
     1213, 1213, 1213, 1213, 1213, 1213, 1214, 1214, 1214, 1214,
     1214, 1214, 1214, 1214, 1214, 1214, 1214, 1214, 1161, 1161,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,

     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161,
     1161, 1161, 1161, 1161, 1161, 1161, 1161, 1161
    } ;

static yy_state_type yy_last_accepting_state;
static char *yy_last_accepting_cpos;

extern int fortran_flex_debug;
int fortran_flex_debug = 0;

/* The intent behind this definition is that it'll catch
 * any uses of REJECT which flex missed.
 */
#define REJECT reject_used_but_not_detected
#define yymore() yymore_used_but_not_detected
#define YY_MORE_ADJ 0
#define YY_RESTORE_YY_MORE_OFFSET
char *fortrantext;
#line 1 "fortran.lex"
/******************************************************************************/
/*                                                                            */
/*     CONV (converter) for Agrif (Adaptive Grid Refinement In Fortran)       */
/*                                                                            */
/* Copyright or   or Copr. Laurent Debreu (Laurent.Debreu@imag.fr)            */
/*                        Cyril Mazauric (Cyril_Mazauric@yahoo.fr)            */
/* This software is governed by the CeCILL-C license under French law and     */
/* abiding by the rules of distribution of free software.  You can  use,      */
/* modify and/ or redistribute the software under the terms of the CeCILL-C   */
/* license as circulated by CEA, CNRS and INRIA at the following URL          */
/* "http://www.cecill.info".                                                  */
/*                                                                            */
/* As a counterpart to the access to the source code and  rights to copy,     */
/* modify and redistribute granted by the license, users are provided only    */
/* with a limited warranty  and the software's author,  the holder of the     */
/* economic rights,  and the successive licensors  have only  limited         */
/* liability.                                                                 */
/*                                                                            */
/* In this respect, the user's attention is drawn to the risks associated     */
/* with loading,  using,  modifying and/or developing or reproducing the      */
/* software by the user in light of its specific status of free software,     */
/* that may mean  that it is complicated to manipulate,  and  that  also      */
/* therefore means  that it is reserved for developers  and  experienced      */
/* professionals having in-depth computer knowledge. Users are therefore      */
/* encouraged to load and test the software's suitability as regards their    */
/* requirements in conditions enabling the security of their systems and/or   */
/* data to be ensured and,  more generally, to use and operate it in the      */
/* same conditions as regards security.                                       */
/*                                                                            */
/* The fact that you are presently reading this means that you have had       */
/* knowledge of the CeCILL-C license and that you accept its terms.           */
/******************************************************************************/
/* version 1.7                                                                */
/******************************************************************************/





#line 41 "fortran.lex"
#include <math.h>
#include <stdlib.h>
#include <string.h>
extern FILE * fortranin;
#define MAX_INCLUDE_DEPTH 30
#define tabsize 6
YY_BUFFER_STATE include_stack[MAX_INCLUDE_DEPTH];
int line_num_fortran=1;
int line_num_fortran_common=1;
int newlinef90 = 0;
char *tmp;
char tmpc;
/******************************************************************************/
/**************PETITS PB NON PREVUS *******************************************/
/******************************************************************************/
/* NEXTLINF77 un ligne fortran 77 peut commencer par -      &a=b or on        */
/*            a prevu seulement       & a=b avec l'espace entre le symbole    */
/*            de la 7eme et le debut de la ligne de commande                  */
/*            le ! est aussi interdit comme symbole de la 7 eme colonne       */
/*            Normalement NEXTLINEF77 \n+[ ]{5}[^ ]                           */
/******************************************************************************/
#define YY_USER_ACTION \
        {\
           if (firstpass == 0) \
           {\
              strcat(curbuf,fortrantext); \
              Save_Length(curbuf,38); \
              strcpy(motparse,fortrantext);\
              Save_Length(motparse,32); \
              colnum = colnum + strlen(motparse);\
              ECHO; \
           }\
           strcpy(motparse1,fortrantext);\
/*           printf("fortrantext = %s\n",fortrantext);*/\
        /*if ( firstpass == 1 ) 
                      printf("fortrantext = %s %d\n",fortrantext,strlen(fortrantext));*/\
        }
#line 2979 "fortran.yy.c"

#define INITIAL 0
#define parameter 1
#define character 2
#define donottreat 3
#define fortran77style 4
#define fortran90style 5

#ifndef YY_NO_UNISTD_H
/* Special case for "unistd.h", since it is non-ANSI. We include it way
 * down here because we want the user's section 1 to have been scanned first.
 * The user has a chance to override it with an option.
 */
#include <unistd.h>
#endif

#ifndef YY_EXTRA_TYPE
#define YY_EXTRA_TYPE void *
#endif

static int yy_init_globals (void );

/* Accessor methods to globals.
   These are made visible to non-reentrant scanners for convenience. */

int fortranlex_destroy (void );

int fortranget_debug (void );

void fortranset_debug (int debug_flag  );

YY_EXTRA_TYPE fortranget_extra (void );

void fortranset_extra (YY_EXTRA_TYPE user_defined  );

FILE *fortranget_in (void );

void fortranset_in  (FILE * in_str  );

FILE *fortranget_out (void );

void fortranset_out  (FILE * out_str  );

yy_size_t fortranget_leng (void );

char *fortranget_text (void );

int fortranget_lineno (void );

void fortranset_lineno (int line_number  );

/* Macros after this point can all be overridden by user definitions in
 * section 1.
 */

#ifndef YY_SKIP_YYWRAP
#ifdef __cplusplus
extern "C" int fortranwrap (void );
#else
extern int fortranwrap (void );
#endif
#endif

    static void yyunput (int c,char *buf_ptr  );
    
#ifndef yytext_ptr
static void yy_flex_strncpy (char *,yyconst char *,int );
#endif

#ifdef YY_NEED_STRLEN
static int yy_flex_strlen (yyconst char * );
#endif

#ifndef YY_NO_INPUT

#ifdef __cplusplus
static int yyinput (void );
#else
static int input (void );
#endif

#endif

/* Amount of stuff to slurp up with each read. */
#ifndef YY_READ_BUF_SIZE
#define YY_READ_BUF_SIZE 8192
#endif

/* Copy whatever the last rule matched to the standard output. */
#ifndef ECHO
/* This used to be an fputs(), but since the string might contain NUL's,
 * we now use fwrite().
 */
#define ECHO fwrite( fortrantext, fortranleng, 1, fortranout )
#endif

/* Gets input and stuffs it into "buf".  number of characters read, or YY_NULL,
 * is returned in "result".
 */
#ifndef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( YY_CURRENT_BUFFER_LVALUE->yy_is_interactive ) \
		{ \
		int c = '*'; \
		yy_size_t n; \
		for ( n = 0; n < max_size && \
			     (c = getc( fortranin )) != EOF && c != '\n'; ++n ) \
			buf[n] = (char) c; \
		if ( c == '\n' ) \
			buf[n++] = (char) c; \
		if ( c == EOF && ferror( fortranin ) ) \
			YY_FATAL_ERROR( "input in flex scanner failed" ); \
		result = n; \
		} \
	else \
		{ \
		errno=0; \
		while ( (result = fread(buf, 1, max_size, fortranin))==0 && ferror(fortranin)) \
			{ \
			if( errno != EINTR) \
				{ \
				YY_FATAL_ERROR( "input in flex scanner failed" ); \
				break; \
				} \
			errno=0; \
			clearerr(fortranin); \
			} \
		}\
\

#endif

/* No semi-colon after return; correct usage is to write "yyterminate();" -
 * we don't want an extra ';' after the "return" because that will cause
 * some compilers to complain about unreachable statements.
 */
#ifndef yyterminate
#define yyterminate() return YY_NULL
#endif

/* Number of entries by which start-condition stack grows. */
#ifndef YY_START_STACK_INCR
#define YY_START_STACK_INCR 25
#endif

/* Report a fatal error. */
#ifndef YY_FATAL_ERROR
#define YY_FATAL_ERROR(msg) yy_fatal_error( msg )
#endif

/* end tables serialization structures and prototypes */

/* Default declaration of generated scanner - a define so the user can
 * easily add parameters.
 */
#ifndef YY_DECL
#define YY_DECL_IS_OURS 1

extern int fortranlex (void);

#define YY_DECL int fortranlex (void)
#endif /* !YY_DECL */

/* Code executed at the beginning of each rule, after fortrantext and fortranleng
 * have been set up.
 */
#ifndef YY_USER_ACTION
#define YY_USER_ACTION
#endif

/* Code executed at the end of each rule. */
#ifndef YY_BREAK
#define YY_BREAK break;
#endif

#define YY_RULE_SETUP \
	if ( fortranleng > 0 ) \
		YY_CURRENT_BUFFER_LVALUE->yy_at_bol = \
				(fortrantext[fortranleng - 1] == '\n'); \
	YY_USER_ACTION

/** The main scanner function which does all the work.
 */
YY_DECL
{
	register yy_state_type yy_current_state;
	register char *yy_cp, *yy_bp;
	register int yy_act;
    
#line 107 "fortran.lex"

  if (infixed) BEGIN(fortran77style) ;
  if (infree) BEGIN(fortran90style)  ;

#line 3174 "fortran.yy.c"

	if ( !(yy_init) )
		{
		(yy_init) = 1;

#ifdef YY_USER_INIT
		YY_USER_INIT;
#endif

		if ( ! (yy_start) )
			(yy_start) = 1;	/* first start state */

		if ( ! fortranin )
			fortranin = stdin;

		if ( ! fortranout )
			fortranout = stdout;

		if ( ! YY_CURRENT_BUFFER ) {
			fortranensure_buffer_stack ();
			YY_CURRENT_BUFFER_LVALUE =
				fortran_create_buffer(fortranin,YY_BUF_SIZE );
		}

		fortran_load_buffer_state( );
		}

	while ( 1 )		/* loops until end-of-file is reached */
		{
		yy_cp = (yy_c_buf_p);

		/* Support of fortrantext. */
		*yy_cp = (yy_hold_char);

		/* yy_bp points to the position in yy_ch_buf of the start of
		 * the current run.
		 */
		yy_bp = yy_cp;

		yy_current_state = (yy_start);
		yy_current_state += YY_AT_BOL();
yy_match:
		do
			{
			register YY_CHAR yy_c = yy_ec[YY_SC_TO_UI(*yy_cp)];
			if ( yy_accept[yy_current_state] )
				{
				(yy_last_accepting_state) = yy_current_state;
				(yy_last_accepting_cpos) = yy_cp;
				}
			while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
				{
				yy_current_state = (int) yy_def[yy_current_state];
				if ( yy_current_state >= 1162 )
					yy_c = yy_meta[(unsigned int) yy_c];
				}
			yy_current_state = yy_nxt[yy_base[yy_current_state] + (unsigned int) yy_c];
			++yy_cp;
			}
		while ( yy_base[yy_current_state] != 9159 );

yy_find_action:
		yy_act = yy_accept[yy_current_state];
		if ( yy_act == 0 )
			{ /* have to back up */
			yy_cp = (yy_last_accepting_cpos);
			yy_current_state = (yy_last_accepting_state);
			yy_act = yy_accept[yy_current_state];
			}

		YY_DO_BEFORE_ACTION;

do_action:	/* This label is used only to access EOF actions. */

		switch ( yy_act )
	{ /* beginning of action switch */
			case 0: /* must back up */
			/* undo the effects of YY_DO_BEFORE_ACTION */
			*yy_cp = (yy_hold_char);
			yy_cp = (yy_last_accepting_cpos);
			yy_current_state = (yy_last_accepting_state);
			goto yy_find_action;

case 1:
YY_RULE_SETUP
#line 111 "fortran.lex"
return TOK_DEBUT;
	YY_BREAK
case 2:
YY_RULE_SETUP
#line 112 "fortran.lex"
return TOK_FIN;
	YY_BREAK
case 3:
YY_RULE_SETUP
#line 113 "fortran.lex"
return TOK_OMP;
	YY_BREAK
case 4:
YY_RULE_SETUP
#line 114 "fortran.lex"
return TOK_DOLLAR;
	YY_BREAK
case 5:
YY_RULE_SETUP
#line 116 "fortran.lex"
{return TOK_REAL8;}
	YY_BREAK
case 6:
YY_RULE_SETUP
#line 117 "fortran.lex"
{return TOK_SUBROUTINE;}
	YY_BREAK
case 7:
YY_RULE_SETUP
#line 118 "fortran.lex"
{return TOK_PROGRAM;}
	YY_BREAK
case 8:
YY_RULE_SETUP
#line 119 "fortran.lex"
{inallocate = 1; return TOK_ALLOCATE;}
	YY_BREAK
case 9:
YY_RULE_SETUP
#line 120 "fortran.lex"
{return TOK_NULLIFY;}
	YY_BREAK
case 10:
YY_RULE_SETUP
#line 121 "fortran.lex"
{inallocate = 1; return TOK_DEALLOCATE;}
	YY_BREAK
case 11:
YY_RULE_SETUP
#line 122 "fortran.lex"
{return TOK_RESULT;}
	YY_BREAK
case 12:
YY_RULE_SETUP
#line 123 "fortran.lex"
{return TOK_FUNCTION;}
	YY_BREAK
case 13:
YY_RULE_SETUP
#line 124 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_ENDSUBROUTINE;}
	YY_BREAK
case 14:
YY_RULE_SETUP
#line 125 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_ENDPROGRAM;}
	YY_BREAK
case 15:
YY_RULE_SETUP
#line 126 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_ENDFUNCTION;}
	YY_BREAK
case 16:
YY_RULE_SETUP
#line 127 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_ENDUNIT;}
	YY_BREAK
case 17:
YY_RULE_SETUP
#line 128 "fortran.lex"
return TOK_INCLUDE;
	YY_BREAK
case 18:
YY_RULE_SETUP
#line 129 "fortran.lex"
{
                            strcpy(yylval.na,fortrantext);
                            tmpc = input();
                            unput(tmpc);
                            if ( (
                               tmpc >= 'a' && tmpc <= 'z'
                                  ) || (
                               tmpc >= 'A' && tmpc <= 'Z'
                               )  )
                               {
                                  return TOK_USE;
                               }
                               else
                               {
                                  return TOK_NAME;
                               }
                         }
	YY_BREAK
case 19:
YY_RULE_SETUP
#line 146 "fortran.lex"
{return TOK_REWIND;}
	YY_BREAK
case 20:
YY_RULE_SETUP
#line 147 "fortran.lex"
return TOK_IMPLICIT;
	YY_BREAK
case 21:
YY_RULE_SETUP
#line 148 "fortran.lex"
return TOK_NONE;
	YY_BREAK
case 22:
YY_RULE_SETUP
#line 149 "fortran.lex"
return TOK_CALL;
	YY_BREAK
case 23:
YY_RULE_SETUP
#line 150 "fortran.lex"
return TOK_TRUE;
	YY_BREAK
case 24:
YY_RULE_SETUP
#line 151 "fortran.lex"
return TOK_FALSE;
	YY_BREAK
case 25:
YY_RULE_SETUP
#line 152 "fortran.lex"
{return TOK_POINT_TO;}
	YY_BREAK
case 26:
YY_RULE_SETUP
#line 153 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_DASTER;}
	YY_BREAK
case 27:
YY_RULE_SETUP
#line 154 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_EQV;}
	YY_BREAK
case 28:
YY_RULE_SETUP
#line 155 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_EQ;}
	YY_BREAK
case 29:
YY_RULE_SETUP
#line 156 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_GT;}
	YY_BREAK
case 30:
YY_RULE_SETUP
#line 157 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_GE;}
	YY_BREAK
case 31:
YY_RULE_SETUP
#line 158 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_LT;}
	YY_BREAK
case 32:
YY_RULE_SETUP
#line 159 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_LE;}
	YY_BREAK
case 33:
YY_RULE_SETUP
#line 160 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_NEQV;}
	YY_BREAK
case 34:
YY_RULE_SETUP
#line 161 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_NE;}
	YY_BREAK
case 35:
YY_RULE_SETUP
#line 162 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_NOT;}
	YY_BREAK
case 36:
YY_RULE_SETUP
#line 163 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_OR;}
	YY_BREAK
case 37:
YY_RULE_SETUP
#line 164 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_XOR;}
	YY_BREAK
case 38:
YY_RULE_SETUP
#line 165 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_AND;}
	YY_BREAK
case 39:
YY_RULE_SETUP
#line 166 "fortran.lex"
{return TOK_MODULE;}
	YY_BREAK
case 40:
YY_RULE_SETUP
#line 167 "fortran.lex"
{return TOK_DOWHILE;}
	YY_BREAK
case 41:
YY_RULE_SETUP
#line 168 "fortran.lex"
return TOK_ENDMODULE;
	YY_BREAK
case 42:
YY_RULE_SETUP
#line 169 "fortran.lex"
return TOK_ENDDO;
	YY_BREAK
case 43:
YY_RULE_SETUP
#line 170 "fortran.lex"
{return TOK_PLAINDO;}
	YY_BREAK
case 44:
YY_RULE_SETUP
#line 171 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_REAL;}
	YY_BREAK
case 45:
YY_RULE_SETUP
#line 172 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_INTEGER;}
	YY_BREAK
case 46:
YY_RULE_SETUP
#line 173 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_LOGICAL;}
	YY_BREAK
case 47:
YY_RULE_SETUP
#line 174 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_CHARACTER;}
	YY_BREAK
case 48:
YY_RULE_SETUP
#line 175 "fortran.lex"
{return TOK_ALLOCATABLE;}
	YY_BREAK
case 49:
YY_RULE_SETUP
#line 176 "fortran.lex"
return TOK_CLOSE;
	YY_BREAK
case 50:
YY_RULE_SETUP
#line 177 "fortran.lex"
return TOK_INQUIRE;
	YY_BREAK
case 51:
YY_RULE_SETUP
#line 178 "fortran.lex"
{return TOK_DIMENSION;}
	YY_BREAK
case 52:
YY_RULE_SETUP
#line 179 "fortran.lex"
return TOK_PAUSE;
	YY_BREAK
case 53:
YY_RULE_SETUP
#line 180 "fortran.lex"
return TOK_EQUIVALENCE;
	YY_BREAK
case 54:
YY_RULE_SETUP
#line 181 "fortran.lex"
return TOK_STOP;
	YY_BREAK
case 55:
YY_RULE_SETUP
#line 182 "fortran.lex"
return TOK_WHERE;
	YY_BREAK
case 56:
YY_RULE_SETUP
#line 183 "fortran.lex"
return TOK_ENDWHERE;
	YY_BREAK
case 57:
YY_RULE_SETUP
#line 184 "fortran.lex"
return TOK_ELSEWHERE;
	YY_BREAK
case 58:
YY_RULE_SETUP
#line 185 "fortran.lex"
{return TOK_COMPLEX;}
	YY_BREAK
case 59:
YY_RULE_SETUP
#line 186 "fortran.lex"
{return TOK_CONTAINS;}
	YY_BREAK
case 60:
YY_RULE_SETUP
#line 187 "fortran.lex"
{return TOK_ONLY;}
	YY_BREAK
case 61:
YY_RULE_SETUP
#line 188 "fortran.lex"
{return TOK_PARAMETER;}
	YY_BREAK
case 62:
YY_RULE_SETUP
#line 189 "fortran.lex"
{return TOK_RECURSIVE;}
	YY_BREAK
case 63:
YY_RULE_SETUP
#line 190 "fortran.lex"
{return TOK_COMMON;}
	YY_BREAK
case 64:
YY_RULE_SETUP
#line 191 "fortran.lex"
{return TOK_GLOBAL;}
	YY_BREAK
case 65:
YY_RULE_SETUP
#line 192 "fortran.lex"
{return TOK_EXTERNAL;}
	YY_BREAK
case 66:
YY_RULE_SETUP
#line 193 "fortran.lex"
{return TOK_INTENT;}
	YY_BREAK
case 67:
YY_RULE_SETUP
#line 194 "fortran.lex"
{return TOK_POINTER;}
	YY_BREAK
case 68:
YY_RULE_SETUP
#line 195 "fortran.lex"
{return TOK_OPTIONAL;}
	YY_BREAK
case 69:
YY_RULE_SETUP
#line 196 "fortran.lex"
{return TOK_SAVE;}
	YY_BREAK
case 70:
YY_RULE_SETUP
#line 197 "fortran.lex"
{return TOK_TYPE;}
	YY_BREAK
case 71:
YY_RULE_SETUP
#line 198 "fortran.lex"
{return TOK_TYPEPAR;}
	YY_BREAK
case 72:
YY_RULE_SETUP
#line 199 "fortran.lex"
{if (inallocate == 1) return TOK_STAT; else {strcpy(yylval.na,fortrantext);return TOK_NAME;}}
	YY_BREAK
case 73:
YY_RULE_SETUP
#line 200 "fortran.lex"
{return TOK_ENDTYPE;}
	YY_BREAK
case 74:
YY_RULE_SETUP
#line 201 "fortran.lex"
return TOK_OPEN;
	YY_BREAK
case 75:
YY_RULE_SETUP
#line 202 "fortran.lex"
return TOK_RETURN;
	YY_BREAK
case 76:
/* rule 76 can match eol */
YY_RULE_SETUP
#line 203 "fortran.lex"
return TOK_EXIT;
	YY_BREAK
case 77:
YY_RULE_SETUP
#line 204 "fortran.lex"
return TOK_PRINT;
	YY_BREAK
case 78:
YY_RULE_SETUP
#line 205 "fortran.lex"
{return TOK_PROCEDURE;}
	YY_BREAK
case 79:
YY_RULE_SETUP
#line 206 "fortran.lex"
{return TOK_READ;}
	YY_BREAK
case 80:
YY_RULE_SETUP
#line 207 "fortran.lex"
{return TOK_NAMELIST;}
	YY_BREAK
case 81:
YY_RULE_SETUP
#line 208 "fortran.lex"
{return TOK_WRITE;}
	YY_BREAK
case 82:
YY_RULE_SETUP
#line 209 "fortran.lex"
{return TOK_TARGET;}
	YY_BREAK
case 83:
YY_RULE_SETUP
#line 210 "fortran.lex"
{return TOK_PUBLIC;}
	YY_BREAK
case 84:
YY_RULE_SETUP
#line 211 "fortran.lex"
{return TOK_PRIVATE;}
	YY_BREAK
case 85:
YY_RULE_SETUP
#line 212 "fortran.lex"
{strcpy(yylval.nac,fortrantext);return TOK_IN;}
	YY_BREAK
case 86:
YY_RULE_SETUP
#line 213 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_DATA;}
	YY_BREAK
case 87:
YY_RULE_SETUP
#line 214 "fortran.lex"
return TOK_CONTINUE;
	YY_BREAK
case 88:
YY_RULE_SETUP
#line 215 "fortran.lex"
{return TOK_PLAINGOTO;}
	YY_BREAK
case 89:
YY_RULE_SETUP
#line 216 "fortran.lex"
{strcpy(yylval.nac,fortrantext);return TOK_OUT;}
	YY_BREAK
case 90:
YY_RULE_SETUP
#line 217 "fortran.lex"
{strcpy(yylval.nac,fortrantext);return TOK_INOUT;}
	YY_BREAK
case 91:
YY_RULE_SETUP
#line 218 "fortran.lex"
{return TOK_INTRINSIC;}
	YY_BREAK
case 92:
YY_RULE_SETUP
#line 219 "fortran.lex"
{return TOK_THEN;}
	YY_BREAK
case 93:
YY_RULE_SETUP
#line 220 "fortran.lex"
{return TOK_ELSEIF;}
	YY_BREAK
case 94:
YY_RULE_SETUP
#line 221 "fortran.lex"
{return TOK_ELSE;}
	YY_BREAK
case 95:
YY_RULE_SETUP
#line 222 "fortran.lex"
{return TOK_ENDIF;}
	YY_BREAK
case 96:
YY_RULE_SETUP
#line 223 "fortran.lex"
{return TOK_LOGICALIF;}
	YY_BREAK
case 97:
YY_RULE_SETUP
#line 224 "fortran.lex"
{return TOK_SUM;}
	YY_BREAK
case 98:
YY_RULE_SETUP
#line 225 "fortran.lex"
{return TOK_MAX;}
	YY_BREAK
case 99:
YY_RULE_SETUP
#line 226 "fortran.lex"
{return TOK_TANH;}
	YY_BREAK
case 100:
YY_RULE_SETUP
#line 227 "fortran.lex"
{return TOK_MAXVAL;}
	YY_BREAK
case 101:
YY_RULE_SETUP
#line 228 "fortran.lex"
{return TOK_TRIM;}
	YY_BREAK
case 102:
YY_RULE_SETUP
#line 229 "fortran.lex"
{return TOK_SQRT;}
	YY_BREAK
case 103:
YY_RULE_SETUP
#line 230 "fortran.lex"
{return TOK_SELECTCASE;}
	YY_BREAK
case 104:
YY_RULE_SETUP
#line 231 "fortran.lex"
{return TOK_CASE;}
	YY_BREAK
case 105:
YY_RULE_SETUP
#line 232 "fortran.lex"
{return TOK_CASEDEFAULT;}
	YY_BREAK
case 106:
YY_RULE_SETUP
#line 233 "fortran.lex"
{return TOK_ENDSELECT;}
	YY_BREAK
case 107:
YY_RULE_SETUP
#line 234 "fortran.lex"
{return TOK_FILE;}
	YY_BREAK
case 108:
YY_RULE_SETUP
#line 235 "fortran.lex"
{return TOK_END;}
	YY_BREAK
case 109:
YY_RULE_SETUP
#line 236 "fortran.lex"
{return TOK_ERR;}
	YY_BREAK
case 110:
YY_RULE_SETUP
#line 237 "fortran.lex"
{return TOK_EXIST;}
	YY_BREAK
case 111:
YY_RULE_SETUP
#line 238 "fortran.lex"
{return TOK_MIN;}
	YY_BREAK
case 112:
YY_RULE_SETUP
#line 239 "fortran.lex"
{return TOK_NINT;}
	YY_BREAK
case 113:
YY_RULE_SETUP
#line 240 "fortran.lex"
{return TOK_FLOAT;}
	YY_BREAK
case 114:
YY_RULE_SETUP
#line 241 "fortran.lex"
{return TOK_EXP;}
	YY_BREAK
case 115:
YY_RULE_SETUP
#line 242 "fortran.lex"
{return TOK_COS;}
	YY_BREAK
case 116:
YY_RULE_SETUP
#line 243 "fortran.lex"
{return TOK_COSH;}
	YY_BREAK
case 117:
YY_RULE_SETUP
#line 244 "fortran.lex"
{return TOK_ACOS;}
	YY_BREAK
case 118:
YY_RULE_SETUP
#line 245 "fortran.lex"
{return TOK_SIN;}
	YY_BREAK
case 119:
YY_RULE_SETUP
#line 246 "fortran.lex"
{return TOK_SINH;}
	YY_BREAK
case 120:
YY_RULE_SETUP
#line 247 "fortran.lex"
{return TOK_ASIN;}
	YY_BREAK
case 121:
YY_RULE_SETUP
#line 248 "fortran.lex"
{return TOK_LOG;}
	YY_BREAK
case 122:
YY_RULE_SETUP
#line 249 "fortran.lex"
{return TOK_TAN;}
	YY_BREAK
case 123:
YY_RULE_SETUP
#line 250 "fortran.lex"
{return TOK_ATAN;}
	YY_BREAK
case 124:
YY_RULE_SETUP
#line 251 "fortran.lex"
{return TOK_CYCLE;}
	YY_BREAK
case 125:
YY_RULE_SETUP
#line 252 "fortran.lex"
{return TOK_ABS;}
	YY_BREAK
case 126:
YY_RULE_SETUP
#line 253 "fortran.lex"
{return TOK_MOD;}
	YY_BREAK
case 127:
YY_RULE_SETUP
#line 254 "fortran.lex"
{return TOK_SIGN;}
	YY_BREAK
case 128:
YY_RULE_SETUP
#line 255 "fortran.lex"
{return TOK_MINLOC;}
	YY_BREAK
case 129:
YY_RULE_SETUP
#line 256 "fortran.lex"
{return TOK_MAXLOC;}
	YY_BREAK
case 130:
YY_RULE_SETUP
#line 257 "fortran.lex"
{return TOK_MINVAL;}
	YY_BREAK
case 131:
YY_RULE_SETUP
#line 258 "fortran.lex"
{return TOK_BACKSPACE;}
	YY_BREAK
case 132:
YY_RULE_SETUP
#line 259 "fortran.lex"
{return TOK_LEFTAB;}
	YY_BREAK
case 133:
YY_RULE_SETUP
#line 260 "fortran.lex"
{return TOK_RIGHTAB;}
	YY_BREAK
case 134:
/* rule 134 can match eol */
YY_RULE_SETUP
#line 261 "fortran.lex"
{return TOK_FORMAT;}
	YY_BREAK
case 135:
YY_RULE_SETUP
#line 262 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_DOUBLEPRECISION;}
	YY_BREAK
case 136:
YY_RULE_SETUP
#line 263 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_DOUBLECOMPLEX;}
	YY_BREAK
case 137:
YY_RULE_SETUP
#line 264 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_SLASH;}
	YY_BREAK
case 138:
YY_RULE_SETUP
#line 265 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_DSLASH;}
	YY_BREAK
case 139:
/* rule 139 can match eol */
YY_RULE_SETUP
#line 266 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_CHAR_CUT;}
	YY_BREAK
case 140:
YY_RULE_SETUP
#line 267 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_CHAR_CONSTANT;}
	YY_BREAK
case 141:
YY_RULE_SETUP
#line 268 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_CHAR_MESSAGE;}
	YY_BREAK
case 142:
YY_RULE_SETUP
#line 269 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_CHAR_INT;}
	YY_BREAK
case 143:
YY_RULE_SETUP
#line 270 "fortran.lex"
{printf("debug interfacer\n");BEGIN(donottreat);}
	YY_BREAK
case 144:
/* rule 144 can match eol */
YY_RULE_SETUP
#line 271 "fortran.lex"
{
						BEGIN(INITIAL);
                        if (infixed) BEGIN(fortran77style) ;
                        if (infree) BEGIN(fortran90style)  ;
                        line_num_fortran++;line_num_fortran_common++;
                        return '\n';
						}
	YY_BREAK
case 145:
YY_RULE_SETUP
#line 278 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_NAME;}
	YY_BREAK
case 146:
YY_RULE_SETUP
#line 279 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_CSTREAL;}
	YY_BREAK
case 147:
YY_RULE_SETUP
#line 280 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_CSTREALDP;}
	YY_BREAK
case 148:
YY_RULE_SETUP
#line 281 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_CSTREALQP;}
	YY_BREAK
case 149:
/* rule 149 can match eol */
*yy_cp = (yy_hold_char); /* undo effects of setting up fortrantext */
(yy_c_buf_p) = yy_cp -= 1;
YY_DO_BEFORE_ACTION; /* set up fortrantext again */
YY_RULE_SETUP
#line 282 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_CSTREAL;}
	YY_BREAK
case 150:
YY_RULE_SETUP
#line 283 "fortran.lex"
{strcpy(yylval.na,fortrantext);return TOK_CSTINT;}
	YY_BREAK
case 151:
YY_RULE_SETUP
#line 284 "fortran.lex"
{}
	YY_BREAK
case 152:
YY_RULE_SETUP
#line 285 "fortran.lex"
{return TOK_QUOTE;}
	YY_BREAK
case 153:
YY_RULE_SETUP
#line 286 "fortran.lex"
{}
	YY_BREAK
case 154:
YY_RULE_SETUP
#line 287 "fortran.lex"
{strcpy(yylval.na,fortrantext);return (int) *fortrantext;}
	YY_BREAK
case 155:
YY_RULE_SETUP
#line 288 "fortran.lex"
{afterpercent = 1; strcpy(yylval.na,fortrantext);return (int) *fortrantext;}
	YY_BREAK
case 156:
YY_RULE_SETUP
#line 289 "fortran.lex"
{return TOK_SEMICOLON;}
	YY_BREAK
case 157:
YY_RULE_SETUP
#line 290 "fortran.lex"
{return (int) *fortrantext;}
	YY_BREAK
case 158:
YY_RULE_SETUP
#line 291 "fortran.lex"
{return (int) *fortrantext;}
	YY_BREAK
case 159:
YY_RULE_SETUP
#line 292 "fortran.lex"
{return (int) *fortrantext;}
	YY_BREAK
case 160:
YY_RULE_SETUP
#line 293 "fortran.lex"
{return (int) *fortrantext;}
	YY_BREAK
case 161:
/* rule 161 can match eol */
YY_RULE_SETUP
#line 294 "fortran.lex"
{colnum=0;line_num_fortran++;line_num_fortran_common++; return (int) *fortrantext;}
	YY_BREAK
case 162:
*yy_cp = (yy_hold_char); /* undo effects of setting up fortrantext */
(yy_c_buf_p) = yy_cp -= 1;
YY_DO_BEFORE_ACTION; /* set up fortrantext again */
YY_RULE_SETUP
#line 295 "fortran.lex"

	YY_BREAK
case 163:
YY_RULE_SETUP
#line 296 "fortran.lex"
{if (newlinef90 == 0) return TOK_LABEL; else newlinef90 = 0;}
	YY_BREAK
case 164:
YY_RULE_SETUP
#line 297 "fortran.lex"

	YY_BREAK
case 165:
YY_RULE_SETUP
#line 298 "fortran.lex"
{colnum=colnum-1+tabsize;}
	YY_BREAK
case 166:
YY_RULE_SETUP
#line 299 "fortran.lex"
;
	YY_BREAK
case 167:
/* rule 167 can match eol */
YY_RULE_SETUP
#line 300 "fortran.lex"
{line_num_fortran++;line_num_fortran_common++;newlinef90=1;colnum=0;}
	YY_BREAK
case 168:
/* rule 168 can match eol */
YY_RULE_SETUP
#line 301 "fortran.lex"
{line_num_fortran++;line_num_fortran_common++;colnum=0;}
	YY_BREAK
case 169:
/* rule 169 can match eol */
YY_RULE_SETUP
#line 302 "fortran.lex"
{
                           convert2lower(motparse1);
                           if ( strncasecmp(motparse1,"contains",8) == 0 )
                           {
                              return TOK_CONTAINS;
                           }
                           else
                           {
                            /*  colnum=0;line_num_fortran++;line_num_fortran_common++;*/
                             if ( !strcasecmp(motparse1,"C$AGRIF_DO_NOT_TREAT\n")) 
                             return TOK_DONOTTREAT;
                             if ( !strcasecmp(motparse1,"C$AGRIF_END_DO_NOT_TREAT\n")) return TOK_ENDDONOTTREAT;
                             unput('\n');
                           }
                         }
	YY_BREAK
case 170:
/* rule 170 can match eol */
YY_RULE_SETUP
#line 317 "fortran.lex"
{
                           convert2lower(&motparse1[1]);
                           if ( strncasecmp(&motparse1[1],"contains",8) == 0 )
                           {
                              return TOK_CONTAINS;
                           }
                           else
                           {
                             /* colnum=0;line_num_fortran++;line_num_fortran_common++;*/
                             if ( !strcasecmp(&motparse1[1],"C$AGRIF_DO_NOT_TREAT\n")) 
                             return TOK_DONOTTREAT;
                             if ( !strcasecmp(&motparse1[1],"C$AGRIF_END_DO_NOT_TREAT\n")) return TOK_ENDDONOTTREAT;
                             unput('\n');
                           }
                         }                            
	YY_BREAK
case 171:
/* rule 171 can match eol */
YY_RULE_SETUP
#line 332 "fortran.lex"
{
						BEGIN(donottreat);
						}
	YY_BREAK
case 172:
/* rule 172 can match eol */
YY_RULE_SETUP
#line 335 "fortran.lex"
{
						BEGIN(INITIAL);
                        if (infixed) BEGIN(fortran77style) ;
                        if (infree) BEGIN(fortran90style)  ;
                        line_num_fortran++;line_num_fortran_common++;
                        return '\n';
						}
	YY_BREAK
case 173:
/* rule 173 can match eol */
YY_RULE_SETUP
#line 342 "fortran.lex"
{line_num_fortran++;line_num_fortran_common++;}
	YY_BREAK
case 174:
/* rule 174 can match eol */
YY_RULE_SETUP
#line 343 "fortran.lex"
{
                             colnum = 0;
                             if ( !strcasecmp(motparse1,"!$AGRIF_DO_NOT_TREAT\n")) return TOK_DONOTTREAT;
                             if ( !strcasecmp(motparse1,"!$AGRIF_END_DO_NOT_TREAT\n")) return TOK_ENDDONOTTREAT;
                          }
	YY_BREAK
case 175:
YY_RULE_SETUP
#line 348 "fortran.lex"
{
                             colnum = 0;
                             if ( !strcasecmp(motparse1,"!$AGRIF_DO_NOT_TREAT\n")) return TOK_DONOTTREAT;
                             if ( !strcasecmp(motparse1,"!$AGRIF_END_DO_NOT_TREAT\n")) return TOK_ENDDONOTTREAT;
                          }
	YY_BREAK
case 176:
YY_RULE_SETUP
#line 353 "fortran.lex"
ECHO;
	YY_BREAK
#line 4224 "fortran.yy.c"
case YY_STATE_EOF(INITIAL):
case YY_STATE_EOF(parameter):
case YY_STATE_EOF(character):
case YY_STATE_EOF(donottreat):
case YY_STATE_EOF(fortran77style):
case YY_STATE_EOF(fortran90style):
	yyterminate();

	case YY_END_OF_BUFFER:
		{
		/* Amount of text matched not including the EOB char. */
		int yy_amount_of_matched_text = (int) (yy_cp - (yytext_ptr)) - 1;

		/* Undo the effects of YY_DO_BEFORE_ACTION. */
		*yy_cp = (yy_hold_char);
		YY_RESTORE_YY_MORE_OFFSET

		if ( YY_CURRENT_BUFFER_LVALUE->yy_buffer_status == YY_BUFFER_NEW )
			{
			/* We're scanning a new file or input source.  It's
			 * possible that this happened because the user
			 * just pointed fortranin at a new source and called
			 * fortranlex().  If so, then we have to assure
			 * consistency between YY_CURRENT_BUFFER and our
			 * globals.  Here is the right place to do so, because
			 * this is the first action (other than possibly a
			 * back-up) that will match for the new input source.
			 */
			(yy_n_chars) = YY_CURRENT_BUFFER_LVALUE->yy_n_chars;
			YY_CURRENT_BUFFER_LVALUE->yy_input_file = fortranin;
			YY_CURRENT_BUFFER_LVALUE->yy_buffer_status = YY_BUFFER_NORMAL;
			}

		/* Note that here we test for yy_c_buf_p "<=" to the position
		 * of the first EOB in the buffer, since yy_c_buf_p will
		 * already have been incremented past the NUL character
		 * (since all states make transitions on EOB to the
		 * end-of-buffer state).  Contrast this with the test
		 * in input().
		 */
		if ( (yy_c_buf_p) <= &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)] )
			{ /* This was really a NUL. */
			yy_state_type yy_next_state;

			(yy_c_buf_p) = (yytext_ptr) + yy_amount_of_matched_text;

			yy_current_state = yy_get_previous_state(  );

			/* Okay, we're now positioned to make the NUL
			 * transition.  We couldn't have
			 * yy_get_previous_state() go ahead and do it
			 * for us because it doesn't know how to deal
			 * with the possibility of jamming (and we don't
			 * want to build jamming into it because then it
			 * will run more slowly).
			 */

			yy_next_state = yy_try_NUL_trans( yy_current_state );

			yy_bp = (yytext_ptr) + YY_MORE_ADJ;

			if ( yy_next_state )
				{
				/* Consume the NUL. */
				yy_cp = ++(yy_c_buf_p);
				yy_current_state = yy_next_state;
				goto yy_match;
				}

			else
				{
				yy_cp = (yy_c_buf_p);
				goto yy_find_action;
				}
			}

		else switch ( yy_get_next_buffer(  ) )
			{
			case EOB_ACT_END_OF_FILE:
				{
				(yy_did_buffer_switch_on_eof) = 0;

				if ( fortranwrap( ) )
					{
					/* Note: because we've taken care in
					 * yy_get_next_buffer() to have set up
					 * fortrantext, we can now set up
					 * yy_c_buf_p so that if some total
					 * hoser (like flex itself) wants to
					 * call the scanner after we return the
					 * YY_NULL, it'll still work - another
					 * YY_NULL will get returned.
					 */
					(yy_c_buf_p) = (yytext_ptr) + YY_MORE_ADJ;

					yy_act = YY_STATE_EOF(YY_START);
					goto do_action;
					}

				else
					{
					if ( ! (yy_did_buffer_switch_on_eof) )
						YY_NEW_FILE;
					}
				break;
				}

			case EOB_ACT_CONTINUE_SCAN:
				(yy_c_buf_p) =
					(yytext_ptr) + yy_amount_of_matched_text;

				yy_current_state = yy_get_previous_state(  );

				yy_cp = (yy_c_buf_p);
				yy_bp = (yytext_ptr) + YY_MORE_ADJ;
				goto yy_match;

			case EOB_ACT_LAST_MATCH:
				(yy_c_buf_p) =
				&YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)];

				yy_current_state = yy_get_previous_state(  );

				yy_cp = (yy_c_buf_p);
				yy_bp = (yytext_ptr) + YY_MORE_ADJ;
				goto yy_find_action;
			}
		break;
		}

	default:
		YY_FATAL_ERROR(
			"fatal flex scanner internal error--no action found" );
	} /* end of action switch */
		} /* end of scanning one token */
} /* end of fortranlex */

/* yy_get_next_buffer - try to read in a new buffer
 *
 * Returns a code representing an action:
 *	EOB_ACT_LAST_MATCH -
 *	EOB_ACT_CONTINUE_SCAN - continue scanning from current position
 *	EOB_ACT_END_OF_FILE - end of file
 */
static int yy_get_next_buffer (void)
{
    	register char *dest = YY_CURRENT_BUFFER_LVALUE->yy_ch_buf;
	register char *source = (yytext_ptr);
	register int number_to_move, i;
	int ret_val;

	if ( (yy_c_buf_p) > &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars) + 1] )
		YY_FATAL_ERROR(
		"fatal flex scanner internal error--end of buffer missed" );

	if ( YY_CURRENT_BUFFER_LVALUE->yy_fill_buffer == 0 )
		{ /* Don't try to fill the buffer, so this is an EOF. */
		if ( (yy_c_buf_p) - (yytext_ptr) - YY_MORE_ADJ == 1 )
			{
			/* We matched a single character, the EOB, so
			 * treat this as a final EOF.
			 */
			return EOB_ACT_END_OF_FILE;
			}

		else
			{
			/* We matched some text prior to the EOB, first
			 * process it.
			 */
			return EOB_ACT_LAST_MATCH;
			}
		}

	/* Try to read more data. */

	/* First move last chars to start of buffer. */
	number_to_move = (int) ((yy_c_buf_p) - (yytext_ptr)) - 1;

	for ( i = 0; i < number_to_move; ++i )
		*(dest++) = *(source++);

	if ( YY_CURRENT_BUFFER_LVALUE->yy_buffer_status == YY_BUFFER_EOF_PENDING )
		/* don't do the read, it's not guaranteed to return an EOF,
		 * just force an EOF
		 */
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars) = 0;

	else
		{
			yy_size_t num_to_read =
			YY_CURRENT_BUFFER_LVALUE->yy_buf_size - number_to_move - 1;

		while ( num_to_read <= 0 )
			{ /* Not enough room in the buffer - grow it. */

			/* just a shorter name for the current buffer */
			YY_BUFFER_STATE b = YY_CURRENT_BUFFER;

			int yy_c_buf_p_offset =
				(int) ((yy_c_buf_p) - b->yy_ch_buf);

			if ( b->yy_is_our_buffer )
				{
				yy_size_t new_size = b->yy_buf_size * 2;

				if ( new_size <= 0 )
					b->yy_buf_size += b->yy_buf_size / 8;
				else
					b->yy_buf_size *= 2;

				b->yy_ch_buf = (char *)
					/* Include room in for 2 EOB chars. */
					fortranrealloc((void *) b->yy_ch_buf,b->yy_buf_size + 2  );
				}
			else
				/* Can't grow it, we don't own it. */
				b->yy_ch_buf = 0;

			if ( ! b->yy_ch_buf )
				YY_FATAL_ERROR(
				"fatal error - scanner input buffer overflow" );

			(yy_c_buf_p) = &b->yy_ch_buf[yy_c_buf_p_offset];

			num_to_read = YY_CURRENT_BUFFER_LVALUE->yy_buf_size -
						number_to_move - 1;

			}

		if ( num_to_read > YY_READ_BUF_SIZE )
			num_to_read = YY_READ_BUF_SIZE;

		/* Read in more data. */
		YY_INPUT( (&YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[number_to_move]),
			(yy_n_chars), num_to_read );

		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars);
		}

	if ( (yy_n_chars) == 0 )
		{
		if ( number_to_move == YY_MORE_ADJ )
			{
			ret_val = EOB_ACT_END_OF_FILE;
			fortranrestart(fortranin  );
			}

		else
			{
			ret_val = EOB_ACT_LAST_MATCH;
			YY_CURRENT_BUFFER_LVALUE->yy_buffer_status =
				YY_BUFFER_EOF_PENDING;
			}
		}

	else
		ret_val = EOB_ACT_CONTINUE_SCAN;

	if ((yy_size_t) ((yy_n_chars) + number_to_move) > YY_CURRENT_BUFFER_LVALUE->yy_buf_size) {
		/* Extend the array by 50%, plus the number we really need. */
		yy_size_t new_size = (yy_n_chars) + number_to_move + ((yy_n_chars) >> 1);
		YY_CURRENT_BUFFER_LVALUE->yy_ch_buf = (char *) fortranrealloc((void *) YY_CURRENT_BUFFER_LVALUE->yy_ch_buf,new_size  );
		if ( ! YY_CURRENT_BUFFER_LVALUE->yy_ch_buf )
			YY_FATAL_ERROR( "out of dynamic memory in yy_get_next_buffer()" );
	}

	(yy_n_chars) += number_to_move;
	YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)] = YY_END_OF_BUFFER_CHAR;
	YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars) + 1] = YY_END_OF_BUFFER_CHAR;

	(yytext_ptr) = &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[0];

	return ret_val;
}

/* yy_get_previous_state - get the state just before the EOB char was reached */

    static yy_state_type yy_get_previous_state (void)
{
	register yy_state_type yy_current_state;
	register char *yy_cp;
    
	yy_current_state = (yy_start);
	yy_current_state += YY_AT_BOL();

	for ( yy_cp = (yytext_ptr) + YY_MORE_ADJ; yy_cp < (yy_c_buf_p); ++yy_cp )
		{
		register YY_CHAR yy_c = (*yy_cp ? yy_ec[YY_SC_TO_UI(*yy_cp)] : 1);
		if ( yy_accept[yy_current_state] )
			{
			(yy_last_accepting_state) = yy_current_state;
			(yy_last_accepting_cpos) = yy_cp;
			}
		while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
			{
			yy_current_state = (int) yy_def[yy_current_state];
			if ( yy_current_state >= 1162 )
				yy_c = yy_meta[(unsigned int) yy_c];
			}
		yy_current_state = yy_nxt[yy_base[yy_current_state] + (unsigned int) yy_c];
		}

	return yy_current_state;
}

/* yy_try_NUL_trans - try to make a transition on the NUL character
 *
 * synopsis
 *	next_state = yy_try_NUL_trans( current_state );
 */
    static yy_state_type yy_try_NUL_trans  (yy_state_type yy_current_state )
{
	register int yy_is_jam;
    	register char *yy_cp = (yy_c_buf_p);

	register YY_CHAR yy_c = 1;
	if ( yy_accept[yy_current_state] )
		{
		(yy_last_accepting_state) = yy_current_state;
		(yy_last_accepting_cpos) = yy_cp;
		}
	while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
		{
		yy_current_state = (int) yy_def[yy_current_state];
		if ( yy_current_state >= 1162 )
			yy_c = yy_meta[(unsigned int) yy_c];
		}
	yy_current_state = yy_nxt[yy_base[yy_current_state] + (unsigned int) yy_c];
	yy_is_jam = (yy_current_state == 1161);

	return yy_is_jam ? 0 : yy_current_state;
}

    static void yyunput (int c, register char * yy_bp )
{
	register char *yy_cp;
    
    yy_cp = (yy_c_buf_p);

	/* undo effects of setting up fortrantext */
	*yy_cp = (yy_hold_char);

	if ( yy_cp < YY_CURRENT_BUFFER_LVALUE->yy_ch_buf + 2 )
		{ /* need to shift things up to make room */
		/* +2 for EOB chars. */
		register yy_size_t number_to_move = (yy_n_chars) + 2;
		register char *dest = &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[
					YY_CURRENT_BUFFER_LVALUE->yy_buf_size + 2];
		register char *source =
				&YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[number_to_move];

		while ( source > YY_CURRENT_BUFFER_LVALUE->yy_ch_buf )
			*--dest = *--source;

		yy_cp += (int) (dest - source);
		yy_bp += (int) (dest - source);
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars =
			(yy_n_chars) = YY_CURRENT_BUFFER_LVALUE->yy_buf_size;

		if ( yy_cp < YY_CURRENT_BUFFER_LVALUE->yy_ch_buf + 2 )
			YY_FATAL_ERROR( "flex scanner push-back overflow" );
		}

	*--yy_cp = (char) c;

	(yytext_ptr) = yy_bp;
	(yy_hold_char) = *yy_cp;
	(yy_c_buf_p) = yy_cp;
}

#ifndef YY_NO_INPUT
#ifdef __cplusplus
    static int yyinput (void)
#else
    static int input  (void)
#endif

{
	int c;
    
	*(yy_c_buf_p) = (yy_hold_char);

	if ( *(yy_c_buf_p) == YY_END_OF_BUFFER_CHAR )
		{
		/* yy_c_buf_p now points to the character we want to return.
		 * If this occurs *before* the EOB characters, then it's a
		 * valid NUL; if not, then we've hit the end of the buffer.
		 */
		if ( (yy_c_buf_p) < &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)] )
			/* This was really a NUL. */
			*(yy_c_buf_p) = '\0';

		else
			{ /* need more input */
			yy_size_t offset = (yy_c_buf_p) - (yytext_ptr);
			++(yy_c_buf_p);

			switch ( yy_get_next_buffer(  ) )
				{
				case EOB_ACT_LAST_MATCH:
					/* This happens because yy_g_n_b()
					 * sees that we've accumulated a
					 * token and flags that we need to
					 * try matching the token before
					 * proceeding.  But for input(),
					 * there's no matching to consider.
					 * So convert the EOB_ACT_LAST_MATCH
					 * to EOB_ACT_END_OF_FILE.
					 */

					/* Reset buffer status. */
					fortranrestart(fortranin );

					/*FALLTHROUGH*/

				case EOB_ACT_END_OF_FILE:
					{
					if ( fortranwrap( ) )
						return 0;

					if ( ! (yy_did_buffer_switch_on_eof) )
						YY_NEW_FILE;
#ifdef __cplusplus
					return yyinput();
#else
					return input();
#endif
					}

				case EOB_ACT_CONTINUE_SCAN:
					(yy_c_buf_p) = (yytext_ptr) + offset;
					break;
				}
			}
		}

	c = *(unsigned char *) (yy_c_buf_p);	/* cast for 8-bit char's */
	*(yy_c_buf_p) = '\0';	/* preserve fortrantext */
	(yy_hold_char) = *++(yy_c_buf_p);

	YY_CURRENT_BUFFER_LVALUE->yy_at_bol = (c == '\n');

	return c;
}
#endif	/* ifndef YY_NO_INPUT */

/** Immediately switch to a different input stream.
 * @param input_file A readable stream.
 * 
 * @note This function does not reset the start condition to @c INITIAL .
 */
    void fortranrestart  (FILE * input_file )
{
    
	if ( ! YY_CURRENT_BUFFER ){
        fortranensure_buffer_stack ();
		YY_CURRENT_BUFFER_LVALUE =
            fortran_create_buffer(fortranin,YY_BUF_SIZE );
	}

	fortran_init_buffer(YY_CURRENT_BUFFER,input_file );
	fortran_load_buffer_state( );
}

/** Switch to a different input buffer.
 * @param new_buffer The new input buffer.
 * 
 */
    void fortran_switch_to_buffer  (YY_BUFFER_STATE  new_buffer )
{
    
	/* TODO. We should be able to replace this entire function body
	 * with
	 *		fortranpop_buffer_state();
	 *		fortranpush_buffer_state(new_buffer);
     */
	fortranensure_buffer_stack ();
	if ( YY_CURRENT_BUFFER == new_buffer )
		return;

	if ( YY_CURRENT_BUFFER )
		{
		/* Flush out information for old buffer. */
		*(yy_c_buf_p) = (yy_hold_char);
		YY_CURRENT_BUFFER_LVALUE->yy_buf_pos = (yy_c_buf_p);
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars);
		}

	YY_CURRENT_BUFFER_LVALUE = new_buffer;
	fortran_load_buffer_state( );

	/* We don't actually know whether we did this switch during
	 * EOF (fortranwrap()) processing, but the only time this flag
	 * is looked at is after fortranwrap() is called, so it's safe
	 * to go ahead and always set it.
	 */
	(yy_did_buffer_switch_on_eof) = 1;
}

static void fortran_load_buffer_state  (void)
{
    	(yy_n_chars) = YY_CURRENT_BUFFER_LVALUE->yy_n_chars;
	(yytext_ptr) = (yy_c_buf_p) = YY_CURRENT_BUFFER_LVALUE->yy_buf_pos;
	fortranin = YY_CURRENT_BUFFER_LVALUE->yy_input_file;
	(yy_hold_char) = *(yy_c_buf_p);
}

/** Allocate and initialize an input buffer state.
 * @param file A readable stream.
 * @param size The character buffer size in bytes. When in doubt, use @c YY_BUF_SIZE.
 * 
 * @return the allocated buffer state.
 */
    YY_BUFFER_STATE fortran_create_buffer  (FILE * file, int  size )
{
	YY_BUFFER_STATE b;
    
	b = (YY_BUFFER_STATE) fortranalloc(sizeof( struct yy_buffer_state )  );
	if ( ! b )
		YY_FATAL_ERROR( "out of dynamic memory in fortran_create_buffer()" );

	b->yy_buf_size = size;

	/* yy_ch_buf has to be 2 characters longer than the size given because
	 * we need to put in 2 end-of-buffer characters.
	 */
	b->yy_ch_buf = (char *) fortranalloc(b->yy_buf_size + 2  );
	if ( ! b->yy_ch_buf )
		YY_FATAL_ERROR( "out of dynamic memory in fortran_create_buffer()" );

	b->yy_is_our_buffer = 1;

	fortran_init_buffer(b,file );

	return b;
}

/** Destroy the buffer.
 * @param b a buffer created with fortran_create_buffer()
 * 
 */
    void fortran_delete_buffer (YY_BUFFER_STATE  b )
{
    
	if ( ! b )
		return;

	if ( b == YY_CURRENT_BUFFER ) /* Not sure if we should pop here. */
		YY_CURRENT_BUFFER_LVALUE = (YY_BUFFER_STATE) 0;

	if ( b->yy_is_our_buffer )
		fortranfree((void *) b->yy_ch_buf  );

	fortranfree((void *) b  );
}

#ifndef __cplusplus
extern int isatty (int );
#endif /* __cplusplus */
    
/* Initializes or reinitializes a buffer.
 * This function is sometimes called more than once on the same buffer,
 * such as during a fortranrestart() or at EOF.
 */
    static void fortran_init_buffer  (YY_BUFFER_STATE  b, FILE * file )

{
	int oerrno = errno;
    
	fortran_flush_buffer(b );

	b->yy_input_file = file;
	b->yy_fill_buffer = 1;

    /* If b is the current buffer, then fortran_init_buffer was _probably_
     * called from fortranrestart() or through yy_get_next_buffer.
     * In that case, we don't want to reset the lineno or column.
     */
    if (b != YY_CURRENT_BUFFER){
        b->yy_bs_lineno = 1;
        b->yy_bs_column = 0;
    }

        b->yy_is_interactive = file ? (isatty( fileno(file) ) > 0) : 0;
    
	errno = oerrno;
}

/** Discard all buffered characters. On the next scan, YY_INPUT will be called.
 * @param b the buffer state to be flushed, usually @c YY_CURRENT_BUFFER.
 * 
 */
    void fortran_flush_buffer (YY_BUFFER_STATE  b )
{
    	if ( ! b )
		return;

	b->yy_n_chars = 0;

	/* We always need two end-of-buffer characters.  The first causes
	 * a transition to the end-of-buffer state.  The second causes
	 * a jam in that state.
	 */
	b->yy_ch_buf[0] = YY_END_OF_BUFFER_CHAR;
	b->yy_ch_buf[1] = YY_END_OF_BUFFER_CHAR;

	b->yy_buf_pos = &b->yy_ch_buf[0];

	b->yy_at_bol = 1;
	b->yy_buffer_status = YY_BUFFER_NEW;

	if ( b == YY_CURRENT_BUFFER )
		fortran_load_buffer_state( );
}

/** Pushes the new state onto the stack. The new state becomes
 *  the current state. This function will allocate the stack
 *  if necessary.
 *  @param new_buffer The new state.
 *  
 */
void fortranpush_buffer_state (YY_BUFFER_STATE new_buffer )
{
    	if (new_buffer == NULL)
		return;

	fortranensure_buffer_stack();

	/* This block is copied from fortran_switch_to_buffer. */
	if ( YY_CURRENT_BUFFER )
		{
		/* Flush out information for old buffer. */
		*(yy_c_buf_p) = (yy_hold_char);
		YY_CURRENT_BUFFER_LVALUE->yy_buf_pos = (yy_c_buf_p);
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars);
		}

	/* Only push if top exists. Otherwise, replace top. */
	if (YY_CURRENT_BUFFER)
		(yy_buffer_stack_top)++;
	YY_CURRENT_BUFFER_LVALUE = new_buffer;

	/* copied from fortran_switch_to_buffer. */
	fortran_load_buffer_state( );
	(yy_did_buffer_switch_on_eof) = 1;
}

/** Removes and deletes the top of the stack, if present.
 *  The next element becomes the new top.
 *  
 */
void fortranpop_buffer_state (void)
{
    	if (!YY_CURRENT_BUFFER)
		return;

	fortran_delete_buffer(YY_CURRENT_BUFFER );
	YY_CURRENT_BUFFER_LVALUE = NULL;
	if ((yy_buffer_stack_top) > 0)
		--(yy_buffer_stack_top);

	if (YY_CURRENT_BUFFER) {
		fortran_load_buffer_state( );
		(yy_did_buffer_switch_on_eof) = 1;
	}
}

/* Allocates the stack if it does not exist.
 *  Guarantees space for at least one push.
 */
static void fortranensure_buffer_stack (void)
{
	yy_size_t num_to_alloc;
    
	if (!(yy_buffer_stack)) {

		/* First allocation is just for 2 elements, since we don't know if this
		 * scanner will even need a stack. We use 2 instead of 1 to avoid an
		 * immediate realloc on the next call.
         */
		num_to_alloc = 1;
		(yy_buffer_stack) = (struct yy_buffer_state**)fortranalloc
								(num_to_alloc * sizeof(struct yy_buffer_state*)
								);
		if ( ! (yy_buffer_stack) )
			YY_FATAL_ERROR( "out of dynamic memory in fortranensure_buffer_stack()" );
								  
		memset((yy_buffer_stack), 0, num_to_alloc * sizeof(struct yy_buffer_state*));
				
		(yy_buffer_stack_max) = num_to_alloc;
		(yy_buffer_stack_top) = 0;
		return;
	}

	if ((yy_buffer_stack_top) >= ((yy_buffer_stack_max)) - 1){

		/* Increase the buffer to prepare for a possible push. */
		int grow_size = 8 /* arbitrary grow size */;

		num_to_alloc = (yy_buffer_stack_max) + grow_size;
		(yy_buffer_stack) = (struct yy_buffer_state**)fortranrealloc
								((yy_buffer_stack),
								num_to_alloc * sizeof(struct yy_buffer_state*)
								);
		if ( ! (yy_buffer_stack) )
			YY_FATAL_ERROR( "out of dynamic memory in fortranensure_buffer_stack()" );

		/* zero only the new slots.*/
		memset((yy_buffer_stack) + (yy_buffer_stack_max), 0, grow_size * sizeof(struct yy_buffer_state*));
		(yy_buffer_stack_max) = num_to_alloc;
	}
}

/** Setup the input buffer state to scan directly from a user-specified character buffer.
 * @param base the character buffer
 * @param size the size in bytes of the character buffer
 * 
 * @return the newly allocated buffer state object. 
 */
YY_BUFFER_STATE fortran_scan_buffer  (char * base, yy_size_t  size )
{
	YY_BUFFER_STATE b;
    
	if ( size < 2 ||
	     base[size-2] != YY_END_OF_BUFFER_CHAR ||
	     base[size-1] != YY_END_OF_BUFFER_CHAR )
		/* They forgot to leave room for the EOB's. */
		return 0;

	b = (YY_BUFFER_STATE) fortranalloc(sizeof( struct yy_buffer_state )  );
	if ( ! b )
		YY_FATAL_ERROR( "out of dynamic memory in fortran_scan_buffer()" );

	b->yy_buf_size = size - 2;	/* "- 2" to take care of EOB's */
	b->yy_buf_pos = b->yy_ch_buf = base;
	b->yy_is_our_buffer = 0;
	b->yy_input_file = 0;
	b->yy_n_chars = b->yy_buf_size;
	b->yy_is_interactive = 0;
	b->yy_at_bol = 1;
	b->yy_fill_buffer = 0;
	b->yy_buffer_status = YY_BUFFER_NEW;

	fortran_switch_to_buffer(b  );

	return b;
}

/** Setup the input buffer state to scan a string. The next call to fortranlex() will
 * scan from a @e copy of @a str.
 * @param yystr a NUL-terminated string to scan
 * 
 * @return the newly allocated buffer state object.
 * @note If you want to scan bytes that may contain NUL values, then use
 *       fortran_scan_bytes() instead.
 */
YY_BUFFER_STATE fortran_scan_string (yyconst char * yystr )
{
    
	return fortran_scan_bytes(yystr,strlen(yystr) );
}

/** Setup the input buffer state to scan the given bytes. The next call to fortranlex() will
 * scan from a @e copy of @a bytes.
 * @param bytes the byte buffer to scan
 * @param len the number of bytes in the buffer pointed to by @a bytes.
 * 
 * @return the newly allocated buffer state object.
 */
YY_BUFFER_STATE fortran_scan_bytes  (yyconst char * yybytes, yy_size_t  _yybytes_len )
{
	YY_BUFFER_STATE b;
	char *buf;
	yy_size_t n, i;
    
	/* Get memory for full buffer, including space for trailing EOB's. */
	n = _yybytes_len + 2;
	buf = (char *) fortranalloc(n  );
	if ( ! buf )
		YY_FATAL_ERROR( "out of dynamic memory in fortran_scan_bytes()" );

	for ( i = 0; i < _yybytes_len; ++i )
		buf[i] = yybytes[i];

	buf[_yybytes_len] = buf[_yybytes_len+1] = YY_END_OF_BUFFER_CHAR;

	b = fortran_scan_buffer(buf,n );
	if ( ! b )
		YY_FATAL_ERROR( "bad buffer in fortran_scan_bytes()" );

	/* It's okay to grow etc. this buffer, and we should throw it
	 * away when we're done.
	 */
	b->yy_is_our_buffer = 1;

	return b;
}

#ifndef YY_EXIT_FAILURE
#define YY_EXIT_FAILURE 2
#endif

static void yy_fatal_error (yyconst char* msg )
{
    	(void) fprintf( stderr, "%s\n", msg );
	exit( YY_EXIT_FAILURE );
}

/* Redefine yyless() so it works in section 3 code. */

#undef yyless
#define yyless(n) \
	do \
		{ \
		/* Undo effects of setting up fortrantext. */ \
        int yyless_macro_arg = (n); \
        YY_LESS_LINENO(yyless_macro_arg);\
		fortrantext[fortranleng] = (yy_hold_char); \
		(yy_c_buf_p) = fortrantext + yyless_macro_arg; \
		(yy_hold_char) = *(yy_c_buf_p); \
		*(yy_c_buf_p) = '\0'; \
		fortranleng = yyless_macro_arg; \
		} \
	while ( 0 )

/* Accessor  methods (get/set functions) to struct members. */

/** Get the current line number.
 * 
 */
int fortranget_lineno  (void)
{
        
    return fortranlineno;
}

/** Get the input stream.
 * 
 */
FILE *fortranget_in  (void)
{
        return fortranin;
}

/** Get the output stream.
 * 
 */
FILE *fortranget_out  (void)
{
        return fortranout;
}

/** Get the length of the current token.
 * 
 */
yy_size_t fortranget_leng  (void)
{
        return fortranleng;
}

/** Get the current token.
 * 
 */

char *fortranget_text  (void)
{
        return fortrantext;
}

/** Set the current line number.
 * @param line_number
 * 
 */
void fortranset_lineno (int  line_number )
{
    
    fortranlineno = line_number;
}

/** Set the input stream. This does not discard the current
 * input buffer.
 * @param in_str A readable stream.
 * 
 * @see fortran_switch_to_buffer
 */
void fortranset_in (FILE *  in_str )
{
        fortranin = in_str ;
}

void fortranset_out (FILE *  out_str )
{
        fortranout = out_str ;
}

int fortranget_debug  (void)
{
        return fortran_flex_debug;
}

void fortranset_debug (int  bdebug )
{
        fortran_flex_debug = bdebug ;
}

static int yy_init_globals (void)
{
        /* Initialization is the same as for the non-reentrant scanner.
     * This function is called from fortranlex_destroy(), so don't allocate here.
     */

    (yy_buffer_stack) = 0;
    (yy_buffer_stack_top) = 0;
    (yy_buffer_stack_max) = 0;
    (yy_c_buf_p) = (char *) 0;
    (yy_init) = 0;
    (yy_start) = 0;

/* Defined in main.c */
#ifdef YY_STDINIT
    fortranin = stdin;
    fortranout = stdout;
#else
    fortranin = (FILE *) 0;
    fortranout = (FILE *) 0;
#endif

    /* For future reference: Set errno on error, since we are called by
     * fortranlex_init()
     */
    return 0;
}

/* fortranlex_destroy is for both reentrant and non-reentrant scanners. */
int fortranlex_destroy  (void)
{
    
    /* Pop the buffer stack, destroying each element. */
	while(YY_CURRENT_BUFFER){
		fortran_delete_buffer(YY_CURRENT_BUFFER  );
		YY_CURRENT_BUFFER_LVALUE = NULL;
		fortranpop_buffer_state();
	}

	/* Destroy the stack itself. */
	fortranfree((yy_buffer_stack) );
	(yy_buffer_stack) = NULL;

    /* Reset the globals. This is important in a non-reentrant scanner so the next time
     * fortranlex() is called, initialization will occur. */
    yy_init_globals( );

    return 0;
}

/*
 * Internal utility routines.
 */

#ifndef yytext_ptr
static void yy_flex_strncpy (char* s1, yyconst char * s2, int n )
{
	register int i;
	for ( i = 0; i < n; ++i )
		s1[i] = s2[i];
}
#endif

#ifdef YY_NEED_STRLEN
static int yy_flex_strlen (yyconst char * s )
{
	register int n;
	for ( n = 0; s[n]; ++n )
		;

	return n;
}
#endif

void *fortranalloc (yy_size_t  size )
{
	return (void *) malloc( size );
}

void *fortranrealloc  (void * ptr, yy_size_t  size )
{
	/* The cast to (char *) in the following accommodates both
	 * implementations that use char* generic pointers, and those
	 * that use void* generic pointers.  It works with the latter
	 * because both ANSI C and C++ allow castless assignment from
	 * any pointer type to void*, and deal with argument conversions
	 * as though doing an assignment.
	 */
	return (void *) realloc( (char *) ptr, size );
}

void fortranfree (void * ptr )
{
	free( (char *) ptr );	/* see fortranrealloc() for (char *) cast */
}

#define YYTABLES_NAME "yytables"

#line 353 "fortran.lex"



fortranerror(char *s)
{
   if (!strcasecmp(curfile,mainfile))
   {
      printf("%s line %d, file %s motclef = %s\n",s,line_num_fortran,curfile,fortrantext);
   }
   else
   {
      printf("%s line %d, file %s motclef = %s curbuf = %s\n",s,line_num_fortran_common,curfile,fortrantext,curbuf);
   }
/*   exit(0);*/
}

int fortranwrap()
{
}

