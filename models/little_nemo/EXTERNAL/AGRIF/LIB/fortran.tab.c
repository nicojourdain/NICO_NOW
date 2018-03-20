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

