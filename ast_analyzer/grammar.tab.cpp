/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "../parser/grammar.ypp"


#include "../parser/error_reporter.hpp"
#include "../parser/ast.hpp"

#include <iostream>
#include <fstream>
#include <cassert>
#include <algorithm>

#include <string>
#include <cstring>
#include <string.h>

#include <vector>
#include <sstream>
#include <set>

using namespace std;

extern int yylex(void);
void yyerror(const char*);

extern int line_num; 
extern string line, prev_line; 

int cur_position_in_line = 0;
extern ast* ast_;

error_reporter reporter;


#line 104 "grammar.tab.cpp"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "grammar.tab.hpp"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_STRING = 3,                     /* STRING  */
  YYSYMBOL_NAME = 4,                       /* NAME  */
  YYSYMBOL_INT = 5,                        /* INT  */
  YYSYMBOL_REAL = 6,                       /* REAL  */
  YYSYMBOL_SCOLON = 7,                     /* SCOLON  */
  YYSYMBOL_COMMA = 8,                      /* COMMA  */
  YYSYMBOL_COLON = 9,                      /* COLON  */
  YYSYMBOL_DOT = 10,                       /* DOT  */
  YYSYMBOL_SHARP = 11,                     /* SHARP  */
  YYSYMBOL_ARROW = 12,                     /* ARROW  */
  YYSYMBOL_LARROW = 13,                    /* LARROW  */
  YYSYMBOL_LARR = 14,                      /* LARR  */
  YYSYMBOL_RARR = 15,                      /* RARR  */
  YYSYMBOL_AMP = 16,                       /* AMP  */
  YYSYMBOL_KW_CF = 17,                     /* KW_CF  */
  YYSYMBOL_KW_DF = 18,                     /* KW_DF  */
  YYSYMBOL_KW_IMPORT = 19,                 /* KW_IMPORT  */
  YYSYMBOL_KW_AS = 20,                     /* KW_AS  */
  YYSYMBOL_KW_FOR = 21,                    /* KW_FOR  */
  YYSYMBOL_KW_IF = 22,                     /* KW_IF  */
  YYSYMBOL_KW_LET = 23,                    /* KW_LET  */
  YYSYMBOL_KW_VALUE = 24,                  /* KW_VALUE  */
  YYSYMBOL_KW_CUDA = 25,                   /* KW_CUDA  */
  YYSYMBOL_KW_NOCPU = 26,                  /* KW_NOCPU  */
  YYSYMBOL_KW_IN = 27,                     /* KW_IN  */
  YYSYMBOL_KW_OUT = 28,                    /* KW_OUT  */
  YYSYMBOL_KW_SIZE = 29,                   /* KW_SIZE  */
  YYSYMBOL_KW_SUB = 30,                    /* KW_SUB  */
  YYSYMBOL_KW_WHILE = 31,                  /* KW_WHILE  */
  YYSYMBOL_KW_INT = 32,                    /* KW_INT  */
  YYSYMBOL_KW_NAME = 33,                   /* KW_NAME  */
  YYSYMBOL_KW_REAL = 34,                   /* KW_REAL  */
  YYSYMBOL_KW_STRING = 35,                 /* KW_STRING  */
  YYSYMBOL_KW_RUSH = 36,                   /* KW_RUSH  */
  YYSYMBOL_KW_STATIC = 37,                 /* KW_STATIC  */
  YYSYMBOL_KW_STATIC_FOR = 38,             /* KW_STATIC_FOR  */
  YYSYMBOL_KW_UNROLLING = 39,              /* KW_UNROLLING  */
  YYSYMBOL_KW_BLOCK = 40,                  /* KW_BLOCK  */
  YYSYMBOL_KW_CPP = 41,                    /* KW_CPP  */
  YYSYMBOL_INTERPRETER = 42,               /* INTERPRETER  */
  YYSYMBOL_EQ = 43,                        /* EQ  */
  YYSYMBOL_LB = 44,                        /* LB  */
  YYSYMBOL_RB = 45,                        /* RB  */
  YYSYMBOL_LT = 46,                        /* LT  */
  YYSYMBOL_GT = 47,                        /* GT  */
  YYSYMBOL_LSB = 48,                       /* LSB  */
  YYSYMBOL_RSB = 49,                       /* RSB  */
  YYSYMBOL_DIAP = 50,                      /* DIAP  */
  YYSYMBOL_QMARK = 51,                     /* QMARK  */
  YYSYMBOL_DBLEQ = 52,                     /* DBLEQ  */
  YYSYMBOL_NEQ = 53,                       /* NEQ  */
  YYSYMBOL_LEQ = 54,                       /* LEQ  */
  YYSYMBOL_EQG = 55,                       /* EQG  */
  YYSYMBOL_GEQ = 56,                       /* GEQ  */
  YYSYMBOL_AT = 57,                        /* AT  */
  YYSYMBOL_PLUS = 58,                      /* PLUS  */
  YYSYMBOL_MINUS = 59,                     /* MINUS  */
  YYSYMBOL_MUL = 60,                       /* MUL  */
  YYSYMBOL_DIV = 61,                       /* DIV  */
  YYSYMBOL_MOD = 62,                       /* MOD  */
  YYSYMBOL_TAB = 63,                       /* TAB  */
  YYSYMBOL_WS = 64,                        /* WS  */
  YYSYMBOL_SWS = 65,                       /* SWS  */
  YYSYMBOL_LN = 66,                        /* LN  */
  YYSYMBOL_BUCK = 67,                      /* BUCK  */
  YYSYMBOL_PIPE = 68,                      /* PIPE  */
  YYSYMBOL_ASSIGN = 69,                    /* ASSIGN  */
  YYSYMBOL_LCB = 70,                       /* LCB  */
  YYSYMBOL_RCB = 71,                       /* RCB  */
  YYSYMBOL_IDX = 72,                       /* IDX  */
  YYSYMBOL_DBLPIPE = 73,                   /* DBLPIPE  */
  YYSYMBOL_DBLAMP = 74,                    /* DBLAMP  */
  YYSYMBOL_IFX = 75,                       /* IFX  */
  YYSYMBOL_KW_ELSE = 76,                   /* KW_ELSE  */
  YYSYMBOL_YYACCEPT = 77,                  /* $accept  */
  YYSYMBOL_all = 78,                       /* all  */
  YYSYMBOL_program = 79,                   /* program  */
  YYSYMBOL_sub_def = 80,                   /* sub_def  */
  YYSYMBOL_opt_ext_params = 81,            /* opt_ext_params  */
  YYSYMBOL_ext_params_seq = 82,            /* ext_params_seq  */
  YYSYMBOL_code_df_param = 83,             /* code_df_param  */
  YYSYMBOL_code_df = 84,                   /* code_df  */
  YYSYMBOL_type = 85,                      /* type  */
  YYSYMBOL_block = 86,                     /* block  */
  YYSYMBOL_opt_dfdecls = 87,               /* opt_dfdecls  */
  YYSYMBOL_dfdecls = 88,                   /* dfdecls  */
  YYSYMBOL_name_seq = 89,                  /* name_seq  */
  YYSYMBOL_statement_seq = 90,             /* statement_seq  */
  YYSYMBOL_control_pragma = 91,            /* control_pragma  */
  YYSYMBOL_statement = 92,                 /* statement  */
  YYSYMBOL_cf_statement = 93,              /* cf_statement  */
  YYSYMBOL_opt_behavior = 94,              /* opt_behavior  */
  YYSYMBOL_behv_pragmas_seq = 95,          /* behv_pragmas_seq  */
  YYSYMBOL_behv_pragma = 96,               /* behv_pragma  */
  YYSYMBOL_id_seq = 97,                    /* id_seq  */
  YYSYMBOL_let_statement = 98,             /* let_statement  */
  YYSYMBOL_for_statement = 99,             /* for_statement  */
  YYSYMBOL_while_statement = 100,          /* while_statement  */
  YYSYMBOL_if_statement = 101,             /* if_statement  */
  YYSYMBOL_assign_seq = 102,               /* assign_seq  */
  YYSYMBOL_assign = 103,                   /* assign  */
  YYSYMBOL_opt_label = 104,                /* opt_label  */
  YYSYMBOL_id = 105,                       /* id  */
  YYSYMBOL_opt_exprs = 106,                /* opt_exprs  */
  YYSYMBOL_exprs_seq = 107,                /* exprs_seq  */
  YYSYMBOL_opt_setdf_rules = 108,          /* opt_setdf_rules  */
  YYSYMBOL_opt_rules = 109,                /* opt_rules  */
  YYSYMBOL_code_id = 110,                  /* code_id  */
  YYSYMBOL_expr = 111,                     /* expr  */
  YYSYMBOL_opt_params = 112,               /* opt_params  */
  YYSYMBOL_params_seq = 113,               /* params_seq  */
  YYSYMBOL_param = 114,                    /* param  */
  YYSYMBOL_where_type = 115                /* where_type  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_uint8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

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
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
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
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE) \
             + YYSIZEOF (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  16
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   661

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  77
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  39
/* YYNRULES -- Number of rules.  */
#define YYNRULES  110
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  230

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   331


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
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
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   155,   155,   158,   161,   167,   174,   180,   186,   191,
     196,   202,   208,   214,   221,   225,   228,   234,   241,   248,
     249,   253,   254,   255,   256,   257,   258,   262,   268,   275,
     280,   284,   291,   296,   303,   309,   318,   325,   333,   338,
     343,   344,   345,   346,   347,   349,   361,   374,   377,   381,
     386,   393,   398,   403,   408,   413,   420,   425,   430,   435,
     440,   445,   454,   459,   466,   470,   475,   481,   487,   494,
     497,   503,   507,   513,   517,   523,   524,   530,   537,   541,
     545,   551,   556,   561,   566,   570,   574,   578,   583,   587,
     591,   595,   599,   603,   607,   612,   616,   620,   624,   628,
     634,   638,   643,   655,   660,   666,   674,   678,   682,   686,
     690
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "STRING", "NAME",
  "INT", "REAL", "SCOLON", "COMMA", "COLON", "DOT", "SHARP", "ARROW",
  "LARROW", "LARR", "RARR", "AMP", "KW_CF", "KW_DF", "KW_IMPORT", "KW_AS",
  "KW_FOR", "KW_IF", "KW_LET", "KW_VALUE", "KW_CUDA", "KW_NOCPU", "KW_IN",
  "KW_OUT", "KW_SIZE", "KW_SUB", "KW_WHILE", "KW_INT", "KW_NAME",
  "KW_REAL", "KW_STRING", "KW_RUSH", "KW_STATIC", "KW_STATIC_FOR",
  "KW_UNROLLING", "KW_BLOCK", "KW_CPP", "INTERPRETER", "EQ", "LB", "RB",
  "LT", "GT", "LSB", "RSB", "DIAP", "QMARK", "DBLEQ", "NEQ", "LEQ", "EQG",
  "GEQ", "AT", "PLUS", "MINUS", "MUL", "DIV", "MOD", "TAB", "WS", "SWS",
  "LN", "BUCK", "PIPE", "ASSIGN", "LCB", "RCB", "IDX", "DBLPIPE", "DBLAMP",
  "IFX", "KW_ELSE", "$accept", "all", "program", "sub_def",
  "opt_ext_params", "ext_params_seq", "code_df_param", "code_df", "type",
  "block", "opt_dfdecls", "dfdecls", "name_seq", "statement_seq",
  "control_pragma", "statement", "cf_statement", "opt_behavior",
  "behv_pragmas_seq", "behv_pragma", "id_seq", "let_statement",
  "for_statement", "while_statement", "if_statement", "assign_seq",
  "assign", "opt_label", "id", "opt_exprs", "exprs_seq", "opt_setdf_rules",
  "opt_rules", "code_id", "expr", "opt_params", "params_seq", "param",
  "where_type", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-169)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-67)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      53,    -1,     8,     4,     0,    20,   133,  -169,  -169,    50,
    -169,    17,   131,    76,    58,    76,  -169,  -169,  -169,   -10,
    -169,  -169,  -169,  -169,    18,    79,    83,    79,    84,  -169,
    -169,  -169,  -169,    71,   122,  -169,   137,   354,  -169,    72,
      91,   140,   109,  -169,   144,   -10,  -169,  -169,  -169,  -169,
    -169,  -169,   107,   116,   129,   354,   105,   246,   130,  -169,
     176,     5,  -169,   175,   186,     4,   354,   190,     4,   178,
    -169,  -169,  -169,  -169,  -169,  -169,  -169,    76,   152,   156,
      76,  -169,   354,   354,   354,   361,   354,   354,  -169,   354,
     354,   354,   354,   354,   354,   354,   354,   354,   354,   354,
     354,   354,   354,  -169,  -169,   -10,  -169,  -169,     1,   199,
     125,   161,    54,  -169,   354,   201,   380,  -169,   166,  -169,
     207,   112,   385,   409,   433,  -169,   462,   318,    28,    28,
     294,    28,    28,    28,    28,    75,    75,  -169,  -169,  -169,
     582,   599,  -169,  -169,   182,  -169,   354,   190,  -169,   270,
    -169,   136,    47,  -169,   218,   203,   174,  -169,   202,  -169,
    -169,  -169,  -169,  -169,   354,   354,   536,  -169,   222,  -169,
     228,   177,  -169,  -169,    11,   536,   166,   221,  -169,   151,
     559,   487,   196,  -169,   171,  -169,   354,  -169,  -169,   166,
     177,  -169,   223,   354,   354,   243,   536,  -169,   241,   244,
     125,   512,    56,   181,     3,  -169,  -169,  -169,  -169,   227,
     354,   185,    77,  -169,  -169,  -169,   186,   155,  -169,   186,
     354,   354,    80,  -169,   105,   184,   213,  -169,  -169,  -169
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,     0,     0,    39,     0,     0,     0,     4,    10,     0,
      77,     0,   110,     0,     0,     0,     1,     5,     9,    15,
     106,   107,   108,   109,     0,     0,     0,     0,    26,    21,
      24,    22,    23,     0,    14,    16,    20,     0,    38,     0,
       0,     0,     0,    25,     0,     0,    19,    18,    80,    67,
      78,    79,     0,     0,     0,     0,    98,     0,     0,   101,
       0,     0,   103,     0,     0,    39,     0,     0,    39,    30,
       6,    27,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    17,     0,     0,     0,     0,     0,     0,    36,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   102,   105,     0,   100,    45,     0,     0,
       0,     0,     0,    62,     0,     0,     0,    29,     0,    13,
       0,     0,     0,     0,     0,    97,     0,     0,    89,    90,
       0,    93,    94,    91,    92,    84,    85,    86,    87,    88,
      96,    95,   104,    65,     0,    61,     0,     0,    58,     0,
      32,     0,     0,    34,     0,    74,     0,     8,     0,    81,
      82,    83,    68,    37,     0,     0,    64,    63,     0,    31,
       0,    48,    35,    70,     0,    71,     0,    76,     7,     0,
      99,     0,     0,    33,     0,    28,     0,    69,    73,     0,
      48,    11,     0,     0,     0,     0,    72,    75,     0,     0,
       0,     0,    32,     0,     0,    49,    46,    12,    59,     0,
       0,     0,    56,    55,    47,    50,     0,     0,    53,     0,
       0,     0,     0,    54,    57,     0,     0,    60,    51,    52
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -169,  -169,  -169,   250,  -169,  -169,   225,  -169,   -38,  -107,
    -169,  -169,   153,  -169,    -2,  -100,  -169,    86,  -169,    73,
    -169,  -169,  -169,  -169,  -169,  -169,   132,  -169,   -62,  -168,
    -169,  -169,  -169,     2,   -55,   253,  -169,   179,  -169
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,     5,     6,     7,    33,    34,    35,    47,    36,    70,
     116,   117,   203,   152,    13,    71,    72,   185,   204,   205,
     211,    73,    74,    75,    76,   112,   113,    77,    56,   155,
     174,   177,   190,    11,    57,    40,    61,    62,    24
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      85,    60,   108,   145,    14,   148,     8,   202,   188,     9,
     143,   110,    10,   105,    28,    25,   153,    27,    12,   186,
      16,   197,    29,    30,    31,    32,    37,   122,   123,   124,
      15,   126,   127,    38,   128,   129,   130,   131,   132,   133,
     134,   135,   136,   137,   138,   139,   140,   141,    63,    86,
     106,   -66,   172,    -3,     1,    63,   187,    18,   -66,   149,
      49,    19,   147,   109,    64,   210,   114,    60,    65,    66,
      67,    64,     2,    58,   214,    65,    66,    67,    68,   118,
      10,    63,   121,     3,   -66,    68,    96,    97,    98,    99,
     100,   166,    63,   208,     4,   -66,    28,    64,    26,   175,
      43,    65,    66,    67,    29,    30,    31,    32,    64,   180,
     181,    68,    65,    66,    67,   227,    44,    59,   171,   157,
     220,   158,    68,    39,    69,    86,    63,    41,    86,   -66,
      45,   196,   221,    -2,     1,    98,    99,   100,   200,   201,
     212,    46,    64,   169,   170,    78,    65,    66,    67,    79,
      69,    82,     2,    86,   222,   217,    68,   224,   191,   192,
      83,    69,   223,     3,    80,   225,   226,    20,    21,    22,
      23,    89,    90,    84,     4,   103,    91,    92,    93,    94,
     104,    95,   107,    96,    97,    98,    99,   100,   213,   170,
      49,   228,   218,   219,   111,    69,   115,   119,   101,   102,
     120,    89,    90,   144,   146,   150,    91,    92,    93,    94,
     154,    95,   156,    96,    97,    98,    99,   100,   176,   178,
     229,    48,    49,    50,    51,   165,   182,   179,   101,   102,
      89,    90,   183,   189,   184,    91,    92,    93,    94,   194,
      95,   195,    96,    97,    98,    99,   100,   202,   206,   199,
      52,   207,    53,    54,    87,   216,    17,   101,   102,    89,
      90,    88,    55,   173,    91,    92,    93,    94,   151,    95,
      81,    96,    97,    98,    99,   100,   198,   215,   168,   167,
      42,     0,     0,     0,   142,     0,   101,   102,     0,     0,
       0,     0,    89,    90,     0,     0,     0,    91,    92,    93,
      94,     0,    95,   164,    96,    97,    98,    99,   100,     0,
       0,     0,     0,     0,     0,     0,    89,    90,     0,   101,
     102,    91,    92,    93,    94,     0,    95,     0,    96,    97,
      98,    99,   100,   163,     0,     0,     0,     0,     0,     0,
      89,    90,     0,   101,   102,    91,    92,    93,    94,     0,
      95,     0,    96,    97,    98,    99,   100,    48,    49,    50,
      51,     0,     0,     0,    89,    90,     0,   101,   102,    91,
      92,    93,    94,     0,    95,     0,    96,    97,    98,    99,
     100,    63,     0,     0,   -66,     0,    52,     0,    53,    54,
       0,   101,   102,     0,     0,     0,     0,    64,    55,     0,
       0,    65,    66,    67,     0,     0,   125,    89,    90,     0,
       0,    68,    91,    92,    93,    94,     0,    95,     0,    96,
      97,    98,    99,   100,     0,     0,     0,     0,     0,     0,
     159,    89,    90,     0,   101,   102,    91,    92,    93,    94,
       0,    95,     0,    96,    97,    98,    99,   100,     0,     0,
       0,     0,     0,     0,   160,    89,    90,     0,   101,   102,
      91,    92,    93,    94,     0,    95,     0,    96,    97,    98,
      99,   100,     0,     0,     0,     0,     0,     0,   161,    89,
      90,     0,   101,   102,    91,    92,    93,    94,     0,    95,
       0,    96,    97,    98,    99,   100,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   101,   102,    89,    90,
       0,   162,     0,    91,    92,    93,    94,     0,    95,     0,
      96,    97,    98,    99,   100,     0,     0,     0,     0,     0,
       0,     0,     0,    89,    90,   101,   102,   193,    91,    92,
      93,    94,     0,    95,     0,    96,    97,    98,    99,   100,
       0,     0,     0,     0,     0,     0,     0,     0,    89,    90,
     101,   102,   209,    91,    92,    93,    94,     0,    95,     0,
      96,    97,    98,    99,   100,     0,     0,     0,     0,     0,
       0,     0,    89,    90,     0,   101,   102,    91,    92,    93,
      94,     0,    95,     0,    96,    97,    98,    99,   100,     0,
       0,     0,     0,     0,     0,    89,    90,     0,     0,   101,
     102,    92,    93,    94,     0,    95,     0,    96,    97,    98,
      99,   100,     0,     0,     0,     0,     0,     0,    89,    90,
       0,     0,   101,   102,    92,    93,    94,     0,    95,     0,
      96,    97,    98,    99,   100,    89,    90,     0,     0,     0,
       0,    92,    93,    94,     0,    95,   102,    96,    97,    98,
      99,   100
};

static const yytype_int16 yycheck[] =
{
      55,    39,    64,   110,     4,   112,     7,     4,   176,     1,
       9,    66,     4,     8,    24,    13,   116,    15,    14,     8,
       0,   189,    32,    33,    34,    35,     8,    82,    83,    84,
      30,    86,    87,    15,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,     1,    48,
      45,     4,   152,     0,     1,     1,    45,     7,     4,   114,
       4,    44,     8,    65,    17,     9,    68,   105,    21,    22,
      23,    17,    19,     1,    71,    21,    22,    23,    31,    77,
       4,     1,    80,    30,     4,    31,    58,    59,    60,    61,
      62,   146,     1,   200,    41,     4,    24,    17,    40,   154,
      16,    21,    22,    23,    32,    33,    34,    35,    17,   164,
     165,    31,    21,    22,    23,   222,    45,    45,    71,     7,
      43,     9,    31,    44,    70,    48,     1,    44,    48,     4,
       8,   186,    55,     0,     1,    60,    61,    62,   193,   194,
     202,     4,    17,     7,     8,     5,    21,    22,    23,    40,
      70,    44,    19,    48,   216,   210,    31,   219,     7,     8,
      44,    70,     7,    30,    20,   220,   221,    36,    37,    38,
      39,    46,    47,    44,    41,    45,    51,    52,    53,    54,
       4,    56,     7,    58,    59,    60,    61,    62,     7,     8,
       4,     7,     7,     8,     4,    70,    18,    45,    73,    74,
      44,    46,    47,     4,    43,     4,    51,    52,    53,    54,
      44,    56,     5,    58,    59,    60,    61,    62,    15,    45,
       7,     3,     4,     5,     6,    43,     4,    25,    73,    74,
      46,    47,     4,    12,    57,    51,    52,    53,    54,    43,
      56,    70,    58,    59,    60,    61,    62,     4,     7,    26,
      32,     7,    34,    35,     8,    28,     6,    73,    74,    46,
      47,    15,    44,    45,    51,    52,    53,    54,   115,    56,
      45,    58,    59,    60,    61,    62,   190,   204,     8,   147,
      27,    -1,    -1,    -1,   105,    -1,    73,    74,    -1,    -1,
      -1,    -1,    46,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    -1,    56,     9,    58,    59,    60,    61,    62,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    46,    47,    -1,    73,
      74,    51,    52,    53,    54,    -1,    56,    -1,    58,    59,
      60,    61,    62,    15,    -1,    -1,    -1,    -1,    -1,    -1,
      46,    47,    -1,    73,    74,    51,    52,    53,    54,    -1,
      56,    -1,    58,    59,    60,    61,    62,     3,     4,     5,
       6,    -1,    -1,    -1,    46,    47,    -1,    73,    74,    51,
      52,    53,    54,    -1,    56,    -1,    58,    59,    60,    61,
      62,     1,    -1,    -1,     4,    -1,    32,    -1,    34,    35,
      -1,    73,    74,    -1,    -1,    -1,    -1,    17,    44,    -1,
      -1,    21,    22,    23,    -1,    -1,    45,    46,    47,    -1,
      -1,    31,    51,    52,    53,    54,    -1,    56,    -1,    58,
      59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,
      45,    46,    47,    -1,    73,    74,    51,    52,    53,    54,
      -1,    56,    -1,    58,    59,    60,    61,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    45,    46,    47,    -1,    73,    74,
      51,    52,    53,    54,    -1,    56,    -1,    58,    59,    60,
      61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    45,    46,
      47,    -1,    73,    74,    51,    52,    53,    54,    -1,    56,
      -1,    58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    74,    46,    47,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    -1,
      58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    46,    47,    73,    74,    50,    51,    52,
      53,    54,    -1,    56,    -1,    58,    59,    60,    61,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    46,    47,
      73,    74,    50,    51,    52,    53,    54,    -1,    56,    -1,
      58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    46,    47,    -1,    73,    74,    51,    52,    53,
      54,    -1,    56,    -1,    58,    59,    60,    61,    62,    -1,
      -1,    -1,    -1,    -1,    -1,    46,    47,    -1,    -1,    73,
      74,    52,    53,    54,    -1,    56,    -1,    58,    59,    60,
      61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    46,    47,
      -1,    -1,    73,    74,    52,    53,    54,    -1,    56,    -1,
      58,    59,    60,    61,    62,    46,    47,    -1,    -1,    -1,
      -1,    52,    53,    54,    -1,    56,    74,    58,    59,    60,
      61,    62
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     1,    19,    30,    41,    78,    79,    80,     7,     1,
       4,   110,    14,    91,     4,    30,     0,    80,     7,    44,
      36,    37,    38,    39,   115,   110,    40,   110,    24,    32,
      33,    34,    35,    81,    82,    83,    85,     8,    15,    44,
     112,    44,   112,    16,    45,     8,     4,    84,     3,     4,
       5,     6,    32,    34,    35,    44,   105,   111,     1,    45,
      85,   113,   114,     1,    17,    21,    22,    23,    31,    70,
      86,    92,    93,    98,    99,   100,   101,   104,     5,    40,
      20,    83,    44,    44,    44,   111,    48,     8,    15,    46,
      47,    51,    52,    53,    54,    56,    58,    59,    60,    61,
      62,    73,    74,    45,     4,     8,    45,     7,   105,    91,
     111,     4,   102,   103,    91,    18,    87,    88,   110,    45,
      44,   110,   111,   111,   111,    45,   111,   111,   111,   111,
     111,   111,   111,   111,   111,   111,   111,   111,   111,   111,
     111,   111,   114,     9,     4,    86,    43,     8,    86,   111,
       4,    89,    90,    92,    44,   106,     5,     7,     9,    45,
      45,    45,    49,    15,     9,    43,   111,   103,     8,     7,
       8,    71,    92,    45,   107,   111,    15,   108,    45,    25,
     111,   111,     4,     4,    57,    94,     8,    45,   106,    12,
     109,     7,     8,    50,    43,    70,   111,   106,    94,    26,
     111,   111,     4,    89,    95,    96,     7,     7,    86,    50,
       9,    97,   105,     7,    71,    96,    28,   111,     7,     8,
      43,    55,   105,     7,   105,   111,   111,    86,     7,     7
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    77,    78,    78,    79,    79,    80,    80,    80,    80,
      80,    80,    80,    80,    81,    81,    82,    82,    83,    84,
      84,    85,    85,    85,    85,    85,    85,    86,    86,    87,
      87,    88,    89,    89,    90,    90,    91,    91,    91,    91,
      92,    92,    92,    92,    92,    92,    93,    94,    94,    95,
      95,    96,    96,    96,    96,    96,    97,    97,    98,    99,
     100,   101,   102,   102,   103,   104,   104,   105,   105,   106,
     106,   107,   107,   108,   108,   109,   109,   110,   111,   111,
     111,   111,   111,   111,   111,   111,   111,   111,   111,   111,
     111,   111,   111,   111,   111,   111,   111,   111,   111,   111,
     112,   112,   112,   113,   113,   114,   115,   115,   115,   115,
     115
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     0,     1,     2,     5,     8,     8,     3,
       2,    10,    12,     6,     1,     0,     1,     3,     2,     1,
       0,     1,     1,     1,     1,     2,     1,     1,     5,     1,
       0,     3,     1,     3,     1,     2,     5,     7,     3,     0,
       1,     1,     1,     1,     1,     2,     7,     4,     0,     1,
       2,     5,     5,     3,     4,     2,     1,     3,     3,     8,
      11,     3,     1,     3,     3,     3,     0,     1,     4,     3,
       2,     1,     3,     2,     0,     2,     0,     1,     1,     1,
       1,     4,     4,     4,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     1,     5,
       3,     2,     3,     1,     3,     2,     1,     1,     1,     1,
       0
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF

/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YYLOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

# ifndef YYLOCATION_PRINT

#  if defined YY_LOCATION_PRINT

   /* Temporary convenience wrapper in case some people defined the
      undocumented and private YY_LOCATION_PRINT macros.  */
#   define YYLOCATION_PRINT(File, Loc)  YY_LOCATION_PRINT(File, *(Loc))

#  elif defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static int
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  int res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
}

#   define YYLOCATION_PRINT  yy_location_print_

    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT(File, Loc)  YYLOCATION_PRINT(File, &(Loc))

#  else

#   define YYLOCATION_PRINT(File, Loc) ((void) 0)
    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT  YYLOCATION_PRINT

#  endif
# endif /* !defined YYLOCATION_PRINT */


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, Location); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  YY_USE (yylocationp);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  YYLOCATION_PRINT (yyo, yylocationp);
  YYFPRINTF (yyo, ": ");
  yy_symbol_value_print (yyo, yykind, yyvaluep, yylocationp);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)],
                       &(yylsp[(yyi + 1) - (yynrhs)]));
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
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






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep, YYLTYPE *yylocationp)
{
  YY_USE (yyvaluep);
  YY_USE (yylocationp);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Location data for the lookahead symbol.  */
YYLTYPE yylloc
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

    /* The location stack: array, bottom, top.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls = yylsa;
    YYLTYPE *yylsp = yyls;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[3];



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  yylsp[0] = yylloc;
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yyls1, yysize * YYSIZEOF (*yylsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
        yyls = yyls1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      yyerror_range[1] = yylloc;
      goto yyerrlab1;
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
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
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
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location. */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  yyerror_range[1] = yyloc;
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* all: program  */
#line 155 "../parser/grammar.ypp"
                { 
        ast_->set_program((yyvsp[0].program_));
    }
#line 1572 "grammar.tab.cpp"
    break;

  case 3: /* all: %empty  */
#line 158 "../parser/grammar.ypp"
      {}
#line 1578 "grammar.tab.cpp"
    break;

  case 4: /* program: sub_def  */
#line 161 "../parser/grammar.ypp"
            {
        (yyval.program_) = new program();
        (yyval.program_)->sub_defs->push_back((yyvsp[0].sub_def_));
        (yyval.program_)->set_position((yylsp[0]).first_line);
    }
#line 1588 "grammar.tab.cpp"
    break;

  case 5: /* program: program sub_def  */
#line 167 "../parser/grammar.ypp"
                      { 
        (yyvsp[-1].program_)->sub_defs->push_back((yyvsp[0].sub_def_));
        (yyval.program_)->set_position((yylsp[-1]).first_line);
    }
#line 1597 "grammar.tab.cpp"
    break;

  case 6: /* sub_def: KW_SUB control_pragma code_id opt_params block  */
#line 174 "../parser/grammar.ypp"
                                                       {
        (yyval.sub_def_) = new luna_sub_def((yyvsp[-3].control_pragma_), (yyvsp[-2].string_), (yyvsp[-1].opt_params_), (yyvsp[0].block_));
        (yyval.sub_def_)->set_position((yylsp[-4]).first_line);
    }
#line 1606 "grammar.tab.cpp"
    break;

  case 7: /* sub_def: KW_CPP KW_SUB code_id opt_params KW_BLOCK LB INT RB  */
#line 180 "../parser/grammar.ypp"
                                                              {
        (yyval.sub_def_) = new cxx_block_with_params_def((yyvsp[-5].string_), (yyvsp[-4].opt_params_));
        (yyval.sub_def_)->set_position((yylsp[-7]).first_line);
    }
#line 1615 "grammar.tab.cpp"
    break;

  case 8: /* sub_def: KW_IMPORT code_id LB opt_ext_params RB KW_AS code_id SCOLON  */
#line 186 "../parser/grammar.ypp"
                                                                      {
        (yyval.sub_def_) = new import((yyvsp[-6].string_), (yyvsp[-4].opt_ext_params_), (yyvsp[-1].string_), std::string(""));
        (yyval.sub_def_)->set_position((yylsp[-7]).first_line);
    }
#line 1624 "grammar.tab.cpp"
    break;

  case 9: /* sub_def: KW_IMPORT error SCOLON  */
#line 191 "../parser/grammar.ypp"
                                 {
        delete (yyval.sub_def_);
        (yyval.sub_def_) = nullptr;
    }
#line 1633 "grammar.tab.cpp"
    break;

  case 10: /* sub_def: error SCOLON  */
#line 196 "../parser/grammar.ypp"
                   {
        delete (yyval.sub_def_);
        (yyval.sub_def_) = nullptr;
    }
#line 1642 "grammar.tab.cpp"
    break;

  case 11: /* sub_def: KW_IMPORT code_id LB opt_ext_params RB KW_AS code_id COLON KW_CUDA SCOLON  */
#line 202 "../parser/grammar.ypp"
                                                                                {
        (yyval.sub_def_) = new import((yyvsp[-8].string_), (yyvsp[-6].opt_ext_params_), (yyvsp[-3].string_), std::string("CUDA"));
        (yyval.sub_def_)->set_position((yylsp[-9]).first_line);
    }
#line 1651 "grammar.tab.cpp"
    break;

  case 12: /* sub_def: KW_IMPORT code_id LB opt_ext_params RB KW_AS code_id COLON KW_CUDA COMMA KW_NOCPU SCOLON  */
#line 208 "../parser/grammar.ypp"
                                                                                               {
        (yyval.sub_def_) = new import((yyvsp[-10].string_), (yyvsp[-8].opt_ext_params_), (yyvsp[-5].string_), std::string("CUDA, NOCPU"));
        (yyval.sub_def_)->set_position((yylsp[-11]).first_line);
    }
#line 1660 "grammar.tab.cpp"
    break;

  case 13: /* sub_def: KW_CPP NAME KW_BLOCK LB INT RB  */
#line 214 "../parser/grammar.ypp"
                                         {
        (yyval.sub_def_) = new cxx_block_def((yyvsp[-4].string_));
        (yyval.sub_def_)->set_position((yylsp[-5]).first_line);
    }
#line 1669 "grammar.tab.cpp"
    break;

  case 14: /* opt_ext_params: ext_params_seq  */
#line 221 "../parser/grammar.ypp"
                   { 
        (yyval.opt_ext_params_) = new opt_ext_params((yyvsp[0].ext_params_seq_));
        (yyval.opt_ext_params_)->set_position((yylsp[0]).first_line);
    }
#line 1678 "grammar.tab.cpp"
    break;

  case 15: /* opt_ext_params: %empty  */
#line 225 "../parser/grammar.ypp"
          { (yyval.opt_ext_params_) = new opt_ext_params(nullptr); }
#line 1684 "grammar.tab.cpp"
    break;

  case 16: /* ext_params_seq: code_df_param  */
#line 228 "../parser/grammar.ypp"
                       { 
        (yyval.ext_params_seq_) = new ext_params_seq();
        (yyval.ext_params_seq_)->params_->push_back((yyvsp[0].code_df_param_));
        (yyval.ext_params_seq_)->set_position((yylsp[0]).first_line);
    }
#line 1694 "grammar.tab.cpp"
    break;

  case 17: /* ext_params_seq: ext_params_seq COMMA code_df_param  */
#line 234 "../parser/grammar.ypp"
                                             {
        (yyvsp[-2].ext_params_seq_)->params_->push_back((yyvsp[0].code_df_param_));
        (yyval.ext_params_seq_)->set_position((yylsp[-2]).first_line);
    }
#line 1703 "grammar.tab.cpp"
    break;

  case 18: /* code_df_param: type code_df  */
#line 241 "../parser/grammar.ypp"
                 {
        (yyval.code_df_param_) = new code_df_param((yyvsp[-1].string_), (yyvsp[0].string_));
        (yyval.code_df_param_)->set_position((yylsp[-1]).first_line);
    }
#line 1712 "grammar.tab.cpp"
    break;

  case 19: /* code_df: NAME  */
#line 248 "../parser/grammar.ypp"
             { (yyval.string_) = (yyvsp[0].string_); }
#line 1718 "grammar.tab.cpp"
    break;

  case 20: /* code_df: %empty  */
#line 249 "../parser/grammar.ypp"
          { (yyval.string_) = nullptr; }
#line 1724 "grammar.tab.cpp"
    break;

  case 21: /* type: KW_INT  */
#line 253 "../parser/grammar.ypp"
               { (yyval.string_) = (yyvsp[0].string_);}
#line 1730 "grammar.tab.cpp"
    break;

  case 22: /* type: KW_REAL  */
#line 254 "../parser/grammar.ypp"
                  { (yyval.string_) = (yyvsp[0].string_); }
#line 1736 "grammar.tab.cpp"
    break;

  case 23: /* type: KW_STRING  */
#line 255 "../parser/grammar.ypp"
                     { (yyval.string_) = (yyvsp[0].string_); }
#line 1742 "grammar.tab.cpp"
    break;

  case 24: /* type: KW_NAME  */
#line 256 "../parser/grammar.ypp"
                  { (yyval.string_) = (yyvsp[0].string_); }
#line 1748 "grammar.tab.cpp"
    break;

  case 25: /* type: KW_VALUE AMP  */
#line 257 "../parser/grammar.ypp"
                       { (yyval.string_) = (yyvsp[-1].string_); }
#line 1754 "grammar.tab.cpp"
    break;

  case 26: /* type: KW_VALUE  */
#line 258 "../parser/grammar.ypp"
                   { (yyval.string_) = (yyvsp[0].string_); }
#line 1760 "grammar.tab.cpp"
    break;

  case 27: /* block: statement  */
#line 262 "../parser/grammar.ypp"
              { 
        (yyval.block_) = new block();
        (yyval.block_)->statement_seq_->statements_->push_back((yyvsp[0].statement_)); 
        (yyval.block_)->set_position((yylsp[0]).first_line);
    }
#line 1770 "grammar.tab.cpp"
    break;

  case 28: /* block: LCB opt_dfdecls statement_seq RCB opt_behavior  */
#line 268 "../parser/grammar.ypp"
                                                         { 
        (yyval.block_) = new block((yyvsp[-3].opt_dfdecls_), (yyvsp[-2].statement_seq_), (yyvsp[0].opt_behavior_)); 
        (yyval.block_)->set_position((yylsp[-4]).first_line);
    }
#line 1779 "grammar.tab.cpp"
    break;

  case 29: /* opt_dfdecls: dfdecls  */
#line 275 "../parser/grammar.ypp"
                { 
        (yyval.opt_dfdecls_) = new opt_dfdecls((yyvsp[0].dfdecls_)); 
        (yyval.opt_dfdecls_)->set_position((yylsp[0]).first_line);
    }
#line 1788 "grammar.tab.cpp"
    break;

  case 30: /* opt_dfdecls: %empty  */
#line 280 "../parser/grammar.ypp"
          { (yyval.opt_dfdecls_) = new opt_dfdecls(nullptr); }
#line 1794 "grammar.tab.cpp"
    break;

  case 31: /* dfdecls: KW_DF name_seq SCOLON  */
#line 284 "../parser/grammar.ypp"
                              { 
        (yyval.dfdecls_) = new dfdecls((yyvsp[-1].name_seq_)); 
        (yyval.dfdecls_)->set_position((yylsp[-2]).first_line);
    }
#line 1803 "grammar.tab.cpp"
    break;

  case 32: /* name_seq: NAME  */
#line 291 "../parser/grammar.ypp"
             { 
        (yyval.name_seq_) = new name_seq();
        (yyval.name_seq_)->names_->push_back((yyvsp[0].string_)); 
        (yyvsp[0].string_)->set_position((yylsp[0]).first_line);
    }
#line 1813 "grammar.tab.cpp"
    break;

  case 33: /* name_seq: name_seq COMMA NAME  */
#line 296 "../parser/grammar.ypp"
                              {
        (yyvsp[-2].name_seq_)->names_->push_back((yyvsp[0].string_));
        (yyvsp[0].string_)->set_position((yylsp[0]).first_line);
    }
#line 1822 "grammar.tab.cpp"
    break;

  case 34: /* statement_seq: statement  */
#line 303 "../parser/grammar.ypp"
                   { 
        (yyval.statement_seq_) = new statement_seq();
        (yyval.statement_seq_)->statements_->push_back((yyvsp[0].statement_)); 
        (yyval.statement_seq_)->set_position((yylsp[0]).first_line);
    }
#line 1832 "grammar.tab.cpp"
    break;

  case 35: /* statement_seq: statement_seq statement  */
#line 309 "../parser/grammar.ypp"
                                  {
        if ((yyvsp[-1].statement_seq_) != nullptr) {
            (yyvsp[-1].statement_seq_)->statements_->push_back((yyvsp[0].statement_));
            (yyval.statement_seq_)->set_position((yylsp[-1]).first_line);
        }
    }
#line 1843 "grammar.tab.cpp"
    break;

  case 36: /* control_pragma: LARR where_type COMMA expr RARR  */
#line 318 "../parser/grammar.ypp"
                                    {
        vector<expr *> exprs;
        exprs.push_back((yyvsp[-1].expr_));
        (yyval.control_pragma_) = new control_pragma((yyvsp[-3].string_), &exprs);
        (yyval.control_pragma_)->set_position((yylsp[-4]).first_line);
    }
#line 1854 "grammar.tab.cpp"
    break;

  case 37: /* control_pragma: LARR where_type COMMA expr COMMA expr RARR  */
#line 325 "../parser/grammar.ypp"
                                                 {
        vector<expr *> exprs;
        exprs.push_back((yyvsp[-3].expr_));
        exprs.push_back((yyvsp[-1].expr_));
        (yyval.control_pragma_) = new control_pragma((yyvsp[-5].string_), &exprs);
        (yyval.control_pragma_)->set_position((yylsp[-6]).first_line);

    }
#line 1867 "grammar.tab.cpp"
    break;

  case 38: /* control_pragma: LARR where_type RARR  */
#line 333 "../parser/grammar.ypp"
                          {
        (yyval.control_pragma_) = new control_pragma((yyvsp[-1].string_), nullptr);
        (yyval.control_pragma_)->set_position((yylsp[-2]).first_line);
    }
#line 1876 "grammar.tab.cpp"
    break;

  case 39: /* control_pragma: %empty  */
#line 338 "../parser/grammar.ypp"
    {
        (yyval.control_pragma_) =  nullptr;
    }
#line 1884 "grammar.tab.cpp"
    break;

  case 40: /* statement: cf_statement  */
#line 343 "../parser/grammar.ypp"
                 { (yyval.statement_) = (yyvsp[0].cf_statement_);}
#line 1890 "grammar.tab.cpp"
    break;

  case 41: /* statement: let_statement  */
#line 344 "../parser/grammar.ypp"
                    { (yyval.statement_) = (yyvsp[0].let_statement_); }
#line 1896 "grammar.tab.cpp"
    break;

  case 42: /* statement: for_statement  */
#line 345 "../parser/grammar.ypp"
                    { (yyval.statement_) = (yyvsp[0].for_statement_); }
#line 1902 "grammar.tab.cpp"
    break;

  case 43: /* statement: while_statement  */
#line 346 "../parser/grammar.ypp"
                      { (yyval.statement_) = (yyvsp[0].while_statement_); }
#line 1908 "grammar.tab.cpp"
    break;

  case 44: /* statement: if_statement  */
#line 347 "../parser/grammar.ypp"
                   { (yyval.statement_) = (yyvsp[0].if_statement_); }
#line 1914 "grammar.tab.cpp"
    break;

  case 45: /* statement: error SCOLON  */
#line 349 "../parser/grammar.ypp"
                   {
	    reporter.report(ERROR_LEVEL::ERROR,
            "Unexpected token",
            line,
            (yylsp[-1]).first_column,
            "cf, if, let, for, while.");
        delete (yyval.statement_);
        (yyval.statement_) = nullptr;
    }
#line 1928 "grammar.tab.cpp"
    break;

  case 46: /* cf_statement: opt_label code_id opt_exprs opt_setdf_rules opt_rules opt_behavior SCOLON  */
#line 361 "../parser/grammar.ypp"
                                                                                        {
        (yyval.cf_statement_) = new cf_statement((yyvsp[-6].opt_label_), (yyvsp[-5].string_), (yyvsp[-4].opt_exprs_), (yyvsp[-3].opt_setdf_rules_), (yyvsp[-2].opt_rules_), (yyvsp[-1].opt_behavior_));

        if ((yyvsp[-6].opt_label_)->line_ != 0) {
            (yyval.cf_statement_)->set_position((yyvsp[-6].opt_label_)->line_);
        }
        else {
            (yyval.cf_statement_)->set_position((yyvsp[-5].string_)->line_);
        }
    }
#line 1943 "grammar.tab.cpp"
    break;

  case 47: /* opt_behavior: AT LCB behv_pragmas_seq RCB  */
#line 374 "../parser/grammar.ypp"
                                    {
        (yyval.opt_behavior_) = new opt_behavior((yyvsp[-1].behv_pragmas_seq_));
    }
#line 1951 "grammar.tab.cpp"
    break;

  case 48: /* opt_behavior: %empty  */
#line 377 "../parser/grammar.ypp"
          { (yyval.opt_behavior_) = new opt_behavior(nullptr); }
#line 1957 "grammar.tab.cpp"
    break;

  case 49: /* behv_pragmas_seq: behv_pragma  */
#line 381 "../parser/grammar.ypp"
                    { 
        (yyval.behv_pragmas_seq_) = new behv_pragmas_seq();
        (yyval.behv_pragmas_seq_)->behv_pragma_->push_back((yyvsp[0].behv_pragma_)); 
        (yyval.behv_pragmas_seq_)->set_position((yylsp[0]).first_line);
    }
#line 1967 "grammar.tab.cpp"
    break;

  case 50: /* behv_pragmas_seq: behv_pragmas_seq behv_pragma  */
#line 386 "../parser/grammar.ypp"
                                       {
        (yyvsp[-1].behv_pragmas_seq_)->behv_pragma_->push_back((yyvsp[0].behv_pragma_));
        (yyval.behv_pragmas_seq_)->set_position((yylsp[-1]).first_line);
    }
#line 1976 "grammar.tab.cpp"
    break;

  case 51: /* behv_pragma: NAME id EQ expr SCOLON  */
#line 393 "../parser/grammar.ypp"
                               {
        (yyval.behv_pragma_) = new behv_pragma_eq((yyvsp[-4].string_), (yyvsp[-3].id_), (yyvsp[-1].expr_));
        (yyval.behv_pragma_)->set_position((yylsp[-4]).first_line);
    }
#line 1985 "grammar.tab.cpp"
    break;

  case 52: /* behv_pragma: NAME id EQG expr SCOLON  */
#line 398 "../parser/grammar.ypp"
                                  {
        (yyval.behv_pragma_) = new behv_pragma_eqg((yyvsp[-4].string_), (yyvsp[-3].id_), (yyvsp[-1].expr_));
        (yyval.behv_pragma_)->set_position((yylsp[-4]).first_line);
    }
#line 1994 "grammar.tab.cpp"
    break;

  case 53: /* behv_pragma: NAME id_seq SCOLON  */
#line 403 "../parser/grammar.ypp"
                             {
        (yyval.behv_pragma_) = new behv_pragma_id_seq((yyvsp[-2].string_), (yyvsp[-1].id_seq_));
        (yyval.behv_pragma_)->set_position((yylsp[-2]).first_line);
    }
#line 2003 "grammar.tab.cpp"
    break;

  case 54: /* behv_pragma: NAME COLON expr SCOLON  */
#line 408 "../parser/grammar.ypp"
                                 {
        (yyval.behv_pragma_) = new behv_pragma_expr((yyvsp[-3].string_), (yyvsp[-1].expr_));
        (yyval.behv_pragma_)->set_position((yylsp[-3]).first_line);
    }
#line 2012 "grammar.tab.cpp"
    break;

  case 55: /* behv_pragma: name_seq SCOLON  */
#line 413 "../parser/grammar.ypp"
                          {
        (yyval.behv_pragma_) = new behv_pragma_seq((yyvsp[-1].name_seq_));
        (yyval.behv_pragma_)->set_position((yylsp[-1]).first_line);
    }
#line 2021 "grammar.tab.cpp"
    break;

  case 56: /* id_seq: id  */
#line 420 "../parser/grammar.ypp"
           { 
        (yyval.id_seq_) = new id_seq();
        (yyval.id_seq_)->seq_->push_back((yyvsp[0].id_)); 
        (yyval.id_seq_)->set_position((yylsp[0]).first_line);
    }
#line 2031 "grammar.tab.cpp"
    break;

  case 57: /* id_seq: id_seq COMMA id  */
#line 425 "../parser/grammar.ypp"
                          {
        (yyvsp[-2].id_seq_)->seq_->push_back((yyvsp[0].id_));
        (yyval.id_seq_)->set_position((yylsp[-2]).first_line);
    }
#line 2040 "grammar.tab.cpp"
    break;

  case 58: /* let_statement: KW_LET assign_seq block  */
#line 430 "../parser/grammar.ypp"
                                       {
    (yyval.let_statement_) = new let_statement((yyvsp[-1].assign_seq_), (yyvsp[0].block_));
    (yyval.let_statement_)->set_position((yylsp[-2]).first_line);
}
#line 2049 "grammar.tab.cpp"
    break;

  case 59: /* for_statement: KW_FOR control_pragma NAME EQ expr DIAP expr block  */
#line 435 "../parser/grammar.ypp"
                                                                  {
    (yyval.for_statement_) = new for_statement((yyvsp[-6].control_pragma_), (yyvsp[-5].string_), (yyvsp[-3].expr_), (yyvsp[-1].expr_), (yyvsp[0].block_));
    (yyval.for_statement_)->set_position((yylsp[-7]).first_line);
}
#line 2058 "grammar.tab.cpp"
    break;

  case 60: /* while_statement: KW_WHILE control_pragma expr COMMA NAME EQ expr DIAP KW_OUT id block  */
#line 440 "../parser/grammar.ypp"
                                                                                      {
    (yyval.while_statement_) = new while_statement((yyvsp[-9].control_pragma_), (yyvsp[-8].expr_), (yyvsp[-6].string_), (yyvsp[-4].expr_), (yyvsp[-1].id_), (yyvsp[0].block_));
    (yyval.while_statement_)->set_position((yylsp[-10]).first_line);
}
#line 2067 "grammar.tab.cpp"
    break;

  case 61: /* if_statement: KW_IF expr block  */
#line 445 "../parser/grammar.ypp"
                                         {
    (yyval.if_statement_) = new if_statement((yyvsp[-1].expr_), (yyvsp[0].block_));
    (yyval.if_statement_)->set_position((yylsp[-2]).first_line);
}
#line 2076 "grammar.tab.cpp"
    break;

  case 62: /* assign_seq: assign  */
#line 454 "../parser/grammar.ypp"
               { 
        (yyval.assign_seq_) = new assign_seq();
        (yyval.assign_seq_)->assign_seq_->push_back((yyvsp[0].assign_));
        (yyval.assign_seq_)->set_position((yylsp[0]).first_line);
    }
#line 2086 "grammar.tab.cpp"
    break;

  case 63: /* assign_seq: assign_seq COMMA assign  */
#line 459 "../parser/grammar.ypp"
                                   {
        (yyvsp[-2].assign_seq_)->assign_seq_->push_back((yyvsp[0].assign_));
        (yyval.assign_seq_)->set_position((yylsp[-2]).first_line);
    }
#line 2095 "grammar.tab.cpp"
    break;

  case 64: /* assign: NAME EQ expr  */
#line 466 "../parser/grammar.ypp"
                     { (yyval.assign_) = new assign((yyvsp[-2].string_), (yyvsp[0].expr_)); }
#line 2101 "grammar.tab.cpp"
    break;

  case 65: /* opt_label: KW_CF id COLON  */
#line 470 "../parser/grammar.ypp"
                       { 
        (yyval.opt_label_) = new opt_label((yyvsp[-1].id_)); 
        (yyval.opt_label_)->set_position((yylsp[-2]).first_line);
    }
#line 2110 "grammar.tab.cpp"
    break;

  case 66: /* opt_label: %empty  */
#line 475 "../parser/grammar.ypp"
    {
        (yyval.opt_label_) = new opt_label(nullptr);
	}
#line 2118 "grammar.tab.cpp"
    break;

  case 67: /* id: NAME  */
#line 481 "../parser/grammar.ypp"
             { 
        (yyval.id_) = new simple_id((yyvsp[0].string_));
        (yyval.id_)->set_position((yylsp[0]).first_line);
        (yyvsp[0].string_)->set_position((yylsp[0]).first_line);
    }
#line 2128 "grammar.tab.cpp"
    break;

  case 68: /* id: id LSB expr RSB  */
#line 487 "../parser/grammar.ypp"
                          {
        (yyval.id_) = new complex_id((yyvsp[-3].id_), (yyvsp[-1].expr_));
        (yyval.id_)->set_position((yylsp[-3]).first_line);
    }
#line 2137 "grammar.tab.cpp"
    break;

  case 69: /* opt_exprs: LB exprs_seq RB  */
#line 494 "../parser/grammar.ypp"
                        { 
        (yyval.opt_exprs_) = new opt_exprs((yyvsp[-1].exprs_seq_)); 
    }
#line 2145 "grammar.tab.cpp"
    break;

  case 70: /* opt_exprs: LB RB  */
#line 497 "../parser/grammar.ypp"
                { 
        (yyval.opt_exprs_) = new opt_exprs(nullptr); 
        }
#line 2153 "grammar.tab.cpp"
    break;

  case 71: /* exprs_seq: expr  */
#line 503 "../parser/grammar.ypp"
             { 
        (yyval.exprs_seq_) = new exprs_seq();
        (yyval.exprs_seq_)->expr_->push_back((yyvsp[0].expr_)); 
    }
#line 2162 "grammar.tab.cpp"
    break;

  case 72: /* exprs_seq: exprs_seq COMMA expr  */
#line 507 "../parser/grammar.ypp"
                               {
        (yyvsp[-2].exprs_seq_)->expr_->push_back((yyvsp[0].expr_));
    }
#line 2170 "grammar.tab.cpp"
    break;

  case 73: /* opt_setdf_rules: RARR opt_exprs  */
#line 513 "../parser/grammar.ypp"
                       { 
        (yyval.opt_setdf_rules_) = new opt_setdf_rules((yyvsp[0].opt_exprs_)); 
        // // // std::cerr << "opt_setdf_rules" << std::endl;
        }
#line 2179 "grammar.tab.cpp"
    break;

  case 74: /* opt_setdf_rules: %empty  */
#line 517 "../parser/grammar.ypp"
      {
        (yyval.opt_setdf_rules_) = new opt_setdf_rules(nullptr); 
    }
#line 2187 "grammar.tab.cpp"
    break;

  case 75: /* opt_rules: ARROW opt_exprs  */
#line 523 "../parser/grammar.ypp"
                    { (yyval.opt_rules_) = new opt_rules((yyvsp[0].opt_exprs_)); }
#line 2193 "grammar.tab.cpp"
    break;

  case 76: /* opt_rules: %empty  */
#line 524 "../parser/grammar.ypp"
      {
        (yyval.opt_rules_) = new opt_rules(nullptr);
    }
#line 2201 "grammar.tab.cpp"
    break;

  case 77: /* code_id: NAME  */
#line 530 "../parser/grammar.ypp"
             { 
        (yyval.string_) = (yyvsp[0].string_);
        (yyval.string_)->set_position((yylsp[0]).first_line);
    }
#line 2210 "grammar.tab.cpp"
    break;

  case 78: /* expr: INT  */
#line 537 "../parser/grammar.ypp"
            { 
        (yyval.expr_) = (yyvsp[0].int_); 
        (yyval.expr_)->set_position((yylsp[0]).first_line);
    }
#line 2219 "grammar.tab.cpp"
    break;

  case 79: /* expr: REAL  */
#line 541 "../parser/grammar.ypp"
               { 
        (yyval.expr_) = (yyvsp[0].double_); 
        (yyval.expr_)->set_position((yylsp[0]).first_line);
    }
#line 2228 "grammar.tab.cpp"
    break;

  case 80: /* expr: STRING  */
#line 545 "../parser/grammar.ypp"
                 { 
        (yyval.expr_) = (yyvsp[0].string_); 
        (yyval.expr_)->set_position((yylsp[0]).first_line);
    }
#line 2237 "grammar.tab.cpp"
    break;

  case 81: /* expr: KW_INT LB expr RB  */
#line 551 "../parser/grammar.ypp"
                            { 
        (yyval.expr_) = new to_int((yyvsp[-1].expr_)); 
        (yyval.expr_)->set_position((yylsp[-3]).first_line);
    }
#line 2246 "grammar.tab.cpp"
    break;

  case 82: /* expr: KW_REAL LB expr RB  */
#line 556 "../parser/grammar.ypp"
                              {
        (yyval.expr_) = new to_real((yyvsp[-1].expr_)); 
        (yyval.expr_)->set_position((yylsp[-3]).first_line);
    }
#line 2255 "grammar.tab.cpp"
    break;

  case 83: /* expr: KW_STRING LB expr RB  */
#line 561 "../parser/grammar.ypp"
                               { 
        (yyval.expr_) = new to_str((yyvsp[-1].expr_)); 
        (yyval.expr_)->set_position((yylsp[-3]).first_line);
    }
#line 2264 "grammar.tab.cpp"
    break;

  case 84: /* expr: expr PLUS expr  */
#line 566 "../parser/grammar.ypp"
                         { 
        (yyval.expr_) = new sum((yyvsp[-2].expr_), (yyvsp[0].expr_)); 
        (yyval.expr_)->set_position((yylsp[-2]).first_line);
    }
#line 2273 "grammar.tab.cpp"
    break;

  case 85: /* expr: expr MINUS expr  */
#line 570 "../parser/grammar.ypp"
                          { 
        (yyval.expr_) = new sub((yyvsp[-2].expr_), (yyvsp[0].expr_));
        (yyval.expr_)->set_position((yylsp[-2]).first_line);
    }
#line 2282 "grammar.tab.cpp"
    break;

  case 86: /* expr: expr MUL expr  */
#line 574 "../parser/grammar.ypp"
                        { 
        (yyval.expr_) = new mul((yyvsp[-2].expr_), (yyvsp[0].expr_));
        (yyval.expr_)->set_position((yylsp[-2]).first_line);
    }
#line 2291 "grammar.tab.cpp"
    break;

  case 87: /* expr: expr DIV expr  */
#line 578 "../parser/grammar.ypp"
                        { 
        (yyval.expr_) = new div1((yyvsp[-2].expr_), (yyvsp[0].expr_)); 
        (yyval.expr_)->set_position((yylsp[-2]).first_line);
    }
#line 2300 "grammar.tab.cpp"
    break;

  case 88: /* expr: expr MOD expr  */
#line 583 "../parser/grammar.ypp"
                        { 
        (yyval.expr_) = new mod((yyvsp[-2].expr_), (yyvsp[0].expr_));
        (yyval.expr_)->set_position((yylsp[-2]).first_line);
    }
#line 2309 "grammar.tab.cpp"
    break;

  case 89: /* expr: expr LT expr  */
#line 587 "../parser/grammar.ypp"
                       { 
        (yyval.expr_) = new lt((yyvsp[-2].expr_), (yyvsp[0].expr_)); 
        (yyval.expr_)->set_position((yylsp[-2]).first_line);
    }
#line 2318 "grammar.tab.cpp"
    break;

  case 90: /* expr: expr GT expr  */
#line 591 "../parser/grammar.ypp"
                       { 
        (yyval.expr_) = new gt((yyvsp[-2].expr_), (yyvsp[0].expr_)); 
        (yyval.expr_)->set_position((yylsp[-2]).first_line);
    }
#line 2327 "grammar.tab.cpp"
    break;

  case 91: /* expr: expr LEQ expr  */
#line 595 "../parser/grammar.ypp"
                        { 
        (yyval.expr_) = new leq((yyvsp[-2].expr_), (yyvsp[0].expr_));
        (yyval.expr_)->set_position((yylsp[-2]).first_line);
     }
#line 2336 "grammar.tab.cpp"
    break;

  case 92: /* expr: expr GEQ expr  */
#line 599 "../parser/grammar.ypp"
                        { 
        (yyval.expr_) = new geq((yyvsp[-2].expr_), (yyvsp[0].expr_));
        (yyval.expr_)->set_position((yylsp[-2]).first_line);
     }
#line 2345 "grammar.tab.cpp"
    break;

  case 93: /* expr: expr DBLEQ expr  */
#line 603 "../parser/grammar.ypp"
                         { 
        (yyval.expr_) = new dbleq((yyvsp[-2].expr_), (yyvsp[0].expr_)); 
        (yyval.expr_)->set_position((yylsp[-2]).first_line);
    }
#line 2354 "grammar.tab.cpp"
    break;

  case 94: /* expr: expr NEQ expr  */
#line 607 "../parser/grammar.ypp"
                        { 
        (yyval.expr_) = new neq((yyvsp[-2].expr_), (yyvsp[0].expr_));
        (yyval.expr_)->set_position((yylsp[-2]).first_line);
    }
#line 2363 "grammar.tab.cpp"
    break;

  case 95: /* expr: expr DBLAMP expr  */
#line 612 "../parser/grammar.ypp"
                           { 
        (yyval.expr_) = new dblamp((yyvsp[-2].expr_), (yyvsp[0].expr_)); 
        (yyval.expr_)->set_position((yylsp[-2]).first_line);
    }
#line 2372 "grammar.tab.cpp"
    break;

  case 96: /* expr: expr DBLPIPE expr  */
#line 616 "../parser/grammar.ypp"
                            { 
        (yyval.expr_) = new dblpipe((yyvsp[-2].expr_), (yyvsp[0].expr_)); 
        (yyval.expr_)->set_position((yylsp[-2]).first_line);
    }
#line 2381 "grammar.tab.cpp"
    break;

  case 97: /* expr: LB expr RB  */
#line 620 "../parser/grammar.ypp"
                     { 
        (yyval.expr_) = (yyvsp[-1].expr_); 
        (yyval.expr_)->set_position((yylsp[-2]).first_line);
    }
#line 2390 "grammar.tab.cpp"
    break;

  case 98: /* expr: id  */
#line 624 "../parser/grammar.ypp"
             { 
        (yyval.expr_) = (yyvsp[0].id_); 
        (yyval.expr_)->set_position((yylsp[0]).first_line);
    }
#line 2399 "grammar.tab.cpp"
    break;

  case 99: /* expr: expr QMARK expr COLON expr  */
#line 628 "../parser/grammar.ypp"
                                     {
        // todo
    }
#line 2407 "grammar.tab.cpp"
    break;

  case 100: /* opt_params: LB params_seq RB  */
#line 634 "../parser/grammar.ypp"
                         { 
        (yyval.opt_params_) = new opt_params((yyvsp[-1].param_seq_)); 
        (yyval.opt_params_)->set_position((yylsp[-2]).first_line);
    }
#line 2416 "grammar.tab.cpp"
    break;

  case 101: /* opt_params: LB RB  */
#line 638 "../parser/grammar.ypp"
                { 
        (yyval.opt_params_) = new opt_params(nullptr);
        (yyval.opt_params_)->set_position((yylsp[-1]).first_line);
    }
#line 2425 "grammar.tab.cpp"
    break;

  case 102: /* opt_params: LB error RB  */
#line 643 "../parser/grammar.ypp"
                  {
        reporter.report(ERROR_LEVEL::ERROR,
            "Unexpected token",
            line,
            (yylsp[-2]).first_column,
            "int, name, value, real, string");
        delete (yyval.opt_params_);
        (yyval.opt_params_) = new opt_params(nullptr);
    }
#line 2439 "grammar.tab.cpp"
    break;

  case 103: /* params_seq: param  */
#line 655 "../parser/grammar.ypp"
               { 
        (yyval.param_seq_) = new param_seq();
        (yyval.param_seq_)->params_->push_back((yyvsp[0].param_)); 
        (yyval.param_seq_)->set_position((yylsp[0]).first_line);
    }
#line 2449 "grammar.tab.cpp"
    break;

  case 104: /* params_seq: params_seq COMMA param  */
#line 660 "../parser/grammar.ypp"
                                 {
        (yyvsp[-2].param_seq_)->params_->push_back((yyvsp[0].param_));
    }
#line 2457 "grammar.tab.cpp"
    break;

  case 105: /* param: type NAME  */
#line 666 "../parser/grammar.ypp"
                  { 
        (yyval.param_) = new param((yyvsp[-1].string_), (yyvsp[0].string_)); 
        (yyval.param_)->set_position((yylsp[0]).first_line);
        (yyvsp[0].string_)->set_position((yylsp[0]).first_line);
    }
#line 2467 "grammar.tab.cpp"
    break;

  case 106: /* where_type: KW_RUSH  */
#line 674 "../parser/grammar.ypp"
            { 
        (yyval.string_) = (yyvsp[0].string_); 
        (yyval.string_)->set_position((yylsp[0]).first_line);
    }
#line 2476 "grammar.tab.cpp"
    break;

  case 107: /* where_type: KW_STATIC  */
#line 678 "../parser/grammar.ypp"
                { 
        (yyval.string_) = (yyvsp[0].string_); 
        (yyval.string_)->set_position((yylsp[0]).first_line);
    }
#line 2485 "grammar.tab.cpp"
    break;

  case 108: /* where_type: KW_STATIC_FOR  */
#line 682 "../parser/grammar.ypp"
                    { 
        (yyval.string_) = (yyvsp[0].string_); 
        (yyval.string_)->set_position((yylsp[0]).first_line);
    }
#line 2494 "grammar.tab.cpp"
    break;

  case 109: /* where_type: KW_UNROLLING  */
#line 686 "../parser/grammar.ypp"
                  { 
        (yyval.string_) = (yyvsp[0].string_); 
        (yyval.string_)->set_position((yylsp[0]).first_line);
    }
#line 2503 "grammar.tab.cpp"
    break;

  case 110: /* where_type: %empty  */
#line 690 "../parser/grammar.ypp"
      { (yyval.string_) = new luna_string(nullptr); }
#line 2509 "grammar.tab.cpp"
    break;


#line 2513 "grammar.tab.cpp"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  yyerror_range[1] = yylloc;
  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
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
                      yytoken, &yylval, &yylloc);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
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
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp, yylsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  ++yylsp;
  YYLLOC_DEFAULT (*yylsp, yyerror_range, 2);

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp, yylsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 692 "../parser/grammar.ypp"

