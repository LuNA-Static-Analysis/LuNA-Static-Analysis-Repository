/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_GRAMMAR_TAB_HPP_INCLUDED
# define YY_YY_GRAMMAR_TAB_HPP_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    STRING = 258,                  /* STRING  */
    NAME = 259,                    /* NAME  */
    INT = 260,                     /* INT  */
    REAL = 261,                    /* REAL  */
    SCOLON = 262,                  /* SCOLON  */
    COMMA = 263,                   /* COMMA  */
    COLON = 264,                   /* COLON  */
    DOT = 265,                     /* DOT  */
    SHARP = 266,                   /* SHARP  */
    ARROW = 267,                   /* ARROW  */
    LARROW = 268,                  /* LARROW  */
    LARR = 269,                    /* LARR  */
    RARR = 270,                    /* RARR  */
    AMP = 271,                     /* AMP  */
    KW_CF = 272,                   /* KW_CF  */
    KW_DF = 273,                   /* KW_DF  */
    KW_IMPORT = 274,               /* KW_IMPORT  */
    KW_AS = 275,                   /* KW_AS  */
    KW_FOR = 276,                  /* KW_FOR  */
    KW_IF = 277,                   /* KW_IF  */
    KW_LET = 278,                  /* KW_LET  */
    KW_VALUE = 279,                /* KW_VALUE  */
    KW_CUDA = 280,                 /* KW_CUDA  */
    KW_NOCPU = 281,                /* KW_NOCPU  */
    KW_IN = 282,                   /* KW_IN  */
    KW_OUT = 283,                  /* KW_OUT  */
    KW_SIZE = 284,                 /* KW_SIZE  */
    KW_SUB = 285,                  /* KW_SUB  */
    KW_WHILE = 286,                /* KW_WHILE  */
    KW_INT = 287,                  /* KW_INT  */
    KW_NAME = 288,                 /* KW_NAME  */
    KW_REAL = 289,                 /* KW_REAL  */
    KW_STRING = 290,               /* KW_STRING  */
    KW_RUSH = 291,                 /* KW_RUSH  */
    KW_STATIC = 292,               /* KW_STATIC  */
    KW_STATIC_FOR = 293,           /* KW_STATIC_FOR  */
    KW_UNROLLING = 294,            /* KW_UNROLLING  */
    KW_BLOCK = 295,                /* KW_BLOCK  */
    KW_CPP = 296,                  /* KW_CPP  */
    INTERPRETER = 297,             /* INTERPRETER  */
    EQ = 298,                      /* EQ  */
    LB = 299,                      /* LB  */
    RB = 300,                      /* RB  */
    LT = 301,                      /* LT  */
    GT = 302,                      /* GT  */
    LSB = 303,                     /* LSB  */
    RSB = 304,                     /* RSB  */
    DIAP = 305,                    /* DIAP  */
    QMARK = 306,                   /* QMARK  */
    DBLEQ = 307,                   /* DBLEQ  */
    NEQ = 308,                     /* NEQ  */
    LEQ = 309,                     /* LEQ  */
    EQG = 310,                     /* EQG  */
    GEQ = 311,                     /* GEQ  */
    AT = 312,                      /* AT  */
    PLUS = 313,                    /* PLUS  */
    MINUS = 314,                   /* MINUS  */
    MUL = 315,                     /* MUL  */
    DIV = 316,                     /* DIV  */
    MOD = 317,                     /* MOD  */
    TAB = 318,                     /* TAB  */
    WS = 319,                      /* WS  */
    SWS = 320,                     /* SWS  */
    LN = 321,                      /* LN  */
    BUCK = 322,                    /* BUCK  */
    PIPE = 323,                    /* PIPE  */
    ASSIGN = 324,                  /* ASSIGN  */
    LCB = 325,                     /* LCB  */
    RCB = 326,                     /* RCB  */
    IDX = 327,                     /* IDX  */
    DBLPIPE = 328,                 /* DBLPIPE  */
    DBLAMP = 329,                  /* DBLAMP  */
    IFX = 330,                     /* IFX  */
    KW_ELSE = 331                  /* KW_ELSE  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 34 "../parser/grammar.ypp"

    program *program_;
    sub_def *sub_def_;
    statement *statement_;
    expr *expr_;
    opt_ext_params *opt_ext_params_;
    ext_params_seq *ext_params_seq_;
    code_df_param *code_df_param_;
    block *block_;
    opt_dfdecls *opt_dfdecls_;
    dfdecls *dfdecls_;
    name_seq *name_seq_;
    statement_seq *statement_seq_;
    control_pragma *control_pragma_;
    let_statement *let_statement_;
    cf_statement *cf_statement_;
    while_statement *while_statement_;
    for_statement *for_statement_;
    if_statement *if_statement_;
    opt_behavior *opt_behavior_;
    behv_pragma *behv_pragma_;
    id_seq *id_seq_;
    assign_seq *assign_seq_;
    assign *assign_;
    opt_label *opt_label_;
    id *id_;
    opt_exprs *opt_exprs_;
    exprs_seq *exprs_seq_;
    opt_setdf_rules *opt_setdf_rules_;
    opt_rules *opt_rules_;
    sum *sum_;
    sub *sub_;
    mul *mul_;
    div1 *div_;
    lt *lt_;
    mod *mod_;
    gt *gt_;
    leq *leq_;
    geq *geq_;
    dbleq *dbleq_;
    neq *neq_;
    dblamp *dblamp_;
    dblpipe *dblpipe_;
    param_seq *param_seq_;
    param *param_;
    opt_params* opt_params_;
    behv_pragmas_seq *behv_pragmas_seq_;
    integer* int_;
    real* double_;
    luna_string* string_;

#line 192 "grammar.tab.hpp"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif


extern YYSTYPE yylval;
extern YYLTYPE yylloc;

int yyparse (void);


#endif /* !YY_YY_GRAMMAR_TAB_HPP_INCLUDED  */
