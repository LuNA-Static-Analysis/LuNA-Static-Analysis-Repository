/* A Bison parser, made by GNU Bison 2.7.  */

/* Bison interface for Yacc-like parsers in C
   
      Copyright (C) 1984, 1989-1990, 2000-2012 Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

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

#ifndef YY_YY_GRAMMAR_TAB_HPP_INCLUDED
# define YY_YY_GRAMMAR_TAB_HPP_INCLUDED
/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     STRING = 258,
     NAME = 259,
     INT = 260,
     REAL = 261,
     SCOLON = 262,
     COMMA = 263,
     COLON = 264,
     DOT = 265,
     SHARP = 266,
     ARROW = 267,
     LARROW = 268,
     LARR = 269,
     RARR = 270,
     AMP = 271,
     KW_CF = 272,
     KW_DF = 273,
     KW_IMPORT = 274,
     KW_AS = 275,
     KW_FOR = 276,
     KW_IF = 277,
     KW_LET = 278,
     KW_VALUE = 279,
     KW_CUDA = 280,
     KW_NOCPU = 281,
     KW_IN = 282,
     KW_OUT = 283,
     KW_SIZE = 284,
     KW_SUB = 285,
     KW_WHILE = 286,
     KW_INT = 287,
     KW_NAME = 288,
     KW_REAL = 289,
     KW_STRING = 290,
     KW_RUSH = 291,
     KW_STATIC = 292,
     KW_STATIC_FOR = 293,
     KW_UNROLLING = 294,
     KW_BLOCK = 295,
     KW_CPP = 296,
     INTERPRETER = 297,
     EQ = 298,
     LB = 299,
     RB = 300,
     LT = 301,
     GT = 302,
     LSB = 303,
     RSB = 304,
     DIAP = 305,
     QMARK = 306,
     DBLEQ = 307,
     NEQ = 308,
     LEQ = 309,
     EQG = 310,
     GEQ = 311,
     AT = 312,
     PLUS = 313,
     MINUS = 314,
     MUL = 315,
     DIV = 316,
     MOD = 317,
     TAB = 318,
     WS = 319,
     SWS = 320,
     LN = 321,
     BUCK = 322,
     PIPE = 323,
     ASSIGN = 324,
     LCB = 325,
     RCB = 326,
     IDX = 327,
     DBLPIPE = 328,
     DBLAMP = 329,
     IFX = 330,
     KW_ELSE = 331
   };
#endif


#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{
/* Line 2058 of yacc.c  */
#line 34 "grammar.ypp"

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


/* Line 2058 of yacc.c  */
#line 186 "grammar.tab.hpp"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;
# define yyltype YYLTYPE /* obsolescent; will be withdrawn */
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;
extern YYLTYPE yylloc;
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

#endif /* !YY_YY_GRAMMAR_TAB_HPP_INCLUDED  */
