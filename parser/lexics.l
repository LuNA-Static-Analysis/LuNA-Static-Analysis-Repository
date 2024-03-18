%{

#include "../parser/ast.hpp"
#include "grammar.tab.hpp"
#include "../parser/error_reporter.hpp"

#include <string>
#include <cstring>

void yyerror(const char*);
extern "C" int yywrap() { return 1; }

void next_line();
std::string yystring();
void next_token(const std::string&);
extern bool has_errors;
using namespace std;
extern error_reporter reporter;

extern int cur_position_in_line;
extern int line_num;
extern ast* ast_;

bool is_end_of_define = false;

#define SAVE_TOKEN std::string* token = new std::string(yytext); \
				luna_string* l = new luna_string(token); \
				l->set_position(cur_position_in_line); \
				yylval.string_ = l; \
				ast_->push_token(token)
				

#define YY_USER_ACTION yylloc.first_line = line_num; \
	yylloc.first_column = cur_position_in_line;

%}

%%

as { next_token(yystring()); return KW_AS; }
cf { next_token(yystring()); return KW_CF; }
df { next_token(yystring()); return KW_DF; }
import { next_token(yystring()); return KW_IMPORT; }
for { next_token(yystring()); return KW_FOR; }
if { next_token(yystring()); return KW_IF; }
else { next_token(yystring()); return KW_ELSE; }
let { next_token(yystring()); return KW_LET; }
in { next_token(yystring()); return KW_IN; }
out { next_token(yystring()); return KW_OUT; }
SIZE {next_token(yystring()); return KW_SIZE; }
sub { next_token(yystring()); return KW_SUB; }
while { next_token(yystring()); return KW_WHILE; }
int { SAVE_TOKEN; next_token(yystring()); return KW_INT; }
real { SAVE_TOKEN; next_token(yystring()); return KW_REAL; }
string { SAVE_TOKEN; next_token(yystring()); return KW_STRING; }
name { SAVE_TOKEN; next_token(yystring()); return KW_NAME; }
value { SAVE_TOKEN; next_token(yystring()); return KW_VALUE; }
rush { next_token(yystring()); return KW_RUSH; }
static { next_token(yystring()); return KW_STATIC; }
static_for { next_token(yystring()); return KW_STATIC_FOR; }
unrolling { next_token(yystring()); return KW_UNROLLING; }
CUDA { next_token(yystring()); return KW_CUDA;}
NOCPU { next_token(yystring()); return KW_NOCPU;}

"$"  {next_token(yystring()); return BUCK; }
"&"  {next_token(yystring()); return AMP; }
"="  {next_token(yystring()); return EQ; }
"<"  {next_token(yystring()); return LT; }
">"  {next_token(yystring()); return GT; }
";"  {next_token(yystring()); return SCOLON; }
":"  {next_token(yystring()); return COLON;}
"?"  {next_token(yystring()); return QMARK;}
","  {next_token(yystring()); return COMMA; }
"."  {next_token(yystring()); return DOT; }
".." {next_token(yystring()); return DIAP; }
"{"  {next_token(yystring()); return LCB; }
"}"  {next_token(yystring()); return RCB; }
"("  {next_token(yystring()); return LB; }
")"  {next_token(yystring()); return RB; }
"["  {next_token(yystring()); return LSB; }
"]"  {next_token(yystring()); return RSB; }
"+"  {next_token(yystring()); return PLUS; }
"-"  {next_token(yystring()); return MINUS; }
"*"  {next_token(yystring()); return MUL; }
"/"  {next_token(yystring()); return DIV; }
"%"  {next_token(yystring()); return MOD; }
"#"  {next_token(yystring()); return SHARP; }
"@"  {next_token(yystring()); return AT; }
"==" {next_token(yystring()); return DBLEQ; }
"<=" {next_token(yystring()); return LEQ; }
"=>" {next_token(yystring()); return EQG; }
">=" {next_token(yystring()); return GEQ; }
"!=" {next_token(yystring()); return NEQ; }
"&&" {next_token(yystring()); return DBLAMP; }
"||" {next_token(yystring()); return DBLPIPE; }
"-->" {next_token(yystring()); return ARROW; }
"<--" {next_token(yystring()); return LARROW; }
"<<" {next_token(yystring()); return LARR; }
">>" {next_token(yystring()); return RARR; }
"C++" {next_token(yystring()); return KW_CPP; }
"__block" {next_token(yystring()); return KW_BLOCK; }

[0-9]+ {
	SAVE_TOKEN;
	next_token(yystring());
	return INT;
}

[0-9]+"."[0-9]+ {
	SAVE_TOKEN;
	next_token(yystring()); 
	return REAL;
}

\/\*.*\*\/ { std::cerr << std::string(yytext, yyleng) << std::endl; /* comments */ }


\`.*\n { std::cerr << std::string(yytext, yyleng) << std::endl; /* comments */ }

\/\/.*$ { next_token(yystring()); std::cerr << std::string(yytext, yyleng) << std::endl;/* comments */ }

\"[^\"]*\" { 
	SAVE_TOKEN;
	next_token(yystring());
	return STRING; 
}

[A-Za-z_][A-Za-z0-9_]* {
	SAVE_TOKEN;
	next_token(yystring()); 
	return NAME;
}

\n { 
	next_line(); 
	next_token(yystring());

}

[ \t]+ { next_token(yystring()); }

. { std::cerr << ((string("invalid symbol: ") + yystring()).c_str()) << std::endl; }

%%

extern int cur_position_in_line;
extern int line_num;
extern string line;
extern string prev_line;
extern uint tokens;

void next_line() {
	if (!line.empty()) {
		prev_line = line;
	}
	line = "";
	line_num++;
	cur_position_in_line = 1;
}

void next_token(const std::string &s) {
	tokens++;
	yylloc.first_column = cur_position_in_line;
	
	cur_position_in_line += s.size();
	line += s;

	yylloc.last_column = cur_position_in_line;
}

std::string yystring() {
	return std::string(yytext, yyleng);
}

void yyerror(const char* msg) {
	/* std::cerr << "yyerror\n"; */
	/* if (has_errors) return; */
	reporter.report(ERROR_LEVEL::ERROR, 
		"Unexpected token",
        line,
	 	line_num
    );
	// has_errors = true; */
}