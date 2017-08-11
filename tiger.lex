%{
#include <string.h>
#include "util.h"
#include "tokens.h"
#include "errormsg.h"

int charPos=1;

int yywrap(void)
{
 charPos=1;
 return 1;
}


void adjust(void)
{
 EM_tokPos=charPos;
 charPos+=yyleng;
}

%}

%x STR ESC COMMENT
%%
	int comment_nest = 0;

" "	 |
\t   |
\r   {adjust(); continue;}
\n	 {adjust(); EM_newline(); continue;}
","	 {adjust(); return COMMA;}
":"  {adjust(); return COLON;}
";"  {adjust(); return SEMICOLON;}
"("  {adjust(); return LPAREN;}
")"  {adjust(); return RPAREN;}
"["  {adjust(); return LBRACK;}
"]"  {adjust(); return RBRACK;}
"{"  {adjust(); return LBRACE;}
"}"  {adjust(); return RBRACE;}
"."  {adjust(); return DOT;}
"+"  {adjust(); return PLUS;}
"-"  {adjust(); return MINUS;}
"*"  {adjust(); return TIMES;}
"/"  {adjust(); return DIVIDE;}
"="  {adjust(); return EQ;}
"<>" {adjust(); return NEQ;}
"<"  {adjust(); return LT;}
"<=" {adjust(); return LE;}
">"  {adjust(); return GT;}
">=" {adjust(); return GE;}
"&"  {adjust(); return AND;}
"|"  {adjust(); return OR;}
":=" {adjust(); return ASSIGN;}
array    {adjust(); return ARRAY;}
if       {adjust(); return IF;}
then     {adjust(); return THEN;}
else     {adjust(); return ELSE;}
while    {adjust(); return WHILE;}
for  	 {adjust(); return FOR;}
to       {adjust(); return TO;}
do       {adjust(); return DO;}
let      {adjust(); return LET;}
in       {adjust(); return IN;}
end      {adjust(); return END;}
of       {adjust(); return OF;}
break    {adjust(); return BREAK;}
nil      {adjust(); return NIL;}
function {adjust(); return FUNCTION;}
var      {adjust(); return VAR;}
type     {adjust(); return TYPE;}
int      {adjust(); return INT_RES;}
string   {adjust(); return STRING_RES;}

[0-9]+	 {adjust(); yylval.ival=atoi(yytext); return INT;}

\"       {adjust(); yymore(); BEGIN(STR);}
<STR>\"  {adjust(); yylval.sval=String(yytext); BEGIN(INITIAL); return STRING;}
<STR>\\  {adjust(); yymore(); BEGIN(ESC);}
<ESC>(n|t|^c|[0-9]{3}|\"|\\) {adjust(); yymore(); BEGIN(STR);}
<ESC>(" "|\t)+\\  {adjust(); BEGIN(STR);}
<ESC>(" "|\t|\n)+\\  {adjust(); EM_newline(); BEGIN(STR);}
<ESC>.|" "|\t|\n|\r  {adjust(); EM_error(EM_tokPos, "illegal escape"); BEGIN(STR);}
<STR>\n      {adjust(); EM_newline(); EM_error(EM_tokPos,"unclosed string"); BEGIN(INITIAL);}
<STR><<EOF>> {adjust(); EM_error(EM_tokPos,"unclosed string"); BEGIN(INITIAL); yyterminate();}
<STR>.|" "|\t|\r  {adjust(); yymore();} 

[a-zA-Z]([a-zA-Z0-9]|"_")* {adjust(); yylval.sval=String(yytext); return ID;}

"/*"     {adjust(); comment_nest++; BEGIN(COMMENT);} 
<COMMENT>"/*"  {adjust(); comment_nest++;}
<COMMENT><<EOF>> {adjust(); EM_error(EM_tokPos, "unclosed comment"); BEGIN(INITIAL); yyterminate();}
<COMMENT>\n {adjust(); EM_newline();}
<COMMENT>.|" "|\t|\r {adjust();}
<COMMENT>"*/"  {adjust(); comment_nest--; if(comment_nest == 0) BEGIN(INITIAL);}

<<EOF>> {adjust(); yyterminate();}
.	    {adjust(); EM_error(EM_tokPos,"illegal token");}
