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
	char str[256];
	char *str_p;
	int str_len;

" "	 |
\t	 | 
\r   {adjust(); continue;}
\n	 {adjust(); EM_newline(); continue;}
","	 {adjust(); yylval.pos = EM_tokPos; return COMMA;}
":"  {adjust(); yylval.pos = EM_tokPos; return COLON;}
";"  {adjust(); yylval.pos = EM_tokPos; return SEMICOLON;}
"("  {adjust(); yylval.pos = EM_tokPos; return LPAREN;}
")"  {adjust(); yylval.pos = EM_tokPos; return RPAREN;}
"["  {adjust(); yylval.pos = EM_tokPos; return LBRACK;}
"]"  {adjust(); yylval.pos = EM_tokPos; return RBRACK;}
"{"  {adjust(); yylval.pos = EM_tokPos; return LBRACE;}
"}"  {adjust(); yylval.pos = EM_tokPos; return RBRACE;}
"."  {adjust(); yylval.pos = EM_tokPos; return DOT;}
"+"  {adjust(); yylval.pos = EM_tokPos; return PLUS;}
"-"  {adjust(); yylval.pos = EM_tokPos; return MINUS;}
"*"  {adjust(); yylval.pos = EM_tokPos; return TIMES;}
"/"  {adjust(); yylval.pos = EM_tokPos; return DIVIDE;}
"="  {adjust(); yylval.pos = EM_tokPos; return EQ;}
"<>" {adjust(); yylval.pos = EM_tokPos; return NEQ;}
"<"  {adjust(); yylval.pos = EM_tokPos; return LT;}
"<=" {adjust(); yylval.pos = EM_tokPos; return LE;}
">"  {adjust(); yylval.pos = EM_tokPos; return GT;}
">=" {adjust(); yylval.pos = EM_tokPos; return GE;}
"&"  {adjust(); yylval.pos = EM_tokPos; return AND;}
"|"  {adjust(); yylval.pos = EM_tokPos; return OR;}
":=" {adjust(); yylval.pos = EM_tokPos; return ASSIGN;}
array    {adjust(); yylval.pos = EM_tokPos; return ARRAY;}
if       {adjust(); yylval.pos = EM_tokPos; return IF;}
then     {adjust(); yylval.pos = EM_tokPos; return THEN;}
else     {adjust(); yylval.pos = EM_tokPos; return ELSE;}
while    {adjust(); yylval.pos = EM_tokPos; return WHILE;}
for  	 {adjust(); yylval.pos = EM_tokPos; return FOR;}
to       {adjust(); yylval.pos = EM_tokPos; return TO;}
do       {adjust(); yylval.pos = EM_tokPos; return DO;}
let      {adjust(); yylval.pos = EM_tokPos; return LET;}
in       {adjust(); yylval.pos = EM_tokPos; return IN;}
end      {adjust(); yylval.pos = EM_tokPos; return END;}
of       {adjust(); yylval.pos = EM_tokPos; return OF;}
break    {adjust(); yylval.pos = EM_tokPos; return BREAK;}
nil      {adjust(); yylval.pos = EM_tokPos; return NIL;}
function {adjust(); yylval.pos = EM_tokPos; return FUNCTION;}
var      {adjust(); yylval.pos = EM_tokPos; return VAR;}
type     {adjust(); yylval.pos = EM_tokPos; return TYPE;}

[0-9]+	 {adjust(); yylval.ival=atoi(yytext); return INT;}

\"						{str_len=yyleng; str_p = str; BEGIN(STR);}
<STR>\"  				{str_len+=yyleng; EM_tokPos=charPos; charPos+=str_len; *str_p='\0'; yylval.sval=String(str); BEGIN(INITIAL); return STRING;}
<STR>\\	 				{str_len+=yyleng; BEGIN(ESC);}
<ESC>n					{str_len+=yyleng; *str_p++='\n'; BEGIN(STR);}
<ESC>t					{str_len+=yyleng; *str_p++='\t'; BEGIN(STR);}
<ESC>\^c				{str_len+=yyleng; *str_p++='\^c'; BEGIN(STR);}
<ESC>[0-9]{3}			{EM_tokPos=charPos+str_len; int d; sscanf(yytext, "%d", &d); if(d > 255) EM_error(EM_tokPos, "ASCII code out of bound"); str_len+=yyleng; *str_p++=d; BEGIN(STR);}
<ESC>\"					{str_len+=yyleng; *str_p++='\"'; BEGIN(STR);}
<ESC>\\					{str_len+=yyleng; *str_p++='\\'; BEGIN(STR);}
<ESC>\n(" "|\t|\r)*\\  	{EM_tokPos=charPos+str_len; EM_newline(); str_len+=yyleng; BEGIN(STR);}
<ESC>(" "|\t|\r)+\\ 	{str_len+=yyleng; BEGIN(STR);}
<ESC>(" "|\t|\r)*		{str_len+=yyleng; BEGIN(STR);}
<STR>\n					{EM_tokPos=charPos; charPos+=str_len; EM_error(EM_tokPos,"unclosed string"); adjust(); EM_newline(); BEGIN(INITIAL);}
<STR><<EOF>> 			{EM_tokPos=charPos; charPos+=str_len; EM_error(EM_tokPos,"unclosed string"); adjust(); BEGIN(INITIAL); yyterminate();}
<STR>.|" "|\t|\r  		{str_len+=yyleng; *str_p++=yytext[0];} 

[a-zA-Z]([a-zA-Z0-9]|"_")* {adjust(); yylval.sval=String(yytext); return ID;}

"/*"     {adjust(); comment_nest++; BEGIN(COMMENT);} 
<COMMENT>"/*"  {adjust(); comment_nest++;}
<COMMENT><<EOF>> {adjust(); EM_error(EM_tokPos, "unclosed comment"); BEGIN(INITIAL); yyterminate();}
<COMMENT>\n {adjust(); EM_newline();}
<COMMENT>.|" "|\t|\r {adjust();}
<COMMENT>"*/"  {adjust(); comment_nest--; if(comment_nest == 0) BEGIN(INITIAL);}

<<EOF>> {adjust(); yyterminate();}
.	    {adjust(); EM_error(EM_tokPos,"illegal token");}
