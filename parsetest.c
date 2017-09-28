#include <stdio.h>
#include "util.h"
#include "symbol.h"
#include "absyn.h"
#include "temp.h"
#include "tree.h"
#include "frame.h"
#include "semant.h"
#include "parse.h"
#include "prabsyn.h"

extern int yyparse(void);

int main(int argc, char **argv) {
 if (argc!=2) {fprintf(stderr,"usage: a.out filename\n"); exit(1);}
 A_exp exp = parse(argv[1]);
 if(exp) {
 	F_fragList frags = SEM_transProg(exp);
 	FILE *out = fopen("absyn", "w");
	pr_exp(out, exp, 0);
	fclose(out);
	out = fopen("tree", "w");
	Tr_printFragList(out, frags);
	fclose(out);
 }
 return 0;
}
