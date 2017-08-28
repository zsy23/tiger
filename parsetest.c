#include <stdio.h>
#include "util.h"
#include "symbol.h"
#include "absyn.h"
#include "semant.h"
#include "parse.h"
#include "prabsyn.h"

extern int yyparse(void);

int main(int argc, char **argv) {
 if (argc!=2) {fprintf(stderr,"usage: a.out filename\n"); exit(1);}
 A_exp exp = parse(argv[1]);
 if(exp) {
 	FILE *out = fopen("absyn", "w");
	 SEM_transProg(exp);
	 pr_exp(out, exp, 0);
	 fclose(out);
 }
 return 0;
}
