#include <stdio.h>
#include "util.h"
#include "symbol.h"
#include "absyn.h"
#include "parse.h"
#include "prabsyn.h"

extern int yyparse(void);

int main(int argc, char **argv) {
 if (argc!=2) {fprintf(stderr,"usage: a.out filename\n"); exit(1);}
 FILE *out = fopen("absyn", "w");
 pr_exp(out, parse(argv[1]), 0);
 fclose(out);
 return 0;
}
