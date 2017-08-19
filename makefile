a.out: parsetest.o  prabsyn.o parse.o y.tab.o lex.yy.o absyn.o symbol.o table.o errormsg.o util.o
	cc -g parsetest.o prabsyn.o parse.o y.tab.o lex.yy.o absyn.o symbol.o table.o errormsg.o util.o 

parsetest.o: parsetest.c
	cc -g -c parsetest.c

y.tab.o: y.tab.c
	cc -g -c y.tab.c

y.tab.c: tiger.grm
	bison -dv tiger.grm -o y.tab.c

y.tab.h: y.tab.c
	echo "y.tab.h was created at the same time as y.tab.c"

prabsyn.o: prabsyn.c
	cc -g -c prabsyn.c

parse.o: parse.c
	cc -g -c parse.c

absyn.o: absyn.c
	cc -g -c absyn.c

symbol.o: symbol.c
	cc -g -c symbol.c

table.o: table.c
	cc -g -c table.c

errormsg.o: errormsg.c
	cc -g -c errormsg.c

lex.yy.o: lex.yy.c
	cc -g -c lex.yy.c

util.o: util.c
	cc -g -c util.c

clean: 
	rm -f a.out util.o parsetest.o lex.yy.o errormsg.o y.tab.c y.tab.h y.tab.o prabsyn.o parse.o absyn.o symbol.o table.o y.output
