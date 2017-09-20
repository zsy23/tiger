a.out: parsetest.o temp.o translate.o amd64frame.o prabsyn.o parse.o y.tab.o lex.yy.o semant.o env.o types.o absyn.o symbol.o table.o errormsg.o util.o
	cc -g parsetest.o temp.o amd64frame.o translate.o prabsyn.o parse.o y.tab.o lex.yy.o semant.o env.o types.o absyn.o symbol.o table.o errormsg.o util.o 

parsetest.o: parsetest.c
	cc -g -c parsetest.c

y.tab.o: y.tab.c
	cc -g -c y.tab.c

y.tab.c: tiger.grm
	bison -dvt tiger.grm -o y.tab.c

y.tab.h: y.tab.c
	echo "y.tab.h was created at the same time as y.tab.c"

prabsyn.o: prabsyn.c
	cc -g -c prabsyn.c

parse.o: parse.c
	cc -g -c parse.c

semant.o: semant.c
	cc -g -c semant.c

translate.o: translate.c
	cc -g -c translate.c

temp.o: temp.c
	cc -g -c temp.c

amd64frame.o: amd64frame.c
	cc -g -c amd64frame.c

env.o: env.c
	cc -g -c env.c

types.o: types.c
	cc -g -c types.c

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
	rm -f a.out translate.o util.o temp.o parsetest.o amd64frame.o lex.yy.o errormsg.o y.tab.c y.tab.h y.tab.o prabsyn.o parse.o absyn.o symbol.o table.o y.output types.o env.o semant.o absyn
