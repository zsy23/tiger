typedef struct Tr_exp_ *Tr_exp;

typedef struct Tr_expList_ *Tr_expList;
struct Tr_expList_ {
	Tr_exp head;
	Tr_expList tail;
};
Tr_expList Tr_ExpList(Tr_exp head, Tr_expList tail);

typedef struct Tr_level_ *Tr_level;
typedef struct Tr_access_ *Tr_access;

typedef struct Tr_accessList_ *Tr_accessList;
struct Tr_accessList_ {
	Tr_access head;
	Tr_accessList tail;
};
Tr_accessList Tr_AccessList(Tr_access head, Tr_accessList tail);

void Tr_printLevel(FILE *out, Tr_level level);
void Tr_printAccess(FILE *out, Tr_access access);

Tr_level Tr_outermost(void);
Tr_level Tr_newLevel(Tr_level parent, Temp_label name, U_boolList formals);
Tr_accessList Tr_formals(Tr_level level);
Tr_access Tr_allocLocal(Tr_level level, bool escape);

void Tr_procEntryExit(Tr_level level, Tr_exp body, Tr_accessList formals);
F_fragList Tr_getResult();
void Tr_printFragList(FILE *out, F_fragList frags);

Tr_exp Tr_simpleVar(Tr_access access, Tr_level level);
Tr_exp Tr_fieldVar(Tr_exp exp, int offset);
Tr_exp Tr_subscriptVar(Tr_exp var, Tr_exp index);
Tr_exp Tr_nilExp();
Tr_exp Tr_intExp(int i);
Tr_exp Tr_stringExp(string str);
Tr_exp Tr_callExp(Temp_label label, Tr_expList args);
Tr_exp Tr_opExp(A_oper op, Tr_exp left, Tr_exp right);
Tr_exp Tr_strCmp(A_oper op, Tr_exp left, Tr_exp right);
Tr_exp Tr_recordExp(int n, Tr_expList args);
Tr_exp Tr_seqExp(Tr_expList seq);
Tr_exp Tr_assignExp(Tr_exp left, Tr_exp right);
Tr_exp Tr_ifExp(Tr_exp iff, Tr_exp then, Tr_exp elsee, int res);
Tr_exp Tr_whileExp(Tr_exp test, Tr_exp body, Temp_label done);
Tr_exp Tr_breakExp(Temp_label done);
Tr_exp Tr_arrayExp(Tr_exp size, Tr_exp init);
Tr_exp Tr_noopExp();
Tr_exp Tr_letExp(Tr_expList decs, Tr_exp body);
Tr_exp Tr_forExp(Tr_exp lo, Tr_exp hi, Tr_exp body, Temp_label done);
