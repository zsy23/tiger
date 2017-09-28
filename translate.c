#include <stdio.h>
#include <assert.h>
#include "util.h"
#include "symbol.h"
#include "temp.h"
#include "tree.h"
#include "printtree.h"
#include "frame.h"
#include "absyn.h"
#include "translate.h"

struct Tr_level_ {
	Tr_level parent;
	F_frame frame;
};

struct Tr_access_ {
	Tr_level level;
	F_access access;
};

Tr_expList Tr_ExpList(Tr_exp head, Tr_expList tail) {
	Tr_expList list = checked_malloc(sizeof(*list));
	list->head = head;
	list->tail = tail;

	return list;
}

static Tr_level Tr_Level(Tr_level parent, F_frame frame) {
    Tr_level level = checked_malloc(sizeof(*level));
    level->parent = parent;
    level->frame = frame;

    return level;
}

static Tr_access Tr_Access(Tr_level level, F_access access) {
    Tr_access tr_access = checked_malloc(sizeof(*tr_access));
    tr_access->level = level;
    tr_access->access = access;

    return tr_access;
}

Tr_accessList Tr_AccessList(Tr_access head, Tr_accessList tail) {
	Tr_accessList list = checked_malloc(sizeof(*list));	
	list->head = head;
	list->tail = tail;

	return list;
}

void Tr_printLevel(FILE *out, Tr_level level) {
	fprintf(out, "Level %s\n", level->frame ? Temp_labelstring(F_name(level->frame)) : "outermost");

	if(level->parent)
		fprintf(out, "parent: Level %s\n", level->parent->frame ? Temp_labelstring(F_name(level->parent->frame)) : "outermost");
	if(level->frame)
		F_printFrame(out, level->frame);
}

void Tr_printAccess(FILE *out, Tr_access access) {
	fprintf(out, "Access ");
	F_printAccess(out, access->access);
	if(access->level)
		fprintf(out, " at level %s\n", access->level->frame ? Temp_labelstring(F_name(access->level->frame)) : "outermost");
}

static struct Tr_level_ lvzero = {NULL, NULL};
Tr_level Tr_outermost(void) {
	return &lvzero;
}

Tr_level Tr_newLevel(Tr_level parent, Temp_label name, U_boolList formals) {
	return Tr_Level(parent, F_newFrame(name, U_BoolList(TRUE, formals)));
}

Tr_accessList Tr_formals(Tr_level level) {
	Tr_accessList trList = NULL;
	F_accessList fList = F_formals(level->frame);
	if(fList && fList->tail) {
        fList = fList->tail;
        trList = Tr_AccessList(Tr_Access(level, fList->head), NULL);
		Tr_accessList tmp = trList;
		for(fList = fList->tail; fList; fList = fList->tail) {
			Tr_accessList trList = Tr_AccessList(Tr_Access(level, fList->head), NULL);
			tmp->tail = trList;
			tmp = tmp->tail;
		}
	}

	return trList;
}

Tr_access Tr_allocLocal(Tr_level level, bool escape) {
	return Tr_Access(level, F_allocLocal(level->frame, escape));
}

typedef struct patchList_ *patchList;
struct patchList_ {
	Temp_label *head;
	patchList tail;
};

static patchList PatchList(Temp_label *head, patchList tail) {
	patchList list = checked_malloc(sizeof(*list));
	list->head = head;
	list->tail = tail;

	return list;
}

static void doPatch(patchList tList, Temp_label label) {
	for(; tList; tList = tList->tail)
		*(tList->head) = label;
}

static patchList joinPatch(patchList first, patchList second) {
	if(!first)
		return second;
	patchList join = first;
	for(; first->tail; first = first->tail);
	first->tail = second;

	return join;
}

struct Cx {
	patchList trues;
	patchList falses;
	T_stm stm;
};

struct Tr_exp_ {
	enum {Tr_ex, Tr_nx, Tr_cx} kind;
	union {
		T_exp ex;
		T_stm nx;
		struct Cx cx;
	} u;
};

static Tr_exp Tr_Ex(T_exp ex) {
	Tr_exp exp = checked_malloc(sizeof(*exp));
	exp->kind = Tr_ex;
	exp->u.ex = ex;

	return exp;
}

static Tr_exp Tr_Nx(T_stm nx) {
	Tr_exp exp = checked_malloc(sizeof(*exp));
	exp->kind = Tr_nx;
	exp->u.nx = nx;

	return exp;	
}

static Tr_exp Tr_Cx(patchList trues, patchList falses, T_stm stm) {
	Tr_exp exp = checked_malloc(sizeof(*exp));
	exp->kind = Tr_cx;
	exp->u.cx.trues = trues;
	exp->u.cx.falses = falses;
	exp->u.cx.stm = stm;

	return exp;
}

static T_exp unEx(Tr_exp e) {
	switch(e->kind) {
		case Tr_ex:
			return e->u.ex;
		case Tr_cx: {
			Temp_temp r = Temp_newtemp();
			Temp_label t = Temp_newlabel(), f = Temp_newlabel();
			doPatch(e->u.cx.trues, t);
			doPatch(e->u.cx.falses, f);
			return T_Eseq(T_Move(T_Temp(r), T_Const(1)),
						T_Eseq(e->u.cx.stm,
							T_Eseq(T_Label(f),
								T_Eseq(T_Move(T_Temp(r), T_Const(0)),
									T_Eseq(T_Label(t),
										T_Temp(r))))));
		}
		case Tr_nx:
			return T_Eseq(e->u.nx, T_Const(0));
	}
	assert(0);
}

static T_stm unNx(Tr_exp e) {
	switch(e->kind) {
		case Tr_ex:
			return T_Exp(e->u.ex);
		case Tr_cx: 
			return e->u.cx.stm;
		case Tr_nx:
			return e->u.nx;
	}
	assert(0);
}

static struct Cx unCx(Tr_exp e) {
	switch(e->kind) {
		case Tr_ex: {
			T_stm s = T_Cjump(T_eq, e->u.ex, T_Const(0), NULL, NULL);
			patchList trues = PatchList(&s->u.CJUMP.true, NULL);
			patchList falses = PatchList(&s->u.CJUMP.false, NULL);
			return Tr_Cx(trues, falses, s)->u.cx;
		}
		case Tr_cx:
			return e->u.cx;
		case Tr_nx:
			assert(0);
	}
	assert(0);
}

F_fragList Tr_fragList = NULL;

void Tr_procEntryExit(Tr_level level, Tr_exp body, Tr_accessList formals) {
	T_stm stm = F_procEntryExit1(level->frame, unNx(body));
	Tr_fragList = F_FragList(F_ProcFrag(stm, level->frame), Tr_fragList);
}

F_fragList Tr_getResult() {
	return Tr_fragList;
}

Tr_exp Tr_simpleVar(Tr_access access, Tr_level level) {
	T_exp fp = NULL;	
	if(level == access->level)
		fp = T_Temp(F_FP());
	else {
		int offset = F_AccOffset(F_formals(level->frame)->head);
		fp = T_Mem(T_Binop(T_plus, T_Const(offset), T_Temp(F_FP())));
		for(level = level->parent; level != NULL && level != access->level; level = level->parent) {
			offset = F_AccOffset(F_formals(level->frame)->head);
			fp = T_Mem(T_Binop(T_plus, T_Const(offset), fp));
		}
		assert(level != NULL);
	}

	T_exp exp = F_Exp(access->access, fp);

	return Tr_Ex(exp);
}

Tr_exp Tr_fieldVar(Tr_exp var, int offset) {
	T_exp e = T_Binop(T_mul, T_Const(F_GetWordSize()), T_Const(offset));
	T_exp exp = T_Mem(T_Binop(T_plus, e, T_Mem(unEx(var))));

	return Tr_Ex(exp);
}

Tr_exp Tr_subscriptVar(Tr_exp var, Tr_exp index) {
	T_exp e = T_Binop(T_mul, T_Const(F_GetWordSize()), unEx(index));
	T_exp exp = T_Mem(T_Binop(T_plus, e, T_Mem(unEx(var))));

	return Tr_Ex(exp);
}

Tr_exp Tr_nilExp() {
	return Tr_Ex(T_Const(0));
}

Tr_exp Tr_intExp(int i) {
	return Tr_Ex(T_Const(i));
}

Tr_exp Tr_stringExp(string str) {
	Tr_fragList = F_FragList(F_StringFrag(Temp_namedlabel(str), str), Tr_fragList);
	
	return Tr_Ex(T_Name(Temp_namedlabel(str)));
}

Tr_exp Tr_callExp(Temp_label label, Tr_expList args) {
	T_expList list = T_ExpList(T_Temp(F_FP()), NULL);
	for(; args; args = args->tail)
		list = T_ExpList(unEx(args->head), list);

	return Tr_Ex(T_Call(T_Name(label), list));
}

Tr_exp Tr_opExp(A_oper op, Tr_exp left, Tr_exp right) {
	T_binOp bop;
	T_relOp rop;
	switch(op) {
	case A_plusOp:
		bop = T_plus;
	case A_minusOp:
		bop = T_minus;
	case A_timesOp:
		bop = T_mul;
	case A_divideOp:
		bop = T_div;
		return Tr_Ex(T_Binop(bop, unEx(left), unEx(right)));
	case A_eqOp:
		rop = T_eq;
	case A_neqOp:
		rop = T_ne;
	case A_ltOp:
		rop = T_lt;
	case A_leOp:
		rop = T_le;
	case A_gtOp:
		rop = T_gt;
	case A_geOp:
		rop = T_ge;
		T_stm stm = T_Cjump(rop, unEx(left), unEx(right), NULL, NULL);
		patchList trues = PatchList(&stm->u.CJUMP.true, NULL);
		patchList falses = PatchList(&stm->u.CJUMP.false, NULL);
		return Tr_Cx(trues, falses, stm);
	}
	assert(0);
}

Tr_exp Tr_strCmp(A_oper op, Tr_exp left, Tr_exp right) {
	switch(op) {
	case A_eqOp: {
		T_exp r = F_externalCall(String("stringEqual"), T_ExpList(left->u.ex, T_ExpList(right->u.ex, NULL)));
		T_stm stm = T_Cjump(T_eq, r, T_Const(1), NULL, NULL);
		patchList trues = PatchList(&stm->u.CJUMP.true, NULL);
		patchList falses = PatchList(&stm->u.CJUMP.false, NULL);
		return Tr_Cx(trues, falses, stm);
	}
	case A_neqOp: {
		T_exp r = F_externalCall(String("stringEqual"), T_ExpList(left->u.ex, T_ExpList(right->u.ex, NULL)));
		T_stm stm = T_Cjump(T_eq, r, T_Const(0), NULL, NULL);
		patchList trues = PatchList(&stm->u.CJUMP.true, NULL);
		patchList falses = PatchList(&stm->u.CJUMP.false, NULL);
		return Tr_Cx(trues, falses, stm);
	}
	}
}

Tr_exp Tr_recordExp(int n, Tr_expList args) {
	Temp_temp r = Temp_newtemp();
	T_exp p = F_externalCall(String("malloc"), T_ExpList(T_Const(n), NULL));
	T_exp exp = T_Eseq(T_Seq(T_Move(T_Temp(r), p), NULL), T_Temp(r));
	T_stm s = exp->u.ESEQ.stm;
	T_exp pos;
	int offset = 0;
	for(; args && args->tail; args = args->tail, ++offset) {
		pos = T_Binop(T_plus, T_Binop(T_mul, T_Const(offset), T_Const(F_GetWordSize())),T_Temp(r));
		s->u.SEQ.right = T_Seq(T_Move(T_Mem(pos), unEx(args->head)), NULL);
		s = s->u.SEQ.right;
	}
	if(args) {
		pos = T_Binop(T_plus, T_Binop(T_mul, T_Const(offset), T_Const(F_GetWordSize())),T_Temp(r));
		s->u.SEQ.right = T_Move(T_Mem(pos), unEx(args->head));
	}
	else
		exp = T_Eseq(T_Move(T_Temp(r), p), T_Temp(r));

	return Tr_Ex(exp);
}

Tr_exp Tr_seqExp(Tr_expList seq) {
	if(!seq)
		return Tr_Nx(T_Exp(T_Const(0)));

	if(!seq->tail) {
		if(seq->head->kind == Tr_nx)
			return seq->head;
		else
			return Tr_Ex(unEx(seq->head));
	}

	if(!seq->tail->tail) {
		if(seq->tail->head->kind == Tr_nx)
			return Tr_Nx(T_Seq(unNx(seq->head), seq->tail->head->u.nx));
		else
			return Tr_Ex(T_Eseq(unNx(seq->head), unEx(seq->tail->head)));
	}

	T_stm stm = T_Seq(unNx(seq->head), NULL);
	for(seq = seq->tail; seq->tail->tail; seq = seq->tail) {
		stm->u.SEQ.right = T_Seq(unNx(seq->head), NULL);
		stm = stm->u.SEQ.right;
	}
	stm->u.SEQ.right = unNx(seq->head);
	if(seq->tail->head->kind == Tr_nx)
		return Tr_Nx(T_Seq(stm, seq->tail->head->u.nx));
	else
		return Tr_Ex(T_Eseq(stm, unEx(seq->tail->head)));
}

Tr_exp Tr_assignExp(Tr_exp left, Tr_exp right) {
	T_stm stm = T_Move(unEx(left), unEx(right));

	return Tr_Nx(stm);
}

Tr_exp Tr_ifExp(Tr_exp iff, Tr_exp then, Tr_exp elsee) {
	Temp_label t = Temp_newlabel(), f = Temp_newlabel(), join = Temp_newlabel();
	struct Cx if_cx = unCx(iff);
	doPatch(if_cx.trues, t);
	doPatch(if_cx.falses, f);
	if(then->kind == Tr_nx) {
		T_stm stm = T_Seq(if_cx.stm, 
						T_Seq(T_Label(t), 
							T_Seq(then->u.nx, 
								T_Seq(T_Jump(T_Name(join), Temp_LabelList(join, NULL)), 
									T_Seq(T_Label(f), 
										T_Seq(elsee->u.nx, 
											T_Label(join)))))));
		return Tr_Nx(stm);
	}
	else {
		Temp_temp r = Temp_newtemp();
		T_exp exp = T_Eseq(if_cx.stm,
						T_Eseq(T_Label(t),
							T_Eseq(T_Move(T_Temp(r), unEx(then)), 
								T_Eseq(T_Jump(T_Name(join), Temp_LabelList(join, NULL)),
									T_Eseq(T_Label(f), 
										T_Eseq(T_Move(T_Temp(r), unEx(elsee)), 
											T_Eseq(T_Label(join), T_Temp(r))))))));
		return Tr_Ex(exp);
	}
}

Tr_exp Tr_whileExp(Tr_exp test, Tr_exp body, Temp_label done) {
	Temp_label start = Temp_newlabel(), t = Temp_newlabel();
	struct Cx test_cx = unCx(test);
	doPatch(test_cx.trues, t);
	doPatch(test_cx.falses, done);
	T_stm stm = T_Seq(T_Label(start), 
					T_Seq(test_cx.stm, 
						T_Seq(T_Label(t),
							T_Seq(unNx(body),
								T_Seq(T_Jump(T_Name(start), Temp_LabelList(start, NULL)),
									T_Label(done))))));

	return Tr_Nx(stm);
}

Tr_exp Tr_breakExp(Temp_label done) {
	return Tr_Nx(T_Jump(T_Name(done), Temp_LabelList(done, NULL)));
}

Tr_exp Tr_arrayExp(Tr_exp size, Tr_exp init) {
	Temp_temp r = Temp_newtemp();
	T_exp p = F_externalCall(String("initArray"), T_ExpList(unEx(size), T_ExpList(unEx(init), NULL)));
	T_exp exp = T_Eseq(T_Move(T_Temp(r), p), T_Temp(r));

	return Tr_Ex(exp);
}

Tr_exp Tr_noopExp() {
	return Tr_Ex(T_Const(0));
}

Tr_exp Tr_letExp(Tr_expList decs, Tr_exp body) {
	if(body->kind == Tr_nx) {
		T_stm stm = T_Seq(unNx(decs->head), NULL);
		T_stm p = stm;
		for(decs = decs->tail; decs; decs = decs->tail) {
			p->u.SEQ.right = T_Seq(unNx(decs->head), NULL);
			p = p->u.SEQ.right;
		}
		p->u.SEQ.right = body->u.nx;

		return Tr_Nx(stm);
	}
	else {
		T_exp exp = T_Eseq(unNx(decs->head), NULL);
		T_exp p = exp;
		for(decs = decs->tail; decs; decs = decs->tail) {
			p->u.ESEQ.exp = T_Eseq(unNx(decs->head), NULL);
			p = p->u.ESEQ.exp;
		}
		p->u.ESEQ.exp = unEx(body);

		return Tr_Ex(exp);
	}
}

Tr_exp Tr_forExp(Tr_exp lo, Tr_exp hi, Tr_exp body, Temp_label done) {
	Temp_temp i = Temp_newtemp(), limit = Temp_newtemp();
	Tr_exp test = Tr_opExp(A_leOp, Tr_Ex(T_Temp(i)), Tr_Ex(T_Temp(limit)));
	T_stm wbody = T_Seq(unNx(body), unNx(Tr_assignExp(Tr_Ex(T_Temp(i)), Tr_Ex(T_Binop(T_plus, T_Temp(i), T_Const(1))))));
	T_stm stm = T_Seq(T_Move(T_Temp(i), unEx(lo)),
					T_Seq(T_Move(T_Temp(limit), unEx(hi)), 
						unNx(Tr_whileExp(test, Tr_Nx(wbody), done))));

	return Tr_Nx(stm);
}

void Tr_printFragList(FILE *out, F_fragList frags) {
	for(int i = 0; frags; frags = frags->tail) {
		F_frag frag = frags->head;
		fprintf(out, "frag %d:\n", i);
		if(frag->kind == F_stringFrag) {
			fprintf(out, "lable %s: %s\n", Temp_labelstring(frag->u.stringg.label), frag->u.stringg.str);
		}
		else {
			if(frag->u.proc.frame)
				F_printFrame(out, frag->u.proc.frame);
			else
				fprintf(out, "outermost frame\n");
			printStmList(out, T_StmList(frag->u.proc.body, NULL));
		}
	}
}
