#include <stdio.h>
#include <assert.h>
#include "util.h"
#include "symbol.h"
#include "temp.h"
#include "frame.h"
#include "translate.h"

struct Tr_level_ {
	Tr_level parent;
	F_frame frame;
};

struct Tr_access_ {
	Tr_level level;
	F_access access;
};

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

void Tr_printLevel(Tr_level level) {
	printf("Level %s\n", level->frame ? Temp_labelstring(F_name(level->frame)) : "outermost");

	if(level->parent)
		printf("parent: Level %s\n", level->parent->frame ? Temp_labelstring(F_name(level->parent->frame)) : "outermost");
	if(level->frame)
		F_printFrame(level->frame);
}

void Tr_printAccess(Tr_access access) {
	printf("Access ");
	F_printAccess(access->access);
	if(access->level)
		printf(" at level %s\n", access->level->frame ? Temp_labelstring(F_name(access->level->frame)) : "outermost");
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
		case Tr_ex:
			T_stm s = T_Cjump(T_eq, e->u.ex, T_Const(0), NULL, NULL);
			patchList trues = PatchList(&s->u.CJUMP.true, NULL);
			patchList falses = PatchList(&s->u.CJUMP.false, NULL);
			return Tr_Cx(trues, falses, s);
		case Tr_cx:
			return e->u.cx;
		case Tr_nx:
			assert(0);
	}
	assert(0);
}
