#include <stdio.h>
#include <assert.h>
#include "util.h"
#include "symbol.h"
#include "temp.h"
#include "frame.h"

#define F_ARG_REG 6

const int F_wordSize = 8;

struct F_frame_ {
    Temp_label name;
    F_accessList formals;
    int locals;
};

struct F_access_ {
    enum {inFrame, inReg} kind;
    union {
        int offset;
        Temp_temp reg;
    } u;
};

static F_frame F_Frame(Temp_label name, F_accessList formals, int locals) {
    F_frame frame = checked_malloc(sizeof(*frame));
    frame->name = name;
    frame->formals = formals;
    frame->locals = locals;

    return frame;
}

static F_access InFrame(int offset) {
    F_access access = checked_malloc(sizeof(*access));
    access->kind = inFrame;
    access->u.offset = offset;

    return access;
}

static F_access InReg(Temp_temp reg) {
    F_access access = checked_malloc(sizeof(*access));
    access->kind = inReg;
    access->u.reg = reg;

    return access;
}

F_accessList F_AccessList(F_access head, F_accessList tail) {
    F_accessList list = checked_malloc(sizeof(*list));
    list->head = head;
    list->tail = tail;

    return list;
}

void F_printAccess(F_access access) {
	if(access->kind == inFrame)
		printf("InFrame(%d)", access->u.offset);
	else if(access->kind == inReg)
		printf("InReg(%d)", Temp_tempnum(access->u.reg));
}

void F_printFrame(F_frame frame) {
	printf("name: %s\n", Temp_labelstring(frame->name));
	printf("foramsl:");
	for(F_accessList formals = frame->formals; formals; formals = formals->tail) {
		printf(" ");
		F_printAccess(formals->head);
	}
	printf("\n");
	printf("locals: %d\n", frame->locals);
}

F_frame F_newFrame(Temp_label name, U_boolList formals) {
    F_frame frame = F_Frame(name, NULL, 0);
    if(formals) {
        int r = 0, f = 2;
        frame->formals = F_AccessList(NULL, NULL);
        if(r < F_ARG_REG && !formals->head) {
            frame->formals->head = InReg(Temp_newtemp());
            r++;
        }
        else {
            frame->formals->head = InFrame(f * F_WORD_SIZE);
            f++;
        }
        F_accessList fList = frame->formals;
        for(formals = formals->tail; formals; formals = formals->tail) {
            F_accessList tmp = F_AccessList(NULL, NULL);
            if(r < F_ARG_REG && !formals->head) {
                tmp->head = InReg(Temp_newtemp());
                r++;
            }
            else {
                tmp->head = InFrame(f * F_WORD_SIZE);
                f++;
            }
            fList->tail = tmp;
            fList = fList->tail;
        }
    }

    return frame;
}

Temp_label F_name(F_frame f) {
    return f->name;
}
F_accessList F_formals(F_frame f) {
    return f->formals;
}

F_access F_allocLocal(F_frame f, bool escape) {
    f->locals++;
    if(escape)
        return InFrame(-f->locals * F_WORD_SIZE);
    else
        return InReg(Temp_newtemp());
}

Temp_temp F_fp = NULL;
Temp_temp F_FP() {
	if(!F_fp)
		F_fp = Temp_newtemp();

	return F_fp;
}

Temp_temp F_rv = NULL;
Temp_temp F_RV() {
	if(!F_rv)
		F_rv = Temp_newtemp();

	return F_rv;
}

T_exp F_Exp(F_access acc, T_exp framePtr) {
	switch(acc->kind) {
		case inFrame:
			return T_Mem(T_Binop(T_plus, framePtr, CONST(acc->u.offset)));
		case inReg:
			return T_Temp(acc->u.reg);
	}
	assert(0);
}

T_stm F_procEntryExit1(F_frame frame, T_stm stm) {
	return stm;
}

F_frag F_StringFrag(Temp_label label, string str) {
	F_frag frag = checked_malloc(sizeof(*frag));
	frag->kind = F_stringFrag;
	frag->u.stringg.label = label;
	frag->u.stringg.str = str;

	return frag;
}

F_frag F_ProcFrag(T_stm body, F_frame frame) {
	F_frag frag = checked_malloc(sizeof(*frag));
	frag->kind = F_procFrag;
	frag->u.proc.body = body;
	frag->u.proc.frame = frame;

	return frag;
}

F_fragList F_FragList(F_frag head, F_fragList tail) {
	F_fragList list = checked_malloc(sizeof(*list));
	list->head = head;
	list->tail = tail;

	return list;
}

int F_AccOffset(F_access access) {
	assert(access->kind == inFrame);

	return access->u.offset;
}

int F_GetWordSize() {
	return F_wordSize;
}

T_exp F_externalCall(string s, T_expList args) {
	return T_Call(T_Name(Temp_namedlabel(s)), args);
}
