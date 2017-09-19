#include "frame.h"

#define F_WORD_SIZE 8
#define F_ARG_REG 6

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
