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

Tr_accessList Tr_AccessList(Tr_acess head, Tr_accessList tail) {
	Tr_accessList list = checked_malloc(sizeof(*list));	
	list->head = head;
	list->tail = tail;

	return list;
}

void Tr_printLevel(Tr_level level) {
	printf("Level %s\n", level->frame ? Temp_labelstring(level->frame->name) : "outermost");

	if(level->parent)
		printf("parent: Level %s\n", level->parnet->frame ? Temp_labelstring(level->parent->frame->name) : "outermost");
	if(level->frame)
		F_printFrame(level->frame);
}

void Tr_printAccess(Tr_access access) {
	printf("Access ");
	F_printAccess(access->access);
	if(access->level)
		printf(" at level %s\n", access->level->frame ? Temp_labelstring(access->level->frame->name) : "outermost");
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
		Tr_accesslist tmp = trList;
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
