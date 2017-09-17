#include "translate.h"

struct Tr_level_ {
	Tr_level parent;
	F_frame frame;
};

struct Tr_access_ {
	Tr_level level;
	F_access access;
};

Tr_accessList Tr_AccessList(Tr_acess head, Tr_accessList tail) {
	Tr_accessList list = checked_malloc(sizeof(*list));	
	list->head = head;
	list->tail = tail;

	return list;
}

static struct Tr_level_ lvzero = {NULL, NULL};
Tr_level Tr_outermost(void) {
	return &lvzero;
}

Tr_level Tr_newLevel(Tr_level parent, Temp_label name, U_boolList formals) {
	Tr_level level = checked_malloc(sizeof(*level));
	level->parent = parent;
	level->frame = F_newFrame(name, formals);

	return level;
}

Tr_accessList Tr_formals(Tr_level level) {
	Tr_accessList trList = checked_malloc(sizeof(*trList));
	trList->head = NULL;
	trList->tail = NULL;
	F_accessList fList = F_formals(level->frame);
	if(fList) {
		Tr_accesslist tmp = trList;
		trList->head->level = level;
		trList->head->access = fList->head;
		for(fList = fList->tail; fList; fList = fList->tail) {
			Tr_accessList trList = checked_malloc(sizeof(*trList));
			trList->head->level = level;
			trList->head->access = fList->head;
			trList->tail = NULL;
			tmp->tail = trList;
			tmp = tmp->tail;
		}
	}

	return trList;
}

Tr_access Tr_allocLocal(Tr_level level, bool escape) {
	Tr_access access = checked_malloc(sizeof(*access));
	access->level = level;
	access->access = F_allocLocal(level->frame, escape);

	return access;
}
