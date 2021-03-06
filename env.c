#include <stdio.h>
#include "util.h"
#include "symbol.h"
#include "types.h"
#include "absyn.h"
#include "temp.h"
#include "tree.h"
#include "frame.h"
#include "translate.h"
#include "env.h"

S_table E_base_tenv(void) {
	S_table t = S_empty();
	S_enter(t, S_Symbol(String("nil")), Ty_Nil());
	S_enter(t, S_Symbol(String("int")), Ty_Int());
	S_enter(t, S_Symbol(String("string")), Ty_String());

	return t;
}

S_table E_base_venv(void) {
	S_table t = S_empty();

	// print
	E_enventry e = E_FunEntry(Tr_outermost(), Temp_newlabel(), Ty_TyList(Ty_String(), NULL), Ty_Void());
	S_enter(t, S_Symbol(String("print")), e);
	
	// flush
	e = E_FunEntry(Tr_outermost(), Temp_newlabel(), NULL, Ty_Void());
	S_enter(t, S_Symbol(String("flush")), e);

	// getchar
	e = E_FunEntry(Tr_outermost(), Temp_newlabel(), NULL, Ty_String());
	S_enter(t, S_Symbol(String("getchar")), e);

	// ord
	e = E_FunEntry(Tr_outermost(), Temp_newlabel(), Ty_TyList(Ty_String(), NULL), Ty_Int());
	S_enter(t, S_Symbol(String("ord")), e);

	// chr
	e = E_FunEntry(Tr_outermost(), Temp_newlabel(), Ty_TyList(Ty_Int(), NULL), Ty_String());
	S_enter(t, S_Symbol(String("chr")), e);
	
	// size
	e = E_FunEntry(Tr_outermost(), Temp_newlabel(), Ty_TyList(Ty_String(), NULL), Ty_Int());
	S_enter(t, S_Symbol(String("size")), e);

	// substring
	Ty_tyList tl = Ty_TyList(Ty_String(), Ty_TyList(Ty_Int(), Ty_TyList(Ty_Int(), NULL)));
	e = E_FunEntry(Tr_outermost(), Temp_newlabel(), tl, Ty_String());
	S_enter(t, S_Symbol(String("substring")), e);

	// concat
	tl = Ty_TyList(Ty_String(), Ty_TyList(Ty_String(), NULL));
	e = E_FunEntry(Tr_outermost(), Temp_newlabel(), tl, Ty_String());
	S_enter(t, S_Symbol(String("concat")), e);

	// not
	e = E_FunEntry(Tr_outermost(), Temp_newlabel(), Ty_TyList(Ty_Int(), NULL), Ty_Int());
	S_enter(t, S_Symbol(String("not")), e);

	// exit
	e = E_FunEntry(Tr_outermost(), Temp_newlabel(), Ty_TyList(Ty_Int(), NULL), Ty_Void());
	S_enter(t, S_Symbol(String("exit")), e);

	return t;
}

E_enventry E_VarEntry(Tr_access access, Ty_ty ty) {
	E_enventry e = checked_malloc(sizeof(*e));
	e->kind = E_varEntry;
	e->u.var.access = access;
	e->u.var.ty = ty;

	return e;
}

E_enventry E_FunEntry(Tr_level level, Temp_label label, Ty_tyList formals, Ty_ty result) {
	E_enventry e = checked_malloc(sizeof(*e));
	e->kind = E_funEntry;
	e->u.fun.level = level;
	e->u.fun.label = label;
	e->u.fun.formals = formals;
	e->u.fun.result = result;

	return e;
}
