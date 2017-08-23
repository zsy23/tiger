#include "absyn.h"
#include "semant.h"

static struct expty {
	Tr_exp exp;
	Ty_ty ty;
}

static struct expty expTy(Tr_exp exp, Ty_ty ty) {
	struct expty e;
	e.exp = exp;
	e.ty = ty;

	return e;
}

static Ty_ty actual_ty(Ty_ty ty) {
	Ty_ty t;
	while(t->kind == Ty_name)
		t = t->u.name.ty;

	return t;
}

static struct expty transVar(S_table venv, S_table tenv, A_var v) {
	switch(v->kind) {
		case A_simpleVar: {
			E_enventry e = S_look(venv, v->u.simple);
			if(e && e->kind == E_varEntry)
				return expTy(NULL, actual_ty(e->u.var.ty));
			else {
				EM_error(v->pos, "undefined variable %s", S_name(v->u.simple));
				return expTy(NULL, Ty_Void());
			}
		}
		case A_fieldVar: {
			struct expty et = transVar(venv, tenv, v->u.field.var);
			if(et->ty->kind == Ty_record) {
				for(Ty_fieldList fl = et->ty->u.record; fl != NULL; fl = fl->tail)
					if(fl->head->name == v->u.field.sym)
						return expTy(NULL, actual_ty(fl->head->ty));
				EM_error(v->pos, "record do not have field %s", S_name(v->u.field.sym));
				return expTy(NULL, Ty_Void());
			}
			else {
				EM_error(v->pos, "only record type has field");
				return expTy(NULL, Ty_Void());
			}
		}
		case A_subscriptVar: {
			struct expty vet = transVar(venv, tenv, v->u.subscript.var);
			if(vet->ty->kind == Ty_array) {
				struct expty eet = transExp(venv, tenv, v->u.subscript.exp);
				if(eet->ty->kind == Ty_int)
					return expTy(NULL, actual_ty(vet->ty->u.array));
				else {
					EM_error(v->pos, "subscript must be int type");
					return expTy(NULL, Ty_Void());
				}
			}
			else {
				EM_error(v->pos, "only array type has subscript");
				return expTy(NULL, Ty_Void());
			}
		}
	}
}

static struct expty transExp(S_table venv, S_table tenv, A_exp e) {
	switch(e->kind) {
		case A_varExp: {
			return transVar(venv, tenv, e->u.var);
		}
		case A_nilExp: {
			return expTy(NULL, Ty_Nil());
		}
		case A_intExp: {
			return expTy(NULL, Ty_Int());
		}
		case A_stringExp: {
			return expTy(NULL, Ty_String());
		}
		case A_callExp: {
			E_enventry e = S_look(venv, e->u.call.func);
			if(e && e->kind == E_funEntry)
				return expTy(NULL, actual_ty(e->u.fun.result));
			else {
				EM_error(v->pos, "undefined function %s", S_name(e->u.call.func));
				return expTy(NULL, Ty_Void());
			}
		}
		case A_opExp: {
			return expTy(NULL, Ty_Int());
		}
		case A_recordExp: {
			Ty_ty ty = S_look(tenv, e->u.record.typ);
			if(ty && ty->kind == Ty_record)
				return expTy(NULL, ty);
			else {
				EM_error(e->pos, "undefined record type %s", S_name(e->u.record.typ));
				return expTy(NULL, Ty_Void());
			}
		}
		case A_seqExp: {
			if(e->u.seq->head == NULL)
				return expTy(NULL, Ty_Void);
			else {
				A_expList el;
				for(el = e->u.seq; el->tail != NULL; el = el->tail);
				return transExp(venv, tenv, el->head);
			}
		}
		case A_assignExp: {
			
		}
	}
}

static void transDec(S_table venv, S_table tenv, A_dec d) {
	switch(d->kind) {
	}
}

static struct Ty_ty transTy(S_table tenv, A_ty t) {
	switch(t->kind) {
		case A_nameTy: {
			Ty_ty ty = S_look(tenv, t->u.name);
			if(ty)
				return expTy(NULL, actual_ty(ty));
			else {
				EM_error(t->pos, "undefined type %s", S_name(t->u.name));
				return expTy(NULL, Ty_Void());
			}
		}
		case A_recordTy: {
			Ty_fieldList tfl = Ty_FieldList(NULL, NULL);
			for(A_fieldList afl = t->u.record, Ty_fieldList p_tfl = tfl; afl != NULL; afl = afl->tail) {
				Ty_ty ty = S_look(tenv, afl->head->typ);
				if(!ty) {
					EM_error(afl->head->pos, "undefined type %s", S_name(afl->head->typ));
					return expTy(NULL, Ty_Void());
				}
				p_tfl->head = Ty_Field(afl->head->name, ty);
				if(afl->tail)
					p_tfl->tail = Ty_FieldList(NULL, NULL);
			}
			return expTy(NULL, Ty_Record(tfl));
		}
		case A_arrayTy: {
			Ty_ty ty = S_look(tenv, t->u.array);
			if(ty)
				return expTy(NULL, Ty_Array(actual_ty(ty)));
			else {
				EM_error(t->pos, "undefined type %s", S_name(t->u.array));
				return expTy(NULL, Ty_Void());
			}
		}
	}
}
