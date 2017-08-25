#include <stdio.h>
#include "util.h"
#include "errormsg.h"
#include "symbol.h"
#include "absyn.h"
#include "types.h"
#include "env.h"
#include "translate.h"
#include "semant.h"

struct expty {
	Tr_exp exp;
	Ty_ty ty;
};

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

static struct expty transVar(S_table venv, S_table tenv, A_var v);
static struct expty transExp(S_table venv, S_table tenv, A_exp e);
static void			transDec(S_table venv, S_table tenv, A_dec d);
static Ty_ty		transTy (S_table tenv, A_ty t);

void SEM_transProg(A_exp exp) {
	S_table tenv = E_base_tenv();
	S_table venv = E_base_venv();
	transExp(tenv, venv, exp);
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
			if(et.ty->kind == Ty_record) {
				for(Ty_fieldList fl = et.ty->u.record; fl != NULL; fl = fl->tail)
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
			if(vet.ty->kind == Ty_array) {
				struct expty eet = transExp(venv, tenv, v->u.subscript.exp);
				if(eet.ty->kind == Ty_int)
					return expTy(NULL, actual_ty(vet.ty->u.array));
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
		default: {
			EM_error(v->pos, "unknown var type");
			return expTy(NULL, Ty_Void());
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
			E_enventry ee = S_look(venv, e->u.call.func);
			if(ee && ee->kind == E_funEntry) {
				A_expList el = e->u.call.args;
				Ty_tyList tl = ee->u.fun.formals;
				for(; el && tl; el = el->tail, tl = tl->tail) {
					A_exp tmp_e = el->head;
					Ty_ty tmp_t = tl->head;
					if(tmp_e && tmp_t) {
						struct expty et = transExp(venv, tenv, tmp_e);
						if(et.ty != tmp_t) {
							EM_error(tmp_e->pos, "function argument type does not match");
							return expTy(NULL, Ty_Void());
						}
					}
					else {
						EM_error(e->pos, "list head should not be null");
						return expTy(NULL, Ty_Void());
					}
				}
				if(el || tl) {
					EM_error(e->pos, "function argument number does not match");
					return expTy(NULL, Ty_Void());
				}

				return expTy(NULL, actual_ty(ee->u.fun.result));
			}
			else {
				EM_error(e->pos, "undefined function %s", S_name(e->u.call.func));
				return expTy(NULL, Ty_Void());
			}
		}
		case A_opExp: {
			struct expty left = transExp(venv, tenv, e->u.op.left);
			struct expty right = transExp(venv, tenv, e->u.op.right);

			if(e->u.op.oper == A_plusOp  ||
			   e->u.op.oper == A_minusOp ||
			   e->u.op.oper == A_timesOp ||
			   e->u.op.oper == A_divideOp) {
				if(left.ty != Ty_Int() || right.ty != Ty_Int()) {
					EM_error(e->pos, "arithmetic operation's arguments must be int");
					return expTy(NULL, Ty_Void());
				}
			}
			else {
				if(!(left.ty == Ty_Int() && right.ty == Ty_Int() ||
					 left.ty == Ty_String() && right.ty == Ty_String())) {
					if(e->u.op.oper == A_eqOp || e->u.op.oper == A_neqOp) {
						if(!(left.ty->kind == Ty_record && right.ty->kind == Ty_record ||
							 left.ty->kind == Ty_array && right.ty->kind == Ty_array)) {
							EM_error(e->pos, "Comparison arguments' type does not match");
							return expTy(NULL, Ty_Void());
						}
					}
					else {
						EM_error(e->pos, "Comparison arguments' type does not match");
						return expTy(NULL, Ty_Void());
					}
				}
			}

			return expTy(NULL, Ty_Int());
		}
		case A_recordExp: {
			Ty_ty ty = S_look(tenv, e->u.record.typ);
			if(ty && ty->kind == Ty_record) {
				if(e->u.record.fields) {
					for(A_efieldList efl = e->u.record.fields; efl; efl = efl->tail) {
						A_efield ef = efl->head;
						if(!ef) {
							EM_error(e->pos, "list head should not be null");
							return expTy(NULL, Ty_Void());
						}
						Ty_fieldList fl;
						for(fl = ty->u.record; fl; fl = fl->tail) {
							Ty_field f = fl->head;
							if(!f) {
								EM_error(e->pos, "list head should not be null");
								return expTy(NULL, Ty_Void());
							}
							if(ef->name == f->name) {
								struct expty et = transExp(venv, tenv, ef->exp);
								if(f->ty != et.ty) {
									EM_error(e->pos, "field %s's type does not match", S_name(ef->name));
									return expTy(NULL, Ty_Void());
								}
								break;
							}
						}
						if(!fl) {
							EM_error(e->pos, "record does not have field %s", S_name(ef->name));
							return expTy(NULL, Ty_Void());
						}
					}
					return expTy(NULL, ty);
				}
				return expTy(NULL, Ty_Nil());
			}
			else {
				EM_error(e->pos, "undefined record type %s", S_name(e->u.record.typ));
				return expTy(NULL, Ty_Void());
			}
		}
		case A_seqExp: {
			if(e->u.seq->head == NULL)
				return expTy(NULL, Ty_Void());
			else {
				A_expList el;
				for(el = e->u.seq; el->tail; el = el->tail);
				return transExp(venv, tenv, el->head);
			}
		}
		case A_assignExp: {
			struct expty vet = transVar(venv, tenv, e->u.assign.var);
			struct expty eet = transExp(venv, tenv, e->u.assign.exp);
			if(vet.ty != eet.ty) {
				EM_error(e->pos, "two sides's type of assignment does not match");
				return expTy(NULL, Ty_Void());
			}
			return expTy(NULL, Ty_Void());
		}
		case A_ifExp: {
			struct expty tet = transExp(venv, tenv, e->u.iff.test);
			if(tet.ty != Ty_Int()) {
				EM_error(e->pos, "if's test must be int");
				return expTy(NULL, Ty_Void());
			}
			tet = transExp(venv, tenv, e->u.iff.then);
			if(e->u.iff.elsee) {
				struct expty eet = transExp(venv, tenv, e->u.iff.elsee);
				if(tet.ty != eet.ty) {
					EM_error(e->pos, "if-then-else then and else must produce the same type");
					return expTy(NULL, Ty_Void());
				}
			}
			else if(!e->u.iff.elsee && tet.ty != Ty_Void()) {
				EM_error(e->pos, "if-then's then must produce no value");
				return expTy(NULL, Ty_Void());
			}
			
			return expTy(NULL, Ty_Void());
		}
		case A_whileExp: {
			struct expty et = transExp(venv, tenv, e->u.whilee.test);
			if(et.ty != Ty_Int()) {
				EM_error(e->pos, "while's test must be int");
				return expTy(NULL, Ty_Void());
			}
			et = transExp(venv, tenv, e->u.whilee.body);
			if(et.ty != Ty_Void()) {
				EM_error(e->pos, "while's body must produce no value");
				return expTy(NULL, Ty_Void());
			}

			return expTy(NULL, Ty_Void());
		}
		case A_forExp: {
			S_beginScope(venv);
			S_enter(venv, e->u.forr.var, E_VarEntry(Ty_Int()));
			struct expty et = transExp(venv, tenv, e->u.forr.lo);
			if(et.ty != Ty_Int()) {
				EM_error(e->pos, "for's lo must be int");
				return expTy(NULL, Ty_Void());
			}
			et = transExp(venv, tenv, e->u.forr.hi);
			if(et.ty != Ty_Int()) {
				EM_error(e->pos, "for's hi must be int");
				return expTy(NULL, Ty_Void());
			} 
			et = transExp(venv, tenv, e->u.forr.body);
			if(et.ty != Ty_Void()) {
				EM_error(e->pos, "for's body must produce no value");
				return expTy(NULL, Ty_Void());
			} 
			S_endScope(venv);
		}
		case A_breakExp: {
			// TODO
		}
		case A_letExp: {
			S_beginScope(venv);
			S_beginScope(tenv);
			for(A_decList dl = e->u.let.decs; dl; dl = dl->tail)
				transDec(venv, tenv, dl->head);
			struct expty et = transExp(venv, tenv, e->u.let.body);
			S_endScope(tenv);
			S_endScope(venv);
			return et;
		}
		case A_arrayExp: {
			Ty_ty ty = S_look(tenv, e->u.array.typ);
			if(ty && ty->kind == Ty_array) {
				struct expty et = transExp(venv, tenv, e->u.array.size);
				if(et.ty != Ty_Int()) {
					EM_error(e->pos, "array's size must be int");
					return expTy(NULL, Ty_Void());
				}
				et = transExp(venv, tenv, e->u.array.init);
				if(ty->u.array != et.ty) {
					EM_error(e->pos, "array's init type does not match");
					return expTy(NULL, Ty_Void());
				}
				return expTy(NULL, ty);
			}
			else {
				EM_error(e->pos, "undefined array type %s", S_name(e->u.array.typ));
				return expTy(NULL, Ty_Void());
			}
		}
		default: {
			EM_error(e->pos, "unknown exp type");
			return expTy(NULL, Ty_Void());
		}
	}
}

static void transDec(S_table venv, S_table tenv, A_dec d) {
	switch(d->kind) {
		case A_typeDec: {
			for(A_nametyList nl = d->u.type; nl; nl = nl->tail) {
				A_namety n = nl->head;
				S_enter(tenv, n->name, transTy(tenv, n->ty));	
			}
		}
		break;
		case A_varDec: {
			struct expty et = transExp(venv, tenv, d->u.var.init);
			if(d->u.var.typ) {
				Ty_ty ty = S_look(tenv, d->u.var.typ);
				if(ty != et.ty) {
					EM_error(d->pos, "variable declaration's type does not match init");
					break;
				}
			}
			S_enter(venv, d->u.var.var, E_VarEntry(et.ty));
		}
		break;
		case A_functionDec: {
			for(A_fundecList fl = d->u.function; fl; fl = fl->tail) {
				A_fundec f = fl->head;
				struct expty et = transExp(venv, tenv, f->body);
				if(f->result) {
					Ty_ty ty = S_look(tenv, f->result);
					if(ty != et.ty) {
						EM_error(d->pos, "function declaration result's type does not match body");
						break;
					}
				}
				else {
					if(et.ty != Ty_Void()) {
						EM_error(d->pos, "procedure declaration' body must produce no value");
						break;
					}
				}
				Ty_tyList tmp_tl, tl = Ty_TyList(NULL, NULL);
				A_fieldList l;
				for(tmp_tl = tl, l = f->params; l; l = l->tail) {
					Ty_ty ty = S_look(tenv, l->head->typ);
					if(!ty) {
						EM_error(l->head->pos, "undefined type %s", S_name(l->head->typ));
						break;
					}
					tmp_tl->head = ty;
					if(l->tail) {
						tmp_tl->tail = Ty_TyList(NULL, NULL);
						tmp_tl = tmp_tl->tail;
					}
				}
				if(!l)
					break;
				S_enter(venv, f->name, E_FunEntry(tl, et.ty));
			}
		}
		break;
		default:
		EM_error(d->pos, "unknown declaration type");
		break;
	}
}

static Ty_ty transTy(S_table tenv, A_ty t) {
	switch(t->kind) {
		case A_nameTy: {
			Ty_ty ty = S_look(tenv, t->u.name);
			if(ty)
				return actual_ty(ty);
			else {
				EM_error(t->pos, "undefined type %s", S_name(t->u.name));
				return Ty_Void();
			}
		}
		case A_recordTy: {
			Ty_fieldList tfl = Ty_FieldList(NULL, NULL);
			A_fieldList afl;
			Ty_fieldList p_tfl;
			for(afl = t->u.record, p_tfl = tfl; afl != NULL; afl = afl->tail) {
				Ty_ty ty = S_look(tenv, afl->head->typ);
				if(!ty) {
					EM_error(afl->head->pos, "undefined type %s", S_name(afl->head->typ));
					return Ty_Void();
				}
				p_tfl->head = Ty_Field(afl->head->name, ty);
				if(afl->tail)
					p_tfl->tail = Ty_FieldList(NULL, NULL);
			}
			return Ty_Record(tfl);
		}
		case A_arrayTy: {
			Ty_ty ty = S_look(tenv, t->u.array);
			if(ty)
				return Ty_Array(actual_ty(ty));
			else {
				EM_error(t->pos, "undefined type %s", S_name(t->u.array));
				return Ty_Void();
			}
		}
		default: {
			EM_error(t->pos, "unknown ty type");
			return Ty_Void();
		}
	}
}
