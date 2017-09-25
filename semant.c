#include <stdio.h>
#include "util.h"
#include "errormsg.h"
#include "symbol.h"
#include "absyn.h"
#include "types.h"
#include "temp.h"
#include "translate.h"
#include "env.h"
#include "semant.h"

//#define TEST_ACTIVATION_RECORDS

static int for_while = 0;

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
	Ty_ty t = ty;
	while(t && t->kind == Ty_name)
		t = actual_ty(t->u.name.ty);

	return t;
}

static struct expty transVar(Tr_level level, S_table venv, S_table tenv, A_var v);
static struct expty transExp(Tr_level level, S_table venv, S_table tenv, A_exp e);
static void			transDec(Tr_level level, S_table venv, S_table tenv, A_dec d);
static Ty_ty		transTy (S_table tenv, A_ty t);

#ifdef TEST_ACTIVATION_RECORDS
static void show_activation_records(S_symbol sym, E_enventry entry) {
	printf("%s:\n", S_name(sym));
	if(entry->kind == E_varEntry)
		Tr_printAccess(entry->u.var.access);
	else if(entry->kind == E_funEntry)
		Tr_printLevel(entry->u.fun.level);
}
#endif

F_fragList SEM_transProg(A_exp exp) {
	S_table tenv = E_base_tenv();
	S_table venv = E_base_venv();
	struct expty e = transExp(Tr_outermost(), venv, tenv, exp);
#ifdef TEST_ACTIVATION_RECORDS
	printf("====== Activation record ======\n");
	S_dump(venv, show_activation_records);
	printf("===============================\n");
#endif
	if(e.exp == NULL)
		return NULL;
	Tr_procEntryExit(Tr_outermost(), e.exp, NULL);

	return Tr_getResult();
}

static struct expty transVar(Tr_level level, S_table venv, S_table tenv, A_var v) {
	switch(v->kind) {
		case A_simpleVar: {
			E_enventry e = S_look(venv, v->u.simple);
			if(e && e->kind == E_varEntry) {
				Tr_exp exp = Tr_simpleVar(e->u.var.access, level);
				return expTy(exp, e->u.var.ty);
			}
			EM_error(v->pos, "undefined variable %s", S_name(v->u.simple));
			return expTy(NULL, Ty_Void());
		}
		case A_fieldVar: {
			struct expty et = transVar(level, venv, tenv, v->u.field.var);
			if(et.ty->kind == Ty_record) {
				int offset = 0;
				for(Ty_fieldList fl = et.ty->u.record; fl != NULL; fl = fl->tail, ++offset)
					if(fl->head->name == v->u.field.sym) {
						Ty_ty ty = actual_ty(fl->head->ty);
						if(ty) {
							Tr_exp exp = Tr_fieldVar(et.exp, offset);
							return expTy(exp, ty);
						}
						else {
							EM_error(v->pos, "undefined field type %s", S_name(v->u.field.sym));
							return expTy(NULL, Ty_Void());
						}
					}
				EM_error(v->pos, "record do not have field %s", S_name(v->u.field.sym));
				return expTy(NULL, Ty_Void());
			}
			else {
				EM_error(v->pos, "only record type has field");
				return expTy(NULL, Ty_Void());
			}
		}
		case A_subscriptVar: {
			struct expty vet = transVar(level, venv, tenv, v->u.subscript.var);
			if(vet.ty->kind == Ty_array) {
				struct expty eet = transExp(level, venv, tenv, v->u.subscript.exp);
				if(eet.ty->kind == Ty_int) {
					Ty_ty ty = actual_ty(vet.ty->u.array);
					if(ty) {
						Tr_exp exp = Tr_subscriptVar(vet.exp, eet.exp);
						return expTy(exp, ty);
					}
					else {
						EM_error(v->pos, "undefined array type");
						return expTy(NULL, Ty_Void());
					}
				}
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

static struct expty transExp(Tr_level level, S_table venv, S_table tenv, A_exp e) {
	switch(e->kind) {
		case A_varExp: {
			return transVar(level, venv, tenv, e->u.var);
		}
		case A_nilExp: {
			return expTy(Tr_Ex(T_Const(0)), Ty_Nil());
		}
		case A_intExp: {
			return expTy(Tr_Ex(T_Const(e->u.intt)), Ty_Int());
		}
		case A_stringExp: {
            Tr_AddStrFrag(e->u.stringg);
			return expTy(Tr_Ex(T_Name(Temp_namedlabel(e->u.stringg))), Ty_String());
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
						struct expty et = transExp(level, venv, tenv, tmp_e);
						if(et.ty != tmp_t && !(tmp_t->kind == Ty_record && et.ty->kind == Ty_nil)) {
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

				return expTy(NULL, ee->u.fun.result);
			}
			else {
				EM_error(e->pos, "undefined function %s", S_name(e->u.call.func));
				return expTy(NULL, Ty_Void());
			}
		}
		case A_opExp: {
			struct expty left = transExp(level, venv, tenv, e->u.op.left);
			struct expty right = transExp(level, venv, tenv, e->u.op.right);

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
							 left.ty->kind == Ty_record && right.ty->kind == Ty_nil ||
							 left.ty->kind == Ty_nil && right.ty->kind == Ty_record ||
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
			Ty_ty ty = actual_ty(S_look(tenv, e->u.record.typ));
			if(ty && ty->kind == Ty_record) {
				for(A_efieldList efl = e->u.record.fields; efl; efl = efl->tail) {
					A_efield ef = efl->head;
					Ty_fieldList fl;
					for(fl = ty->u.record; fl; fl = fl->tail) {
						Ty_field f = fl->head;
						if(ef->name == f->name) {
							struct expty et = transExp(level, venv, tenv, ef->exp);
							Ty_ty tmp_ty = actual_ty(f->ty);
							if(tmp_ty != et.ty && !(tmp_ty->kind == Ty_record && et.ty->kind == Ty_nil)) {
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
			else {
				EM_error(e->pos, "undefined record type %s", S_name(e->u.record.typ));
				return expTy(NULL, Ty_Void());
			}
		}
		case A_seqExp: {
			if(e->u.seq == NULL)
				return expTy(NULL, Ty_Void());
			else {
				A_expList el;
				struct expty et = expTy(NULL, Ty_Void());
				for(el = e->u.seq; el; el = el->tail)
					et = transExp(level, venv, tenv, el->head);
				return et;
			}
		}
		case A_assignExp: {
			struct expty vet = transVar(level, venv, tenv, e->u.assign.var);
			struct expty eet = transExp(level, venv, tenv, e->u.assign.exp);
			if(vet.ty != eet.ty && !(vet.ty->kind == Ty_record && eet.ty->kind == Ty_nil)) {
				EM_error(e->pos, "two sides's type of assignment does not match");
				return expTy(NULL, Ty_Void());
			}
			return expTy(NULL, Ty_Void());
		}
		case A_ifExp: {
			struct expty tet = transExp(level, venv, tenv, e->u.iff.test);
			if(tet.ty != Ty_Int()) {
				EM_error(e->pos, "if's test must be int");
				return expTy(NULL, Ty_Void());
			}
			tet = transExp(level, venv, tenv, e->u.iff.then);
			if(e->u.iff.elsee) {
				struct expty eet = transExp(level, venv, tenv, e->u.iff.elsee);
				if(tet.ty != eet.ty && 
				   !(tet.ty->kind == Ty_record && eet.ty->kind == Ty_nil) &&
				   !(tet.ty->kind == Ty_nil && eet.ty->kind == Ty_record)) {
					EM_error(e->pos, "if-then-else then and else must produce the same type");
					return expTy(NULL, Ty_Void());
				}
			}
			else if(!e->u.iff.elsee && tet.ty != Ty_Void()) {
				EM_error(e->pos, "if-then's then must produce no value");
				return expTy(NULL, Ty_Void());
			}
			
			return expTy(NULL, tet.ty);
		}
		case A_whileExp: {
			for_while = 1;
			struct expty et = transExp(level, venv, tenv, e->u.whilee.test);
			if(et.ty != Ty_Int()) {
				EM_error(e->pos, "while's test must be int");
				for_while = 0;
				return expTy(NULL, Ty_Void());
			}
			et = transExp(level, venv, tenv, e->u.whilee.body);
			if(et.ty != Ty_Void()) {
				EM_error(e->pos, "while's body must produce no value");
				for_while = 0;
				return expTy(NULL, Ty_Void());
			}
			for_while = 0;
			return expTy(NULL, Ty_Void());
		}
		case A_forExp: {
			for_while = 1;
			S_beginScope(venv);
			S_enter(venv, e->u.forr.var, E_VarEntry(NULL, Ty_Int()));
			struct expty et = transExp(level, venv, tenv, e->u.forr.lo);
			if(et.ty != Ty_Int()) {
				EM_error(e->pos, "for's lo must be int");
				for_while = 0;
				return expTy(NULL, Ty_Void());
			}
			et = transExp(level, venv, tenv, e->u.forr.hi);
			if(et.ty != Ty_Int()) {
				EM_error(e->pos, "for's hi must be int");
				for_while = 0;
				return expTy(NULL, Ty_Void());
			} 
			et = transExp(level, venv, tenv, e->u.forr.body);
			if(et.ty != Ty_Void()) {
				EM_error(e->pos, "for's body must produce no value");
				for_while = 0;
				return expTy(NULL, Ty_Void());
			} 
			S_endScope(venv);
			for_while = 0;
			return expTy(NULL, Ty_Void());
		}
		case A_breakExp: {
			if(!for_while)
				EM_error(e->pos, "break must within for or while");
			return expTy(NULL, Ty_Void());
		}
		case A_letExp: {
#ifndef TEST_ACTIVATION_RECORDS
			S_beginScope(venv);
#endif
			S_beginScope(tenv);
			for(A_decList dl = e->u.let.decs; dl; dl = dl->tail)
				transDec(level, venv, tenv, dl->head);
			struct expty et = transExp(level, venv, tenv, e->u.let.body);
			S_endScope(tenv);
#ifndef TEST_ACTIVATION_RECORDS
			S_endScope(venv);
#endif
			return et;
		}
		case A_arrayExp: {
			Ty_ty ty = actual_ty(S_look(tenv, e->u.array.typ));
			if(ty && ty->kind == Ty_array) {
				struct expty et = transExp(level, venv, tenv, e->u.array.size);
				if(et.ty != Ty_Int()) {
					EM_error(e->pos, "array's size must be int");
					return expTy(NULL, Ty_Void());
				}
				et = transExp(level, venv, tenv, e->u.array.init);
				Ty_ty aty = actual_ty(ty->u.array);
				if(aty != et.ty) {
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

static void transDec(Tr_level level, S_table venv, S_table tenv, A_dec d) {
	switch(d->kind) {
		case A_typeDec: {
			int pass = 0;
			if(d->u.type->tail == NULL)
				pass = 1;
			else {
				for(A_nametyList nl = d->u.type; nl; nl = nl->tail) {
					if(nl->head->ty->kind == A_recordTy ||
					   nl->head->ty->kind == A_arrayTy)
						pass = 1;
				}
				S_table tmp = S_empty();
				for(A_nametyList nl = d->u.type; nl; nl = nl->tail) {
					if(S_look(tmp, nl->head->name) != NULL) {
						EM_error(d->pos, "same name in the same batch");
						return;
					}
					S_enter(tmp, nl->head->name, Ty_Void());
				}
			}
			if(!pass) {
				EM_error(d->pos, "mutual recursive types must pass through record or array");
				return;
			}
			for(A_nametyList nl = d->u.type; nl; nl = nl->tail) {
				A_namety n = nl->head;
				S_enter(tenv, n->name, Ty_Name(n->name, NULL));	
			}
			for(A_nametyList nl = d->u.type; nl; nl = nl->tail) {
				A_namety n = nl->head;
				Ty_ty ty = S_look(tenv, n->name);
				ty->u.name.ty = transTy(tenv, n->ty);
			}
		}
		break;
		case A_varDec: {
			struct expty et = transExp(level, venv, tenv, d->u.var.init);
			if(d->u.var.typ) {
				Ty_ty ty = actual_ty(S_look(tenv, d->u.var.typ));
				if(!ty) {
					EM_error(d->pos, "undefined variable's type %s", S_name(d->u.var.typ));
					break;
				}
				if(ty != et.ty && !(ty->kind == Ty_record && et.ty->kind == Ty_nil)) {
					EM_error(d->pos, "variable declaration's type does not match init");
					break;
				}
				S_enter(venv, d->u.var.var, E_VarEntry(Tr_allocLocal(level, TRUE), ty));
			}
			else {
				if(et.ty->kind == Ty_nil) {
					EM_error(d->pos, "nil must be in a context where its type can be determined");
					break;
				}
				S_enter(venv, d->u.var.var, E_VarEntry(Tr_allocLocal(level, TRUE), et.ty));
			}
		}
		break;
		case A_functionDec: {
			S_table tmp = S_empty();
			for(A_fundecList fl = d->u.function; fl; fl = fl->tail) {
				if(S_look(tmp, fl->head->name) != NULL) {
					EM_error(d->pos, "same name in the same batch");
					return;
				}
				S_enter(tmp, fl->head->name, Ty_Void());
			}
			for(A_fundecList fl = d->u.function; fl; fl = fl->tail) {
				A_fundec f = fl->head;
				Ty_tyList tmp_tl, tl = Ty_TyList(NULL, NULL);
				A_fieldList l;
				for(tmp_tl = tl, l = f->params; l; l = l->tail) {
					Ty_ty ty = actual_ty(S_look(tenv, l->head->typ));
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
				if(l)
					continue;
				if(tl->head == NULL)
					tl = NULL;
				Ty_ty ty;
				if(f->result) {
					ty = actual_ty(S_look(tenv, f->result));
					if(!ty) {
						EM_error(l->head->pos, "undefined type %s", S_name(f->result));
						continue;
					}
				}
				else
					ty = Ty_Void();
				Temp_label name = Temp_newlabel();
				U_boolList formals = NULL;
				if(f->params) {
					formals = U_BoolList(TRUE, NULL);
					U_boolList list = formals;
					for(l = f->params->tail; l; l = l->tail) {
						list->tail = U_BoolList(TRUE, NULL);
						list = list->tail;
					}
				}
				Tr_level newlevel = Tr_newLevel(level, name, formals);
				S_enter(venv, f->name, E_FunEntry(newlevel, name, tl, ty));
			}
			for(A_fundecList fl = d->u.function; fl; fl = fl->tail) {
#ifndef TEST_ACTIVATION_RECORDS
				S_beginScope(venv);
#endif
				A_fundec f = fl->head;
				E_enventry ee = S_look(venv, f->name);
				A_fieldList l;
				Tr_level newlevel = ee->u.fun.level;
				Tr_accessList aList = Tr_formals(newlevel);
				for(l = f->params; l; l = l->tail, aList = aList->tail) {
					Ty_ty ty = actual_ty(S_look(tenv, l->head->typ));
					if(!ty) {
						break;
					}
					S_enter(venv, l->head->name, E_VarEntry(aList->head, ty));
				}
				if(l) {
#ifndef TEST_ACTIVATION_RECORDS
					S_endScope(venv);
#endif
					continue;
				}
				struct expty et = transExp(newlevel, venv, tenv, f->body);
				if(ee->u.fun.result != et.ty) {
					EM_error(d->pos, "function declaration result's type does not match body");
					continue;
				}
#ifndef TEST_ACTIVATION_RECORDS
				S_endScope(venv);
#endif
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
				return ty;
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
				if(afl->tail) {
					p_tfl->tail = Ty_FieldList(NULL, NULL);
					p_tfl = p_tfl->tail;
				}
			}
			return Ty_Record(tfl);
		}
		case A_arrayTy: {
			Ty_ty ty = S_look(tenv, t->u.array);
			if(ty)
				return Ty_Array(ty);
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
