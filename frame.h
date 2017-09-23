typedef struct F_frame_ *F_frame;
typedef struct F_access_ *F_access;

typedef struct F_accessList_ *F_accessList;
struct F_accessList_ {
	F_access head;
	F_accessList tail;
};
F_accessList F_AccessList(F_access head, F_accessList tail);

void F_printAccess(F_access access);
void F_printFrame(F_frame frame);

F_frame F_newFrame(Temp_label name, U_boolList formals);
Temp_label F_name(F_frame f);
F_accessList F_formals(F_frame f);
F_access F_allocLocal(F_frame f, bool escape);

Temp_temp F_FP();
extern const int F_wordSize;
T_exp F_Exp(F_access acc, T_exp framePtr);
