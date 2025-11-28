-module(ast).
-export([
    %% Declaration constructors
    'DeclModule'/1, 'DeclFunction'/1, 'DeclType'/1, 'DeclTypeAlias'/1,
    'DeclDataType'/1, 'DeclNewtype'/1, 'DeclTypeClass'/1, 'DeclTypeClassInstance'/1,
    'DeclImport'/1, 'DeclForeignImport'/1, 'DeclTypeSig'/1, 'DeclInfix'/1,

    %% Type expression constructors (both curried and uncurried)
    'TyExprCon'/1, 'TyExprVar'/1,
    'TyExprApp'/0, 'TyExprApp'/2,
    'TyExprArrow'/0, 'TyExprArrow'/2,
    'TyExprRecord'/0, 'TyExprRecord'/2,
    'TyExprForAll'/0, 'TyExprForAll'/2,
    'TyExprConstrained'/0, 'TyExprConstrained'/2,
    'TyExprParens'/1,
    'TyExprTuple'/1,

    %% Pattern constructors (both curried and uncurried)
    'PatVar'/1, 'PatWildcard'/0, 'PatLit'/1,
    'PatCon'/0, 'PatCon'/2,
    'PatRecord'/1,
    'PatList'/1,
    'PatCons'/0, 'PatCons'/2,
    'PatAs'/0, 'PatAs'/2,
    'PatParens'/1,

    %% Expression constructors (both curried and uncurried)
    'ExprVar'/1,
    'ExprQualified'/0, 'ExprQualified'/2,
    'ExprLit'/1,
    'ExprApp'/0, 'ExprApp'/2,
    'ExprLambda'/0, 'ExprLambda'/2,
    'ExprLet'/0, 'ExprLet'/2,
    'ExprIf'/0, 'ExprIf'/3,
    'ExprCase'/0, 'ExprCase'/2,
    'ExprDo'/1,
    'ExprBinOp'/0, 'ExprBinOp'/3,
    'ExprUnaryOp'/0, 'ExprUnaryOp'/2,
    'ExprList'/1, 'ExprTuple'/1, 'ExprRecord'/1,
    'ExprRecordAccess'/0, 'ExprRecordAccess'/2,
    'ExprRecordUpdate'/0, 'ExprRecordUpdate'/2,
    'ExprTyped'/0, 'ExprTyped'/2,
    'ExprParens'/1,
    'ExprSection'/1,
    'ExprSectionLeft'/0, 'ExprSectionLeft'/2,
    'ExprSectionRight'/0, 'ExprSectionRight'/2,

    %% Literal constructors
    'LitInt'/1, 'LitNumber'/1, 'LitString'/1, 'LitChar'/1, 'LitBool'/1,

    %% Do statement constructors
    'DoLet'/1, 'DoBind'/0, 'DoBind'/2, 'DoExpr'/1,

    %% Guard constructors
    'GuardExpr'/1, 'GuardPat'/0, 'GuardPat'/2,

    %% Import item constructors
    'ImportValue'/1, 'ImportType'/0, 'ImportType'/2,

    %% Import spec constructors
    'ImportAll'/0, 'ImportSome'/1, 'ImportNone'/0,

    %% Associativity constructors
    'AssocLeft'/0, 'AssocRight'/0, 'AssocNone'/0
]).

%% Declaration constructors
'DeclModule'(R) -> {'_decl_module', R}.
'DeclFunction'(R) -> {'_decl_function', R}.
'DeclType'(R) -> {'_decl_type', R}.
'DeclTypeAlias'(R) -> {'_decl_type_alias', R}.
'DeclDataType'(R) -> {'_decl_data_type', R}.
'DeclNewtype'(R) -> {'_decl_newtype', R}.
'DeclTypeClass'(R) -> {'_decl_type_class', R}.
'DeclTypeClassInstance'(R) -> {'_decl_type_class_instance', R}.
'DeclImport'(R) -> {'_decl_import', R}.
'DeclForeignImport'(R) -> {'_decl_foreign_import', R}.
'DeclTypeSig'(R) -> {'_decl_type_sig', R}.
'DeclInfix'(R) -> {'_decl_infix', R}.

%% Type expression constructors
'TyExprCon'(S) -> {'_ty_expr_con', S}.
'TyExprVar'(S) -> {'_ty_expr_var', S}.
'TyExprApp'() -> fun(T1) -> fun(T2) -> {'_ty_expr_app', T1, T2} end end.
'TyExprApp'(T1, T2) -> {'_ty_expr_app', T1, T2}.
'TyExprArrow'() -> fun(T1) -> fun(T2) -> {'_ty_expr_arrow', T1, T2} end end.
'TyExprArrow'(T1, T2) -> {'_ty_expr_arrow', T1, T2}.
'TyExprRecord'() -> fun(Fields) -> fun(Row) -> {'_ty_expr_record', Fields, Row} end end.
'TyExprRecord'(Fields, Row) -> {'_ty_expr_record', Fields, Row}.
'TyExprForAll'() -> fun(Vars) -> fun(T) -> {'_ty_expr_for_all', Vars, T} end end.
'TyExprForAll'(Vars, T) -> {'_ty_expr_for_all', Vars, T}.
'TyExprConstrained'() -> fun(Cs) -> fun(T) -> {'_ty_expr_constrained', Cs, T} end end.
'TyExprConstrained'(Cs, T) -> {'_ty_expr_constrained', Cs, T}.
'TyExprParens'(T) -> {'_ty_expr_parens', T}.
'TyExprTuple'(Ts) -> {'_ty_expr_tuple', Ts}.

%% Pattern constructors
'PatVar'(S) -> {'_pat_var', S}.
'PatWildcard'() -> '_pat_wildcard'.
'PatLit'(L) -> {'_pat_lit', L}.
'PatCon'() -> fun(Name) -> fun(Pats) -> {'_pat_con', Name, Pats} end end.
'PatCon'(Name, Pats) -> {'_pat_con', Name, Pats}.
'PatRecord'(Fields) -> {'_pat_record', Fields}.
'PatList'(Ps) -> {'_pat_list', Ps}.
'PatCons'() -> fun(H) -> fun(T) -> {'_pat_cons', H, T} end end.
'PatCons'(H, T) -> {'_pat_cons', H, T}.
'PatAs'() -> fun(Name) -> fun(P) -> {'_pat_as', Name, P} end end.
'PatAs'(Name, P) -> {'_pat_as', Name, P}.
'PatParens'(P) -> {'_pat_parens', P}.

%% Expression constructors
'ExprVar'(S) -> {'_expr_var', S}.
'ExprQualified'() -> fun(M) -> fun(N) -> {'_expr_qualified', M, N} end end.
'ExprQualified'(M, N) -> {'_expr_qualified', M, N}.
'ExprLit'(L) -> {'_expr_lit', L}.
'ExprApp'() -> fun(F) -> fun(A) -> {'_expr_app', F, A} end end.
'ExprApp'(F, A) -> {'_expr_app', F, A}.
'ExprLambda'() -> fun(Ps) -> fun(E) -> {'_expr_lambda', Ps, E} end end.
'ExprLambda'(Ps, E) -> {'_expr_lambda', Ps, E}.
'ExprLet'() -> fun(Bs) -> fun(E) -> {'_expr_let', Bs, E} end end.
'ExprLet'(Bs, E) -> {'_expr_let', Bs, E}.
'ExprIf'() -> fun(C) -> fun(T) -> fun(E) -> {'_expr_if', C, T, E} end end end.
'ExprIf'(C, T, E) -> {'_expr_if', C, T, E}.
'ExprCase'() -> fun(E) -> fun(Cs) -> {'_expr_case', E, Cs} end end.
'ExprCase'(E, Cs) -> {'_expr_case', E, Cs}.
'ExprDo'(Stmts) -> {'_expr_do', Stmts}.
'ExprBinOp'() -> fun(Op) -> fun(L) -> fun(R) -> {'_expr_bin_op', Op, L, R} end end end.
'ExprBinOp'(Op, L, R) -> {'_expr_bin_op', Op, L, R}.
'ExprUnaryOp'() -> fun(Op) -> fun(E) -> {'_expr_unary_op', Op, E} end end.
'ExprUnaryOp'(Op, E) -> {'_expr_unary_op', Op, E}.
'ExprList'(Es) -> {'_expr_list', Es}.
'ExprTuple'(Es) -> {'_expr_tuple', Es}.
'ExprRecord'(Fields) -> {'_expr_record', Fields}.
'ExprRecordAccess'() -> fun(E) -> fun(F) -> {'_expr_record_access', E, F} end end.
'ExprRecordAccess'(E, F) -> {'_expr_record_access', E, F}.
'ExprRecordUpdate'() -> fun(E) -> fun(Fields) -> {'_expr_record_update', E, Fields} end end.
'ExprRecordUpdate'(E, Fields) -> {'_expr_record_update', E, Fields}.
'ExprTyped'() -> fun(E) -> fun(T) -> {'_expr_typed', E, T} end end.
'ExprTyped'(E, T) -> {'_expr_typed', E, T}.
'ExprParens'(E) -> {'_expr_parens', E}.
'ExprSection'(Op) -> {'_expr_section', Op}.
'ExprSectionLeft'() -> fun(E) -> fun(Op) -> {'_expr_section_left', E, Op} end end.
'ExprSectionLeft'(E, Op) -> {'_expr_section_left', E, Op}.
'ExprSectionRight'() -> fun(Op) -> fun(E) -> {'_expr_section_right', Op, E} end end.
'ExprSectionRight'(Op, E) -> {'_expr_section_right', Op, E}.

%% Literal constructors
'LitInt'(N) -> {'_lit_int', N}.
'LitNumber'(N) -> {'_lit_number', N}.
'LitString'(S) -> {'_lit_string', S}.
'LitChar'(C) -> {'_lit_char', C}.
'LitBool'(B) -> {'_lit_bool', B}.

%% Do statement constructors
'DoLet'(Bs) -> {'_do_let', Bs}.
'DoBind'() -> fun(P) -> fun(E) -> {'_do_bind', P, E} end end.
'DoBind'(P, E) -> {'_do_bind', P, E}.
'DoExpr'(E) -> {'_do_expr', E}.

%% Guard constructors
'GuardExpr'(E) -> {'_guard_expr', E}.
'GuardPat'() -> fun(P) -> fun(E) -> {'_guard_pat', P, E} end end.
'GuardPat'(P, E) -> {'_guard_pat', P, E}.

%% Import item constructors
'ImportValue'(S) -> {'_import_value', S}.
'ImportType'() -> fun(S) -> fun(Spec) -> {'_import_type', S, Spec} end end.
'ImportType'(S, Spec) -> {'_import_type', S, Spec}.

%% Import spec constructors
'ImportAll'() -> '_import_all'.
'ImportSome'(Items) -> {'_import_some', Items}.
'ImportNone'() -> '_import_none'.

%% Associativity constructors
'AssocLeft'() -> '_assoc_left'.
'AssocRight'() -> '_assoc_right'.
'AssocNone'() -> '_assoc_none'.
