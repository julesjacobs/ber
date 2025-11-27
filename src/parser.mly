%{
open Ast
open Location
module L = Location

let mk_loc start stop = L.span start stop
let with_loc start stop node = L.with_loc (mk_loc start stop) node

let span_nodes a b = L.span a.loc.start b.loc.stop

let list_span = function
  | [] -> invalid_arg "list_span"
  | first :: rest ->
    let last = List.fold_left (fun _ x -> x) first rest in
    L.span first.loc.start last.loc.stop

let build_binding pat expr =
  let loc = L.span pat.loc.start expr.loc.stop in
  L.with_loc loc { lhs = pat; rhs = expr }

let build_tuple_expr elems =
  L.with_loc (list_span elems) (ETuple elems)

let build_tuple_pat elems =
  L.with_loc (list_span elems) (PTuple elems)

let build_tuple_type elems =
  L.with_loc (list_span elems) (TyTuple elems)

let build_type_app parts =
  match List.rev parts with
  | [] -> invalid_arg "build_type_app"
  | ({ node = TyConstr (ctor, ctor_args); _ } as ctor_ty) :: rev_args ->
    let args = List.rev rev_args in
    let loc = L.span (List.hd parts).loc.start ctor_ty.loc.stop in
    L.with_loc loc (TyConstr (ctor, ctor_args @ args))
  | [single] -> single
  | last :: _ ->
    let first = List.hd parts in
    let loc = L.span first.loc.start last.loc.stop in
    raise (Syntax_error.Syntax_error (loc, "type application must end in a constructor"))

let last_exn = function
  | [] -> invalid_arg "last_exn"
  | _ :: _ as lst -> List.hd (List.rev lst)

let apply_expr head args =
  let loc = L.span head.loc.start (last_exn args).loc.stop in
  match head.node with
  | EConstr (id, existing) -> L.with_loc loc (EConstr (id, existing @ args))
  | EApp (fn, existing) -> L.with_loc loc (EApp (fn, existing @ args))
  | _ -> L.with_loc loc (EApp (head, args))

let apply_pattern head args =
  let loc = L.span head.loc.start (last_exn args).loc.stop in
  match head.node with
  | PConstr (id, existing) -> L.with_loc loc (PConstr (id, existing @ args))
  | _ -> raise (Syntax_error.Syntax_error (loc, "only constructors can be applied in patterns"))
%}

%token LET REC AND IN FUN MATCH WITH TYPE OF AS WHEN
%token TRUE FALSE
%token UNDERSCORE
%token COLON EQUAL BAR COMMA STAR LPAREN RPAREN ARROW
%token EOF
%token <int> INT
%token <string> STRING
%token <string> LIDENT
%token <string> UIDENT
%token <string> TYVAR

%start <Ast.program> program
%start <Ast.expr> expression
%start <Ast.type_expr> type_expression
%start <Ast.toplevel> toplevel_entry
%start <Ast.toplevel> toplevel_item
%start <Ast.toplevel> toplevel_naked

%%

expression:
  | e=expr EOF { e }

type_expression:
  | t=type_expr EOF { t }

program:
  | items=program_items EOF { items }

toplevel_entry:
  | item=toplevel EOF { item }

toplevel_item:
  | item=toplevel { item }

toplevel_naked:
  | item=toplevel { item }

program_items:
  | /* empty */ { [] }
  | item=toplevel rest=program_items { item :: rest }

toplevel:
  | d=let_def { d }
  | d=type_def { d }
  | e=expr { with_loc $startpos $endpos (TExpr e) }

let_def:
  | LET r=rec_opt b=let_binding rest=let_binding_tail {
      with_loc $startpos $endpos (TLet (r, b :: rest))
    }

rec_opt:
  | REC { Recursive }
  | /* empty */ { Nonrecursive }

let_binding_tail:
  | AND b=let_binding rest=let_binding_tail { b :: rest }
  | /* empty */ { [] }

let_binding:
  | fn=pattern params=fun_params EQUAL body=expr {
      let start = fn.loc.start in
      let stop = body.loc.stop in
      let lam = L.with_loc (L.span start stop) (ELambda { params; fn_body = body }) in
      build_binding fn lam
    }
  | pat=pattern EQUAL expr=expr { build_binding pat expr }

expr:
  | LET r=rec_opt b=let_binding rest=let_binding_tail IN body=expr {
      let bindings = b :: rest in
      with_loc $startpos $endpos (ELet { rec_flag = r; bindings; in_expr = body })
    }
  | FUN params=fun_params ARROW body=expr {
      with_loc $startpos $endpos (ELambda { params; fn_body = body })
    }
  | MATCH scrut=expr WITH cases=match_cases {
      with_loc $startpos $endpos (EMatch (scrut, cases))
    }
  | e=ascribed_expr { e }

ascribed_expr:
  | e=tuple_expr COLON t=type_expr {
      with_loc $startpos $endpos (EAnnot (e, t))
    }
  | tuple_expr { $1 }

tuple_expr:
  | e1=app_expr COMMA e2=app_expr rest=tuple_expr_more {
      let elems = e1 :: e2 :: rest in
      build_tuple_expr elems
    }
  | app_expr { $1 }

tuple_expr_more:
  | COMMA e=app_expr rest=tuple_expr_more { e :: rest }
  | /* empty */ { [] }

app_expr:
  | atom_expr { $1 }
  | app_expr atom_expr { apply_expr $1 [$2] }

atom_expr:
  | id=lident {
      with_loc $startpos $endpos (EVar id)
    }
  | id=uident {
      with_loc $startpos $endpos (EConstr (id, []))
    }
  | n=INT {
      with_loc $startpos $endpos (EInt n)
    }
  | TRUE {
      with_loc $startpos $endpos (EBool true)
    }
  | FALSE {
      with_loc $startpos $endpos (EBool false)
    }
  | s=STRING {
      with_loc $startpos $endpos (EString s)
    }
  | LPAREN RPAREN {
      with_loc $startpos $endpos (ETuple [])
    }
  | LPAREN e=expr RPAREN { e }

match_cases:
  | maybe_bar first=match_case rest=match_case_tail { first :: rest }

maybe_bar:
  | BAR { () }
  | /* empty */ { () }

match_case_tail:
  | BAR c=match_case rest=match_case_tail { c :: rest }
  | /* empty */ { [] }

match_case:
  | pat=pattern guard=guard_opt ARROW rhs=expr {
      let loc = L.span pat.loc.start rhs.loc.stop in
      L.with_loc loc { pattern = pat; guard; body = rhs }
    }

guard_opt:
  | WHEN e=expr { Some e }
  | /* empty */ { None }

fun_params:
  | p=pattern rest=fun_params { p :: rest }
  | p=pattern { [p] }

pattern:
  | p=pattern_ascribed { p }

pattern_ascribed:
  | p=pattern_as COLON t=type_expr {
      with_loc $startpos $endpos (PAnnot (p, t))
    }
  | pattern_as { $1 }

pattern_as:
  | p=pattern_tuple AS x=lident {
      with_loc $startpos $endpos (PAs (p, x))
    }
  | pattern_tuple { $1 }

pattern_tuple:
  | p1=pattern_app COMMA p2=pattern_app rest=pattern_tuple_more {
      let elems = p1 :: p2 :: rest in
      build_tuple_pat elems
    }
  | pattern_app { $1 }

pattern_tuple_more:
  | COMMA p=pattern_app rest=pattern_tuple_more { p :: rest }
  | /* empty */ { [] }

pattern_app:
  | head=pattern_ctor args=pattern_atom_list { apply_pattern head args }
  | pattern_atom { $1 }

pattern_ctor:
  | id=uident { with_loc $startpos $endpos (PConstr (id, [])) }

pattern_atom_list:
  | p=pattern_atom ps=pattern_atom_list { p :: ps }
  | p=pattern_atom { [p] }

pattern_atom:
  | UNDERSCORE {
      with_loc $startpos $endpos PWildcard
    }
  | id=lident {
      with_loc $startpos $endpos (PVar id)
    }
  | id=uident {
      with_loc $startpos $endpos (PConstr (id, []))
    }
  | n=INT {
      with_loc $startpos $endpos (PInt n)
    }
  | TRUE {
      with_loc $startpos $endpos (PBool true)
    }
  | FALSE {
      with_loc $startpos $endpos (PBool false)
    }
  | s=STRING {
      with_loc $startpos $endpos (PString s)
    }
  | LPAREN RPAREN {
      with_loc $startpos $endpos (PTuple [])
    }
  | LPAREN p=pattern RPAREN { p }

type_def:
  | TYPE params=type_params name=lident EQUAL constrs=constructor_list {
      let loc = mk_loc $startpos $endpos in
      with_loc $startpos $endpos (TType (L.with_loc loc { tname = name; params; constructors = constrs }))
    }

type_params:
  | /* empty */ { [] }
  | params=type_params_list { params }

type_params_list:
  | LPAREN params=tyvar_list RPAREN { params }
  | tv=tyvar rest=type_params_tail { tv :: rest }

type_params_tail:
  | tv=tyvar rest=type_params_tail { tv :: rest }
  | /* empty */ { [] }

tyvar_list:
  | tv=tyvar { [tv] }
  | tv=tyvar COMMA rest=tyvar_list { tv :: rest }

constructor_list:
  | c=constructor_decl rest=constructor_decl_tail { c :: rest }

constructor_decl_tail:
  | BAR c=constructor_decl rest=constructor_decl_tail { c :: rest }
  | /* empty */ { [] }

constructor_decl:
  | name=uident args=constructor_args {
      let loc = mk_loc $startpos $endpos in
      L.with_loc loc { cname = name; args }
    }

constructor_args:
  | /* empty */ { [] }
  | OF args=constructor_arg_list { args }

constructor_arg_list:
  | t=type_expr STAR rest=constructor_arg_list { t :: rest }
  | t=type_expr { [t] }

type_expr:
  | t=type_arrow { t }

type_arrow:
  | t1=type_tuple ARROW t2=type_arrow {
      let loc = span_nodes t1 t2 in
      L.with_loc loc (TyArrow (t1, t2))
    }
  | type_tuple { $1 }

type_tuple:
  | t1=type_app STAR t2=type_app rest=type_tuple_more {
      let elems = t1 :: t2 :: rest in
      build_tuple_type elems
    }
  | type_app { $1 }

type_tuple_more:
  | STAR t=type_app rest=type_tuple_more { t :: rest }
  | /* empty */ { [] }

type_app:
  | parts=type_app_parts { build_type_app parts }

type_app_parts:
  | p=type_atom rest=type_app_parts { p :: rest }
  | p=type_atom { [p] }

type_atom:
  | tv=tyvar {
      with_loc $startpos $endpos (TyVar tv)
    }
  | ctor=type_ctor {
      with_loc $startpos $endpos (TyConstr (ctor, []))
    }
  | LPAREN t=type_expr RPAREN { t }

type_ctor:
  | id=lident { id }
  | id=uident { id }

tyvar:
  | tv=TYVAR { L.with_loc (mk_loc $startpos $endpos) tv }

lident:
  | id=LIDENT { L.with_loc (mk_loc $startpos $endpos) id }

uident:
  | id=UIDENT { L.with_loc (mk_loc $startpos $endpos) id }
