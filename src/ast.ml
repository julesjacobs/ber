open Location

type ident = string located

type rec_flag =
  | Recursive
  | Nonrecursive

type type_expr = type_expr_node located

and type_expr_node =
  | TyVar of ident
  | TyConstr of ident * type_expr list
  | TyArrow of type_expr * type_expr
  | TyTuple of type_expr list

type pattern = pattern_node located

and pattern_node =
  | PWildcard
  | PVar of ident
  | PAs of pattern * ident
  | PInt of int
  | PBool of bool
  | PString of string
  | PTuple of pattern list
  | PConstr of ident * pattern list
  | PAnnot of pattern * type_expr

type expr = expr_node located

and expr_node =
  | EVar of ident
  | EInt of int
  | EBool of bool
  | EString of string
  | ETuple of expr list
  | EConstr of ident * expr list
  | ELambda of lambda
  | EApp of expr * expr list
  | ELet of let_expr
  | EMatch of expr * match_case list
  | EAnnot of expr * type_expr

and lambda =
  { params : pattern list
  ; fn_body : expr
  }

and let_expr =
  { rec_flag : rec_flag
  ; bindings : binding list
  ; in_expr : expr
  }

and binding = binding_node located

and binding_node =
  { lhs : pattern
  ; rhs : expr
  }

and match_case = match_case_node located

and match_case_node =
  { pattern : pattern
  ; guard : expr option
  ; body : expr
  }

type constructor_decl = constructor_decl_node located

and constructor_decl_node =
  { cname : ident
  ; args : type_expr list
  }

type type_decl = type_decl_node located

and type_decl_node =
  { tname : ident
  ; params : ident list
  ; constructors : constructor_decl list
  }

type toplevel_node =
  | TLet of rec_flag * binding list
  | TType of type_decl
  | TExpr of expr

type toplevel = toplevel_node located

type program = toplevel list
