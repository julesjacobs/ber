open Ast

let pp_ident fmt (id : ident) = Format.pp_print_string fmt id.node

let is_pattern_atom (p : pattern) =
  match p.node with
  | PWildcard | PVar _ | PInt _ | PBool _ | PString _ | PConstr (_, []) -> true
  | PTuple _ | PConstr _ | PAs _ | PAnnot _ -> false

let is_expr_atom (e : expr) =
  match e.node with
  | EVar _ | EInt _ | EBool _ | EString _ | EConstr (_, []) -> true
  | ETuple _ | EConstr _ | ELambda _ | EApp _ | ELet _ | EMatch _ | EAnnot _ -> false

let rec pp_type ?(prec = 0) fmt (t : type_expr) =
  match t.node with
  | TyVar id -> pp_ident fmt id
  | TyConstr (ctor, []) -> pp_ident fmt ctor
  | TyConstr (ctor, [arg]) ->
    let need_paren = prec > 1 in
    if need_paren then Format.pp_print_char fmt '(';
    Format.fprintf fmt "%a %a" (pp_type ~prec:2) arg pp_ident ctor;
    if need_paren then Format.pp_print_char fmt ')'
  | TyConstr (ctor, args) ->
    let need_paren = prec > 1 in
    if need_paren then Format.pp_print_char fmt '(';
    let pp_sep fmt () = Format.pp_print_string fmt ", " in
    Format.fprintf fmt "(%a) %a"
      (Format.pp_print_list ~pp_sep (pp_type ~prec:0)) args
      pp_ident ctor;
    if need_paren then Format.pp_print_char fmt ')'
  | TyTuple elems ->
    let pp_sep fmt () = Format.pp_print_string fmt " * " in
    Format.fprintf fmt "%a"
      (Format.pp_print_list ~pp_sep (pp_type ~prec:1))
      elems
  | TyArrow (a, b) ->
    let need_paren = prec > 0 in
    if need_paren then Format.pp_print_char fmt '(';
    Format.fprintf fmt "%a -> %a" (pp_type ~prec:1) a (pp_type ~prec:0) b;
    if need_paren then Format.pp_print_char fmt ')'

let rec pp_pattern ?(prec = 0) fmt (p : pattern) =
  match p.node with
  | PWildcard -> Format.pp_print_char fmt '_'
  | PVar id -> pp_ident fmt id
  | PAs (pat, id) ->
    let need_paren = prec > 0 in
    if need_paren then Format.pp_print_char fmt '(';
    Format.fprintf fmt "%a as %a" (pp_pattern ~prec:1) pat pp_ident id;
    if need_paren then Format.pp_print_char fmt ')'
  | PInt i -> Format.pp_print_int fmt i
  | PBool b -> Format.pp_print_bool fmt b
  | PString s -> Format.fprintf fmt "%S" s
  | PTuple elems ->
    let pp_sep fmt () = Format.pp_print_string fmt ", " in
    Format.fprintf fmt "%a"
      (Format.pp_print_list ~pp_sep (pp_pattern ~prec:0))
      elems
  | PConstr (ctor, args) ->
    begin
      match args with
      | [] -> pp_ident fmt ctor
      | _ ->
        let need_paren = prec > 1 in
        if need_paren then Format.pp_print_char fmt '(';
        let pp_arg fmt p =
          if is_pattern_atom p then pp_pattern ~prec:2 fmt p
          else Format.fprintf fmt "(%a)" (pp_pattern ~prec:0) p
        in
        Format.fprintf fmt "%a %a" pp_ident ctor
          (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ' ') pp_arg) args;
        if need_paren then Format.pp_print_char fmt ')'
    end
  | PAnnot (p, t) ->
    let need_paren = prec > 0 in
    if need_paren then Format.pp_print_char fmt '(';
    Format.fprintf fmt "%a : %a" (pp_pattern ~prec:0) p (pp_type ~prec:0) t;
    if need_paren then Format.pp_print_char fmt ')'

let rec pp_expr ?(prec = 0) fmt (e : expr) =
  match e.node with
  | EVar id -> pp_ident fmt id
  | EInt i -> Format.pp_print_int fmt i
  | EBool b -> Format.pp_print_bool fmt b
  | EString s -> Format.fprintf fmt "%S" s
  | ETuple elems ->
    let pp_sep fmt () = Format.pp_print_string fmt ", " in
    Format.fprintf fmt "%a"
      (Format.pp_print_list ~pp_sep (pp_expr ~prec:0))
      elems
  | EConstr (ctor, args) ->
    begin
      match args with
      | [] -> pp_ident fmt ctor
      | _ ->
        let need_paren = prec > 1 in
        if need_paren then Format.pp_print_char fmt '(';
        let pp_arg fmt e =
          if is_expr_atom e then pp_expr ~prec:2 fmt e
          else Format.fprintf fmt "(%a)" (pp_expr ~prec:0) e
        in
        Format.fprintf fmt "%a %a" pp_ident ctor
          (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ' ') pp_arg) args;
        if need_paren then Format.pp_print_char fmt ')'
    end
  | ELambda { params; fn_body } ->
    let need_paren = prec > 0 in
    if need_paren then Format.pp_print_char fmt '(';
    Format.fprintf fmt "fun %a -> %a"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ' ') (pp_pattern ~prec:2)) params
      (pp_expr ~prec:0) fn_body;
    if need_paren then Format.pp_print_char fmt ')'
  | EApp (fn, args) ->
    let need_paren = prec > 1 in
    if need_paren then Format.pp_print_char fmt '(';
    let pp_arg fmt e =
      if is_expr_atom e then pp_expr ~prec:2 fmt e
      else Format.fprintf fmt "(%a)" (pp_expr ~prec:0) e
    in
    Format.fprintf fmt "%a %a" (pp_expr ~prec:2) fn
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ' ') pp_arg) args;
    if need_paren then Format.pp_print_char fmt ')'
  | ELet { rec_flag; bindings; in_expr } ->
    let pp_binding fmt (b : binding) =
      let lhs = b.Location.node.lhs in
      let rhs = b.Location.node.rhs in
      Format.fprintf fmt "%a = %a" (pp_pattern ~prec:0) lhs (pp_expr ~prec:0) rhs
    in
    let rec_kw = match rec_flag with Recursive -> "let rec" | Nonrecursive -> "let" in
    let pp_sep fmt () = Format.fprintf fmt " and " in
    let need_paren = prec > 0 in
    if need_paren then Format.pp_print_char fmt '(';
    Format.fprintf fmt "%s %a in %a"
      rec_kw
      (Format.pp_print_list ~pp_sep pp_binding) bindings
      (pp_expr ~prec:0) in_expr;
    if need_paren then Format.pp_print_char fmt ')'
  | EMatch (scrut, cases) ->
    let pp_case fmt (c : match_case) =
      let n = c.Location.node in
      match n.guard with
      | None -> Format.fprintf fmt "| %a -> %a" (pp_pattern ~prec:0) n.pattern (pp_expr ~prec:0) n.body
      | Some g ->
        Format.fprintf fmt "| %a when %a -> %a" (pp_pattern ~prec:0) n.pattern (pp_expr ~prec:0) g (pp_expr ~prec:0) n.body
    in
    let need_paren = prec > 0 in
    if need_paren then Format.pp_print_char fmt '(';
    Format.fprintf fmt "match %a with@ %a"
      (pp_expr ~prec:0) scrut
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_space fmt ()) pp_case) cases;
    if need_paren then Format.pp_print_char fmt ')'
  | EAnnot (expr, typ) ->
    let need_paren = prec > 0 in
    if need_paren then Format.pp_print_char fmt '(';
    Format.fprintf fmt "%a : %a" (pp_expr ~prec:0) expr (pp_type ~prec:0) typ;
    if need_paren then Format.pp_print_char fmt ')'

let pp_constructor_decl fmt (c : constructor_decl) =
  match c.node.args with
  | [] -> Format.fprintf fmt "%a" pp_ident c.node.cname
  | args ->
    let pp_sep fmt () = Format.pp_print_string fmt " * " in
    Format.fprintf fmt "%a of %a"
      pp_ident c.node.cname
      (Format.pp_print_list ~pp_sep (pp_type ~prec:0)) args

let pp_type_decl fmt (d : type_decl) =
  let pp_params fmt params =
    match params with
    | [] -> ()
    | [p] -> Format.fprintf fmt "%a " pp_ident p
    | ps ->
      let pp_sep fmt () = Format.pp_print_string fmt ", " in
      Format.fprintf fmt "(%a) " (Format.pp_print_list ~pp_sep pp_ident) ps
  in
  let pp_sep fmt () = Format.pp_print_string fmt " | " in
  Format.fprintf fmt "type %a%a = %a"
    pp_params d.node.params
    pp_ident d.node.tname
    (Format.pp_print_list ~pp_sep pp_constructor_decl) d.node.constructors

let pp_binding fmt (b : binding) =
  let lhs = b.Location.node.lhs in
  let rhs = b.Location.node.rhs in
  Format.fprintf fmt "%a = %a" (pp_pattern ~prec:0) lhs (pp_expr ~prec:0) rhs

let pp_toplevel fmt (t : toplevel) =
  match t.Location.node with
  | TLet (rec_flag, bindings) ->
    let kw = match rec_flag with Recursive -> "let rec" | Nonrecursive -> "let" in
    let pp_sep fmt () = Format.pp_print_string fmt " and " in
    Format.fprintf fmt "%s %a" kw (Format.pp_print_list ~pp_sep pp_binding) bindings
  | TType d -> pp_type_decl fmt d
  | TExpr e -> pp_expr fmt e
