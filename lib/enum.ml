open Ppxlib

module Str = struct
  let string_to_constant_expression ~loc ~str =
    Ast_builder.Default.pexp_constant
      ~loc
      (Pconst_string (str, None))

  let string_to_constant_pattern ~loc ~str =
    Ast_builder.Default.ppat_constant
      ~loc
      (Pconst_string (str, None))

  let string_to_constructor_pattern ~loc ~str =
    Ast_builder.Default.ppat_construct
      ~loc
      {txt = Lident str; loc}
      None

  let string_to_constructor_expression ~loc ~str =
    Ast_builder.Default.pexp_construct
      ~loc
      {txt = Lident str; loc}
      None

  let to_string_case_from_constructor ~loc constructor =
    let {pcd_name = {txt = value_name; _}; _} = constructor in
    let lhs = string_to_constructor_pattern ~loc ~str:value_name in
    let rhs = string_to_constant_expression ~loc ~str:(String.lowercase_ascii value_name) in
    Ast_builder.Default.case ~lhs ~guard:None ~rhs

  let to_string_function ~loc ~type_name ~constructors =
    let function_name = Utils.to_string_function_name ~enum_name:type_name in
    let pat = Ast_builder.Default.ppat_var ~loc {txt=function_name; loc} in
    let expr =
      Ast_builder.Default.pexp_function
        ~loc
        (List.map (to_string_case_from_constructor ~loc) constructors)
    in
    let value_description =
      Ast_builder.Default.value_binding
      ~loc
      ~pat
      ~expr
    in
    Ast_builder.Default.pstr_value ~loc Nonrecursive [value_description]

  let from_string_case_from_constructor ~loc constructor =
    let {pcd_name = {txt = value_name; _}; _} = constructor in
    let lhs = string_to_constant_pattern ~loc ~str:(String.lowercase_ascii value_name) in
    let rhs = string_to_constructor_expression ~loc ~str:value_name in
    Ast_builder.Default.case ~lhs ~guard:None ~rhs

  let invalid_case_for_from_string ~loc ~function_name =
    let lhs = Ast_builder.Default.ppat_any ~loc in
    let rhs =
      [%expr
        invalid_arg
        (__MODULE__ ^ [%e string_to_constant_expression ~loc ~str:("." ^ function_name)])
      ]
    in
    Ast_builder.Default.case ~lhs ~guard:None ~rhs

  let from_string_function ~loc ~type_name ~constructors =
    let function_name = Utils.from_string_function_name ~enum_name:type_name in
    let pat = Ast_builder.Default.ppat_var ~loc {txt=function_name; loc} in
    let cases = List.map (from_string_case_from_constructor ~loc) constructors in
    let cases = cases @ [invalid_case_for_from_string ~loc ~function_name] in
    let expr = Ast_builder.Default.pexp_function ~loc cases in
    let value_description =
      Ast_builder.Default.value_binding
      ~loc
      ~pat
      ~expr
    in
    Ast_builder.Default.pstr_value ~loc Nonrecursive [value_description]

  let from_enummable_variant
    ~loc
    ~type_name
    ~constructors
  =
    [ to_string_function ~loc ~type_name ~constructors
    ; from_string_function ~loc ~type_name ~constructors
    ]

  let from_type_declaration ~loc type_ =
    match type_ with
    | { ptype_kind = Ptype_variant constructors
      ; ptype_params = []
      ; ptype_name = {txt = type_name; _}
      ; ptype_loc
      ; _
      }
      when (Utils.constructors_are_bare constructors)
      ->
        from_enummable_variant ~loc:ptype_loc ~type_name ~constructors
    | {ptype_kind = Ptype_variant _; ptype_params = []; _}
      ->
        Raise.Enum.unhandled_type_kind ~loc "variant with arguments"
    | {ptype_kind = Ptype_variant _; ptype_params = _::_; _}
      ->
        Raise.Enum.unhandled_type_kind ~loc "parametrized variant"
    | {ptype_kind = Ptype_record _; _}
      ->
        Raise.Enum.unhandled_type_kind ~loc "record"
    | {ptype_kind = Ptype_abstract; _}
      ->
        Raise.Enum.unhandled_type_kind ~loc "abstract"
    | {ptype_kind = Ptype_open; _}
      ->
        Raise.Enum.unhandled_type_kind ~loc "open"

  (** By giving this to the Deriving.Generator.make_noarg function below, ppxlib
   *  will apply the function the parameters:
   *  ~loc: Information about the current location in the code base (file, lineno etc)
   *  ~path: The current file path?
   *  rec_flag: ???
   *  type_declarations: A list of the type declarations at the point of call
   *)
  let from_type_decl ~loc ~path:_ (_rec_flag, type_declarations) =
    List.flatten @@ List.map (from_type_declaration ~loc) type_declarations
end

module Sig = struct
  let to_string_function_val ~loc ~type_name =
    let function_name = Utils.to_string_function_name ~enum_name:type_name in
    let type_lident = {txt = Lident type_name; loc} in
    let lhs_type = Ast_builder.Default.ptyp_constr ~loc type_lident [] in
    let type_ = [%type: [%t lhs_type] -> string] in
    let value_description =
      Ast_builder.Default.value_description
      ~loc
      ~name:{txt = function_name; loc}
      ~type_
      ~prim:[]
    in
    Ast_builder.Default.psig_value ~loc value_description

  let from_string_function_val ~loc ~type_name =
    let function_name = Utils.from_string_function_name ~enum_name:type_name in
    let type_lident = {txt = Lident type_name; loc} in
    let rhs_type = Ast_builder.Default.ptyp_constr ~loc type_lident [] in
    let type_ = [%type: string -> [%t rhs_type]] in
    let value_description =
      Ast_builder.Default.value_description
      ~loc
      ~name:{txt = function_name; loc}
      ~type_
      ~prim:[]
    in
    Ast_builder.Default.psig_value ~loc value_description

  let from_enummable_variant ~loc ~type_ =
    let {ptype_name = {txt = type_name; _}; _} = type_ in
    [ to_string_function_val ~loc ~type_name
    ; from_string_function_val ~loc ~type_name
    ]

  let from_variant ~loc ~type_ =
    match type_ with
    | {ptype_kind = Ptype_variant constructors; _} when not (Utils.constructors_are_bare constructors)
      ->
        Raise.Enum.unhandled_type_kind ~loc "variant with arguments"
    | {ptype_params; _} when ((List.length ptype_params) > 0)
      ->
        Raise.Enum.unhandled_type_kind ~loc "parametrized variant"
    | _
      ->
        from_enummable_variant ~loc ~type_

  let from_type_declaration ~loc type_ =
    let {ptype_kind; ptype_loc; _} = type_ in
    match ptype_kind with
    | Ptype_variant _ -> from_variant ~loc:ptype_loc ~type_
    | Ptype_record _ -> Raise.Enum.unhandled_type_kind ~loc "record"
    | Ptype_abstract -> Raise.Enum.unhandled_type_kind ~loc "abstract"
    | Ptype_open -> Raise.Enum.unhandled_type_kind ~loc "open"

  let from_type_decl ~loc ~path:_ (_rec_flag, type_declarations) =
    List.flatten @@ List.map (from_type_declaration ~loc) type_declarations
end

let from_str_type_decl =
  Deriving.Generator.make_noarg Str.from_type_decl

let from_sig_type_decl =
  Deriving.Generator.make_noarg Sig.from_type_decl
