module type Sig_module  =
  sig
    type simple_enum =
      | Foo 
      | Bar [@@deriving enum]
    val simple_enum_to_string : simple_enum -> string
    val simple_enum_from_string : string -> simple_enum
  end
module type Sig_module_with_t_type  =
  sig
    type t =
      | Foo 
      | Bar [@@deriving enum]
    val to_string : t -> string
    val from_string : string -> t
  end
type simple_enum =
  | Foo 
  | Bar [@@deriving enum]
let simple_enum_to_string = function | Foo -> "foo" | Bar -> "bar"
let simple_enum_from_string =
  function
  | "foo" -> Foo
  | "bar" -> Bar
  | _ -> invalid_arg (__MODULE__ ^ ".simple_enum_from_string")
module Sig_module_with_subsequent_implementation :
  sig
    type simple_enum =
      | Foo 
      | Bar [@@deriving enum]
    val simple_enum_to_string : simple_enum -> string
    val simple_enum_from_string : string -> simple_enum
  end =
  struct
    type simple_enum =
      | Foo 
      | Bar [@@deriving enum]
    let simple_enum_to_string = function | Foo -> "foo" | Bar -> "bar"
    let simple_enum_from_string =
      function
      | "foo" -> Foo
      | "bar" -> Bar
      | _ -> invalid_arg (__MODULE__ ^ ".simple_enum_from_string")
  end 
module Sig_module_with_subsequent_implementation_and_t_type :
  sig
    type t =
      | Foo 
      | Bar [@@deriving enum]
    val to_string : t -> string
    val from_string : string -> t
  end =
  struct
    type t =
      | Foo 
      | Bar [@@deriving enum]
    let to_string = function | Foo -> "foo" | Bar -> "bar"
    let from_string =
      function
      | "foo" -> Foo
      | "bar" -> Bar
      | _ -> invalid_arg (__MODULE__ ^ ".from_string")
  end 
