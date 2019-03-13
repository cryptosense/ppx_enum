module S :
  sig
    type t =
      | Foo 
      | Bar [@@deriving enum]
    val to_string : t -> string
    val from_string : string -> t
    type simple_enum =
      | Foo 
      | Bar [@@deriving enum]
    val simple_enum_to_string : simple_enum -> string
    val simple_enum_from_string : string -> simple_enum
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
