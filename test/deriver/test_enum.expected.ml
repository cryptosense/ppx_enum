module S :
  sig
    type t =
      | Foo 
      | Bar [@@deriving enum]
    val to_string : t -> string
    val from_string : string -> (t, string) result
    val from_string_exn : string -> t
    type simple_enum =
      | Foo 
      | Bar [@@deriving enum]
    val simple_enum_to_string : simple_enum -> string
    val simple_enum_from_string : string -> (simple_enum, string) result
    val simple_enum_from_string_exn : string -> simple_enum
  end =
  struct
    type t =
      | Foo 
      | Bar [@@deriving enum]
    let to_string = function | Foo -> "foo" | Bar -> "bar"
    let from_string =
      function
      | "foo" -> Ok Foo
      | "bar" -> Ok Bar
      | _ -> Error (__MODULE__ ^ ".from_string")
    let from_string_exn =
      function
      | "foo" -> Foo
      | "bar" -> Bar
      | _ -> invalid_arg (__MODULE__ ^ ".from_string_exn")
    type simple_enum =
      | Foo 
      | Bar [@@deriving enum]
    let simple_enum_to_string = function | Foo -> "foo" | Bar -> "bar"
    let simple_enum_from_string =
      function
      | "foo" -> Ok Foo
      | "bar" -> Ok Bar
      | _ -> Error (__MODULE__ ^ ".simple_enum_from_string")
    let simple_enum_from_string_exn =
      function
      | "foo" -> Foo
      | "bar" -> Bar
      | _ -> invalid_arg (__MODULE__ ^ ".simple_enum_from_string_exn")
  end 
