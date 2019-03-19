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
      | s ->
          Error
            (Printf.sprintf "Unexpected value for %s.%s: %s" __MODULE__
               "from_string" s)
    let from_string_exn =
      function
      | "foo" -> Foo
      | "bar" -> Bar
      | s ->
          invalid_arg
            (Printf.sprintf "Unexpected value for %s.%s: %s" __MODULE__
               "from_string_exn" s)
    type simple_enum =
      | Foo 
      | Bar [@@deriving enum]
    let simple_enum_to_string = function | Foo -> "foo" | Bar -> "bar"
    let simple_enum_from_string =
      function
      | "foo" -> Ok Foo
      | "bar" -> Ok Bar
      | s ->
          Error
            (Printf.sprintf "Unexpected value for %s.%s: %s" __MODULE__
               "simple_enum_from_string" s)
    let simple_enum_from_string_exn =
      function
      | "foo" -> Foo
      | "bar" -> Bar
      | s ->
          invalid_arg
            (Printf.sprintf "Unexpected value for %s.%s: %s" __MODULE__
               "simple_enum_from_string_exn" s)
  end 
