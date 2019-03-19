module S : sig
  type t =
    | Foo
    | Bar
  [@@deriving enum]

  type simple_enum =
    | Foo
    | Bar
  [@@deriving enum]
end = struct
  type t =
    | Foo
    | Bar
  [@@deriving enum]

  type simple_enum =
    | Foo
    | Bar
  [@@deriving enum]
end
