module type Sig_module = sig
  type simple_enum =
    | Foo
    | Bar
  [@@deriving enum]
end

module type Sig_module_with_t_type = sig
  type t =
    | Foo
    | Bar
  [@@deriving enum]
end

type simple_enum =
  | Foo
  | Bar
[@@deriving enum]

module Sig_module_with_subsequent_implementation : sig
  type simple_enum =
    | Foo
    | Bar
  [@@deriving enum]
end = struct
  type simple_enum =
    | Foo
    | Bar
  [@@deriving enum]
end

module Sig_module_with_subsequent_implementation_and_t_type : sig
  type t =
    | Foo
    | Bar
  [@@deriving enum]
end = struct
  type t =
    | Foo
    | Bar
  [@@deriving enum]
end
