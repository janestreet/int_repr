module type T = sig
  type t [@@deriving compare, equal, hash, sexp, typerep]

  val signed : bool
  val num_bits : int
  val zero : t
  val min_value : t
  val max_value : t

end

let raise_cannot_repr ~mod_name ~to_string x =
  Base.raise_s [%message (mod_name ^ ": value cannot be represented") ~_:(to_string x)]
[@@cold]
;;

let trunc_unsigned : type a b. conv:(a -> b) -> logand:(a -> a -> a) -> mask:a -> a -> b =
  fun ~conv ~logand ~mask x -> conv (logand x mask)
[@@inline always]
;;

let exn_unsigned
  : type a b.
    conv:(a -> b)
    -> logand:(a -> a -> a)
    -> mask:a
    -> equal:(a -> a -> bool)
    -> mod_name:string
    -> to_string:(a -> string)
    -> a
    -> b
  =
  fun ~conv ~logand ~mask ~equal ~mod_name ~to_string x ->
  let y = trunc_unsigned ~conv:Fun.id ~logand ~mask x in
  if equal x y then conv y else raise_cannot_repr ~mod_name ~to_string x
[@@inline always]
;;

let trunc_signed
  : type a.
    shift_left:(a -> int -> a) -> shift_right:(a -> int -> a) -> shift:int -> a -> a
  =
  fun ~shift_left ~shift_right ~shift x -> shift_right (shift_left x shift) shift
[@@inline always]
;;

let exn_signed
  : type a.
    shift_left:(a -> int -> a)
    -> shift_right:(a -> int -> a)
    -> shift:int
    -> equal:(a -> a -> bool)
    -> mod_name:string
    -> to_string:(a -> string)
    -> a
    -> a
  =
  fun ~shift_left ~shift_right ~shift ~equal ~mod_name ~to_string x ->
  let y = trunc_signed ~shift_left ~shift_right ~shift x in
  if equal x y then y else raise_cannot_repr ~mod_name ~to_string x
;;

let identity_if_positive ~greater_equal ~zero ~mod_name ~to_string x =
  if greater_equal x zero then x else raise_cannot_repr ~mod_name ~to_string x
[@@inline always]
;;

module Repr32 = struct
  include Stdlib.Sys.Immediate64.Make (Base.Int) (Base.Int32)

  let to_int32 (x : t) : Base.Int32.t =
    match repr with
    | Immediate -> Base.Int32.of_int_trunc x
    | Non_immediate -> x
  ;;

  (* sign-extending conversion to int64 *)
  let to_int64 (x : t) : Base.Int64.t =
    match repr with
    | Immediate -> Base.Int64.of_int x
    | Non_immediate -> Base.Int64.of_int32 x
  ;;
end

(* Internal representation types *)
type int8 = int
type uint8 = int
type int16 = int
type uint16 = int
type int32 = Repr32.t
type uint32 = Repr32.t
type int63 = Base.Int63.t
type uint63 = Base.Int63.t
type int64 = Base.Int64.t
type uint64 = Base.Int64.t

module Int8 = struct
  type t = Base.Int.t [@@immediate] [@@deriving compare, equal, hash, sexp]

  let typerep_of_t = Typerep_lib.Std.typerep_of_int
  let typename_of_t = Typerep_lib.Std.typename_of_int
  let zero = Base.Int.zero
  let min_value = -128
  let max_value = 127
  let signed = true
  let num_bits = 8
  let shift_left = Base.Int.shift_left
  let shift_right = Base.Int.shift_right
  let shift = Base.Int.num_bits - num_bits
  let mod_name = "Int8"
  let to_string = Base.Int.to_string

  (* Utilities *)
  let trunc x = trunc_signed ~shift_left ~shift_right ~shift x [@@inline always]

  let exn x = exn_signed ~shift_left ~shift_right ~shift ~equal ~mod_name ~to_string x
  [@@inline always]
  ;;

  (* "Base" conversions. *)
  let of_base_int_trunc x = trunc x [@@inline always]
  let of_base_int_exn x = exn x [@@inline always]
  let to_base_int x = x [@@inline always]

  (* Same-signedness conversions. *)
  let of_int16_trunc x = trunc x [@@inline always]
  let of_int16_exn x = exn x [@@inline always]

  let of_int32_trunc x = trunc (Base.Int32.to_int_trunc (Repr32.to_int32 x))
  [@@inline always]
  ;;

  let of_int32_exn x = exn (Base.Int32.to_int_exn (Repr32.to_int32 x)) [@@inline always]
  let of_int63_trunc x = trunc (Base.Int63.to_int_trunc x) [@@inline always]
  let of_int63_exn x = exn (Base.Int63.to_int_exn x) [@@inline always]
  let of_int64_trunc x = trunc (Base.Int64.to_int_trunc x) [@@inline always]
  let of_int64_exn x = exn (Base.Int64.to_int_exn x) [@@inline always]

  (* Same-width conversions. *)
  let of_uint8_trunc x = trunc x [@@inline always]
  let of_uint8_exn x = exn x [@@inline always]
end

module Uint8 = struct
  type t = Base.Int.t [@@immediate] [@@deriving compare, equal, hash, sexp]

  let typerep_of_t = Typerep_lib.Std.typerep_of_int
  let typename_of_t = Typerep_lib.Std.typename_of_int
  let zero = Base.Int.zero
  let min_value = Base.Int.zero
  let max_value = 255
  let signed = false
  let num_bits = 8
  let conv = Base.Int64.to_int_trunc
  let logand = Base.Int.( land )
  let logand64 = Base.Int64.( land )
  let mask = 0xFF
  let mask64 = Base.Int64.of_int mask
  let equal = Base.Int.equal
  let equal64 = Base.Int64.equal
  let mod_name = "Uint8"
  let to_string = Base.Int.to_string
  let to_string64 = Base.Int64.to_string

  (* Utilities *)
  let trunc x = trunc_unsigned ~conv:Fun.id ~logand ~mask x [@@inline always]

  let exn x = exn_unsigned ~conv:Fun.id ~logand ~mask ~equal ~mod_name ~to_string x
  [@@inline always]
  ;;

  let trunc64 x = trunc_unsigned ~conv ~logand:logand64 ~mask:mask64 x [@@inline always]

  let exn64 x =
    exn_unsigned
      ~conv
      ~logand:logand64
      ~mask:mask64
      ~equal:equal64
      ~mod_name
      ~to_string:to_string64
      x
  [@@inline always]
  ;;

  (* "Base" conversions. *)
  let of_base_int_trunc x = trunc x [@@inline always]
  let of_base_int_exn x = exn x [@@inline always]
  let to_base_int x = x [@@inline always]

  (* Same-signedness conversions. *)
  let of_uint16_trunc x = trunc x [@@inline always]
  let of_uint16_exn x = exn x [@@inline always]
  let of_uint32_trunc x = trunc64 (Repr32.to_int64 x) [@@inline always]
  let of_uint32_exn x = exn64 (Repr32.to_int64 x) [@@inline always]
  let of_uint63_trunc x = trunc64 (Base.Int63.to_int64 x) [@@inline always]
  let of_uint63_exn x = exn64 (Base.Int63.to_int64 x) [@@inline always]
  let of_uint64_trunc x = trunc64 x [@@inline always]
  let of_uint64_exn x = exn64 x [@@inline always]

  (* Same-width conversions. *)
  let of_int8_trunc x = trunc x [@@inline always]
  let of_int8_exn x = exn x [@@inline always]
end

module Int16 = struct
  type t = Base.Int.t [@@immediate] [@@deriving compare, equal, hash, sexp]

  let typerep_of_t = Typerep_lib.Std.typerep_of_int
  let typename_of_t = Typerep_lib.Std.typename_of_int
  let zero = Base.Int.zero
  let min_value = -32768
  let max_value = 32767
  let signed = true
  let num_bits = 16
  let shift_left = Base.Int.shift_left
  let shift_right = Base.Int.shift_right
  let shift = Base.Int.num_bits - num_bits
  let mod_name = "Int16"
  let to_string = Base.Int.to_string

  (* Utilities *)
  let trunc x = trunc_signed ~shift_left ~shift_right ~shift x [@@inline always]

  let exn x = exn_signed ~shift_left ~shift_right ~shift ~equal ~mod_name ~to_string x
  [@@inline always]
  ;;

  (* "Base" conversions. *)
  let of_base_int_trunc x = trunc x [@@inline always]
  let of_base_int_exn x = exn x [@@inline always]
  let to_base_int x = x [@@inline always]

  (* Same-signedness conversions. *)
  let of_int8 x = x [@@inline always]

  let of_int32_trunc x = trunc (Base.Int32.to_int_trunc (Repr32.to_int32 x))
  [@@inline always]
  ;;

  let of_int32_exn x = exn (Base.Int32.to_int_exn (Repr32.to_int32 x)) [@@inline always]
  let of_int63_trunc x = trunc (Base.Int63.to_int_trunc x) [@@inline always]
  let of_int63_exn x = exn (Base.Int63.to_int_exn x) [@@inline always]
  let of_int64_trunc x = trunc (Base.Int64.to_int_trunc x) [@@inline always]
  let of_int64_exn x = exn (Base.Int64.to_int_exn x) [@@inline always]

  (* Same-width conversions. *)
  let of_uint16_trunc x = trunc x [@@inline always]
  let of_uint16_exn x = exn x [@@inline always]
end

module Uint16 = struct
  type t = Base.Int.t [@@immediate] [@@deriving compare, equal, hash, sexp]

  let typerep_of_t = Typerep_lib.Std.typerep_of_int
  let typename_of_t = Typerep_lib.Std.typename_of_int
  let zero = Base.Int.zero
  let min_value = Base.Int.zero
  let max_value = 65535
  let signed = false
  let num_bits = 16
  let conv = Base.Int64.to_int_trunc
  let logand = Base.Int.( land )
  let logand64 = Base.Int64.( land )
  let mask = 0xFFFF
  let mask64 = Base.Int64.of_int mask
  let equal = Base.Int.equal
  let equal64 = Base.Int64.equal
  let mod_name = "Uint16"
  let to_string = Base.Int.to_string
  let to_string64 = Base.Int64.to_string

  (* Utilities *)
  let trunc x = trunc_unsigned ~conv:Fun.id ~logand ~mask x [@@inline always]

  let exn x = exn_unsigned ~conv:Fun.id ~logand ~mask ~equal ~mod_name ~to_string x
  [@@inline always]
  ;;

  let trunc64 x = trunc_unsigned ~conv ~logand:logand64 ~mask:mask64 x [@@inline always]

  let exn64 x =
    exn_unsigned
      ~conv
      ~logand:logand64
      ~mask:mask64
      ~equal:equal64
      ~mod_name
      ~to_string:to_string64
      x
  [@@inline always]
  ;;

  (* "Base" conversions. *)
  let of_base_int_trunc x = trunc x [@@inline always]
  let of_base_int_exn x = exn x [@@inline always]
  let to_base_int x = x [@@inline always]

  (* Same-signedness conversions. *)
  let of_uint8 x = x [@@inline always]
  let of_uint32_trunc x = trunc64 (Repr32.to_int64 x) [@@inline always]
  let of_uint32_exn x = exn64 (Repr32.to_int64 x) [@@inline always]
  let of_uint63_trunc x = trunc64 (Base.Int63.to_int64 x) [@@inline always]
  let of_uint63_exn x = exn64 (Base.Int63.to_int64 x) [@@inline always]
  let of_uint64_trunc x = trunc64 x [@@inline always]
  let of_uint64_exn x = exn64 x [@@inline always]

  (* Same-width conversions. *)
  let of_int16_trunc x = trunc x [@@inline always]
  let of_int16_exn x = exn x [@@inline always]
end

module type Backend32_S = sig
  module Signed : sig
    type t = Repr32.t

    include T with type t := t

    (* "Base" conversions. *)
    val of_base_int32 : Base.Int32.t -> t
    val to_base_int32 : t -> Base.Int32.t

    (* Same-signedness conversions. *)
    val of_int8 : int8 -> t
    val of_int16 : int16 -> t
    val of_int63_trunc : int63 -> t
    val of_int63_exn : int63 -> t
    val of_int64_trunc : int64 -> t
    val of_int64_exn : int64 -> t

    (* Same-width conversions. *)
    val of_uint32_trunc : uint32 -> t
    val of_uint32_exn : uint32 -> t
  end

  module Unsigned : sig
    type t = Repr32.t

    include T with type t := t

    (* "Base" conversions. *)
    val of_base_int32_trunc : Base.Int32.t -> t
    val of_base_int32_exn : Base.Int32.t -> t
    val to_base_int32_trunc : t -> Base.Int32.t
    val to_base_int32_exn : t -> Base.Int32.t
    val of_base_int64_trunc : Base.Int64.t -> t
    val of_base_int64_exn : Base.Int64.t -> t
    val to_base_int64 : t -> Base.Int64.t

    (* Same-signedness conversions. *)
    val of_uint8 : uint8 -> t
    val of_uint16 : uint16 -> t
    val of_uint63_trunc : uint63 -> t
    val of_uint63_exn : uint63 -> t
    val of_uint64_trunc : uint64 -> t
    val of_uint64_exn : uint64 -> t

    (* Same-width conversions. *)
    val of_int32_trunc : int32 -> t
    val of_int32_exn : int32 -> t
  end
end

module Backend32 : sig
  val impl : (module Backend32_S)
end = struct
  module I = struct
    module Signed = struct
      type t = Base.Int.t [@@immediate] [@@deriving compare, equal, hash, sexp]

      let typerep_of_t = Typerep_lib.Std.typerep_of_int
      let typename_of_t = Typerep_lib.Std.typename_of_int
      let zero = Base.Int.zero
      let min_value = Base.Int32.to_int_trunc Base.Int32.min_value
      let max_value = Base.Int32.to_int_trunc Base.Int32.max_value
      let signed = true
      let num_bits = 32
      let shift_left = Base.Int.shift_left
      let shift_right = Base.Int.shift_right
      let shift = Base.Int.num_bits - num_bits
      let mod_name = "Int32"
      let to_string = Base.Int.to_string

      (* Utilities *)
      let trunc x = trunc_signed ~shift_left ~shift_right ~shift x [@@inline always]

      let exn x = exn_signed ~shift_left ~shift_right ~shift ~equal ~mod_name ~to_string x
      [@@inline always]
      ;;

      (* "Base" conversions. *)
      let of_base_int32 x = Base.Int32.to_int_trunc x [@@inline always]
      let to_base_int32 x = Base.Int32.of_int_trunc x [@@inline always]

      (* Same-signedness conversions. *)
      let of_int8 x = (x : Int8.t :> int) [@@inline always]
      let of_int16 x = (x : Int16.t :> int) [@@inline always]
      let of_int63_trunc x = trunc (Base.Int63.to_int_trunc x) [@@inline always]
      let of_int63_exn x = exn (Base.Int63.to_int_exn x) [@@inline always]
      let of_int64_trunc x = trunc (Base.Int64.to_int_trunc x) [@@inline always]
      let of_int64_exn x = exn (Base.Int64.to_int_exn x) [@@inline always]

      (* Same-width conversions. *)
      let of_uint32_trunc x = trunc x
      let of_uint32_exn x = exn x
    end

    module Unsigned = struct
      type t = Base.Int.t [@@immediate] [@@deriving compare, equal, hash, sexp]

      let typerep_of_t = Typerep_lib.Std.typerep_of_int
      let typename_of_t = Typerep_lib.Std.typename_of_int
      let zero = Base.Int.zero
      let min_value = Base.Int.zero
      let max_value = Base.Int.of_int64_trunc 4294967295L
      let signed = false
      let num_bits = 32
      let conv = Base.Int64.to_int_trunc
      let logand = Base.Int.( land )
      let logand64 = Base.Int64.( land )
      let mask = Base.Int64.to_int_trunc 0xFFFFFFFFL
      let mask64 = Base.Int64.of_int mask
      let equal = Base.Int.equal
      let equal64 = Base.Int64.equal
      let mod_name = "Uint32"
      let to_string = Base.Int.to_string
      let to_string64 = Base.Int64.to_string

      (* Utilities *)
      let trunc x = trunc_unsigned ~conv:Fun.id ~logand ~mask x [@@inline always]

      let exn x = exn_unsigned ~conv:Fun.id ~logand ~mask ~equal ~mod_name ~to_string x
      [@@inline always]
      ;;

      let trunc64 x = trunc_unsigned ~conv ~logand:logand64 ~mask:mask64 x
      [@@inline always]
      ;;

      let exn64 x =
        exn_unsigned
          ~conv
          ~logand:logand64
          ~mask:mask64
          ~equal:equal64
          ~mod_name
          ~to_string:to_string64
          x
      [@@inline always]
      ;;

      (* "Base" conversions. *)
      let of_base_int32_trunc x = trunc (Base.Int32.to_int_trunc x) [@@inline always]
      let of_base_int32_exn x = exn (Base.Int32.to_int_trunc x) [@@inline always]
      let to_base_int32_trunc x = Base.Int.to_int32_trunc x [@@inline always]

      let to_base_int32_exn x =
        if Base.Int.( <= ) x (Base.Int.of_int64_trunc 2147483647L)
        then Base.Int32.of_int_trunc x
        else raise_cannot_repr ~mod_name ~to_string:Base.Int.to_string x
      [@@inline always]
      ;;

      let of_base_int64_trunc x = trunc64 x [@@inline always]
      let of_base_int64_exn x = exn64 x [@@inline always]
      let to_base_int64 x = Base.Int.to_int64 x [@@inline always]

      (* Same-signedness conversions. *)
      let of_uint8 x = (x : Uint8.t :> int) [@@inline always]
      let of_uint16 x = (x : Uint16.t :> int) [@@inline always]
      let of_uint63_trunc x = trunc64 (Base.Int63.to_int64 x) [@@inline always]
      let of_uint63_exn x = exn64 (Base.Int63.to_int64 x) [@@inline always]
      let of_uint64_trunc x = trunc64 x [@@inline always]
      let of_uint64_exn x = exn64 x [@@inline always]

      (* Same-width conversions. *)
      let of_int32_trunc x = trunc x [@@inline always]
      let of_int32_exn x = exn x [@@inline always]
    end
  end

  module N = struct
    module Signed = struct
      type t = Base.Int32.t [@@deriving compare, equal, hash, sexp]

      let typerep_of_t = Typerep_lib.Std.typerep_of_int32
      let typename_of_t = Typerep_lib.Std.typename_of_int32
      let zero = Base.Int32.zero
      let min_value = Base.Int32.min_value
      let max_value = Base.Int32.max_value
      let signed = true
      let num_bits = 32
      let mod_name = "Int32"
      let greater_equal = Base.Int32.( >= )
      let to_string = Base.Int32.to_string

      (* "Base" conversions. *)
      let of_base_int32 x = x [@@inline always]
      let to_base_int32 x = x [@@inline always]

      (* Same-signedness conversions. *)
      let of_int8 x = Base.Int32.of_int_trunc (x : Int8.t :> int) [@@inline always]
      let of_int16 x = Base.Int32.of_int_trunc (x : Int16.t :> int) [@@inline always]
      let of_int63_trunc x = Base.Int63.to_int32_trunc x [@@inline always]
      let of_int63_exn x = Base.Int63.to_int32_exn x [@@inline always]
      let of_int64_trunc x = Base.Int64.to_int32_trunc x [@@inline always]
      let of_int64_exn x = Base.Int64.to_int32_exn x [@@inline always]

      (* Same-width conversions. *)
      let of_uint32_trunc x = x [@@inline always]

      let of_uint32_exn x : t =
        identity_if_positive ~greater_equal ~zero ~mod_name ~to_string x
      [@@inline always]
      ;;
    end

    module Unsigned = struct
      type t = Base.Int32.t [@@deriving equal, hash, sexp]

      let compare x y = Stdlib.Int32.unsigned_compare x y [@@inline always]
      let typerep_of_t = Typerep_lib.Std.typerep_of_int32
      let typename_of_t = Typerep_lib.Std.typename_of_int32
      let zero = Base.Int32.zero
      let min_value = Base.Int32.zero
      let max_value = -1l
      let signed = false
      let num_bits = 32
      let mod_name = "Uint32"
      let greater_equal = Base.Int32.( >= )
      let to_string = Base.Int32.to_string

      (* "Base" conversions. *)
      let of_base_int32_trunc x = x [@@inline always]

      let of_base_int32_exn x =
        identity_if_positive ~greater_equal ~zero ~mod_name ~to_string x
      [@@inline always]
      ;;

      let to_base_int32_trunc x = x [@@inline always]

      let to_base_int32_exn x =
        identity_if_positive ~greater_equal ~zero ~mod_name ~to_string x
      [@@inline always]
      ;;

      let of_base_int64_trunc x = Base.Int32.of_int64_trunc x [@@inline always]

      let of_base_int64_exn x =
        if Base.Int64.( >= ) x 0L && Base.Int64.( <= ) x 4294967295L
        then Base.Int64.to_int32_trunc x
        else raise_cannot_repr ~mod_name ~to_string:Base.Int64.to_string x
      [@@inline always]
      ;;

      let to_base_int64 x = Base.Int64.( land ) (Base.Int32.to_int64 x) 0xFFFFFFFFL
      [@@inline always]
      ;;

      (* Same-signedness conversions. *)
      let of_uint8 x = Base.Int32.of_int_trunc (x : Uint8.t :> int) [@@inline always]
      let of_uint16 x = Base.Int32.of_int_trunc (x : Uint16.t :> int) [@@inline always]

      let of_uint63_trunc x = of_base_int64_trunc (Base.Int63.to_int64 x)
      [@@inline always]
      ;;

      let of_uint63_exn x = of_base_int64_exn (Base.Int63.to_int64 x) [@@inline always]
      let of_uint64_trunc x = of_base_int64_trunc x [@@inline always]
      let of_uint64_exn x = of_base_int64_exn x [@@inline always]

      (* Same-width conversions. *)
      let of_int32_trunc x = x [@@inline always]

      let of_int32_exn x =
        identity_if_positive ~greater_equal ~zero ~mod_name ~to_string x
      [@@inline always]
      ;;
    end
  end

  let impl : (module Backend32_S) =
    match Repr32.repr with
    | Immediate -> (module I : Backend32_S)
    | Non_immediate -> (module N : Backend32_S)
  ;;
end

module Int32 = struct
  module M = (val Backend32.impl : Backend32_S)
  include M.Signed
end

module Uint32 = struct
  module M = (val Backend32.impl : Backend32_S)
  include M.Unsigned
end

module Int63 = struct
  type t = Base.Int63.t [@@immediate64] [@@deriving compare, equal, hash, sexp]

  let typerep_of_t = Typerep_lib.Std.typerep_of_int63
  let typename_of_t = Typerep_lib.Std.typename_of_int63
  let zero = Base.Int63.zero
  let min_value = Base.Int63.min_value
  let max_value = Base.Int63.max_value
  let signed = true
  let num_bits = 63
  let mod_name = "Int63"
  let greater_equal = Base.Int63.( >= )
  let to_string = Base.Int63.to_string

  (* Same-signedness conversions. *)
  let of_int8 x = Base.Int63.of_int x [@@inline always]
  let of_int16 x = Base.Int63.of_int x [@@inline always]
  let of_int32 x = Base.Int63.of_int32 (Int32.to_base_int32 x) [@@inline always]
  let of_int64_trunc x = Base.Int63.of_int64_trunc x [@@inline always]
  let of_int64_exn x = Base.Int63.of_int64_exn x [@@inline always]

  (* Same-width conversions. *)
  let of_uint63_trunc x = x [@@inline always]

  let of_uint63_exn x = identity_if_positive ~greater_equal ~zero ~mod_name ~to_string x
  [@@inline always]
  ;;
end

module Uint63 = struct
  type t = Base.Int63.t [@@deriving equal, hash, sexp]

  let compare x y =
    (* x and y are sign-extended, which preserves the high bit *)
    Stdlib.Int64.unsigned_compare (Base.Int63.to_int64 x) (Base.Int63.to_int64 y)
  ;;

  let typerep_of_t = Typerep_lib.Std.typerep_of_int63
  let typename_of_t = Typerep_lib.Std.typename_of_int63
  let zero = Base.Int63.zero
  let min_value = Base.Int63.zero
  let max_value = Base.Int63.of_int64_trunc 9223372036854775807L
  let signed = false
  let num_bits = 63
  let greater_equal = Base.Int63.( >= )
  let mod_name = "Uint63"
  let to_string = Base.Int63.to_string

  (* Utilities *)
  let exn x = identity_if_positive ~greater_equal ~zero ~mod_name ~to_string x
  [@@inline always]
  ;;

  (* "Base" conversions. *)
  let of_base_int64_trunc x = Base.Int63.of_int64_trunc x [@@inline always]

  let of_base_int64_exn x =
    if Base.Int64.( >= ) x 0L
    then Base.Int63.of_int64_trunc x
    else raise_cannot_repr ~mod_name ~to_string:Base.Int64.to_string x
  [@@inline always]
  ;;

  let to_base_int64 x = Base.Int64.( land ) (Base.Int63.to_int64 x) 0x7FFFFFFFFFFFFFFFL
  [@@inline always]
  ;;

  (* Same-signedness conversions. *)
  let of_uint8 x = Base.Int63.of_int x [@@inline always]
  let of_uint16 x = Base.Int63.of_int x [@@inline always]
  let of_uint32 x = Base.Int63.of_int64_trunc (Uint32.to_base_int64 x) [@@inline always]
  let of_uint64_trunc x = Base.Int63.of_int64_trunc x [@@inline always]

  let of_uint64_exn (x : uint64) : t =
    if Base.Int64.( >= ) x 0L
    then Base.Int63.of_int64_trunc x
    else raise_cannot_repr ~mod_name ~to_string:Base.Int64.to_string x
  [@@inline always]
  ;;

  (* Same-width conversions. *)
  let of_int63_trunc x = x [@@inline always]
  let of_int63_exn x = exn x [@@inline always]
end

module Int64 = struct
  type t = Base.Int64.t [@@deriving compare, equal, hash, sexp]

  let typerep_of_t = Typerep_lib.Std.typerep_of_int64
  let typename_of_t = Typerep_lib.Std.typename_of_int64
  let zero = Base.Int64.zero
  let min_value = Base.Int64.min_value
  let max_value = Base.Int64.max_value
  let signed = true
  let num_bits = 64
  let mod_name = "Int64"
  let greater_equal = Base.Int64.( >= )
  let to_string = Base.Int64.to_string

  (* Same-signedness conversions. *)
  let of_int8 x = Base.Int64.of_int x [@@inline always]
  let of_int16 x = Base.Int64.of_int x [@@inline always]
  let of_int32 x = Base.Int64.of_int32 (Int32.to_base_int32 x) [@@inline always]
  let of_int63 x = Base.Int63.to_int64 x [@@inline always]

  (* Same-width conversions. *)
  let of_uint64_trunc x = x [@@inline always]

  let of_uint64_exn x = identity_if_positive ~greater_equal ~zero ~mod_name ~to_string x
  [@@inline always]
  ;;
end

module Uint64 = struct
  type t = Base.Int64.t [@@deriving equal, hash, sexp]

  let compare = Stdlib.Int64.unsigned_compare
  let typerep_of_t = Typerep_lib.Std.typerep_of_int64
  let typename_of_t = Typerep_lib.Std.typename_of_int64
  let zero = Base.Int64.zero
  let min_value = Base.Int64.zero
  let max_value = -1L
  let signed = false
  let num_bits = 64
  let greater_equal = Base.Int64.( >= )
  let mod_name = "Uint64"
  let to_string = Base.Int64.to_string

  (* Utilities *)
  let exn x = identity_if_positive ~greater_equal ~zero ~mod_name ~to_string x
  [@@inline always]
  ;;

  (* "Base" conversions. *)
  let of_base_int64_trunc x = x [@@inline always]
  let of_base_int64_exn x = exn x [@@inline always]
  let to_base_int64_trunc x = x [@@inline always]
  let to_base_int64_exn x = exn x [@@inline always]

  (* Same-signedness conversions. *)
  let of_uint8 x = Base.Int64.of_int x [@@inline always]
  let of_uint16 x = Base.Int64.of_int x [@@inline always]
  let of_uint32 x = Uint32.to_base_int64 x [@@inline always]

  let of_uint63 (x : uint63) : int64 =
    Base.Int64.( land ) (Base.Int63.to_int64 x) 0x7FFFFFFFFFFFFFFFL
  [@@inline always]
  ;;

  (* Same-width conversions. *)
  let of_int64_trunc x = x [@@inline always]
  let of_int64_exn x = exn x [@@inline always]
end

module type Get = sig
  type t

  (* 8-bit signed values *)

  val get_int8 : t -> pos:int -> int8

  (* 8-bit unsigned values *)

  val get_uint8 : t -> pos:int -> uint8

  (* 16-bit signed values *)

  val get_int16_le : t -> pos:int -> int16
  val get_int16_be : t -> pos:int -> int16

  (* 16-bit unsigned values *)

  val get_uint16_le : t -> pos:int -> uint16
  val get_uint16_be : t -> pos:int -> uint16

  (* 32-bit signed values *)

  val get_int32_le : t -> pos:int -> int32
  val get_int32_be : t -> pos:int -> int32

  (* 32-bit unsigned values *)

  val get_uint32_le : t -> pos:int -> uint32
  val get_uint32_be : t -> pos:int -> uint32

  (* 64-bit signed values *)

  val get_int64_le : t -> pos:int -> int64
  val get_int64_be : t -> pos:int -> int64

  (* 64-bit unsigned values *)

  val get_uint64_le : t -> pos:int -> uint64
  val get_uint64_be : t -> pos:int -> uint64
end

module type Set = sig
  type t

  (* 8-bit signed values *)

  val set_int8 : t -> pos:int -> int8 -> unit

  (* 8-bit unsigned values *)

  val set_uint8 : t -> pos:int -> uint8 -> unit

  (* 16-bit signed values *)

  val set_int16_le : t -> pos:int -> int16 -> unit
  val set_int16_be : t -> pos:int -> int16 -> unit

  (* 16-bit unsigned values *)

  val set_uint16_le : t -> pos:int -> uint16 -> unit
  val set_uint16_be : t -> pos:int -> uint16 -> unit

  (* 32-bit signed values *)

  val set_int32_le : t -> pos:int -> int32 -> unit
  val set_int32_be : t -> pos:int -> int32 -> unit

  (* 32-bit unsigned values *)

  val set_uint32_le : t -> pos:int -> uint32 -> unit
  val set_uint32_be : t -> pos:int -> uint32 -> unit

  (* 64-bit signed values *)

  val set_int64_le : t -> pos:int -> int64 -> unit
  val set_int64_be : t -> pos:int -> int64 -> unit

  (* 64-bit unsigned values *)

  val set_uint64_le : t -> pos:int -> uint64 -> unit
  val set_uint64_be : t -> pos:int -> uint64 -> unit
end

module type Get_functions = sig
  type t

  (* The following functions must use native endianness (hence the `_ne` suffix). *)
  val get_uint8 : t -> int -> Base.Int.t
  val get_uint16_ne : t -> int -> Base.Int.t
  val get_int32_ne : t -> int -> Base.Int32.t
  val get_int64_ne : t -> int -> Base.Int64.t
end

module type Set_functions = sig
  type t

  (* The following functions must use native endianness (hence the `_ne` suffix). *)
  val set_uint8 : t -> int -> Base.Int.t -> unit
  val set_uint16_ne : t -> int -> Base.Int.t -> unit
  val set_int32_ne : t -> int -> Base.Int32.t -> unit
  val set_int64_ne : t -> int -> Base.Int64.t -> unit
end

external swap16 : int -> int = "%bswap16"
external swap32 : Caml.Int32.t -> Caml.Int32.t = "%bswap_int32"
external swap64 : Caml.Int64.t -> Caml.Int64.t = "%bswap_int64"

module Make_get (F : Get_functions) : Get with type t := F.t = struct
  (* 8-bit signed values *)

  let get_int8 t ~pos = Int8.of_base_int_trunc (F.get_uint8 t pos)

  (* 8-bit unsigned values *)

  let get_uint8 t ~pos = Uint8.of_base_int_trunc (F.get_uint8 t pos)

  (* 16-bit signed values *)

  let get_int16_le t ~pos =
    let x = F.get_uint16_ne t pos in
    Int16.of_base_int_trunc (if Sys.big_endian then swap16 x else x)
  ;;

  let get_int16_be t ~pos =
    let x = F.get_uint16_ne t pos in
    Int16.of_base_int_trunc (if Sys.big_endian then x else swap16 x)
  ;;

  (* 16-bit unsigned values *)

  let get_uint16_le t ~pos =
    let x = F.get_uint16_ne t pos in
    Uint16.of_base_int_trunc (if Sys.big_endian then swap16 x else x)
  ;;

  let get_uint16_be t ~pos =
    let x = F.get_uint16_ne t pos in
    Uint16.of_base_int_trunc (if Sys.big_endian then x else swap16 x)
  ;;

  (* 32-bit signed values *)

  let get_int32_le t ~pos =
    let x = F.get_int32_ne t pos in
    Int32.of_base_int32 (if Sys.big_endian then swap32 x else x)
  ;;

  let get_int32_be t ~pos =
    let x = F.get_int32_ne t pos in
    Int32.of_base_int32 (if Sys.big_endian then x else swap32 x)
  ;;

  (* 32-bit unsigned values *)

  let get_uint32_le t ~pos =
    let x = F.get_int32_ne t pos in
    Uint32.of_base_int32_trunc (if Sys.big_endian then swap32 x else x)
  ;;

  let get_uint32_be t ~pos =
    let x = F.get_int32_ne t pos in
    Uint32.of_base_int32_trunc (if Sys.big_endian then x else swap32 x)
  ;;

  (* 64-bit signed values *)

  let get_int64_le t ~pos =
    let x = F.get_int64_ne t pos in
    if Sys.big_endian then swap64 x else x
  ;;

  let get_int64_be t ~pos =
    let x = F.get_int64_ne t pos in
    if Sys.big_endian then x else swap64 x
  ;;

  (* 64-bit unsigned values *)

  let get_uint64_le t ~pos =
    let x = F.get_int64_ne t pos in
    Uint64.of_base_int64_trunc (if Sys.big_endian then swap64 x else x)
  ;;

  let get_uint64_be t ~pos =
    let x = F.get_int64_ne t pos in
    Uint64.of_base_int64_trunc (if Sys.big_endian then x else swap64 x)
  ;;
end
[@@inline always]

module Make_set (F : Set_functions) : Set with type t := F.t = struct
  (* 8-bit unsigned values *)

  let set_uint8 t ~pos x = F.set_uint8 t pos x

  (* 8-bit signed values *)

  let set_int8 t ~pos x = set_uint8 t ~pos (Uint8.of_int8_trunc x)

  (* 16-bit unsigned values *)

  let set_uint16_le t ~pos x =
    let x = if Sys.big_endian then swap16 x else x in
    F.set_uint16_ne t pos x
  ;;

  let set_uint16_be t ~pos x =
    let x = if Sys.big_endian then x else swap16 x in
    F.set_uint16_ne t pos x
  ;;

  (* 16-bit signed values *)

  let set_int16_le t ~pos x = set_uint16_le t ~pos (Uint16.of_int16_trunc x)
  let set_int16_be t ~pos x = set_uint16_be t ~pos (Uint16.of_int16_trunc x)

  (* 32-bit signed values *)

  let set_int32_le t ~pos x =
    let x = Int32.to_base_int32 x in
    let x = if Sys.big_endian then swap32 x else x in
    F.set_int32_ne t pos x
  ;;

  let set_int32_be t ~pos x =
    let x = Int32.to_base_int32 x in
    let x = if Sys.big_endian then x else swap32 x in
    F.set_int32_ne t pos x
  ;;

  (* 32-bit unsigned values *)

  let set_uint32_le t ~pos x = set_int32_le t ~pos (Int32.of_uint32_trunc x)
  let set_uint32_be t ~pos x = set_int32_be t ~pos (Int32.of_uint32_trunc x)

  (* 64-bit signed values *)

  let set_int64_le t ~pos x =
    let x = if Sys.big_endian then swap64 x else x in
    F.set_int64_ne t pos x
  ;;

  let set_int64_be t ~pos x =
    let x = if Sys.big_endian then x else swap64 x in
    F.set_int64_ne t pos x
  ;;

  (* 64-bit unsigned values *)

  let set_uint64_le t ~pos x = set_int64_le t ~pos (Int64.of_uint64_trunc x)
  let set_uint64_be t ~pos x = set_int64_be t ~pos (Int64.of_uint64_trunc x)
end
[@@inline always]

module Bytes0Unsafe = struct
  type t = Bytes.t

  external get_uint8 : Bytes.t -> int -> int = "%bytes_unsafe_get"
  external get_uint16_ne : Bytes.t -> int -> int = "%caml_bytes_get16u"
  external get_int32_ne : Bytes.t -> int -> Caml.Int32.t = "%caml_bytes_get32u"
  external get_int64_ne : Bytes.t -> int -> Caml.Int64.t = "%caml_bytes_get64u"
  external set_uint8 : Bytes.t -> int -> int -> unit = "%bytes_unsafe_set"
  external set_uint16_ne : Bytes.t -> int -> int -> unit = "%caml_bytes_set16u"
  external set_int32_ne : Bytes.t -> int -> Caml.Int32.t -> unit = "%caml_bytes_set32u"
  external set_int64_ne : Bytes.t -> int -> Caml.Int64.t -> unit = "%caml_bytes_set64u"
end

module Bytes = struct
  include Make_get (Bytes)
  include Make_set (Bytes)

  module Unsafe = struct
    include Make_get (Bytes0Unsafe)
    include Make_set (Bytes0Unsafe)
  end
end

module String0 = struct
  include String

  external get_uint8 : String.t -> int -> int = "%string_safe_get"
  external get_uint16_ne : String.t -> int -> int = "%caml_string_get16"
  external get_int32_ne : String.t -> int -> Caml.Int32.t = "%caml_string_get32"
  external get_int64_ne : String.t -> int -> Caml.Int64.t = "%caml_string_get64"
end

module String0Unsafe = struct
  type t = String.t

  external get_uint8 : String.t -> int -> int = "%string_unsafe_get"
  external get_uint16_ne : String.t -> int -> int = "%caml_string_get16u"
  external get_int32_ne : String.t -> int -> Caml.Int32.t = "%caml_string_get32u"
  external get_int64_ne : String.t -> int -> Caml.Int64.t = "%caml_string_get64u"
end

module String = struct
  include Make_get (String0)

  module Unsafe = struct
    include Make_get (String0Unsafe)
  end
end
