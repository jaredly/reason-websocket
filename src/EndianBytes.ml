external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"

external set_16 : Bytes.t -> int -> int -> unit = "%caml_string_set16"
external get_16 : string -> int -> int = "%caml_string_get16"

external get_32 : string -> int -> int32 = "%caml_string_get32"
external get_64 : string -> int -> int64 = "%caml_string_get64"

external set_32 : Bytes.t -> int -> int32 -> unit = "%caml_string_set32"
external set_64 : Bytes.t -> int -> int64 -> unit = "%caml_string_set64"

let get_char (s:string) off =
  String.get s off
let set_char (s:Bytes.t) off v =
  Bytes.set s off v
let unsafe_get_char (s:string) off =
  String.unsafe_get s off
let unsafe_set_char (s:Bytes.t) off v =
  Bytes.unsafe_set s off v

let sign8 v =
  (v lsl ( Sys.word_size - 9 )) asr ( Sys.word_size - 9 )

let sign16 v =
  (v lsl ( Sys.word_size - 17 )) asr ( Sys.word_size - 17 )

let get_uint8 s off =
  Char.code (get_char s off)
let get_int8 s off =
  ((get_uint8 s off) lsl ( Sys.word_size - 9 )) asr ( Sys.word_size - 9 )
let set_int8 s off v =
  (* It is ok to cast using unsafe_chr because both String.set
     and Bigarray.Array1.set (on bigstrings) use the 'store unsigned int8'
     primitives that effectively extract the bits before writing *)
  set_char s off (Char.unsafe_chr v)

let unsafe_get_uint8 s off =
  Char.code (unsafe_get_char s off)
let unsafe_get_int8 s off =
  ((unsafe_get_uint8 s off) lsl ( Sys.word_size - 9 )) asr ( Sys.word_size - 9 )
let unsafe_set_int8 s off v =
  unsafe_set_char s off (Char.unsafe_chr v)

let set_int16 s off v =
    if not Sys.big_endian
    then (set_16 s off (swap16 v))
    else set_16 s off v

let get_uint16 s off =
    if not Sys.big_endian
    then swap16 (get_16 s off)
    else get_16 s off

  let get_int64 s off =
    if not Sys.big_endian
    then swap64 (get_64 s off)
    else get_64 s off


      let get_uint16 s off =
    if not Sys.big_endian
    then swap16 (get_16 s off)
    else get_16 s off

  let get_int16 s off =
   ((get_uint16 s off) lsl ( Sys.word_size - 17 )) asr ( Sys.word_size - 17 )

  let get_int32 s off =
    if not Sys.big_endian
    then swap32 (get_32 s off)
    else get_32 s off

  let get_int64 s off =
    if not Sys.big_endian
    then swap64 (get_64 s off)
    else get_64 s off

  let set_int16 s off v =
    if not Sys.big_endian
    then (set_16 s off (swap16 v))
    else set_16 s off v

  let set_int32 s off v =
    if not Sys.big_endian
    then set_32 s off (swap32 v)
    else set_32 s off v

  let set_int64 s off v =
    if not Sys.big_endian
    then set_64 s off (swap64 v)
    else set_64 s off v