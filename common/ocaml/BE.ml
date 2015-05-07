(* this fails with dangling module for me

module EU = struct include ExtUnix.Specific end
module BE = struct include EU.BigEndian end

   So I provide some basic functions here for the time being.
*)

let get_uint8 buf off = int_of_char (Bytes.get buf off)

let set_uint8 buf off v = Bytes.set buf off (char_of_int v)

let get_uint16 buf off =
  let x0 = int_of_char (Bytes.get buf (off + 0)) in
  let x1 = int_of_char (Bytes.get buf (off + 1))
  in
  (x0 lsl 8) + x1

let set_uint16 buf off v =
  let x0 = v asr 8 in
  let x1 = v mod 256
  in
  Bytes.set buf (off + 0) (char_of_int x0);
  Bytes.set buf (off + 1) (char_of_int x1)

let get_uint31 buf off =
  let x0 = int_of_char (Bytes.get buf (off + 0)) in
  let x1 = int_of_char (Bytes.get buf (off + 1)) in
  let x2 = int_of_char (Bytes.get buf (off + 2)) in
  let x3 = int_of_char (Bytes.get buf (off + 3))
  in
  (x0 lsl 24) + (x1 lsl 16) + (x2 lsl 8) + x3

let set_uint31 buf off v =
  let x0 = v asr 24 in
  let x1 = (v asr 16) mod 256 in
  let x2 = (v asr 8) mod 256 in
  let x3 = v mod 256
  in
  Bytes.set buf (off + 0) (char_of_int x0);
  Bytes.set buf (off + 1) (char_of_int x1);
  Bytes.set buf (off + 2) (char_of_int x2);
  Bytes.set buf (off + 3) (char_of_int x3)

let get_str buf off len = Bytes.sub buf off len

let set_str buf off s =
  let len = Bytes.length s
  in
  Bytes.blit s 0 buf off len
