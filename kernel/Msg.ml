module BE = struct include OneMQCommon.BE end
  
module Hello = struct
  include OneMQCommon.Msg.Hello

  let set buf off s =
    let len = Bytes.length s
    in
    assert (len < 256);
    BE.set_uint8 buf (off + 0) len;
    BE.set_str buf (off + 1) s;
    off + 1 + len
end

module Quit = struct
  include OneMQCommon.Msg.Quit

  let get buf off = (off, make ())
end

include OneMQCommon.Msg.Union

let hello text = Hello (Hello.make text)

let parse buf =
  match BE.get_uint8 buf 0 with
  | net_id as x when net_id == Hello.net_id ->
    let s = Printf.sprintf "Wrong kind for kernel: %d" x
    in
    raise (OneMQCommon.Msg.ParseError s)
  | net_id when net_id == Quit.net_id ->
    let (off, quit) = Quit.get buf 1
    in
    Quit quit
  | x ->
    let s = Printf.sprintf "Unknown kind %d" x
    in
    raise (OneMQCommon.Msg.ParseError s)

let dump = function
  | Hello hello ->
    let len = Hello.length hello in
    let buf = Bytes.create (len + 1)
    in
    BE.set_uint8 buf 0 Hello.net_id;
    let _ = Hello.set buf 1 hello
    in
    buf
