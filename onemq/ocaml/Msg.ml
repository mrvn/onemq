module BE = struct include OneMQCommon.BE end
  
module Hello = struct
  include OneMQCommon.Msg.Hello
  let get buf off =
    let len = BE.get_uint8 buf off in
    let s = BE.get_str buf (off + 1) len
    in
    (off + 1 + len, s)
end

module Quit = struct
  include OneMQCommon.Msg.Quit

  let set buf off _ = off
end

include OneMQCommon.Msg.Union

let quit () = Quit (Quit.make ())
    
let parse buf =
  match BE.get_uint8 buf 0 with
  | net_id when net_id == Hello.net_id ->
    let (off, hello) = Hello.get buf 1
    in
    Hello hello
  | net_id as x when net_id == Quit.net_id ->
    let s = Printf.sprintf "Wrong kind for peer: %d" x
    in
    raise (OneMQCommon.Msg.ParseError s)
  | x ->
    let s = Printf.sprintf "Unknown kind %d" x
    in
    raise (OneMQCommon.Msg.ParseError s)

let dump = function
  | Quit quit ->
    let len = Quit.length quit in
    let buf = Bytes.create (len + 1)
    in
    BE.set_uint8 buf 0 Quit.net_id;
    let _ = Quit.set buf 1 quit
    in
    buf
