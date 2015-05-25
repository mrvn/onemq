exception ParseError of string

module Hello = struct
  type t = string

  let net_id = 1
    
  let make s = s
    
  let length t = String.length t + 1
end

module Quit = struct
  type t = unit

  let net_id = 2

  let make () = ()
    
  let length _ = 0
end

module Union = struct
  type kernel_msg =
  | Hello of Hello.t

  type client_msg =
  | Quit of Quit.t
end
