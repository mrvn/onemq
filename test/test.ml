let () =
  Printf.printf "OneMQ test suite\n%!";
  Printf.printf "  Kernel = %s\n%!" Sys.argv.(1)

(* create peer *)
let peer = OneMQ.Peer.make Sys.argv.(1)

(* receive hello message from kernel *)
let () =
  match OneMQ.Peer.recv peer with
  | OneMQ.Msg.Hello text ->
    Printf.printf "OneMQ test: Kernel said '%s'\n%!" text
  | _ -> assert false

(* send quit *)
let () = OneMQ.Peer.quit peer
      
let () = OneMQ.Peer.destroy peer
