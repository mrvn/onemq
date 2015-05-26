let () =
  Printf.printf "OneMQ test suite\n%!";
  Printf.printf "  Kernel = %s\n%!" Sys.argv.(1)

(* create peer *)
let peer = OneMQ.Client.make ~kernel:Sys.argv.(1) ()

(* receive hello message from kernel *)
let () =
  match OneMQ.Client.recv peer with
  | OneMQ.Msg.Hello text ->
    Printf.printf "OneMQ test: Kernel said '%s'\n%!" text
  | _ -> assert false

(* send quit *)
let () = OneMQ.Client.quit peer
      
let () = OneMQ.Client.destroy peer
