let () =
  Printf.printf "OneMQKernel: starting ...\n%!"

let peer_fd = Obj.magic OneMQCommon.Globals.control_socket_fd
let () = Unix.set_nonblock peer_fd
let sockets = []

let send_peer msg =
  Printf.printf "OneMQKernel.send_peer\n%!";
  let buf = Msg.dump msg in
  let len = Bytes.length buf in
  let res = Unix.send peer_fd buf 0 len []
  in
  assert (len == res)

let recv_peer () =
  Printf.printf "OneMQKernel.recv_peer\n%!";
  let buf = Bytes.create OneMQCommon.Globals.buf_size in
  let res = Unix.recv peer_fd buf 0 OneMQCommon.Globals.buf_size []
  in
  if res == 0
  then raise End_of_file
  else
    let buf = Bytes.sub buf 0 res
    in
    Msg.parse buf

let () = Printf.printf "OneMQKernel: saying hello\n%!"
let msg = Msg.hello "Hello Peer"
let () = send_peer msg

(* main loop *)
let rec loop () =
  Printf.printf "OneMQKernel: looping ...\n%!";
  let (rd, _, _) =
    Unix.select (peer_fd :: sockets) [] (peer_fd :: sockets) (-1.)
  in
  let finished =
    List.fold_left
      (fun _(*finished*) fd ->
	if fd == peer_fd
	then
	  try
	    match recv_peer () with
	    | Msg.Quit _ ->
	      Printf.printf "OneMQKernel: got quit\n%!";
	      true
	  with End_of_file ->
	    Printf.printf "OneMQKernel: End_of_file on control socket\n%!";
	    true
	else assert false)
      false
      rd
  in
  if not finished
  then loop ()
  else
    Unix.close peer_fd

let () = loop ()
  
let () =
  Printf.printf "OneMQKernel: done\n%!"
