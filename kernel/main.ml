let () =
  Printf.printf "OneMQKernel: starting ...\n%!"

let client_fd = Obj.magic OneMQCommon.Globals.control_socket_fd
let () = Unix.set_nonblock client_fd
let sockets = []

let send_client msg =
  Printf.printf "OneMQKernel.send_client\n%!";
  let buf = Msg.dump msg in
  let len = Bytes.length buf in
  let res = Unix.send client_fd buf 0 len []
  in
  assert (len == res)

let recv_client () =
  Printf.printf "OneMQKernel.recv_client\n%!";
  let buf = Bytes.create OneMQCommon.Globals.buf_size in
  let res = Unix.recv client_fd buf 0 OneMQCommon.Globals.buf_size []
  in
  if res == 0
  then raise End_of_file
  else
    let buf = Bytes.sub buf 0 res
    in
    Msg.parse buf

let () = Printf.printf "OneMQKernel: saying hello\n%!"
let msg = Msg.hello "Hello Client"
let () = send_client msg

(* main loop *)
let rec loop () =
  Printf.printf "OneMQKernel: looping ...\n%!";
  let (rd, _, _) =
    Unix.select (client_fd :: sockets) [] (client_fd :: sockets) (-1.)
  in
  let finished =
    List.fold_left
      (fun _(*finished*) fd ->
	if fd == client_fd
	then
	  try
	    match recv_client () with
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
    Unix.close client_fd

let () = loop ()
  
let () =
  Printf.printf "OneMQKernel: done\n%!"
