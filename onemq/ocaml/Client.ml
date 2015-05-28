type t = {
  pid : int;
  fd : Unix.file_descr;
}

let make ?(kernel=OneMQCommon.Globals.default_kernel_path) ?(port=0) () =
  Printf.printf "OneMQ: initializing ...\n";
  Printf.printf "  Kernel = %s\n%!" kernel;
  Printf.printf "  Port = %d\n%!" port;
  let (sock_parent, sock_child) =
    Unix.socketpair Unix.PF_UNIX Unix.SOCK_SEQPACKET 0
  in
  Unix.set_close_on_exec sock_parent;
  match Unix.fork () with
  | 0 ->
    let fd = Obj.magic OneMQCommon.Globals.control_socket_fd
    in
    Unix.dup2 sock_child fd;
    (* Make sure sock_child gets closed unless it already is FD 3 *)
    Unix.set_close_on_exec sock_child;
    Unix.clear_close_on_exec fd;
    Unix.handle_unix_error
      (fun () -> Unix.execve kernel [| kernel |] [| |])
      ()
  | pid ->
    Unix.close sock_child;
    { pid; fd = sock_parent; }

let destroy client =
  Printf.printf "OneMQ: shuting down ...\n%!";
  Unix.shutdown client.fd Unix.SHUTDOWN_ALL;
  Unix.close client.fd;
  Printf.printf "OneMQ: waiting for kernel to die ...\n%!";
  let (_, status) = Unix.waitpid [] client.pid
  in begin
  match status with
  | Unix.WEXITED x ->
    Printf.printf "OneMQ: kernel exited with %d\n" x
  | Unix.WSIGNALED x ->
    Printf.printf "OneMQ: kernel killed by signal %d\n" x
  | Unix.WSTOPPED x ->
    Printf.printf "OneMQ: kernel stopped by signal %d\n" x
  end;
  Printf.printf "OneMQ: done\n%!"

let recv client =
  Printf.printf "OneMQ.recv\n%!";
  let buf = Bytes.create OneMQCommon.Globals.buf_size in
  let res = Unix.recv client.fd buf 0 OneMQCommon.Globals.buf_size [] in
  if res == 0
  then raise End_of_file
  else
    let buf = Bytes.sub buf 0 res in
    Msg.parse buf

let send client msg =
  Printf.printf "OneMQ.send\n%!";
  let buf = Msg.dump msg in
  let len = Bytes.length buf in
  let res = Unix.send client.fd buf 0 len [] in
  assert (res == len)

let quit client =
  Printf.printf "OneMQ.quit\n%!";
  let msg = Msg.quit () in
  send client msg
