# onemq
 OneMQ is a messaging system based ontop the UDP protocol adding
 reliable messaging, heartbeats, flow control, authentication,
 encryption and multicasting on top.

 The design uses a multi-process approach running a messaging kernel,
 written in ocaml, as separate process while communicating with the
 application with a single socket. This keeps the messaging kernel
 language independent of the applications language with only a small
 glue module in the application to simplify talking to the kernel.
