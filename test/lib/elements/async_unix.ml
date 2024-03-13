open Picos
include Unix

let await file_descr op =
  let c = Computation.create () in
  Picos_select.return_on c file_descr op file_descr;
  try Computation.await c
  with exn ->
    Computation.cancel c (Exn_bt.get_callstack 0 exn);
    raise exn

let read file_descr bytes pos len =
  try Unix.read file_descr bytes pos len
  with Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
    Unix.read (await file_descr `R) bytes pos len

let write file_descr bytes pos len =
  try Unix.write file_descr bytes pos len
  with Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
    Unix.write (await file_descr `W) bytes pos len

let accept ?cloexec file_descr =
  try Unix.accept ?cloexec file_descr
  with Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
    Unix.accept ?cloexec (await file_descr `R)
