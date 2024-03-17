include Unix

let read file_descr bytes pos len =
  try Unix.read file_descr bytes pos len
  with Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
    Unix.read (Picos_select.await_on file_descr `R) bytes pos len

let write file_descr bytes pos len =
  try Unix.write file_descr bytes pos len
  with Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
    Unix.write (Picos_select.await_on file_descr `W) bytes pos len

let accept ?cloexec file_descr =
  try Unix.accept ?cloexec file_descr
  with Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
    Unix.accept ?cloexec (Picos_select.await_on file_descr `R)
