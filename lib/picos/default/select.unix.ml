type file_descr = Unix.file_descr

let stdin = Unix.stdin
let close = Unix.close
let read = Unix.read
let write = Unix.write
let pipe = Unix.pipe
let select = Unix.select
