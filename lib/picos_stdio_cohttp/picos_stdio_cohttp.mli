(** Minimalistic Cohttp implementation using {!Picos_stdio} for {!Picos}.

    ⚠️ This library is currently minimalistic and experimental and is highly
    likely to change.  Feedback from potential users is welcome! *)

open Picos_stdio

(** Convenience funtions for constructing and processing requests.

    Please consult the
    {{:https://ocaml.org/p/cohttp/latest/doc/Cohttp/Generic/Client/module-type-S/index.html} CoHTTP documentation}. *)
module Client : sig
  include
    Cohttp.Generic.Client.S
      with type 'a io = 'a
      with type 'a with_context = 'a
      with type body = Cohttp.Body.t
end

(** Convenience funtions for processing requests and constructing responses.

    Please consult the
    {{:https://ocaml.org/p/cohttp/latest/doc/Cohttp/Generic/Server/module-type-S/index.html} CoHTTP documentation}. *)
module Server : sig
  include
    Cohttp.Generic.Server.S
      with type 'a IO.t = 'a
      with type IO.conn = Unix.file_descr
      with type body = Cohttp.Body.t

  val run : t -> IO.conn -> unit
  (** [run server socket] starts running a server that {{!Unix.accept} accepts}
      clients on the specified [socket].  This never returns normally. *)
end
