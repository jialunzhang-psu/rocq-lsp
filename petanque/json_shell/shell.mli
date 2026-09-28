open Petanque

(** I/O handling, by default, print to stderr *)

(** [trace header extra message] *)
val trace_ref : (string -> ?verbose:string -> string -> unit) ref

(** [message level message] *)
val message_ref : (lvl:Fleche.Io.Level.t -> message:string -> unit) ref

(** Start the shell, must be called only once. *)
val init_agent :
     token:Coq.Limits.Token.t
  -> debug:bool
  -> record_comments:bool
  -> roots:string list
  -> unit Agent.R.t

(** [set_workspace ~root] Sets project and workspace settings from [root].
    [root] needs to be in URI format. If called repeteadly, overrides the
    previous call. *)
val set_workspace :
     token:Coq.Limits.Token.t
  -> debug:bool
  -> root:Lang.LUri.File.t
  -> load_paths:(string * string * bool) list
  -> unit Agent.R.t

(** Rebuild the selected workspace after source or build-artifact mutation and
    clear every PET/Fleche filesystem-derived cache. *)
val refresh_workspace : token:Coq.Limits.Token.t -> unit Agent.R.t

val build_doc :
  token:Coq.Limits.Token.t -> uri:Lang.LUri.File.t -> Fleche.Doc.t Agent.R.t

val get_toc :
     token:Coq.Limits.Token.t
  -> doc:Fleche.Doc.t
  -> (string * Lang.Ast.Info.t list option) list Agent.R.t

val get_declarations :
     token:Coq.Limits.Token.t
  -> doc:Fleche.Doc.t
  -> Document_declaration.t list Agent.R.t

(** Return the byte offset immediately before the closing [End] of one exact
    nested module path, or EOF for the top-level path. *)
val insertion_point : doc:Fleche.Doc.t -> string list -> int Agent.R.t
