(************************************************************************)
(* PET document declaration protocol records.                           *)
(* These records are produced from Flèche's checked document, so the      *)
(* declaration path and source range have one semantic owner: PET.        *)
(************************************************************************)

(** A byte range in the exact checked source snapshot.  [end_] is exclusive. *)
type range =
  { start : int
  ; end_ : int [@key "end"]
  }
[@@deriving yojson]

(** One supported proof/definition declaration in a checked document.
    [qualified_path] is relative to the document's compilation unit and keeps
    enclosing modules while deliberately omitting sections. *)
type t =
  { qualified_path : string list
  ; kind : string
  ; range : range
  ; statement : string
  }
[@@deriving yojson]
