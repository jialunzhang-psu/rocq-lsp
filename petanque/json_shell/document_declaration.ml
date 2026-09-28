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
    [qualified_path] is the canonical compilation-unit-qualified Rocq name.
    It keeps enclosing modules while deliberately omitting sections. *)
type t =
  { qualified_path : string list
  ; kind : string
  ; range : range
        (** Range of the declaration header sentence. *)
  ; declaration_range : range
        (** Range from the header through its current terminator, or just the
            header when the source proof has no terminator. *)
  ; proof_finished : bool
        (** [true] only for a direct definition or a proof closed by
            [Qed]/[Defined].  [Admitted], [Abort], and an unterminated proof
            are unfinished. *)
  ; statement : string
  }
[@@deriving yojson]
