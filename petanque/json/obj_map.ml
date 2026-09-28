module type Obj = sig
  val name : string

  type t
  (* Not yet *)
  (* val equal : t -> t -> bool *)
end

module type S = sig
  type t [@@deriving yojson]

  (** Remove exactly the supplied exported object identifiers.  This operation
      is idempotent and deliberately knows nothing about relationships between
      the objects.  The pair is [(released, missing)]. *)
  val release : int list -> int list * int list

  (** Remove every exported identifier without resetting the monotonic
      allocator.  A later object can therefore never alias an identifier that
      was valid before a workspace refresh. *)
  val clear : unit -> unit

  (** Number of identifiers currently exported.  This is diagnostic lifecycle
      metadata, not a semantic property of the stored objects. *)
  val cardinal : unit -> int
end

module Make (O : Obj) : S with type t = O.t = struct
  type t = O.t
  type _t = int [@@deriving yojson]

  module Memo = Hashtbl.Make (Int)

  let memo = Memo.create 1000

  let dump_memo () =
    let keys = Memo.to_seq_keys memo |> List.of_seq in
    Format.(eprintf "@[size: %d@]@\n%!" (List.length keys));
    Format.(eprintf "@[<v>%a@]@\n%!" (pp_print_list pp_print_int) keys)

  let last_id = ref 0

  let mk_id _ =
    incr last_id;
    !last_id

  let of_obj (s : O.t) : int =
    let id = mk_id s in
    let () = Memo.add memo id s in
    id

  let to_obj (id : int) : (O.t, _) Result.t =
    match Memo.find_opt memo id with
    | Some v -> Ok v
    | None ->
      if false then dump_memo ();
      Error (Format.asprintf "key %d for object %s not found" id O.name)

  let release ids =
    let one (released, missing) id =
      if Memo.mem memo id then (
        Memo.remove memo id;
        (id :: released, missing))
      else (released, id :: missing)
    in
    let released, missing = List.fold_left one ([], []) ids in
    (List.rev released, List.rev missing)

  let clear () = Memo.clear memo
  let cardinal () = Memo.length memo

  let of_yojson json = _t_of_yojson json |> fun r -> Result.bind r to_obj
  let to_yojson st : Yojson.Safe.t = of_obj st |> _t_to_yojson
end
