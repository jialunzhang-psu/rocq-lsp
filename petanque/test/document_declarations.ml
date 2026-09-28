open Petanque_shell

let prepare_paths () =
  let to_uri file =
    Lang.LUri.of_string file |> Lang.LUri.File.of_uri |> Result.get_ok
  in
  let cwd = Sys.getcwd () in
  let file = Filename.concat cwd "document_declarations.v" in
  (to_uri cwd, to_uri file, file)

let trace ?verbose:_ _ = ()
let message ~lvl:_ ~message:_ = ()

let write_file file contents =
  let channel = open_out_bin file in
  output_string channel contents;
  close_out channel

let read_file file =
  Coq.Compat.Ocaml_414.In_channel.(with_open_text file input_all)

let compile_injected file =
  let command =
    Format.asprintf "rocq compile -Q . Injected %s >/dev/null 2>&1"
      (Filename.quote file)
  in
  assert (Sys.command command = 0)

let remove_if_present file = if Sys.file_exists file then Sys.remove file

let run (ic, oc) =
  let open Coq.Compat.Result.O in
  let module S = Client.S (struct
    let ic = ic
    let oc = oc
    let trace = trace
    let message = message
  end) in
  let root, uri, file = prepare_paths () in
  let load_paths =
    [ Protocol_shell.SetWorkspace.LoadPath.
        { physical = Sys.getcwd (); logical = "Injected"; implicit = false }
    ]
  in
  let* () = S.set_workspace { debug = false; root; load_paths } in
  let* capabilities = S.capabilities { marker = None } in
  List.iter
    (fun capability -> assert (List.mem capability capabilities))
    [ "document_declarations_v2"
    ; "dune_workspace_v1"
    ; "insertion_point_v1"
    ; "atomic_run_v1"
    ; "release_states_v1"
    ; "refresh_workspace_v1"
    ; "state_count_v1"
    ; "structured_assumptions_v1"
    ; "typed_errors_v1"
    ];
  let* declarations = S.document_declarations { uri } in
  let compilation_unit = [ "Injected"; "document_declarations" ] in
  let q path = compilation_unit @ path in
  let expected =
    [ (q [ "top" ], "Theorem", true)
    ; (q [ "mutual_first" ], "Theorem", true)
    ; (q [ "mutual_second" ], "Theorem", true)
    ; (q [ "A"; "same" ], "Theorem", true)
    ; (q [ "A"; "value" ], "Definition", true)
    ; (q [ "A"; "Inner"; "deep" ], "Lemma", true)
    ; (q [ "B"; "same" ], "Fact", true)
    ; (q [ "section_leaf" ], "Remark", true)
    ; (q [ "permitted_axiom" ], "Axiom", true)
    ; ( q
          [ "dependency_with_a_name_long_enough_to_wrap_in_human_readable_output"
          ]
      , "Axiom"
      , true )
    ; (q [ "uses_long_dependency" ], "Theorem", true)
    ; (q [ "admitted_leaf" ], "Theorem", false)
    ; (q [ "aborted_leaf" ], "Theorem", false)
    ]
  in
  let actual =
    List.map
      (fun (declaration : Document_declaration.t) ->
        ( declaration.qualified_path
        , declaration.kind
        , declaration.proof_finished ))
      declarations
  in
  if actual <> expected then
    List.iter
      (fun (path, kind, finished) ->
        Format.eprintf "actual declaration: %s %s %b@." (String.concat "." path)
          kind finished)
      actual;
  assert (actual = expected);
  let mutual =
    List.filter
      (fun (declaration : Document_declaration.t) ->
        match List.rev declaration.qualified_path with
        | ("mutual_first" | "mutual_second") :: _ -> true
        | _ -> false)
      declarations
  in
  (match mutual with
  | [ first; second ] ->
    (* One Rocq command can introduce distinct constants. PET must preserve the
       shared range so publication wrappers can reject partial replacement of
       that command rather than guessing from the leaf identities. *)
    assert (first.declaration_range = second.declaration_range)
  | _ -> assert false);
  let source = read_file file in
  List.iter
    (fun (declaration : Document_declaration.t) ->
      let { Document_declaration.start; end_ } = declaration.range in
      assert (start >= 0 && start < end_ && end_ <= String.length source);
      let declaration_end = declaration.declaration_range.end_ in
      assert (declaration.declaration_range.start = start);
      assert (declaration_end >= end_ && declaration_end <= String.length source);
      let raw = String.sub source start (end_ - start) |> String.trim in
      let raw_length = String.length raw in
      let normalized =
        if raw_length > 0 && Char.equal raw.[raw_length - 1] '.' then
          String.sub raw 0 (raw_length - 1) |> String.trim
        else raw
      in
      assert (String.equal declaration.statement normalized))
    declarations;
  let* insertion = S.insertion_point { uri; modules = [ "A"; "Inner" ] } in
  assert (
    String.starts_with ~prefix:"End Inner." (String.sub source insertion 10));

  (* Assumption identities cross the protocol as name components, never as
     width-sensitive [Print Assumptions] or [Locate] feedback. *)
  let eof_line = List.length (String.split_on_char '\n' source) - 1 in
  let eof_position =
    Lang.Point.{ line = eof_line; character = 0; offset = -1 }
  in
  let* final_state =
    S.get_state_at_pos { uri; opts = None; position = eof_position }
  in
  let* closed =
    S.assumptions { st = final_state.st; qualified_path = q [ "top" ] }
  in
  assert (closed.assumptions = []);
  assert (not closed.theory.rewrite_rules);
  assert (not closed.theory.impredicative_set);
  assert (not closed.theory.type_in_type);
  let* long =
    S.assumptions
      { st = final_state.st; qualified_path = q [ "uses_long_dependency" ] }
  in
  (match long.assumptions with
  | [ { Petanque.Agent.Assumption.kind = Petanque.Agent.Assumption.Axiom
      ; qualified_path
      }
    ] ->
    assert (
      qualified_path
      = q
          [ "dependency_with_a_name_long_enough_to_wrap_in_human_readable_output"
          ])
  | _ -> assert false);
  let* released_final = S.release_states { states = [ final_state.st ] } in
  assert (released_final.released = [ final_state.st ]);

  (* Exact release is idempotent and non-cascading. *)
  let position = Lang.Point.{ line = 2; character = 0; offset = -1 } in
  let* parent = S.get_state_at_pos { uri; opts = None; position } in
  let* child = S.run { opts = None; st = parent.st; tac = "Check True." } in
  let* released_parent = S.release_states { states = [ parent.st ] } in
  assert (released_parent.released = [ parent.st ]);
  assert (released_parent.missing = []);
  (match S.state_hash { st = parent.st } with
  | Error _ -> ()
  | Ok _ -> assert false);
  let* _child_hash = S.state_hash { st = child.st } in
  let unknown = max_int in
  let* released_child =
    S.release_states { states = [ child.st; child.st; unknown ] }
  in
  assert (released_child.released = [ child.st ]);
  assert (released_child.missing = [ child.st; unknown ]);
  let* count_after_release = S.state_count { marker = None } in
  assert (count_after_release = 0);

  (* A failed multi-sentence run must not export its successfully evaluated
     prefix. Repeated allocate/release cycles must return to the same count. *)
  let* base = S.get_state_at_pos { uri; opts = None; position } in
  let* before_rejection = S.state_count { marker = None } in
  assert (before_rejection = 1);
  (match
     S.run
       { opts = None
       ; st = base.st
       ; tac = "Check True. ThisCommandMustNotExist."
       }
   with
  | Error _ -> ()
  | Ok _ -> assert false);
  let* after_rejection = S.state_count { marker = None } in
  assert (after_rejection = before_rejection);
  let* release_base = S.release_states { states = [ base.st ] } in
  assert (release_base.released = [ base.st ]);
  let rec repeat_cycles remaining =
    if remaining = 0 then Ok ()
    else
      let* temporary = S.get_state_at_pos { uri; opts = None; position } in
      let* child =
        S.run { opts = None; st = temporary.st; tac = "Check True." }
      in
      let* released =
        S.release_states { states = [ temporary.st; child.st ] }
      in
      assert (released.released = [ temporary.st; child.st ]);
      repeat_cycles (remaining - 1)
  in
  let* () = repeat_cycles 25 in
  let* count_after_cycles = S.state_count { marker = None } in
  assert (count_after_cycles = 0);

  (* Refresh retains the process connection, invalidates every old ID, and
     forces the next document request to read the modified source. *)
  let* old_state = S.get_state_at_pos { uri; opts = None; position } in
  let refresh_file =
    Filename.concat (Sys.getcwd ()) "refresh_workspace_runtime.v"
  in
  let refresh_uri =
    Lang.LUri.of_string refresh_file |> Lang.LUri.File.of_uri |> Result.get_ok
  in
  write_file refresh_file "Theorem refresh_before : True. exact I. Qed.\n";
  let* _before_refresh = S.document_declarations { uri = refresh_uri } in
  let refreshed_name = "refresh_visible" in
  write_file refresh_file
    ("Theorem " ^ refreshed_name ^ " : True. exact I. Qed.\n");
  let* () = S.refresh_workspace { marker = None } in
  (match S.state_hash { st = old_state.st } with
  | Error _ -> ()
  | Ok _ -> assert false);
  let* after_refresh = S.document_declarations { uri = refresh_uri } in
  assert (
    List.exists
      (fun (declaration : Document_declaration.t) ->
        List.hd (List.rev declaration.qualified_path) = refreshed_name)
      after_refresh);
  let* post_refresh_state = S.get_state_at_pos { uri; opts = None; position } in
  (* [Obj_map.clear] keeps its monotonic allocator. A greater ID therefore
     proves that normal refresh retained this exact PET process. *)
  assert (post_refresh_state.st > old_state.st);
  let* post_refresh_release =
    S.release_states { states = [ post_refresh_state.st ] }
  in
  assert (post_refresh_release.released = [ post_refresh_state.st ]);
  Sys.remove refresh_file;
  let* () = S.refresh_workspace { marker = None } in

  (* PET's [.glob] memo is independently invalidated. Change only the
     declaration offset in the compiled dependency's glob file and observe it
     through a fresh post-refresh premise query. *)
  let dependency = "refresh_dependency.v" in
  let dependency_glob = "refresh_dependency.glob" in
  let consumer = "refresh_consumer.v" in
  let consumer_uri =
    Filename.concat (Sys.getcwd ()) consumer
    |> Lang.LUri.of_string |> Lang.LUri.File.of_uri |> Result.get_ok
  in
  write_file dependency "Definition refreshed_value : nat := 1.\n";
  compile_injected dependency;
  write_file consumer
    "From Injected Require Import refresh_dependency.\n\
     Theorem refresh_consumer : refreshed_value = 1. reflexivity. Qed.\n";
  let consumer_position = Lang.Point.{ line = 2; character = 0; offset = -1 } in
  let premise_offset premises =
    List.find_map
      (fun { Petanque.Agent.Premise.full_name; file = _; info } ->
        if String.ends_with ~suffix:".refreshed_value" full_name then
          match info with
          | Ok { Petanque.Agent.Premise.Info.offset; _ } -> Some offset
          | Error _ -> None
        else None)
      premises
  in
  let* consumer_state =
    S.get_state_at_pos
      { uri = consumer_uri; opts = None; position = consumer_position }
  in
  let* initial_premises = S.premises { st = consumer_state.st } in
  let initial_offset = premise_offset initial_premises |> Option.get in
  let glob_lines = String.split_on_char '\n' (read_file dependency_glob) in
  let glob_lines =
    List.map
      (fun line ->
        if
          String.starts_with ~prefix:"def " line
          && String.ends_with ~suffix:" refreshed_value" line
        then "def 0:0 <> refreshed_value"
        else line)
      glob_lines
  in
  write_file dependency_glob (String.concat "\n" glob_lines);
  let* cached_premises = S.premises { st = consumer_state.st } in
  assert (premise_offset cached_premises = Some initial_offset);
  let* () = S.refresh_workspace { marker = None } in
  let* refreshed_consumer_state =
    S.get_state_at_pos
      { uri = consumer_uri; opts = None; position = consumer_position }
  in
  let* refreshed_premises = S.premises { st = refreshed_consumer_state.st } in
  assert (premise_offset refreshed_premises = Some (0, 0));
  let* released_consumer =
    S.release_states { states = [ refreshed_consumer_state.st ] }
  in
  assert (released_consumer.released = [ refreshed_consumer_state.st ]);

  (* Changing and recompiling a dependency must invalidate the old [.vo]
     environment. The unchanged consumer is valid against value [1], but it must
     fail after refresh exposes the rebuilt value [2]. *)
  write_file dependency "Definition refreshed_value : nat := 2.\n";
  compile_injected dependency;
  let* _cached_consumer = S.document_declarations { uri = consumer_uri } in
  let* () = S.refresh_workspace { marker = None } in
  (match S.document_declarations { uri = consumer_uri } with
  | Error _ -> ()
  | Ok _ -> assert false);
  write_file consumer
    "From Injected Require Import refresh_dependency.\n\
     Theorem refresh_consumer : refreshed_value = 2. reflexivity. Qed.\n";
  let* () = S.refresh_workspace { marker = None } in
  let* repaired_consumer = S.document_declarations { uri = consumer_uri } in
  assert (List.length repaired_consumer = 1);

  List.iter remove_if_present
    [ dependency
    ; dependency_glob
    ; "refresh_dependency.vo"
    ; "refresh_dependency.vos"
    ; "refresh_dependency.vok"
    ; ".refresh_dependency.aux"
    ; consumer
    ];
  let* () = S.refresh_workspace { marker = None } in
  let* capabilities_after = S.capabilities { marker = None } in
  assert (capabilities_after = capabilities);
  Ok ()

let main () =
  let server_out, server_in = Unix.open_process "pet" in
  run (server_out, Format.formatter_of_out_channel server_in)

let () =
  match main () with
  | Ok () -> exit 0
  | Error message ->
    Format.eprintf "document declaration protocol failed: %s@\n%!" message;
    exit 1
