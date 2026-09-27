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

let run (ic, oc) =
  let open Coq.Compat.Result.O in
  let module S = Client.S (struct
    let ic = ic
    let oc = oc
    let trace = trace
    let message = message
  end) in
  let root, uri, file = prepare_paths () in
  let* () = S.set_workspace { debug = false; root } in
  let* declarations = S.document_declarations { uri } in
  let expected =
    [ ([ "top" ], "Theorem")
    ; ([ "A"; "same" ], "Theorem")
    ; ([ "A"; "value" ], "Definition")
    ; ([ "A"; "Inner"; "deep" ], "Lemma")
    ; ([ "B"; "same" ], "Fact")
    ; ([ "section_leaf" ], "Remark")
    ]
  in
  let actual =
    List.map
      (fun (declaration : Document_declaration.t) ->
        (declaration.qualified_path, declaration.kind))
      declarations
  in
  assert (actual = expected);
  let source =
    Coq.Compat.Ocaml_414.In_channel.(with_open_text file input_all)
  in
  List.iter
    (fun (declaration : Document_declaration.t) ->
      let { Document_declaration.start; end_ } = declaration.range in
      assert (start >= 0 && start < end_ && end_ <= String.length source);
      let raw = String.sub source start (end_ - start) |> String.trim in
      let raw_length = String.length raw in
      let normalized =
        if raw_length > 0 && Char.equal raw.[raw_length - 1] '.' then
          String.sub raw 0 (raw_length - 1) |> String.trim
        else raw
      in
      assert (String.equal declaration.statement normalized))
    declarations;
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
