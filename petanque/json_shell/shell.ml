module SM = Lang.Compat.String.Map

open Vernacexpr

let init_coq ~debug ~record_comments =
  let load_module = Dynlink.loadfile in
  let load_plugin = Coq.Loader.plugin_handler None in
  let vm, warnings = (true, None) in
  Coq.Init.(
    coq_init { debug; record_comments; load_module; load_plugin; vm; warnings })

let cmdline load_paths : Coq.Workspace.CmdLine.t =
  let vo_load_path =
    List.map
      (fun (physical, logical, implicit) ->
        { Loadpath.unix_path = physical
        ; coq_path = Libnames.dirpath_of_string logical
        ; implicit
        ; installed = false
        ; recursive = true
        })
      load_paths
  in
  { coqlib = Coq.Args.coqlib_dyn
  ; findlib_config = None
  ; ocamlpath = []
  ; vo_load_path
  ; args = []
  ; require_libraries = []
  }

let setup_workspace ~token ~init ~debug ~root ~files ~load_paths =
  let dir = Lang.LUri.File.to_string_file root in
  (let open Coq.Compat.Result.O in
   let+ workspace =
     Coq.Workspace.guess ~token ~debug ~cmdline:(cmdline load_paths) ~dir ()
   in
   Fleche.Doc.Env.make ~init ~workspace ~files)
  |> Result.map_error (fun msg -> Petanque.Agent.Error.(make_request (coq msg)))

let trace_stderr hdr ?verbose:_ msg =
  Format.eprintf "@[[trace] %s | %s @]@\n%!" hdr msg

let trace_ref = ref trace_stderr

let message_stderr ~lvl:_ ~message =
  Format.eprintf "@[[message] %s @]@\n%!" message

let message_ref = ref message_stderr

let io =
  let trace hdr ?verbose msg = !trace_ref hdr ?verbose msg in
  let message ~lvl ~message = !message_ref ~lvl ~message in
  let diagnostics ~uri:_ ~version:_ _diags = () in
  let fileProgress ~uri:_ ~version:_ _pinfo = () in
  let perfData ~uri:_ ~version:_ _perf = () in
  let serverVersion _ = () in
  let serverStatus _ = () in
  let execInfo ~uri:_ ~version:_ ~range:_ = () in
  { Fleche.Io.CallBack.trace
  ; message
  ; diagnostics
  ; fileProgress
  ; perfData
  ; serverVersion
  ; serverStatus
  ; execInfo
  }

let init_st = ref None
let env = ref None
let workspace_root = ref None
let workspace_debug = ref false
let workspace_load_paths = ref []
let files = ref (Coq.Files.make ())

let set_workspace ~token ~debug ~root ~load_paths =
  let init = Option.get !init_st in
  (* Design note: [Files.t] participates in Fleche's memo keys.  Bump it on
     every workspace selection so two Dune roots, or a root selected after a
     refresh, cannot reuse a cached [.vo] environment from an older view. *)
  files := Coq.Files.bump !files;
  let open Coq.Compat.Result.O in
  let+ env_ =
    setup_workspace ~token ~init ~debug ~root ~files:!files ~load_paths
  in
  env := Some env_;
  workspace_root := Some root;
  workspace_debug := debug;
  workspace_load_paths := load_paths

let refresh_workspace ~token =
  match !workspace_root with
  | None ->
    Error
      Petanque.Agent.Error.(make_request (system "workspace is not configured"))
  | Some root ->
    (* [Theory.workspace_update] clears Fleche's compiled-library intern cache;
       the shell constructs documents on demand rather than retaining them in
       Theory's table.  Re-selecting the root with a bumped [Files.t] makes all
       subsequent document construction use the new Dune/source state. *)
    let _invalid_requests = Fleche.Theory.workspace_update () in
    Petanque.Agent.Memo.clear ();
    set_workspace ~token ~debug:!workspace_debug ~root
      ~load_paths:!workspace_load_paths

(* likely duplicated somewhere else *)
let pp_diag fmt { Lang.Diagnostic.message; _ } =
  Format.fprintf fmt "%a" Pp.pp_with message

let print_diags (doc : Fleche.Doc.t) =
  let d = Fleche.Doc.diags doc in
  Format.(eprintf "@[<v>%a@]" (pp_print_list pp_diag) d)

let read_raw ~uri =
  let file = Lang.LUri.File.to_string_file uri in
  try Ok Coq.Compat.Ocaml_414.In_channel.(with_open_text file input_all)
  with Sys_error err -> Error Petanque.Agent.Error.(make_request (system err))

let languageId = "rocq"

let setup_doc ~token env uri =
  match read_raw ~uri with
  | Ok raw ->
    let doc = Fleche.Doc.create ~token ~env ~uri ~languageId ~version:0 ~raw in
    print_diags doc;
    let target = Fleche.Doc.Target.End in
    Ok (Fleche.Doc.check ~io ~token ~target ~doc ())
  | Error err -> Error err

let build_doc ~token ~uri = setup_doc ~token (Option.get !env) uri

(* Flèche LSP backend handles the conversion at the protocol level *)
let to_uri uri : _ Request.R.t =
  Lang.LUri.of_string uri |> Lang.LUri.File.of_uri
  |> Result.map_error (fun msg ->
         Petanque.Agent.Error.(make_request (system msg)))

let uri_of_path path = Format.asprintf "file:///%s" path |> to_uri

let set_roots ~token ~debug ~roots =
  let root =
    match roots with
    | [] -> Sys.getcwd ()
    | root :: _ -> root
  in
  let open Coq.Compat.Result.O in
  let* root = uri_of_path root in
  set_workspace ~token ~debug ~root ~load_paths:[]

let init_agent ~token ~debug ~record_comments ~roots =
  init_st := Some (init_coq ~debug ~record_comments);
  Fleche.Io.CallBack.set io;
  set_roots ~token ~debug ~roots

let toc_to_info (name, node) =
  let open Coq.Compat.Option.O in
  let+ ast = Fleche.Doc.Node.ast node in
  (name, ast.Fleche.Doc.Node.Ast.ast_info)

let get_toc ~token:_ ~(doc : Fleche.Doc.t) :
    (string * Lang.Ast.Info.t list option) list Petanque.Agent.R.t =
  let { Fleche.Doc.toc; _ } = doc in
  let toc = SM.bindings toc |> List.filter_map toc_to_info in
  Ok toc

(* The old [toc] is a String.Map keyed by leaf name.  That representation
   cannot express [A.foo] and [B.foo] simultaneously.  Build the document
   response from Flèche's checked AST sequence instead: the sequence retains
   source order and PET owns both declaration ranges and module transitions.
   This is deliberately a document pass, not a sentence-at-a-time RPC loop. *)
module Declaration_scope = struct
  type t = Module of string | Section of string

  let modules scopes =
    scopes
    |> List.filter_map (function Module name -> Some name | Section _ -> None)
    |> List.rev
end

let lident_name (id : Names.lident) = Names.Id.to_string id.CAst.v

let lname_name (id : Names.lname) =
  match id.CAst.v with
  | Names.Anonymous -> None
  | Names.Name id -> Some (Names.Id.to_string id)

let option_to_list = function None -> [] | Some value -> [ value ]

let command_of_ast (ast : Coq.Ast.t) =
  let control = Coq.Ast.to_coq ast in
  let CAst.{ v = { Vernacexpr.expr; _ }; _ } = control in
  expr

let names_for_info command (info : Lang.Ast.Info.t) =
  match command with
  | VernacSynPure (VernacStartTheoremProof (_, proofs)) ->
    let names =
      List.map (fun ((id, _), _) -> lident_name id) proofs
    in
    if names = [] then option_to_list info.name.v else names
  | VernacSynPure (VernacDefinition (_, (name, _), _)) ->
    option_to_list (lname_name name)
  | _ -> option_to_list info.name.v

let scope_transition command =
  match command with
  | VernacSynterp (VernacDefineModule (_, name, _, _, body))
    when body = [] -> Some (`Open (Declaration_scope.Module (lident_name name)))
  | VernacSynterp (VernacDeclareModuleType (name, _, _, body))
    when body = [] ->
    Some (`Open (Declaration_scope.Module (lident_name name)))
  | VernacSynterp (VernacBeginSection name) ->
    Some (`Open (Declaration_scope.Section (lident_name name)))
  | VernacSynterp (VernacEndSegment name) -> Some (`Close (lident_name name))
  | _ -> None

let declaration_kind command detail =
  match command, detail with
  | VernacSynPure (VernacAssumption ((NoDischarge, _), _, _)), _ ->
    Some "Axiom"
  | _,
    ("Theorem" | "Lemma" | "Fact" | "Remark" | "Corollary" | "Proposition"
    | "Definition") -> Some detail
  | _ -> None

let normalize_statement source =
  let source = String.trim source in
  let length = String.length source in
  if length > 0 && Char.equal source.[length - 1] '.' then
    String.sub source 0 (length - 1) |> String.trim
  else source

let source_relative_to ~root source =
  let separator = Filename.dir_sep in
  let prefix =
    if String.ends_with ~suffix:separator root then root else root ^ separator
  in
  if String.starts_with ~prefix source then
    Some (String.sub source (String.length prefix) (String.length source - String.length prefix))
  else None

let source_stem file =
  match Filename.chop_suffix_opt ~suffix:".v.tex" file with
  | Some file -> file
  | None -> Filename.remove_extension file

let compilation_unit ~(doc : Fleche.Doc.t) =
  let source = Lang.LUri.File.to_string_file doc.uri in
  let candidates =
    List.filter_map
      (fun (physical, logical, _implicit) ->
        Option.map
          (fun relative -> (String.length physical, logical, relative))
          (source_relative_to ~root:physical source))
      !workspace_load_paths
  in
  match List.sort (fun (left, _, _) (right, _, _) -> Int.compare right left) candidates with
  | (_, logical, relative) :: _ ->
    let relative = source_stem relative in
    String.split_on_char '.' logical
    @ String.split_on_char Filename.dir_sep.[0] relative
  | [] ->
    Coq.Workspace.dirpath_of_uri ~uri:doc.uri
    |> Names.DirPath.to_string |> String.split_on_char '.'
    |> List.filter (fun component -> not (String.equal component ""))

let declaration_records ~(doc : Fleche.Doc.t) scopes ast =
  let { Fleche.Doc.Node.Ast.v; ast_info } = ast in
  let ast_info = match ast_info with None -> [] | Some infos -> infos in
  let range =
    let loc = Coq.Ast.loc v |> Option.get in
    Coq.Utils.to_range ~lines:(Fleche.Doc.lines doc) loc
  in
  let statement =
    Fleche.Doc.extract_raw doc ~range |> normalize_statement
  in
  (* The document environment, not the JSON wrapper, owns the compilation
     unit's logical path.  Returning it here makes declaration identities
     canonical even when two source files contain the same module/leaf path. *)
  let qualified_prefix = compilation_unit ~doc @ Declaration_scope.modules scopes in
  let command = command_of_ast v in
  let opens_proof =
    match command with
    | VernacSynPure (VernacStartTheoremProof _)
    | VernacSynPure (VernacDefinition (_, _, ProveBody _)) -> true
    | _ -> false
  in
  let rows =
    List.concat_map
    (fun (info : Lang.Ast.Info.t) ->
      let kind =
        match command with
        | VernacSynPure (VernacAssumption ((NoDischarge, _), _, _)) ->
          Some "Axiom"
        | _ -> Option.bind info.detail (declaration_kind command)
      in
      match info.name.v, kind with
      | Some _, None -> []
      | None, _ -> []
      | Some _, Some kind ->
        names_for_info (command_of_ast v) info
        |> List.map (fun name ->
               Document_declaration.
                 { qualified_path = qualified_prefix @ [ name ]
                 ; kind
                 ; range =
                     { Document_declaration.start = range.start.offset
                     ; end_ = range.end_.offset
                     }
                 ; declaration_range =
                     { Document_declaration.start = range.start.offset
                     ; end_ = range.end_.offset
                     }
                 ; proof_finished = not opens_proof
                 ; statement
                 }))
      ast_info
  in
  (rows, if opens_proof then List.length rows else 0)

let close_declarations declarations count ~end_ ~proof_finished =
  let rec close remaining = function
    | rows when remaining = 0 -> rows
    | [] -> []
    | row :: rows ->
      let row =
        Document_declaration.
          { row with
            declaration_range = { row.declaration_range with end_ }
          ; proof_finished
          }
      in
      row :: close (remaining - 1) rows
  in
  close count declarations

let get_declarations ~token:_ ~(doc : Fleche.Doc.t) :
    Document_declaration.t list Petanque.Agent.R.t =
  let errors =
    Fleche.Doc.diags doc |> List.filter Lang.Diagnostic.is_error
  in
  if errors <> [] then
    Error
      Petanque.Agent.Error.(
        make_request
          (coq
             (Format.asprintf "document has Rocq errors: %a"
                (Format.pp_print_list pp_diag) errors)))
  else
  let scopes = ref [] in
  let declarations = ref [] in
  let pending = ref 0 in
  let asts = Fleche.Doc.asts doc in
  let fail message =
    Error Petanque.Agent.Error.(make_request (coq message))
  in
  let rec visit = function
    | [] -> Ok (List.rev !declarations)
    | ({ Fleche.Doc.Node.Ast.v; _ } as ast) :: rest ->
      let command = command_of_ast v in
      let rows, opened = declaration_records ~doc !scopes ast in
      declarations := List.rev_append rows !declarations;
      if opened > 0 then pending := opened;
      let range =
        let loc = Coq.Ast.loc v |> Option.get in
        Coq.Utils.to_range ~lines:(Fleche.Doc.lines doc) loc
      in
      (match command with
      | VernacSynPure (VernacEndProof end_kind) when !pending > 0 ->
        let proof_finished =
          match end_kind with Admitted -> false | Proved _ -> true
        in
        declarations :=
          close_declarations !declarations !pending ~end_:range.end_.offset
            ~proof_finished;
        pending := 0
      | VernacSynPure VernacAbort when !pending > 0 ->
        declarations :=
          close_declarations !declarations !pending ~end_:range.end_.offset
            ~proof_finished:false;
        pending := 0
      | _ -> ());
      (match scope_transition command with
      | None -> visit rest
      | Some (`Open scope) ->
        scopes := scope :: !scopes;
        visit rest
      | Some (`Close name) -> (
        match !scopes with
        | Declaration_scope.Module open_name :: tail
        | Declaration_scope.Section open_name :: tail
          when String.equal open_name name ->
          scopes := tail;
          visit rest
        | _ -> fail "PET document scope closes out of order"))
  in
  visit asts

let insertion_point ~(doc : Fleche.Doc.t) modules :
    int Petanque.Agent.R.t =
  let fail message =
    Error Petanque.Agent.Error.(make_request (coq message))
  in
  match modules with
  | [] -> (
    match read_raw ~uri:doc.uri with
    | Ok raw -> Ok (String.length raw)
    | Error error -> Error error)
  | _ ->
    let scopes = ref [] in
    let matches = ref [] in
    List.iter
      (fun ({ Fleche.Doc.Node.Ast.v; _ } as _ast) ->
        let command = command_of_ast v in
        let range =
          let loc = Coq.Ast.loc v |> Option.get in
          Coq.Utils.to_range ~lines:(Fleche.Doc.lines doc) loc
        in
        (match command, !scopes with
        | VernacSynterp (VernacEndSegment name),
          Declaration_scope.Module open_name :: _
          when String.equal open_name (lident_name name)
               && Declaration_scope.modules !scopes = modules ->
          matches := range.start.offset :: !matches
        | _ -> ());
        match scope_transition command with
        | None -> ()
        | Some (`Open scope) -> scopes := scope :: !scopes
        | Some (`Close name) -> (
          match !scopes with
          | Declaration_scope.Module open_name :: tail
          | Declaration_scope.Section open_name :: tail
            when String.equal open_name name -> scopes := tail
          | _ -> ()))
      (Fleche.Doc.asts doc);
    (match !matches with
    | [ offset ] -> Ok offset
    | [] -> fail "requested PET module path does not exist"
    | _ -> fail "requested PET module path is ambiguous")
