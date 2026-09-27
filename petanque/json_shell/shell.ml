module SM = Lang.Compat.String.Map

open Vernacexpr

let init_coq ~debug ~record_comments =
  let load_module = Dynlink.loadfile in
  let load_plugin = Coq.Loader.plugin_handler None in
  let vm, warnings = (true, None) in
  Coq.Init.(
    coq_init { debug; record_comments; load_module; load_plugin; vm; warnings })

let cmdline : Coq.Workspace.CmdLine.t =
  { coqlib = Coq.Args.coqlib_dyn
  ; findlib_config = None
  ; ocamlpath = []
  ; vo_load_path = []
  ; args = []
  ; require_libraries = []
  }

let setup_workspace ~token ~init ~debug ~root =
  let dir = Lang.LUri.File.to_string_file root in
  (let open Coq.Compat.Result.O in
   let+ workspace = Coq.Workspace.guess ~token ~debug ~cmdline ~dir () in
   let files = Coq.Files.make () in
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

let set_workspace ~token ~debug ~root =
  let init = Option.get !init_st in
  let open Coq.Compat.Result.O in
  let+ env_ = setup_workspace ~token ~init ~debug ~root in
  env := Some env_

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
  set_workspace ~token ~debug ~root

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

let declaration_kind detail =
  match detail with
  | "Theorem" | "Lemma" | "Fact" | "Remark" | "Corollary" | "Proposition"
    | "Definition" -> Some detail
  | _ -> None

let normalize_statement source =
  let source = String.trim source in
  let length = String.length source in
  if length > 0 && Char.equal source.[length - 1] '.' then
    String.sub source 0 (length - 1) |> String.trim
  else source

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
  let qualified_prefix = Declaration_scope.modules scopes in
  List.concat_map
    (fun (info : Lang.Ast.Info.t) ->
      match info.name.v, Option.bind info.detail declaration_kind with
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
                 ; statement
                 }))
    ast_info

let get_declarations ~token:_ ~(doc : Fleche.Doc.t) :
    Document_declaration.t list Petanque.Agent.R.t =
  let scopes = ref [] in
  let declarations = ref [] in
  let asts = Fleche.Doc.asts doc in
  let fail message =
    Error Petanque.Agent.Error.(make_request (coq message))
  in
  let rec visit = function
    | [] -> Ok (List.rev !declarations)
    | ({ Fleche.Doc.Node.Ast.v; _ } as ast) :: rest ->
      let command = command_of_ast v in
      declarations :=
        List.rev_append (declaration_records ~doc !scopes ast) !declarations;
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
