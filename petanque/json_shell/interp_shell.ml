(************************************************************************)
(* Coq Petanque                                                         *)
(* Copyright 2019 MINES ParisTech -- Dual License LGPL 2.1 / GPL3+      *)
(* Copyright 2019-2024 Inria      -- Dual License LGPL 2.1 / GPL3+      *)
(************************************************************************)

module Lsp = Fleche_lsp
open Petanque_json.Interp
open Protocol_shell

let do_handle ~fn ~token action =
  match action with
  | Action.Now handler -> handler ~token
  | Action.Doc { uri; handler } ->
    let open Coq.Compat.Result.O in
    let* doc = fn ~token ~uri |> of_pet_err in
    handler ~token ~doc
  | Action.Pos { uri; point; handler } ->
    let open Coq.Compat.Result.O in
    let* doc = fn ~token ~uri |> of_pet_err in
    handler ~token ~doc ~point

(* Duplicate with lsp_core *)
let feedback_to_message (level, { Coq.Message.Payload.range; msg; _ }) =
  let point ({ line; character; offset } : Lsp.JLang.Point.t) =
    `Assoc
      [ ("line", `Int line)
      ; ("character", `Int character)
      ; ("offset", `Int offset)
      ]
  in
  let range =
    match range with
    | None -> `Null
    | Some ({ start; end_ } : Lsp.JLang.Range.t) ->
      `Assoc [("start", point start); ("end", point end_)]
  in
  `Assoc
    [ ("range", range)
    ; ("level", `Int level)
    ; ("text", `String (Pp.string_of_ppcmds msg))
    ]

let feedback_to_data fbs =
  match fbs with
  | [] -> None
  | fbs -> Some (`List (List.map feedback_to_message fbs))

let request ~fn ~token ~id ~method_ ~params =
  let unhandled ~token ~method_ =
    match method_ with
    | s when String.equal SetWorkspace.method_ s ->
      do_handle ~fn ~token (do_request (module SetWorkspace) ~params)
    | s when String.equal TableOfContents.method_ s ->
      do_handle ~fn ~token (do_request (module TableOfContents) ~params)
    | s when String.equal DocumentDeclarations.method_ s ->
      do_handle ~fn ~token (do_request (module DocumentDeclarations) ~params)
    | s when String.equal InsertionPoint.method_ s ->
      do_handle ~fn ~token (do_request (module InsertionPoint) ~params)
    | s when String.equal Capabilities.method_ s ->
      do_handle ~fn ~token (do_request (module Capabilities) ~params)
    | s when String.equal ReleaseStates.method_ s ->
      do_handle ~fn ~token (do_request (module ReleaseStates) ~params)
    | s when String.equal StateCount.method_ s ->
      do_handle ~fn ~token (do_request (module StateCount) ~params)
    | s when String.equal RefreshWorkspace.method_ s ->
      do_handle ~fn ~token (do_request (module RefreshWorkspace) ~params)
    | _ ->
      (* JSON-RPC method not found *)
      let code = -32601 in
      let message = Format.asprintf "method %s not found" method_ in
      Error (Request.Error.make code message)
  in
  let do_handle = do_handle ~fn in
  match handle_request ~do_handle ~unhandled ~token ~method_ ~params with
  | Ok result -> Lsp.Base.Response.mk_ok ~id ~result
  | Error Request.Error.{ code; payload; feedback } ->
    (* for now *)
    let message = payload in
    let data = feedback_to_data feedback in
    Lsp.Base.Response.mk_error ~id ~code ~message ~data

type doc_handler =
     token:Coq.Limits.Token.t
  -> uri:Lang.LUri.File.t
  -> Fleche.Doc.t Petanque.Agent.R.t

let interp ~fn ~token (r : Lsp.Base.Message.t) : Lsp.Base.Message.t option =
  match r with
  | Request { id; method_; params } ->
    let response = request ~fn ~token ~id ~method_ ~params in
    Some (Lsp.Base.Message.response response)
  | Notification { method_; params = _ } ->
    let message = "unhandled notification: " ^ method_ in
    let log = Lsp.Base.mk_logTrace ~message ~verbose:None in
    Some (Lsp.Base.Message.Notification log)
  | Response (Ok { id; _ }) | Response (Error { id; _ }) ->
    let message = "unhandled response: " ^ string_of_int id in
    let log = Lsp.Base.mk_logTrace ~message ~verbose:None in
    Some (Lsp.Base.Message.Notification log)
