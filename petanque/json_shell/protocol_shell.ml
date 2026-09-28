(************************************************************************)
(* Flèche => RL agent: petanque                                         *)
(* Copyright 2019 MINES ParisTech -- Dual License LGPL 2.1 / GPL3+      *)
(* Copyright 2019-2024 Inria      -- Dual License LGPL 2.1 / GPL3+      *)
(* Written by: Emilio J. Gallego Arias & coq-lsp contributors           *)
(************************************************************************)

module Lsp = Fleche_lsp
open Petanque_json

(** [set_workspace { debug; root }] sets the current workspace to the directory
    specified in [root] *)
module SetWorkspace = struct
  let method_ = "petanque/setWorkspace"

  module LoadPath = struct
    type t =
      { physical : string
      ; logical : string
      ; implicit : bool
      }
    [@@deriving yojson]
  end

  module Params = struct
    type t =
      { debug : bool
      ; root : Lsp.JLang.LUri.File.t
      ; load_paths : LoadPath.t list [@default []]
      }
    [@@deriving yojson]
  end

  module Response = struct
    type t = unit [@@deriving yojson]
  end

  module Handler = struct
    module Params = Params
    module Response = Response

    let handler =
      Protocol.HType.Immediate
        (fun ~token { Params.debug; root; load_paths } ->
          let load_paths =
            List.map
              (fun { LoadPath.physical; logical; implicit } ->
                (physical, logical, implicit))
              load_paths
          in
          Shell.set_workspace ~token ~debug ~root ~load_paths)
  end
end

(** [toc { uri } ] returns the table of contents for a document; the semantics
    are quite Coq-specific, in particular, each sentence of the document can
    contribute *)
module TableOfContents = struct
  let method_ = "petanque/toc"

  module Params = struct
    type t = { uri : Lsp.JLang.LUri.File.t } [@@deriving yojson]
  end

  module Response = struct
    type t = (string * Lsp.JLang.Ast.Info.t list option) list
    [@@deriving yojson]
  end

  module Handler = struct
    module Params = Params
    module Response = Response

    let handler =
      Protocol.HType.FullDoc
        { uri_fn = (fun { Params.uri } -> uri)
        ; handler = (fun ~token ~doc _ -> Shell.get_toc ~token ~doc)
        }
  end
end

(** [document_declarations { uri } ] returns the declarations represented by the
    checked document. Unlike the historical leaf-keyed [toc] map, this response
    is an ordered list and therefore preserves duplicate leaves in distinct
    module paths. *)
module DocumentDeclarations = struct
  let method_ = "petanque/document_declarations"

  module Params = struct
    type t = { uri : Lsp.JLang.LUri.File.t } [@@deriving yojson]
  end

  module Response = struct
    type t = Document_declaration.t list [@@deriving yojson]
  end

  module Handler = struct
    module Params = Params
    module Response = Response

    let handler =
      Protocol.HType.FullDoc
        { uri_fn = (fun { Params.uri } -> uri)
        ; handler = (fun ~token ~doc _ -> Shell.get_declarations ~token ~doc)
        }
  end
end

(** Return the PET-owned insertion anchor for one exact module path. *)
module InsertionPoint = struct
  let method_ = "petanque/insertion_point"

  module Params = struct
    type t =
      { uri : Lsp.JLang.LUri.File.t
      ; modules : string list
      }
    [@@deriving yojson]
  end

  module Response = struct
    type t = int [@@deriving yojson]
  end

  module Handler = struct
    module Params = Params
    module Response = Response

    let handler =
      Protocol.HType.FullDoc
        { uri_fn = (fun { Params.uri; _ } -> uri)
        ; handler =
            (fun ~token:_ ~doc { Params.uri = _; modules } ->
              Shell.insertion_point ~doc modules)
        }
  end
end

(** A fail-fast description of the non-stock lifecycle operations required by
    the Rocq MCP wrapper. Capability names are stable protocol tokens. *)
module Capabilities = struct
  let method_ = "petanque/capabilities"

  module Params = struct
    type t = { marker : bool option [@default None] } [@@deriving yojson]
  end

  module Response = struct
    type t = string list [@@deriving yojson]
  end

  module Handler = struct
    module Params = Params
    module Response = Response

    let handler =
      Protocol.HType.Immediate
        (fun ~token:_ { Params.marker = _ } ->
          Ok
            [ "document_declarations_v2"
            ; "dune_workspace_v1"
            ; "insertion_point_v1"
            ; "atomic_run_v1"
            ; "release_states_v1"
            ; "refresh_workspace_v1"
            ; "state_count_v1"
            ; "structured_assumptions_v1"
            ; "typed_errors_v1"
            ])
  end
end

(** Remove exactly the requested JSON-exported state identifiers. *)
module ReleaseStates = struct
  let method_ = "petanque/release_states"

  module Params = struct
    type t = { states : int list } [@@deriving yojson]
  end

  module Response = struct
    type t =
      { released : int list
      ; missing : int list
      }
    [@@deriving yojson]
  end

  module Handler = struct
    module Params = Params
    module Response = Response

    let handler =
      Protocol.HType.Immediate
        (fun ~token:_ { Params.states } ->
          let released, missing = JAgent.State.release states in
          Ok { Response.released; missing })
  end
end

(** Return the number of state identifiers exported through this JSON shell.
    This diagnostic endpoint exists for lifecycle/leak tests; it exposes no
    state identity and is not part of the public MCP interface. *)
module StateCount = struct
  let method_ = "petanque/state_count"

  module Params = struct
    type t = { marker : bool option [@default None] } [@@deriving yojson]
  end

  module Response = struct
    type t = int [@@deriving yojson]
  end

  module Handler = struct
    module Params = Params
    module Response = Response

    let handler =
      Protocol.HType.Immediate
        (fun ~token:_ { Params.marker = _ } -> Ok (JAgent.State.cardinal ()))
  end
end

(** Invalidate all exported states and rebuild the shell workspace from the
    current source/build filesystem. *)
module RefreshWorkspace = struct
  let method_ = "petanque/refresh_workspace"

  module Params = struct
    type t = { marker : bool option [@default None] } [@@deriving yojson]
  end

  module Response = struct
    type t = unit [@@deriving yojson]
  end

  module Handler = struct
    module Params = Params
    module Response = Response

    let handler =
      Protocol.HType.Immediate
        (fun ~token { Params.marker = _ } ->
          (* Old IDs are invalid even when rebuilding the workspace fails; a
             caller must then replace this PET process rather than reuse it. *)
          JAgent.State.clear ();
          Shell.refresh_workspace ~token)
  end
end
