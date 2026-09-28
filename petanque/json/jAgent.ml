(************************************************************************)
(* Copyright 2019 MINES ParisTech -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2019-2024 Inria      -- Dual License LGPL 2.1+ / GPL3+     *)
(* Copyright 2024-2025 Emilio J. Gallego Arias -- LGPL 2.1+ / GPL3+     *)
(* Copyright 2025      CNRS                    -- LGPL 2.1+ / GPL3+     *)
(* Written by: Emilio J. Gallego Arias & rocq-lsp contributors          *)
(************************************************************************)
(* Flèche => RL agent: petanque                                         *)
(************************************************************************)

(* Serialization for agent types *)
module Lsp = Fleche_lsp

(* Implement State.t and Env.t serialization methods *)
module State = Obj_map.Make (Petanque.Agent.State)

module Inspect = struct
  type t = [%import: Petanque.Agent.State.Inspect.t] [@@deriving yojson]
end

(* The typical protocol dance *)
module Error = struct
  type t = [%import: Petanque.Agent.Error.t] [@@deriving yojson]
end

module Run_opts = struct
  type t = [%import: Petanque.Agent.Run_opts.t] [@@deriving yojson]
end

module Run_result = struct
  type 'a t = [%import: 'a Petanque.Agent.Run_result.t] [@@deriving yojson]
end

(* Both are needed as of today *)
module Stdlib = Lsp.JStdlib
module Result = Stdlib.Result

module Goal_opts = struct
  type t = [%import: Petanque.Agent.Goal_opts.t] [@@deriving yojson]
end

module Goals = struct
  type t = (string, string) Lsp.JCoq.Goals.reified option [@@deriving yojson]
end

module Ast = struct
  type t = Lsp.JCoq.Ast.t [@@deriving yojson]
end

module Lang = Lsp.JLang

module Premise = struct
  module Info = struct
    type t = [%import: Petanque.Agent.Premise.Info.t] [@@deriving yojson]
  end

  type t = [%import: Petanque.Agent.Premise.t] [@@deriving yojson]
end

module Assumption = struct
  type kind = [%import: Petanque.Agent.Assumption.kind]

  let kind_to_yojson = function
    | Axiom -> `String "axiom"
    | Positive -> `String "positive"
    | Guarded -> `String "guarded"
    | Type_in_type -> `String "type_in_type"
    | Uip -> `String "uip"
    | Section_variable -> `String "section_variable"
    | Opaque -> `String "opaque"
    | Transparent -> `String "transparent"

  let kind_of_yojson = function
    | `String "axiom" -> Ok Axiom
    | `String "positive" -> Ok Positive
    | `String "guarded" -> Ok Guarded
    | `String "type_in_type" -> Ok Type_in_type
    | `String "uip" -> Ok Uip
    | `String "section_variable" -> Ok Section_variable
    | `String "opaque" -> Ok Opaque
    | `String "transparent" -> Ok Transparent
    | _ -> Error "invalid assumption kind"

  type t = [%import: Petanque.Agent.Assumption.t] [@@deriving yojson]
  type theory = [%import: Petanque.Agent.Assumption.theory] [@@deriving yojson]
  type report = [%import: Petanque.Agent.Assumption.report] [@@deriving yojson]
end

module Proof_info = struct
  type t = [%import: Petanque.Agent.Proof_info.t] [@@deriving yojson]
end

module Notation_analysis = Fleche_lsp.JCoq.Notation_analysis
