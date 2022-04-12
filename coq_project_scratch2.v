Require Vectors.Vector.
Require Vectors.Fin.
Import Vector.
Require Import Coq.Strings.Byte.
Require Import Ascii.

(* Multiparty Session Types *)

Inductive string : Set :=
  | EmptyString : string
  | String : ascii -> string -> string.

Definition Participant := string. 

Definition Label := string. 

Inductive Expression : Type :=
| Value : Expression
| Plus : Expression -> Expression -> Expression

with ProcessType : Type :=
  | ProcessEnd : ProcessType
  | Send : Participant -> Label -> Expression -> ProcessType -> ProcessType
  | Recv : Participant -> Label -> Expression -> ProcessType -> ProcessType
  | Branch : SessionType -> ProcessType -> ProcessType -> ProcessType

with SessionType : Type := 
  | Compose : Participant -> ProcessType -> SessionType
  | Parallel : SessionType -> SessionType -> SessionType.