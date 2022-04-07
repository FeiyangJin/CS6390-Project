Require Vectors.Vector.
Require Vectors.Fin.
Import Vector.VectorNotations.

(*Right now assume all message types are nat*)
Inductive SessionType : Type := 
  | End : SessionType
  | Send : nat -> SessionType -> SessionType
  | Rece : nat -> SessionType -> SessionType
  | Choose : forall {n}, Vector.t SessionType n -> SessionType
  | Offer : forall {n}, Vector.t SessionType n -> SessionType
.

Definition testST (st : SessionType) : SessionType :=
  match st with 
  | End => End
  | Send m s => s
  | Rece m s => s
  | Choose ss => End
  | Offer ss => End
  end.

Compute testST (Send 5 (Send 6 End)).
Compute testST (Rece 7 (Rece 8 End)). 


