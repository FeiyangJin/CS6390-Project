Require Vectors.Vector.
Require Vectors.Fin.
Import Vector.

Inductive MessageType : Type :=
| Base : nat -> MessageType
| Channel : SessionType -> MessageType

with SessionType : Type := 
  | End : SessionType
  | Send : MessageType -> SessionType -> SessionType
  | Rece : MessageType -> SessionType -> SessionType
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

Compute testST (Send (Base 5) (Send (Base 6) End)).
Compute testST (Rece (Base 50) (Rece (Base 60) End)). 
