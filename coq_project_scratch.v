Require Vectors.Vector.
Require Vectors.Fin.
Import Vector.

Inductive MessageType : Type :=
| Base : Type -> MessageType
| Channel : SessionType -> MessageType

with SessionType : Type := 
  | End : SessionType
  | Send : MessageType -> SessionType -> SessionType
  | Rece : MessageType -> SessionType -> SessionType
  | Choose : SessionType -> SessionType -> SessionType
  | Offer : forall {n}, Vector.t SessionType n -> SessionType
.

Definition testST (st : SessionType) : SessionType :=
  match st with 
  | End => End
  | Send m s => s
  | Rece m s => s
  | Choose a b => a
  | Offer ss => End
  end.

Compute testST (Send (Base nat) (Send (Base nat) End)).
Compute testST (Rece (Base nat) (Rece (Base nat) End)). 
Compute testST (Choose End End). 


(*ATM example: https://stanford-cs242.github.io/f18/lectures/07-2-session-types.html#session-type-formalism*)

(*As far as I understand, SessionType does not require explicit implementation such as checking balance or
update account balance. All we implement is just ``TYPE``, which is a high level abstract *)

Definition ATMDeposit (amount : nat) : SessionType :=
  Rece (Base nat) (Send (Base nat) End)
.

Compute ATMDeposit 100.

Definition ATMWithdraw (amount : nat) : SessionType :=
  Rece (Base nat) (Choose End End)
.

Compute ATMWithdraw 10.
