Require Vectors.Vector.
Require Vectors.Fin.
Import Vector.

Inductive MessageType : Type :=
| Base : Type -> MessageType
| natBase : nat -> MessageType
| Channel : SessionType -> MessageType

with SessionType : Type := 
  | End : SessionType
  | Send : MessageType -> SessionType -> SessionType
  | Rece : MessageType -> SessionType -> SessionType
  | Choose : SessionType -> SessionType -> SessionType
  | Offer : SessionType -> SessionType -> SessionType
  | atmoffer : (nat -> SessionType) -> (nat -> SessionType) -> SessionType
  (* | Offer : forall {n}, Vector.t SessionType n -> SessionType *)
.

Definition testST (st : SessionType) : SessionType :=
  match st with 
  | End => End
  | Send m s => s
  | Rece m s => s
  | Choose a b => a
  | Offer a b => End
  | atmoffer a b => a 0
  end.

Compute testST (Send (Base nat) (Send (Base nat) End)).
Compute testST (Rece (Base nat) (Rece (Base nat) End)). 
Compute testST (Choose (Send (Base nat) End) End). 
Compute testST (Offer (Choose (Send (Base nat) End) End) (Send (Base nat) (Send (Base nat) End))).

(*ATM example: https://stanford-cs242.github.io/f18/lectures/07-2-session-types.html#session-type-formalism*)

(*As far as I understand, SessionType does not require explicit implementation such as checking balance or
update account balance. All we implement is just ``TYPE``, which is a high level abstract *)

Definition ATMDeposit : SessionType :=
  Rece (Base nat) (Send (Base nat) End)
.

Compute ATMDeposit.

Definition ATMWithdraw : SessionType :=
  Rece (Base nat) (Choose End End)
.

Compute ATMWithdraw.

Definition ATMServer : SessionType :=
  Rece (Base nat) (Choose (Offer ATMDeposit ATMWithdraw) (End))
.

Compute ATMServer. 


(* The following is the atm example implemented by intuition. *)


Definition balance: nat := 100.


(* TODO: figure out how to update balance*)
Definition atmdeposit (amount : nat) : SessionType :=
  Send (natBase (amount + balance)) End
.

Compute atmdeposit 200.

Fixpoint leb (n m : nat) : bool :=
  match n with
  | O => true
  | S n' =>
      match m with
      | O => false
      | S m' => leb n' m'
      end
  end.

(* TODO: figure out how to update balance*)
Definition atmwithdraw (amount : nat) : SessionType :=
  match (leb amount balance) with
  | true =>  End
  | false => End
  end
.


Definition checkid (id : nat) : SessionType :=
  match id with 
  | 0 => End
  | S n => (atmoffer atmdeposit atmwithdraw)
  (* | S n => (Offer ATMDeposit ATMWithdraw) *)
  end.

Definition testid : nat := 55.
Definition testamount : nat := 40.


(* TODO: 
  1. how to print
  2. how to read user input
  3. how to organize this as a real session type application *)
Definition atmserver (id choice amount : nat) : SessionType :=
  match (checkid id) with 
  | (atmoffer deposit withdraw) => 
      match choice with
      | 0 => (* go left *) match (deposit amount) with 
                           | Send a b => (* TODO: how to print the updated balance here*) End
                           | _ => End
                           end
      | S n => (* go right *) (withdraw amount)
      end
  | _ => End
  end
.

Compute atmserver 100 0 50.




