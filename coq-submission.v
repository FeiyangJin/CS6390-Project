Inductive MessageType : Type :=
| Base : Type -> MessageType
| Channel : SessionType -> MessageType

with SessionType : Type := 
  | End : SessionType
  | Send : MessageType -> SessionType -> SessionType
  | Rece : MessageType -> SessionType -> SessionType
  | Choose : SessionType -> SessionType -> SessionType
  | Offer : SessionType -> SessionType -> SessionType
  | Mu : RecSessionType -> SessionType

with RecSessionType : Type :=
  RecTerm : SessionType -> RecSessionType
.

  
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
  
(* Recursion ATM *)
(* An ATM run at most n times *)
Fixpoint ATMServer' (n: nat) : SessionType :=
  match n with
  | O => End
  | S m => let ATMDeposit' : SessionType :=
             Rece (Base nat) (Send (Base nat) (ATMServer' m)) in 
           let ATMWithdraw' : SessionType :=
             Rece (Base nat) (Choose (ATMServer' m) (ATMServer' m)) in
           Rece (Base nat) (Mu (RecTerm (Offer ATMDeposit' (Offer ATMWithdraw' End))))
  end.

Compute ATMServer'. 

(* Client Duality *)
Definition ClientServer : SessionType := 
  Send (Base nat) (Offer (Choose (Offer ATMDeposit ATMWithdraw) End) End)
.
