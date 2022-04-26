(* Author: Feiyang Jin
Contact: fjin35@gatech.edu *)

(* let bank = [[ref 1; ref 999]; [ref 2; ref 888]; [ref 3; ref 777]] ;;

let rec user_in_bank (l : int ref list list) (user_id : int) : bool = 
    match l with
    | [] -> false
    | h :: t -> match h with 
                | [] -> user_in_bank t user_id
                | id :: balance -> if !id = user_id then true else user_in_bank t user_id ;; *)

let bank = [(1, ref 999); (2, ref 888); (3, ref 777)] ;;

let rec user_in_bank (l : (int * int ref) list) (user_id : int) : bool = 
    match l with
    | [] -> false
    | h :: t -> match h with 
                | (id, balance) -> if id = user_id then true else user_in_bank t user_id ;;

let valid_id = 3;;
let invalid_id = 99 ;;

Printf.printf "%d is in bank account: %b \n" valid_id (user_in_bank bank valid_id) ;;

Printf.printf "%d is in bank account: %b \n" invalid_id (user_in_bank bank invalid_id) ;;


let rec get_balance (l : (int * int ref) list) (user_id: int) : int = 
  match l with
    | [] -> -1
    | h :: t -> match h with 
                | (id, balance) -> if id = user_id then !balance else get_balance t user_id ;;

Printf.printf "%d has balance %d \n" valid_id (get_balance bank valid_id) ;;

let rec update_balance (l : (int * int ref) list) (user_id: int) (new_balance: int) : unit = 
  match l with
    | [] -> ()
    | h :: t -> match h with 
                | (id, balance) -> if id = user_id then (balance := new_balance)
                                   else update_balance t user_id new_balance;;

(* update_balance bank 3 1999 ;;
Printf.printf "%d has balance %d \n" valid_id (get_balance bank valid_id) ;; *)

type messageType =
| Base
| IntBase of int
| Channel of sessionType
and sessionType =
| End
| Send of messageType * sessionType
| Rece of messageType * sessionType
| Choose of sessionType * sessionType
| Offer of sessionType * sessionType ;;


let aTMDeposit (user_id : int) (i : int) : sessionType=
(* update and return balance in bank*)
  let oldbalance = (get_balance bank user_id) in 
    let newbalance = i + oldbalance in
      let () = (update_balance bank user_id newbalance) in
        Send (IntBase newbalance, End) ;;
        (* Rece (IntBase i, (Send (IntBase newbalance, End))) ;; *)

aTMDeposit valid_id 200 ;;
Printf.printf "After deposit 200, %d has balance %d \n" valid_id (get_balance bank valid_id) ;;


let aTMWithdraw (user_id : int) (amount : int) : sessionType =
  let oldbalance = (get_balance bank user_id) in
    if oldbalance < amount then 
      let () = Printf.printf "Error: you do not have enough balance \n" in End
      else 
          let newbalance = oldbalance - amount in
          let () = (update_balance bank user_id newbalance) in 
            let () = Printf.printf "New balance : %d \n" newbalance in
              Send (IntBase newbalance, End) ;;
              (* Rece (IntBase amount) (Choose End End) *)

Printf.printf "try to withdrawl 1 million \n" ;;
aTMWithdraw valid_id 1000000 ;;

Printf.printf "try to withdrawl 500 \n" ;;
aTMWithdraw valid_id 500 ;;

let c : string Event.channel = Event.new_channel ();;

let v : string ref = ref "before event" ;;

let f () = 
  let e = Event.receive c in 
    v := Event.sync e ;;

let g () = 
  let e2 = Event.send c "hello" in
  Event.sync e2 ;;

let t1,t2 = Thread.create f (), Thread.create g () ;;

Thread.join t2 ;;
Thread.join t1 ;;

Printf.printf "the value in v is: %s \n" !v ;;

(* let condition : bool ref = ref false ;;

let h () = 
  while not (!condition) do
    Printf.printf "condition is false \n" ;
    Thread.delay 0.5 ;
  done ;
  Printf.printf "out of loop \n"
  ;;

let j () = 
  Thread.delay 2.5 ;
  condition := true;;

let t3,t4 = Thread.create h (), Thread.create j () ;;

Thread.join t4 ;;
Thread.join t3 ;; *)

(* let () =
  match v with
  | None -> Printf.printf "we do not receive any value from the channel ! \n"
  | Some a  -> Printf.printf "we do receive the value and it is %s \n" a
;; *)

let atmChannel : sessionType Event.channel = Event.new_channel ();;

let atmServerSession () : sessionType = 
    let e = Event.receive atmChannel in 
      let id_session = Event.sync e in
        match id_session with
        | Send (m,s) ->
          ( match m with
            | IntBase (id) -> 
              (
              if (user_in_bank bank id) then
                let () = Printf.printf "atmServer: you entered %d as id and you are in the bank; press 1 for deposit and 2 for withdrawl: " id in
                let e2 = Event.send atmChannel (Offer (Rece (IntBase (-1), (Send (IntBase (-1), End))), Rece (IntBase (-1), Choose (End,End)))) in
                  Event.sync e2;
                let e3 = Event.receive atmChannel in
                let userChoise = Event.sync e3 in
                match userChoise with
                | Choose (choice, other) -> 
                  (
                    match choice with 
                    | Rece (message, nextSession) ->
                    (
                      match nextSession with
                      | Send (m,s) -> 
                        let () = Printf.printf "Enter the amount you want to deposit: " in
                        let e4 = Event.send atmChannel (Rece (IntBase (-1), (Send (IntBase (-1), End)))) in
                          Event.sync e4;

                        let e5 = Event.receive atmChannel in
                        let depositResponse = Event.sync e5 in
                        (
                          match depositResponse with
                          | Send (intmessage, nexts) ->
                          (
                            match intmessage with
                            | IntBase (amount) -> 
                            let () = Printf.printf "atmServer: you deposit amount is %d \n" amount in
                            let oldbalance = (get_balance bank id) in 
                            let newbalance = amount + oldbalance in
                            let () = (update_balance bank id newbalance) in
                            let e6 = Event.send atmChannel (Send ((IntBase (newbalance), End))) in
                            Event.sync e6; 
                            End
                            | _ -> let () = Printf.printf "atmServer: Error. Session ends \n " in End
                          ) 
                          | _ -> let () = Printf.printf "atmServer: Error. Session ends \n " in End
                        )
                      | Choose (a,b) -> let () = Printf.printf "atmServer: You choose to withdraw \n " in End
                      (* TODO *)
                      | _ -> let () = Printf.printf "atmServer: Error. Session ends \n " in End
                    )
                    | _ -> let () = Printf.printf "atmServer: Error. Session ends \n " in End
                  )

                
                | _ -> let () = Printf.printf "atmServer: Error. Session ends \n " in End
              else
                let () = Printf.printf "atmServer: you entered %d and you are not in the bank. Session Ends \n" id in
                let e2 = Event.send atmChannel End in
                  Event.sync e2; End
              )  
            | _ -> let () = Printf.printf "atmServer: Error. Session ends \n " in End )
        | _ -> let () = Printf.printf "atmServer: Error. Session ends \n " in End 

;;


let clientServerSession () : sessionType = 
  Printf.printf "clientServer: Please enter your user id: " ;
  let id = read_int () in
    let e = Event.send atmChannel (Send (IntBase id, End)) in
    Event.sync e ;

  let e2 = Event.receive atmChannel in 
  let atmResponse = Event.sync e2 in
    match atmResponse with
    | End -> let () = Printf.printf "clientServer: atm response with End \n" in End
    | Offer (deposit, withdraw) ->
      let choice = read_int() in 
      if choice = 1 then
        let e3 = Event.send atmChannel (Choose (deposit,End)) in 
        Event.sync e3;
        let e4 = Event.receive atmChannel in
        let atmDepositRequest = Event.sync e4 in
        match atmDepositRequest with 
        | Rece (m,s) ->
          let amount = read_int() in 
          let e5 = Event.send atmChannel (Send (IntBase (amount), End)) in
          Event.sync e5;

          let e6 = Event.receive atmChannel in
          let newbalanceSession = Event.sync e6 in 
          (
            match newbalanceSession with
            | Send (newblanceMessage, endSession) ->
            (
              match newblanceMessage with
              | IntBase (newbalance) ->
              let () = Printf.printf "clientServer: your new balance is %d. Session Ends \n" newbalance in endSession
              | _ -> let () = Printf.printf "clientServer: Error. Session Ends \n" in End
            )
            | _ -> let () = Printf.printf "clientServer: Error. Session Ends \n" in End
          )
        | _ -> let () = Printf.printf "clientServer: Error. Session Ends \n" in End
      else
        let e3 = Event.send atmChannel (Choose (withdraw,End)) in 
        Event.sync e3; End
    | _ -> let () = Printf.printf "clientServer: Error. Session Ends \n" in End  
;;

let atmThread,clientThread = Thread.create atmServerSession (), Thread.create clientServerSession () ;;

Thread.join clientThread ;;
Thread.join atmThread ;;

(* let atmServer () : sessionType =
  Printf.printf "Please enter your user id: " ;
  let id = read_int () in
    if (user_in_bank bank id) then 
      let () = Printf.printf "you entered %d and you are in the bank; press 1 for deposit and 2 for withdrawl: " id in 
        let choice = read_int() in
          if choice = 1 then
            let () = Printf.printf "you choose to deposit; now enter the amount you want to deposit: " in
            let amount = read_int () in 
            let deposit_result = (aTMDeposit id amount) in 
              match deposit_result with
                | Send (m,s) -> 
                  ( match m with
                    | IntBase (newbalance) -> let () = Printf.printf "Your new balance is %d. Session Ends \n" newbalance in s
                    | Channel (s) -> End )
                | _ -> let () = Printf.printf "Error. Session ends \n " in End
          else
            let () = Printf.printf "you choose to withdraw; now enter the amount you want to withdraw:" in
            let amount = read_int () in
            let withdrawl_result = (aTMWithdraw id amount) in
              match withdrawl_result with
              | Send (m,s) ->
                ( match m with
                    | IntBase (newbalance) -> let () = Printf.printf "Your new balance is %d. Session Ends \n" newbalance in s
                    | Channel (s) -> End )
              | End -> let () = Printf.printf "You do not have enough balance. Session ends \n " in End
              | _ -> let () = Printf.printf "Error. Session ends \n " in End
    else
      let () = Printf.printf "you entered %d and you are not in the bank. Session Ends \n" id in 
        End ;;  *)