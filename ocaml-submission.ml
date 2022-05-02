(* Author: Feiyang Jin
Contact: fjin35@gatech.edu *)

(*To build this file, use the following command:
ocamlc -thread -I +threads unix.cma threads.cma ocaml-submission.ml
*)

let bank = [(1, ref 999); (2, ref 888); (3, ref 777)] ;;

let rec user_in_bank (l : (int * int ref) list) (user_id : int) : bool = 
    match l with
    | [] -> false
    | h :: t -> match h with 
                | (id, balance) -> if id = user_id then true else user_in_bank t user_id ;;


let rec get_balance (l : (int * int ref) list) (user_id: int) : int = 
  match l with
    | [] -> -1
    | h :: t -> match h with 
                | (id, balance) -> if id = user_id then !balance else get_balance t user_id ;;

let rec update_balance (l : (int * int ref) list) (user_id: int) (new_balance: int) : unit = 
  match l with
    | [] -> ()
    | h :: t -> match h with 
                | (id, balance) -> if id = user_id then (balance := new_balance)
                                   else update_balance t user_id new_balance;;

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
                        let () = Printf.printf "atmServer: Enter the amount you want to deposit: " in
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
                      | Choose (a,b) -> let () = Printf.printf "atmServer: Enter the amount you want to withdraw: " in
                        let e7 = Event.send atmChannel (Rece (IntBase (-1), (Send (IntBase (-1), End)))) in
                          Event.sync e7;

                        let e8 = Event.receive atmChannel in
                        let withdrawlResponse = Event.sync e8 in
                        (
                          match withdrawlResponse with
                          | Send (intmessage, nexts) ->
                          (
                            match intmessage with
                            | IntBase (amount) -> 
                              let () = Printf.printf "atmServer: you withdraw amount is %d \n" amount in
                              let oldbalance = (get_balance bank id) in
                              if oldbalance < amount then
                                let () = Printf.printf "atmServer: You do not have enough balance. Session ends \n " in
                                let e9 = Event.send atmChannel End in 
                                Event.sync e9; 
                                End
                              else
                                let newbalance = oldbalance - amount in
                                let () = (update_balance bank id newbalance) in
                                let e9 = Event.send atmChannel (Send ((IntBase (newbalance), End))) in
                                Event.sync e9; 
                                End
                            | _ -> let () = Printf.printf "atmServer: Error. Session ends \n " in End
                          )
                          | _ -> let () = Printf.printf "atmServer: Error. Session ends \n " in End
                        )
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
        Event.sync e3;

        let e7 = Event.receive atmChannel in
        let atmWithdrawRequest = Event.sync e7 in
        (
          match atmWithdrawRequest with
          | Rece (m,s) ->
            let amount = read_int() in
            let e8 = Event.send atmChannel (Send (IntBase (amount), End)) in
            Event.sync e8;

            let e9 = Event.receive atmChannel in
            let newbalanceSession = Event.sync e9 in
            (
              match newbalanceSession with
              | End -> End
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
        )  
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
