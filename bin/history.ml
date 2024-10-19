open Helpers
open Protocols

type history_action = 
  | Forward
  | Back

module History = struct
  let history : (string * content_type) list ref = ref []
  let history_index = ref 0

  let add_entry entry =
    (* Trim the history to the current point (discard any "forward" entries) *)
    let new_history = (Helpers.take (!history_index + 1) (List.rev !history)) in

    (* Only add the entry if it's different from the last entry in the trimmed history *)
    let should_add = match new_history with
      | last_entry :: _ -> fst last_entry <> fst entry
      | [] -> true
    in
    
    if should_add then begin
      history := entry :: new_history;
      history_index := List.length new_history
    end
  
  let history_forward () =
    history_index := Helpers.clamp 0 ((List.length !history) - 1) (!history_index + 1)

  let history_back () =
    history_index := Helpers.clamp 0 ((List.length !history) - 1) (!history_index - 1)

  let can_go_forward () = 
    List.length !history - 1 > !history_index
  let can_go_backward () = 
    !history_index > 0

  let get_history () =
    List.nth (List.rev !history) !history_index
end
