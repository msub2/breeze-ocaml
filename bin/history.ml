open Utils
open Protocols

type history_action = 
  | Forward
  | Back

module History = struct
  let history : (string * content_type) list ref = ref []
  let history_index = ref 0

  let add_entry entry =
    (* Trim the history to the current point (discard any "forward" entries) *)
    let new_history = (Utils.take (!history_index + 1) (List.rev !history)) in
    history := entry :: new_history;
    history_index := List.length new_history
  
  let history_forward () =
    history_index := Utils.clamp 0 ((List.length !history) - 1) (!history_index + 1)

  let history_back () =
    history_index := Utils.clamp 0 ((List.length !history) - 1) (!history_index - 1)

  let can_go_forward () = 
    List.length !history - 1 > !history_index
  let can_go_backward () = 
    !history_index > 0

  let get_history () =
    List.nth (List.rev !history) !history_index
end
