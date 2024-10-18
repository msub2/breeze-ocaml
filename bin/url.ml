open Protocols

(** Parsing/Validation for URLs *)

type parse_error =
  | InvalidProtocol
  | Malformed

(** The result of parsing the given URL 
    - Success: Returns URL, port, request body
    - Failure: Returns a parsing error *)
type parse_result =
  | Success of string * int * string * protocol
  | Failure of parse_error

(** Extracts the host, port, and selector from a gopher URL.
    This assumes that the gopher:// prefix has already been stripped. *)
let parse_gopher_url url =
  match String.split_on_char '/' url with
  | host :: selector_parts -> 
    let request_body = String.concat "/" selector_parts in
    Success (host, 70, request_body, Gopher)
  | [] -> Failure Malformed

(** Extracts the host, port, and selector from a gemini URL.
    This assumes that the gemini:// prefix has already been stripped. *)
let parse_gemini_url url =
  (* TODO: More stringent parsing *)
  match String.split_on_char '/' url with
  | host :: _ -> 
    Success (host, 1965, "gemini://" ^ url, Gemini)
  | [] -> Failure Malformed

(** Takes in a URL and attempts to extract the necessary parameters to pass
    to the actual network request *)
let parse_url url = 
  match url with
  | url when String.starts_with ~prefix:"gopher://" url -> parse_gopher_url (String.sub url 9 (String.length url - 9))
  | url when String.starts_with ~prefix:"gemini://" url -> parse_gemini_url (String.sub url 9 (String.length url - 9))
  | _ ->  "Bad URL: " ^ url |> failwith;