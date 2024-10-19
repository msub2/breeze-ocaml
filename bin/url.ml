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

let parse_finger_url url = 
  match String.split_on_char '/' url with
  | host :: selector_parts -> 
    let request_body = String.concat "/" selector_parts in
    Success (host, 79, request_body ^ "\r\n", Plaintext)
  | [] -> Failure Malformed

(** Extracts the host, port, and selector from a gopher URL.
    This assumes that the gopher:// prefix has already been stripped. *)
let parse_gopher_url url =
  match String.split_on_char '/' url with
  | host :: selector_parts -> 
    let request_body = String.concat "/" selector_parts in
    Success (host, 70, request_body ^ "\r\n", Gopher)
  | [] -> Failure Malformed

(** Extracts the host, port, and selector from a gemini URL.
    This assumes that the gemini:// prefix has already been stripped. *)
let parse_gemini_url url =
  (* TODO: More stringent parsing *)
  match String.split_on_char '/' url with
  | host :: _ -> 
    Success (host, 1965, "gemini://" ^ url ^ "\r\n", Gemini)
  | [] -> Failure Malformed

(** Extracts the host, port, and selector from a spartan URL.
    This assumes that the spartan:// prefix has already been stripped. *)
let parse_spartan_url host path =
  (* TODO: More stringent parsing *)
  let request_body = String.concat " " [host; "/" ^ path; "0\r\n"] in
  Success (host, 300, request_body, Spartan)

(** Takes in a URL and attempts to extract the necessary parameters to pass
    to the actual network request *)
let parse_url url = 
  let uri = Uri.of_string url in
  let host = match Uri.host uri with
  | Some host -> host
  | None -> failwith "bad host" in
  let path = Uri.path uri in
  match Uri.scheme uri with
  | Some "finger" -> parse_finger_url (host ^ path)
  | Some "gopher" -> parse_gopher_url (host ^ path)
  | Some "gemini" -> parse_gemini_url (host ^ path)
  | Some "spartan" -> parse_spartan_url host path
  | _ ->  "Bad URL: " ^ url |> failwith;