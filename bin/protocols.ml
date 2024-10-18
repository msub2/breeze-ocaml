open Bogue

(* Not yet used *)
type protocol = 
  | Plaintext
  | Gopher
  | GopherPlus
  | Gemini
  | Spartan
  | Guppy
  | Nex
  | Unknown

type content_type = 
  | Plaintext
  | Image
  | Gophermap
  | Gemtext
  | HTML
  | Unknown

let network_request ?(ssl = false) host port request_body =
  Draw.set_system_cursor Tsdl.Sdl.System_cursor.wait;

  (* Create a socket and connect to the server *)
  let socket = Unix.socket PF_INET SOCK_STREAM 0 in
  let server_addr = 
    try 
      (Unix.gethostbyname host).h_addr_list.(0)
    with 
    | Not_found ->
      print_endline ("Host not found: " ^ host);
      raise (Failure "Error: Could not connect")
  in
  let server_sockaddr = Unix.ADDR_INET (server_addr, port) in
  Unix.connect socket server_sockaddr;

  match ssl with
  | true ->
    (* Set up the TLS configuration *)
    let config = Tls.Config.client ~authenticator:(fun ?ip ~host:_ _ -> let _ = ip in Ok None) ~version:(`TLS_1_2, `TLS_1_3) () in
    let client_config = match config with
      | Ok client -> client
      | Error _ -> failwith "failed while making TLS client"
    in

    let (tls_state, client_hello) = Tls.Engine.client client_config in

    (* Send the client hello to initiate the handshake *)
    let _ = Unix.send socket (String.to_bytes client_hello) 0 (String.length client_hello) [] in

    (* Function to handle the TLS handshake *)
    let rec handshake tls_state =
      (* Receive data from the server *)
      let buffer = Bytes.create 16384 in
      let len = Unix.recv socket buffer 0 (Bytes.length buffer) [] in
      let server_response = Bytes.sub_string buffer 0 len in

      match Tls.Engine.handle_tls tls_state server_response with
      | Ok (new_state, _eof, `Response (Some response), `Data data) ->
          (* Send the TLS response back to the server *)
          let _ = Unix.send socket (String.to_bytes response) 0 (String.length response) [] in
          (* If handshake is complete, proceed to communication *)
          if Tls.Engine.handshake_in_progress new_state then
            handshake new_state
          else
            (new_state, data)
      | Error (alert, _) ->
          failwith ("TLS handshake failed: " ^ Tls.Engine.string_of_failure alert)
      | _ ->
          failwith "Unexpected TLS handshake result"
    in

    let (tls_state, _handshake_data) = handshake tls_state in

    (* Function to send and receive application data *)
    let send_data tls_state data =
      match Tls.Engine.send_application_data tls_state [data] with
      | Some (new_state, message) ->
          (* Send encrypted data *)
          let _ = Unix.send socket (String.to_bytes message) 0 (String.length message) [] in
          new_state
      | None ->
          failwith "Error: Could not send application data"
    in

    let rec receive_data tls_state =
      let buffer = Bytes.create 16384 in
      let len = Unix.recv socket buffer 0 (Bytes.length buffer) [] in
      let server_response = Bytes.sub_string buffer 0 len in

      match Tls.Engine.handle_tls tls_state server_response with
      | Ok (new_state, Some `Eof, _, `Data (Some data)) -> (new_state, data)
      | Ok (new_state, None, _, `Data (Some data)) -> 
        let (new_new_state, new_data) = receive_data new_state in
        (new_new_state, data ^ new_data)
      | Ok (_, _, _, `Data None) -> failwith "Error: No data received"
      | Error (alert, _) -> failwith ("TLS error: " ^ Tls.Engine.string_of_failure alert)
    in

    (* Send the request body over TLS *)
    let tls_state = send_data tls_state request_body in

    (* Receive the server's response *)
    let (_tls_state, response) = receive_data tls_state in

    (* Close the socket *)
    Unix.close socket;
    response
  | false ->
    (* Send the request *)
    let _ = Unix.send socket (Bytes.of_string request_body) 0 (String.length request_body) [] in

    (* Buffer to store the entire response *)
    let buffer_size = 16384 in
    let buffer = Bytes.create buffer_size in
    let rec read_response acc =
      match Unix.recv socket buffer 0 buffer_size [] with
      | 0 -> acc  (* End of data *)
      | len ->
          let response_part = Bytes.sub_string buffer 0 len in
          read_response (acc ^ response_part)
    in

    (* Read all data and close the socket *)
    let raw_response = String.trim (read_response "") in
    Unix.close socket;
    Draw.set_system_cursor Tsdl.Sdl.System_cursor.arrow;
    let response = match String.ends_with ~suffix:"\n." raw_response with
    | true -> String.sub raw_response 0 ((String.length raw_response) - 1)
    | false -> raw_response in
    response