open Bogue

let network_request host port request_body =
  Draw.set_system_cursor Tsdl.Sdl.System_cursor.wait;

  (* Create a socket and connect to the server *)
  let socket = Unix.socket PF_INET SOCK_STREAM 0 in
  let server_addr = (Unix.gethostbyname host).h_addr_list.(0) in
  let server_sockaddr = Unix.ADDR_INET (server_addr, port) in
  Unix.connect socket server_sockaddr;

  (* Send the request *)
  let _ = Unix.send socket (Bytes.of_string request_body) 0 (String.length request_body) [] in

  (* Buffer to store the entire response *)
  let buffer_size = 4096 in
  let buffer = Bytes.create buffer_size in
  let rec read_response acc =
    match Unix.recv socket buffer 0 buffer_size [] with
    | 0 -> acc  (* End of data *)
    | len ->
        let response_part = Bytes.sub_string buffer 0 len in
        read_response (acc ^ response_part)
  in

  (* Read all data and close the socket *)
  let response = read_response "" in
  Unix.close socket;
  Draw.set_system_cursor Tsdl.Sdl.System_cursor.arrow;
  response