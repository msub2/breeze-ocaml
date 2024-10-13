open Bogue

(* Window size constants *)
let _width = ref 640
let _height = ref 480

type gopher_line = {
  line_kind : char;
  text : string;
  selector : string;
  server : string;
  port : int;
};;

let new_gopher_line line_kind text selector server port = 
  let line = {
    line_kind = line_kind;
    text = text; 
    selector = selector; 
    server = server; 
    port = port;
  } in
  line

let parse_gopher_url url = 
  let chunks = String.split_on_char '/' url in
  let host = List.nth chunks 0 in
  let selector = String.concat "/" (List.tl chunks) in
  let port = 70 in
  (host, port, selector)

let build_gopher_line line =
  let chunks = String.split_on_char '\t' line in
  (* print_string (String.concat "|" chunks); *)
  if (List.length chunks >= 4) then
    let both = List.nth chunks 0 in
    let line_kind = String.get both 0 in
    let text = String.sub both 1 (String.length both - 1) in
    let gopher_line = new_gopher_line 
      line_kind text (List.nth chunks 1) (List.nth chunks 2) (Option.value (int_of_string_opt (List.nth chunks 3)) ~default:70) in
    gopher_line
  else
    new_gopher_line 'i' "" "" "" 70

let gopher_request host port selector =
  Draw.set_system_cursor Tsdl.Sdl.System_cursor.wait;
  (* Create a socket and connect to the server *)
  let socket = Unix.socket PF_INET SOCK_STREAM 0 in
  let server_addr = (Unix.gethostbyname host).h_addr_list.(0) in
  let server_sockaddr = Unix.ADDR_INET (server_addr, port) in
  Unix.connect socket server_sockaddr;

  (* Send the request *)
  let request = selector ^ "\r\n" in
  let _ = Unix.send socket (Bytes.of_string request) 0 (String.length request) [] in

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

let trim_leading_slash s =
  if String.length s > 0 && s.[0] = '/' then
    String.sub s 1 (String.length s - 1)
  else
    s

let rec parse_gopher_response response gopher_view urlbar = 
  let tokens = String.split_on_char '\n' response in
  let lines = List.map build_gopher_line tokens in
  let style_line line =
    let line_widgets = match line.line_kind with
    | '0' | '1' -> 
      let icon = Widget.icon (if line.line_kind = '0' then "file-text" else "folder-open") in
      let text = Widget.rich_text [(Text_display.underline (Text_display.raw line.text))] ~w:!_width ~h:18 in
      Widget.mouse_over ~enter:(fun _ -> Draw.set_system_cursor Tsdl.Sdl.System_cursor.hand) text;
      let on_click _ =
        Widget.set_text urlbar (String.concat "/" [line.server; String.make 1 line.line_kind; trim_leading_slash line.selector]);
        let response = gopher_request line.server line.port line.selector in
        parse_gopher_response response gopher_view urlbar in
      Widget.on_click ~click:on_click text;
      [icon; text]
    | 'i' -> [Widget.text_display line.text ~w:!_width ~h:18]
    | _ -> 
      let icon = Widget.icon "question" in
      let text = Widget.rich_text [(Text_display.italic (Text_display.raw line.text))] ~w:!_width ~h:18 in
      [icon; text] in
    Layout.flat_of_w line_widgets ~sep:0 in
  let widgets = List.map style_line lines
    |> Layout.tower
    |> Layout.make_clip ~scrollbar:false ~w:!_width ~h:(!_height - 0) in

  Layout.set_rooms gopher_view [widgets]
