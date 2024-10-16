open Bogue
open History
open Networking

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
  { line_kind; text; selector; server; port }

let build_gopher_line line =
  let chunks = String.split_on_char '\t' line in
  if (List.length chunks >= 4) then
    let both = List.nth chunks 0 in
    let line_kind = String.get both 0 in
    let text = String.sub both 1 (String.length both - 1) in
    let gopher_line = new_gopher_line
      line_kind text (List.nth chunks 1) (List.nth chunks 2) (Option.value (int_of_string_opt (List.nth chunks 3)) ~default:70) in
    gopher_line
  else
    new_gopher_line 'i' line "" "" 70

let trim_leading_slash s =
  if String.length s > 0 && s.[0] = '/' then
    String.sub s 1 (String.length s - 1)
  else
    s

let parse_plaintext_response response gopher_view =
  let height = String.split_on_char '\n' response
    |> List.length in
  let text = Widget.text_display response
    |> Layout.resident ~w:!_width ~h:(height * 18)
    |> Layout.make_clip ~scrollbar:false ~w:!_width ~h:!_height in

  Layout.set_rooms gopher_view [text]

let parse_image_response filename response gopher_view = 
  let file_path = "_cache/" ^ filename in
  let exists = Sys.file_exists file_path in
  let _ = if not exists then
    let oc = open_out file_path in
    Printf.fprintf oc "%s\n" response;
    close_out oc in
  let image = file_path
    |> Widget.image ~noscale:true
    |> Layout.resident
    |> Layout.make_clip ~scrollbar:false ~w:!_width ~h:!_height in
  
  Layout.set_rooms gopher_view [image]

let get_icon line_kind = 
  match line_kind with
    (* From original RFC or Gophernicus *)
    | '0' -> "file-text"
    | '1' -> "folder-open"
    | '2' -> "phone"
    | '3' -> "bug"
    | '5' -> "file-archive-o"
    | '7' -> "search"
    | '9' -> "gears"
    | 'h' -> "globe"
    | 'I' -> "image"
    | 'd' -> "document"
    | 's' -> "sound"
    | ';' -> "video"
    | 'c' -> "calendar"
    | 'M' -> "file" (* MIME file, figure out a better symbol *)
    (* Seen in the wild *)
    | 'p' -> "image" (* PNG *)
    | _ -> "question"

let rec parse_gopher_response response gopher_view urlbar = 
  let tokens = String.split_on_char '\n' response in
  let lines = List.map build_gopher_line tokens in
  let style_line line =
    let icon = get_icon line.line_kind |> Widget.icon in
    let line_widgets = match line.line_kind with
    (* Interactive types *)
    | '0' | '1' | 'h' | 'I' | 'p' -> 
      let text = Widget.rich_text [(Text_display.underline (Text_display.raw line.text))] ~w:!_width ~h:18 in
      Widget.mouse_over ~enter:(fun _ -> Draw.set_system_cursor Tsdl.Sdl.System_cursor.hand) text;
      let on_click _ =
        let url = String.concat "/" ["gopher://" ^ line.server; trim_leading_slash line.selector] in
        Widget.set_text urlbar url;
        let request_body = line.selector ^ "\r\n" in
        let response = network_request line.server line.port request_body in
        match line.line_kind with
        | '0' | 'h' -> 
          History.add_entry (url, Plaintext);
          parse_plaintext_response response gopher_view
        | '1' -> 
          History.add_entry (url, Gophermap);
          parse_gopher_response response gopher_view urlbar
        | 'I' | 'p' ->
          History.add_entry (url, Image);
          parse_image_response line.text response gopher_view
        | _ -> () in (* Unreachable *)
      Widget.on_click ~click:on_click text;
      [icon; text]
    | '2' ->
      let description = Text_display.strikethrough (Text_display.raw (String.trim line.text)) in
      let text = Widget.rich_text [description] ~w:!_width ~h:18 in
      [icon; text]
    | '7' -> 
      let text = Widget.text_display line.text in
      let search_field = Widget.text_input () in
      let search_action _ =
        "gopher://" ^ line.server ^ line.selector ^ "\t" ^ Widget.get_text search_field ^ "\r\n"
          |> Widget.set_text urlbar;
        let request_body = line.selector ^ "\t" ^ Widget.get_text search_field ^ "\r\n" in
        let response = network_request line.server line.port request_body in
        History.add_entry (Widget.get_text urlbar, Gophermap);
        parse_gopher_response response gopher_view urlbar in
      let search_button = Widget.button ~action:(fun _ -> search_action ())"Search" in
      [icon; text; search_field; search_button]
    | 'i' -> 
      let box = Widget.box ~w:18 ~h:18 () in
      [box; Widget.text_display line.text ~w:!_width ~h:18]
    | _ -> 
      print_endline ("Unhandled line type: " ^ Char.escaped line.line_kind);
      let text = Widget.rich_text [(Text_display.italic (Text_display.raw line.text))] ~w:!_width ~h:18 in
      [icon; text] in
    Layout.flat_of_w line_widgets ~sep:0 in
  let widgets = List.map style_line lines
    |> Layout.tower
    |> Layout.make_clip ~scrollbar:false ~w:!_width ~h:!_height in

  Layout.set_rooms gopher_view [widgets]
