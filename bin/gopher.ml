open Bogue
open History
open Helpers
open Protocols
open Parsers

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

let rec parse_gopher_response response breeze_view urlbar = 
  let tokens = String.split_on_char '\n' response in
  let lines = List.map build_gopher_line tokens in
  let style_line line =
    let icon = get_icon line.line_kind |> Widget.icon in
    let line_widgets = match line.line_kind with
    | '0' | '1' | 'h' | 'I' | 'p' | 'g' -> (* Link types *)
      let text = Widget.rich_text [(Text_display.underline (Text_display.raw line.text))] ~w:!_width ~h:18 in
      Widget.mouse_over ~enter:(fun _ -> Draw.set_system_cursor Tsdl.Sdl.System_cursor.hand) text;
      let on_click _ =
        let url = String.concat "/" ["gopher://" ^ line.server; Helpers.trim_leading_slash line.selector] in
        Widget.set_text urlbar url;
        let request_body = line.selector ^ "\r\n" in
        let response = network_request line.server line.port request_body in
        match line.line_kind with
        | '0' | 'h' -> 
          History.add_entry (url, Plaintext);
          parse_plaintext_response response breeze_view urlbar Gopher
        | '1' -> 
          History.add_entry (url, Gophermap);
          parse_gopher_response response breeze_view urlbar
        | 'I' | 'p' | 'g' ->
          History.add_entry (url, Image);
          parse_image_response line.text response breeze_view
        | _ -> () in (* Unreachable *)
      Widget.on_click ~click:on_click text;
      [icon; text]
    | '2' -> (* CSO, leaving struckthrough to indicate it's not usable in Breeze *)
      let description = Text_display.strikethrough (Text_display.raw (String.trim line.text)) in
      let text = Widget.rich_text [description] ~w:!_width ~h:18 in
      [icon; text]
    | '3' -> (* Server error message *)
      let text = Widget.rich_text [(Text_display.italic (Text_display.raw line.text))] ~w:!_width ~h:18 in
      [icon; text]
    | '7' -> (* Search *)
      let text = Widget.text_display line.text in
      let search_field = Widget.text_input () in
      let search_action _ =
        "gopher://" ^ line.server ^ line.selector ^ "\t" ^ Widget.get_text search_field ^ "\r\n"
          |> Widget.set_text urlbar;
        let request_body = line.selector ^ "\t" ^ Widget.get_text search_field ^ "\r\n" in
        let response = network_request line.server line.port request_body in
        History.add_entry (Widget.get_text urlbar, Gophermap);
        parse_gopher_response response breeze_view urlbar in
      let search_button = Widget.button ~action:(fun _ -> search_action ())"Search" in
      [icon; text; search_field; search_button]
    | 'i' -> (* Regular text *)
      let box = Widget.box ~w:18 ~h:18 () in
      [box; Widget.text_display line.text ~w:!_width ~h:18]
    | _ -> (* Any of the other currently unhandled types *)
      print_endline ("Unhandled line type: " ^ Char.escaped line.line_kind);
      let text = Widget.rich_text [(Text_display.italic (Text_display.raw line.text))] ~w:!_width ~h:18 in
      [icon; text] in
    Layout.flat_of_w line_widgets ~sep:0 in
  let widgets = List.map style_line lines
    |> Layout.tower
    |> Layout.make_clip ~scrollbar:false ~w:!_width ~h:!_height in

  Layout.set_rooms breeze_view [widgets]
