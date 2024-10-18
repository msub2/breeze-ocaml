open Bogue
open History
open Protocols

(* Window size constants *)
let _width = ref 640
let _height = ref 480

type line_type = 
  | Text
  | Link
  | Heading of int
  | ListItem
  | Quote
  | PreformatToggle

type gemini_line = {
  line_type : line_type;
  content : string;
  description : string option;
};;

let string_of_line_type line_type =
  match line_type with
  | Text -> "Text"
  | Link -> "Link"
  | Heading 1 -> "Heading 1"
  | Heading 2 -> "Heading 2"
  | Heading 3 -> "Heading 3"
  | Heading _ -> failwith "Invalid Gemini heading!"
  | ListItem -> "ListItem"
  | Quote -> "Quote"
  | PreformatToggle -> "PreformatToggle"

let in_preformat_mode = ref false

let new_gemini_line ?(description) line_type content = 
  { line_type; content; description }

let parse_link_line line =
  let parts = Str.split (Str.regexp "[ \t]+") line in (* =>, URL, optional description *)
  match List.length parts with
  | 2 -> (Link, List.nth parts 1, None)
  | 3 -> (Link, List.nth parts 1, Some (List.nth parts 2))
  | _ when List.length parts > 3 -> 
    let start_index = 3 + String.length (List.nth parts 1) in
    let end_index = String.length line - start_index in
    (Link, List.nth parts 1, Some (String.sub line start_index end_index |> String.trim))
  | _ -> failwith ("Bad link line: " ^ line)

let build_gemini_line line =
  let chunks = String.split_on_char ' ' line in
  let identifier = List.nth chunks 0 in
  let (line_kind, text, description) = match identifier with
  | "=>" -> parse_link_line line
  | "#" -> (Heading 1, String.sub line 2 (String.length line - 2), None)
  | "##" -> (Heading 2, String.sub line 3 (String.length line - 3), None)
  | "###" -> (Heading 3, String.sub line 4 (String.length line - 4), None)
  | _ -> (Text, line, None) in
  let gemini_line = new_gemini_line
    line_kind text ?description in
  gemini_line

let get_rough_height size content =
  let length = String.length content |> float_of_int in
  let glyphs_per_line = Float.div (float_of_int !_width) (float_of_int size) in
  (Float.div length glyphs_per_line) |> Float.round |> int_of_float

let rec parse_gemini_response response breeze_view urlbar = 
  let (_content_type, tokens) = match String.split_on_char '\n' response with
    | x :: xs -> (x, xs)
    | _ -> failwith "Unable to parse tokens" in
  let lines = List.map build_gemini_line tokens in
  let style_line line =
    let line_widgets = match line.line_type with
    | Link ->
      let description = match line.description with
      | Some d -> d
      | None -> line.content in
      let text = Widget.rich_text [(Text_display.underline (Text_display.raw description))] ~w:!_width ~h:18 in
      Widget.mouse_over ~enter:(fun _ -> Draw.set_system_cursor Tsdl.Sdl.System_cursor.hand) text;
      let on_click _ =
        let url = line.content in
        Widget.set_text urlbar url;
        let request_body = url ^ "\r\n" in
        let server = List.nth (String.split_on_char '/' url) 2 in
        let response = network_request server 1965 request_body in
        History.add_entry (url, Gemtext);
        parse_gemini_response response breeze_view urlbar in
      Widget.on_click ~click:on_click text;
      [text]
    | Heading level ->
      let size = match level with
      | 1 -> 32
      | 2 -> 28
      | 3 -> 24
      | _ -> failwith "Invalid heading level!" in
      let content = Text_display.paragraphs_of_string line.content in
      let text = Widget.rich_text content ~w:!_width ~h:(size * get_rough_height size line.content) ~size in
      [text]
    | Text ->
      let content = Text_display.paragraphs_of_string line.content in
      let text = Widget.rich_text content ~w:!_width ~h:(16 * get_rough_height 16 line.content) in
      [text]
    | _ -> (* Any of the other currently unhandled types *)
      print_endline ("Unhandled Gemini line: " ^ string_of_line_type line.line_type ^ " " ^ line.content);
      let text = Widget.rich_text [(Text_display.italic (Text_display.raw line.content))] ~w:!_width ~h:18 in
      [text] in
    Layout.flat_of_w line_widgets ~sep:0 in
  let widgets = List.map style_line lines
    |> Layout.tower 
    |> Layout.make_clip ~scrollbar:false ~w:!_width ~h:!_height in

  Layout.set_rooms breeze_view [widgets]
