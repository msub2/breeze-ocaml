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

type parser_mode = 
  | Normal
  | Preformatted

type gemini_line = {
  line_type : line_type;
  content : string;
  parser_mode : parser_mode;
  description : string option;
};;

let string_of_line_type line_type =
  match line_type with
  | Text -> ""
  | Link -> "=>"
  | Heading 1 -> "#"
  | Heading 2 -> "##"
  | Heading 3 -> "###"
  | Heading _ -> failwith "Invalid Gemini heading!"
  | ListItem -> "*"
  | Quote -> ">"
  | PreformatToggle -> "```"

let current_parser_mode : parser_mode ref = ref Normal
let in_normal_mode () =
  let result = match (!current_parser_mode) with
  | Normal -> true
  | Preformatted -> false in
  result

let new_gemini_line ?(description) line_type content parser_mode = 
  { line_type; content; description; parser_mode }

let parse_link_line line =
  let parts = Str.split (Str.regexp "[ \t]+") line in (* =>, URL, optional description *)
  match List.length parts with
  | 2 -> (Link, List.nth parts 1, None, !current_parser_mode)
  | 3 -> (Link, List.nth parts 1, Some (List.nth parts 2), !current_parser_mode)
  | _ when List.length parts > 3 -> 
    let start_index = 3 + String.length (List.nth parts 1) in
    let end_index = String.length line - start_index in
    (Link, List.nth parts 1, Some (String.sub line start_index end_index |> String.trim), !current_parser_mode)
  | _ -> failwith ("Bad link line: " ^ line)

let build_gemini_line line =
  let chunks = String.split_on_char ' ' line in
  let identifier = List.nth chunks 0 in
  let (line_kind, text, description, parser_mode) = match identifier with
  | "=>" -> parse_link_line line
  | "#" -> (Heading 1, String.sub line 2 (String.length line - 2), None, !current_parser_mode)
  | "##" -> (Heading 2, String.sub line 3 (String.length line - 3), None, !current_parser_mode)
  | "###" -> (Heading 3, String.sub line 4 (String.length line - 4), None, !current_parser_mode)
  | ">" -> (Quote, String.sub line 1 (String.length line - 1), None, !current_parser_mode)
  | "```" -> 
    let _ = match !current_parser_mode with
      | Normal -> current_parser_mode := Preformatted
      | Preformatted -> current_parser_mode := Normal in
    (PreformatToggle, "", None, !current_parser_mode)
  | _ -> (Text, line, None, !current_parser_mode) in
  new_gemini_line line_kind text parser_mode ?description

let get_wrapped_line_count size content =
  let length = String.length content |> float_of_int in
  let glyphs_per_line = Float.div (float_of_int !_width) (float_of_int size) in
  let lines = length /. glyphs_per_line |> Float.round |> int_of_float in
  max lines 1

let render_plaintext plaintext =
  let content = Text_display.paragraphs_of_string plaintext in
  let text = Widget.rich_text content ~w:!_width ~h:(16 * get_wrapped_line_count 16 plaintext) in
  [text]

let rec parse_gemini_response response breeze_view urlbar = 
  let (_content_type, tokens) = match String.split_on_char '\n' response with
    | x :: xs -> (x, xs)
    | _ -> failwith "Unable to parse tokens" in
  let lines = List.map build_gemini_line tokens in
  let style_line line =
    let line_widgets = match line.line_type with
    | Link | Heading _ | Quote when line.parser_mode == Preformatted ->
      let full_line = String.concat "\t" [string_of_line_type line.line_type; line.content; Option.value ~default:"" line.description] in
      let content = Text_display.paragraphs_of_string full_line in
      let text = Widget.rich_text content ~w:!_width ~h:(16 * get_wrapped_line_count 16 line.content) in
      [text]
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
      let text = Widget.rich_text content ~w:!_width ~h:(size * get_wrapped_line_count size line.content) ~size in
      [text]
    | Text | ListItem ->
      let content = Text_display.paragraphs_of_string line.content in
      let text = Widget.rich_text content ~w:!_width ~h:(16 * get_wrapped_line_count 16 line.content) in
      [text]
    | PreformatToggle ->
      []
    | Quote ->
      let content = Text_display.para ("\t" ^ line.content) |> Text_display.italic in
      let text = Widget.rich_text [content] ~w:!_width ~h:(16 * get_wrapped_line_count 16 line.content) in
      [text] in

    let background = Layout.color_bg (31, 31, 31, 31) in
    match line.parser_mode with
    | Normal -> Layout.flat_of_w line_widgets ~sep:0 
    | Preformatted -> Layout.flat_of_w line_widgets ~sep:4 ~background in

  let widgets = List.map style_line lines
    |> List.filter (fun layout -> Layout.height layout > 10)
    |> Layout.tower 
    |> Layout.make_clip ~scrollbar:false ~w:!_width ~h:!_height in

  Layout.set_rooms breeze_view [widgets]
