open Bogue
open Gopher
open Gemini
open History
open Protocols
open Parsers
open Url
open Display

let go_action breeze_view urlbar = 
  let url = Widget.get_text urlbar in
  let (host, port, request_body, protocol) = match parse_url url with
  | Success (host, port, request_body, protocol) -> (host, port, request_body, protocol)
  | Failure _ -> ("gopher.floodgap.com", 70, "\r\n", Gopher) in
  (* Update this as needed *)
  let ssl = protocol == Gemini in
  let response = try network_request ~ssl host port request_body with Failure message -> message in
  (* The assumptions here are outdated, need to account for resource links in history now *)
  match protocol with
  | Finger | Nex | Text ->
    History.add_entry (url, Plaintext);
    let show_link = String.ends_with ~suffix:".txt" url |> not in
    parse_plaintext_response ~show_link response breeze_view urlbar Plaintext
  | Gopher -> 
    History.add_entry (url, Gophermap);
    parse_gopher_response response breeze_view urlbar
  | Gemini ->
    History.add_entry (url, Gemtext);
    parse_gemtext_response response breeze_view urlbar Gemini
  | Spartan -> 
    History.add_entry (url, Gemtext);
    parse_gemtext_response response breeze_view urlbar Spartan
  | _ -> parse_plaintext_response ~show_link:false response breeze_view urlbar Plaintext

let history_action (action : history_action) breeze_view urlbar = 
  let can_navigate = match action with 
  | Forward -> History.can_go_forward ()
  | Back -> History.can_go_backward () in

  if can_navigate then
    let _ = match action with 
    | Forward -> History.history_forward ()
    | Back -> History.history_back () in
    let (url, content_type) = History.get_history () in
    let (host, port, selector, protocol) = match parse_url url with
    | Success (host, port, request_body, protocol) -> (host, port, request_body, protocol)
    | Failure _ -> ("gopher.floodgap.com", 70, "\r\n", Gopher) in
    let request_body = selector ^ "\r\n" in
    (* Update this as needed *)
    let ssl = protocol == Gemini in
    let response = try network_request ~ssl host port request_body with Failure message -> message in
    let _ = match content_type with
    | Gophermap -> parse_gopher_response response breeze_view urlbar
    | Gemtext -> parse_gemtext_response response breeze_view urlbar protocol
    | Plaintext -> parse_plaintext_response response breeze_view urlbar protocol
    | Image -> 
      let filename = List.nth (List.rev (String.split_on_char '/' selector)) 0 in
      parse_image_response filename response breeze_view
    | _ -> parse_plaintext_response response breeze_view urlbar protocol in
    
    Widget.set_text urlbar url

(* Main Loop *)
let () =
  Theme.set_text_font "./Inconsolata.ttf";
  Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna);
  let width = Display.width () in
  let height = Display.height () in
  let breezeview_widget = Widget.text_display "" in
  let breeze_view = breezeview_widget
    |> Layout.resident ~w:width ~h:height
    |> Layout.make_clip ~w:width ~h:height ~scrollbar:false in
  let urlbar = Widget.text_input ~text:"gemini://geminiprotocol.net/" ~prompt:"Enter URL..." () ~size:16 in
  let go_button = Widget.button "Go" ~action:(fun _ -> go_action breeze_view urlbar) in
  let back_button = Widget.button "<" ~action:(fun _ -> history_action Back breeze_view urlbar) in
  let forward_button = Widget.button ">" ~action:(fun _ -> history_action Forward breeze_view urlbar) in
  let toolbar = Layout.flat_of_w [back_button; forward_button; urlbar; go_button] ~background:(Layout.color_bg (Draw.transp Draw.grey)) in
  Layout.set_width ~keep_resize:true toolbar width;
  go_action breeze_view urlbar;

  let window_layout = [toolbar; breeze_view]
    |> Layout.tower ~name:"Breeze - A SmolNet Browser" ~hmargin:0 ~vmargin:0 ~sep:0 in 
  let on_resize () =
    let (x, y) = Layout.get_size window_layout in
    let (_, y2) = Layout.get_size toolbar in
    Display.update_display_dimensions x (y - y2);
    (* sign, hack but gets the job done for now. find a better solution later *)
    go_action breeze_view urlbar;
  in
  Layout.on_resize breeze_view on_resize;

  window_layout
    |> Bogue.of_layout
    |> Bogue.run
