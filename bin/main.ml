open Bogue
open Gopher
open History
open Networking

(* Window size constants *)
let _width = ref 640
let _height = ref 480

let button_action gopher_view urlbar = 
  let url = Widget.get_text urlbar in
  History.add_entry url;
  let (host, port, selector) = parse_gopher_url url in
  let request_body = selector ^ "\r\n" in
  let response = network_request host port request_body in
  parse_gopher_response response gopher_view urlbar

let back_action gopher_view urlbar =
  match History.can_go_backward () with
  | true -> 
    History.history_back ();
    let url = History.get_history () in
    if url != "" then 
      let (host, port, selector) = parse_gopher_url url in
      let request_body = selector ^ "\r\n" in
      let response = network_request host port request_body in
      parse_gopher_response response gopher_view urlbar;
      Widget.set_text urlbar url;
  | false -> ()

let forward_action gopher_view urlbar =
  match History.can_go_forward () with
  | true ->
    History.history_forward ();
    let url = History.get_history () in
    if url != "" then 
      let (host, port, selector) = parse_gopher_url url in
      let request_body = selector ^ "\r\n" in
      let response = network_request host port request_body in
      parse_gopher_response response gopher_view urlbar;
      Widget.set_text urlbar url;
  | false -> ()

(* Main Loop *)
let () =
  Theme.set_text_font "./Inconsolata.ttf";
  let gopherview_widget = Widget.text_display "" in
  let gopher_view = gopherview_widget
    |> Layout.resident ~w:!_width ~h:!_height
    |> Layout.make_clip ~w:!_width ~h:!_height in
  let urlbar = Widget.text_input ~text:"gopher.floodgap.com" ~prompt:"Enter URL..." () ~size:16 in
  let go_button = Widget.button "Go" ~action:(fun _ -> button_action gopher_view urlbar) in
  let back_button = Widget.button "<" ~action:(fun _ -> back_action gopher_view urlbar) in
  let forward_button = Widget.button ">" ~action:(fun _ -> forward_action gopher_view urlbar) in
  let toolbar = Layout.flat_of_w [back_button; forward_button; urlbar; go_button] ~background:(Layout.color_bg (Draw.transp Draw.grey)) in
  Layout.set_width toolbar !_width;
  button_action gopher_view urlbar;

  [toolbar; gopher_view]
    |> Layout.tower ~name:"Breeze - A SmolNet Browser"
    |> Bogue.of_layout
    |> Bogue.run
