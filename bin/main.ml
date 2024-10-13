open Bogue
open Gopher

(* Window size constants *)
let _width = ref 640
let _height = ref 480

let button_action gopher_view urlbar = 
  let (host, port, selector) = parse_gopher_url (Widget.get_text urlbar) in
  let response = gopher_request host port selector in
  parse_gopher_response response gopher_view urlbar;
  ()

(* Main Loop *)
let () =
  let gopherview_widget = Widget.text_display "" in
  let gopher_view = gopherview_widget
    |> Layout.resident ~w:!_width ~h:!_height
    |> Layout.make_clip ~w:!_width ~h:!_height in

  let urlbar = Widget.text_input ~text:"gopher.floodgap.com" () in
  let button = Widget.button "Go" ~action:(fun _ -> button_action gopher_view urlbar) in
  let toolbar = Layout.flat_of_w [urlbar; button] ~background:(Layout.opaque_bg Draw.grey) in
  Layout.set_width toolbar !_width;
  button_action gopher_view urlbar;

  [toolbar; gopher_view]
    |> Layout.tower ~name:"Breeze - A SmolNet Browser"
    |> Bogue.of_layout
    |> Bogue.run
