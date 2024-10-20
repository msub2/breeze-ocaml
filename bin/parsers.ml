open Bogue
open History
open Protocols
open Url
open Helpers
open Gemini

let rec parse_plaintext_response ?(show_link = true) response breeze_view urlbar protocol =
  let lines = String.split_on_char '\n' response in
  (* let height = List.length lines in *)
  let style_line line =
    if show_link && String.starts_with ~prefix:"=>" line then
      let prefix = Widget.rich_text [Text_display.raw "=> "] ~w:18 ~h:16 in
      let link = List.nth (String.split_on_char ' ' line) 1 in
      let link_text = Widget.label ~fg:(Draw.(opaque blue)) ~style:Tsdl_ttf.Ttf.Style.underline ~align:Draw.Max link in
      Widget.mouse_over ~enter:(fun _ -> Draw.set_system_cursor Tsdl.Sdl.System_cursor.hand) link_text;
      let on_click _ =
        let (url, request_body, port, ssl) = match link with
        | url when String.starts_with ~prefix:"gemini://" link -> (url, url, 1965, true) (* Absolute URL *)
        | url when String.starts_with ~prefix:"spartan://" link -> (* Absolute URL *)
          let request_body = match parse_url link with
          | Success (_, _, request_body, _) -> request_body
          | Failure _ -> failwith "Failed to parse spartan url" in
          (url, request_body, 300, false) 
        | _ -> (* Relative URL *)
          let current_url = Widget.get_text urlbar in
          let uri = Uri.of_string current_url in
          let path = Uri.path uri in
          let path_components = String.split_on_char '/' path in
          let all_but_last = Helpers.take (max (List.length path_components - 1) 0) path_components |> List.rev in
          let new_path = match String.starts_with ~prefix:"/" link with
          | true -> link
          | false -> String.concat "/" all_but_last ^ "/" ^ link in
          let (request_body, port, ssl) =
            let request_body = new_path ^ "\r\n" in
            (request_body, 1900, false) in
          (Uri.with_uri ~path:(Some new_path) uri |> Uri.to_string, request_body, port, ssl) in
        Widget.set_text urlbar url;
        let server = List.nth (String.split_on_char '/' url) 2 in
        let response = network_request ~ssl server port request_body in
        History.add_entry (url, Plaintext);
        parse_plaintext_response response breeze_view urlbar protocol in
      Widget.on_click ~click:on_click link_text;
      Layout.flat_of_w [prefix; link_text] ~sep:0
    else
      let text = Widget.text_display line ~w:!_width ~h:16 in
      Layout.flat_of_w [text] ~sep:0
  in
  let text = List.map style_line lines
    |> Layout.tower ~sep:0
    |> Layout.make_clip ~scrollbar:false ~w:!_width ~h:!_height in

  Layout.set_rooms breeze_view [text]

let parse_image_response filename response breeze_view = 
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
  
  Layout.set_rooms breeze_view [image]