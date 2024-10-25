module Display = struct
  let _width = ref 640
  let _height = ref 480

  let width () = 
    !_width
  
  let height () =
    !_height
  
  let update_display_dimensions x y =
    _width := x;
    _height := y;
end