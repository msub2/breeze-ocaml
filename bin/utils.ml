module Utils = struct
  let clamp a b k =
    if k < a then a else if k > b then b else k  
end
