module Utils = struct
  let clamp a b k =
    if k < a then a else if k > b then b else k
  let take amount list  =
    let rec aux acc n = function
      | [] -> acc
      | x :: xs ->
          if n = 0 then acc
          else aux (x :: acc) (n - 1) xs
    in
    aux [] amount list
end
