(* Not yet used *)
type protocol = 
  | Plaintext
  | Gopher
  | GopherPlus
  | Gemini
  | Spartan
  | Guppy
  | Nex
  | Unknown

type content_type = 
  | Plaintext
  | Image
  | Gophermap
  | Gemtext
  | HTML
  | Unknown