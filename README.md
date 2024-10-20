# Breeze - A SmolNet Browser

Breeze is an OCaml-based browser for various "SmolNet" protocols like Gopher, Gemini, Spartan, Guppy, Nex, and more.

## Protocol Support

Below are the following protocols that Breeze should ideally be able to speak. "Usable" means that base functionality for interacting with the protocol exists. "Navigable" implies that you can click links to move between pages. If the protocol has an associated file/document format, additional implementation statuses will be indicated afterwards.

- Finger
  - [x] Usable
  - [ ] Navigable
- Gopher
  - [x] Usable
  - [x] Navigable
  - [ ] Gophermap Rendering
    - Currently have support for text, link, image, and search 
- Gopher+ (this is my best guess, don't know any Gopher+ pages yet)
  - [x] Usable 
  - [x] Navigable
  - [ ] Gophermap Additions
- Gopher w/ TLS
  - [ ] Usable
  - [ ] Navigable
- Gemini
  - [x] Usable
  - [x] Navigable
  - [x] Gemtext Rendering
    - Not yet perfect, line height calculation needs work
    - Link line parsing also needs work (doesn't catch links with missing whitespace yet)
- Spartan
  - [x] Usable
  - [x] Navigable
  - [ ] Gemtext Additions
- Titan
  - [ ] Usable
- Scorpion
  - [ ] Usable
  - [ ] Navigable
- Text Protocol
  - [x] Usable
  - [x] Navigable
- SuperTXT
  - [ ] Usable
  - [ ] Navigable
- Molerat
  - [ ] Usable
  - [ ] Navigable
- Nightfall Express
  - [x] Usable
  - [x] Navigable
  - [ ] NPS
- Guppy
  - [ ] Usable
  - [ ] Navigable
- Scroll
  - [ ] Usable
  - [ ] Navigable