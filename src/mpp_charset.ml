(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)
include Set.Make(Char) 

let of_list l =
  List.fold_left (fun r e -> add e r) empty l 

let to_string s =
  let b = Buffer.create 42 in
    iter (Buffer.add_char b) s;
    Buffer.contents b

let to_escaped_string s =
  let b = Buffer.create 42 in
    iter (fun c -> Buffer.add_string b (Char.escaped c)) s;
    Buffer.contents b

