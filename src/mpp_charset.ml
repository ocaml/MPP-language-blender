include Set.Make(Char) 

let of_list l =
  List.fold_left (fun r e -> add e r) empty l 
