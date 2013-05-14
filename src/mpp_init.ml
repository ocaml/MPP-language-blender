
let debug = 
  try ignore(Sys.getenv "DEBUG") ; true with _ -> false

exception Exit of int
let exit i = raise (Exit i)

let ignore_errors = ref false

(* tokens *)
let open_token = ref "[["
let close_token = ref "]]"
let endline_comments_token = ref "%%"
let open_comments_token = ref "/*"
let close_comments_token = ref "*/"


let newline_chars = Mpp_charset.of_list ['\n'; '\r']

(* [space_chars] is like the IFS Bash special variable, 
   but this way of using it has not been tested yet. *)
let space_chars = Mpp_charset.of_list [' '; '\t']

(* [blank_chars] might not be used at all *)
let blank_chars = Mpp_charset.of_list ['\n'; '\r';' '; '\t']


