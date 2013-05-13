
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
let space_chars = Mpp_charset.of_list [' '; '\t']
let blank_chars = Mpp_charset.of_list ['\n'; '\r';' '; '\t']


