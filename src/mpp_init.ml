(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)

let debug = 
  try ignore(Sys.getenv "DEBUG") ; true with _ -> false

exception Exit of int
let exit i = raise (Exit i)

let ignore_non_existing_commands = ref false
let ignore_orphan_closing_tokens = ref false

(* tokens *)
let open_token = ref "(("
let close_token = ref "))"
let endline_comments_token = ref "%%"
let open_comments_token = ref "/*"
let close_comments_token = ref "*/"

let open_special_token = ref "{{"
let close_special_token = ref "}}"

let newline_chars = Mpp_charset.of_list ['\n'; '\r']

(* [space_chars] is like the IFS Bash special variable, 
   but this way of using it has not been tested yet. *)
let space_chars = Mpp_charset.of_list [' '; '\t']

(* [blank_chars] might not be used at all *)
let blank_chars = Mpp_charset.of_list ['\n'; '\r';' '; '\t']


(** special blocks *)
type special_block_description = {
  name : string;
  command : string;
  suffix: string;
  print: string -> string;
  char_escape : char -> string;
  string_escape : string -> string;
  force_line_number : ?filename:string -> int -> string;
}
    

let special_blocks = [
  { name = "ocaml";
    command = "ocaml";
    suffix = ".ml";
    print = (fun s -> Printf.sprintf " let _ = print_string \"%s\\n\"\n" s);
    string_escape = String.escaped;
    char_escape = Char.escaped;
    force_line_number = (
      fun ?(filename="") n ->
        if filename = "" then
          Printf.sprintf "\n# %d\n" n
        else
          Printf.sprintf "\n# %d \"%s\"\n" n filename
    );
  };
]

let default_special_block = 
  { name = "";
    command = "";
    suffix = "";
    print = (fun s -> s);
    string_escape = (fun x -> x);
    char_escape = (fun x -> Char.escaped x);
    force_line_number = (fun ?(filename="") _ -> "");
  }

let special : special_block_description ref = ref default_special_block

let set_special s =
  let r = List.find (fun { name ; _ } -> name = s) special_blocks in
  special := r

let list_specials () = 
  Printf.printf "List of special blocks:\n";
  List.iter 
    (fun {name; _} -> Printf.printf "%s\n" name) 
    special_blocks;
  Pervasives.exit 0
  
