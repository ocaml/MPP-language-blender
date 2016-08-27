(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

(**************************************************)
(**************CONFIGURATION***********************)
(**************************************************)

exception Exit of int
let exit i = raise (Exit i)

(* tokens *)
let open_token = ref "(("
let close_token = ref "))"

let open_nesting_token = ref "{{"
let close_nesting_token = ref "}}"

let endline_comments_token = ref "%%"
let open_comments_token = ref "/*"
let close_comments_token = ref "*/"

let open_foreign_token = ref "{<"
let open_foreign_token_no_location = ref "{<!"
let close_foreign_token = ref ">}"

let target_language_location_handler = ref (fun ?filename line -> "")

let cpp_location_handler ?filename line =
  match filename with
  | Some filename ->
     Printf.sprintf "\n# %d %S\n" line filename
  | None ->
     Printf.sprintf "\n# %d\n" line

let target_language_location_handlers = [
  ("cpp", cpp_location_handler);
  ("ocaml", cpp_location_handler);
]

let list_target () =
  Printf.printf "List of target language handlers available:\n";
  List.iter
    (fun (name,  _) -> Printf.printf "%s\n" name)
    target_language_location_handlers;
  Pervasives.exit 0


let set_target_language_location_handler h =
  try
    match List.assoc h target_language_location_handlers with
    | h -> target_language_location_handler := h
  with Not_found ->
    Printf.eprintf
      "MPP: Error: Target language location handler <%s> is not available.\n%!"
      h;
    Pervasives.exit 1

let newline_chars = Mpp_charset.of_list ['\n'; '\r']

(* [space_chars] is like the IFS Bash special variable,
   but this way of using it has not been tested yet. *)
let space_chars = Mpp_charset.of_list [' '; '\t']

(* [blank_chars] might not be used at all *)
let blank_chars = Mpp_charset.of_list ['\n'; '\r';' '; '\t']


(** foreign blocks *)
type foreign_block_description = {
  name : string;
  command : string;
  suffix: string;
  print: string -> string;
  char_escape : char -> string;
  string_escape : string -> string;
  force_line_number : ?filename:string -> int -> string;
}


let bash_string_escape s =
  let b = Buffer.create (String.length s * 2) in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '$' | '\\' | ' ' | '\t' | '\'' | '\"' | '`' | '&'
    | '(' | ')' | '{' | '}' | '[' | ']' as c ->
      Buffer.add_char b '\\';
      Buffer.add_char b c
    | c ->
      Buffer.add_char b c
  done;
  Buffer.contents b

let php_string_escape s =
  let b = Buffer.create (String.length s * 2) in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '$' | '\"' | '\\' as c ->
      Buffer.add_char b '\\';
      Buffer.add_char b c
    | c ->
      Buffer.add_char b c
  done;
  Buffer.contents b

let perl_string_escape s =
  let b = Buffer.create (String.length s * 2) in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '$' | '\"' | '\\' | '@' as c ->
      Buffer.add_char b '\\';
      Buffer.add_char b c
    | c ->
      Buffer.add_char b c
  done;
  Buffer.contents b

let foreign_blocks = [
  { name = "ocaml";
    command = "ocaml";
    suffix = ".ml";
    print =
      (fun s ->
       if s <> "" then
         Printf.sprintf " let _ = print_string \"%s\"\n" s
       else "");
    string_escape = String.escaped;
    char_escape = Char.escaped;
    force_line_number = (
      fun ?filename n ->
      (match filename with
       | None -> Printf.sprintf "\n# %d\n" n
       | Some filename ->
          Printf.sprintf "\n# %d \"%s\"\n" n filename)
      ^
        (match !target_language_location_handler ?filename n with
         | "" -> ""
         | l -> Printf.sprintf "let _ = print_string %S" l)
    );
  };
  { name = "bash";
    command = "bash";
    suffix = ".bash";
    print = (fun s -> if s <> "" then Printf.sprintf "echo %s" s else "");
    string_escape = bash_string_escape;
    char_escape = (fun c -> bash_string_escape (String.make 1 c));
    force_line_number = (
      fun ?filename n ->
        match filename with
        | None -> Printf.sprintf "\n# %d\n" n
        | Some filename ->
          Printf.sprintf "\n# %d \"%s\"\n" n filename
    );
  };
  { name = "php";
    command = "php";
    suffix = ".php";
    print =
      (fun s ->
       if s <> "" then Printf.sprintf "echo \"%s\n\";" s else "");
    string_escape = php_string_escape;
    char_escape = (fun c -> php_string_escape (String.make 1 c));
    force_line_number = (
      fun ?filename n ->
        match filename with
        | None -> Printf.sprintf "\n# %d\n" n
        | Some filename ->
          Printf.sprintf "\n# %d \"%s\"\n" n filename
    );
  };
  { name = "perl";
    command = "perl";
    suffix = ".pl";
    print =
      (fun s ->
       if s <> "" then Printf.sprintf "echo \"%s\n\";" s else "");
    string_escape = perl_string_escape;
    char_escape = (fun c -> perl_string_escape (String.make 1 c));
    force_line_number = (
      fun ?filename n ->
        match filename with
        | None -> Printf.sprintf "\n# %d\n" n
        | Some filename ->
          Printf.sprintf "\n# %d \"%s\"\n" n filename
    );
  };
]

let default_foreign_block =
  { name = "";
    command = "";
    suffix = "";
    print = (fun s -> s);
    string_escape = (fun x -> x);
    char_escape = (fun x -> String.make 1 x);
    force_line_number = (fun ?(filename="") _ -> "");
  }

let foreign : foreign_block_description ref = ref default_foreign_block

let set_foreign s =
  let r = List.find (fun { name ; _ } -> name = s) foreign_blocks in
  foreign := r

let list_foreign () =
  Printf.printf "List of foreign block languages available:\n";
  List.iter
    (fun {name; _} -> Printf.printf "%s\n" name)
    foreign_blocks;
  Pervasives.exit 0

(* PROPAGATION *)
let () = Mpp_actions.space_chars := space_chars
let () = Mpp_actions.blank_chars := blank_chars
