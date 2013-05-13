open Mpp_charstream
open Mpp_init


type action =
  | Function of (string -> charstream -> out_channel -> unit)
  | Command of string
type action_set = action Mpp_stringmap.t

let actions : action_set ref = ref Mpp_stringmap.empty



(* *********************************************************** *)
(* **begin library ******************************************* *)
let cat out filename =
  if Sys.file_exists filename then
    let i = open_in filename in
      try while true do
        output_char out (input_char i)
      done with End_of_file -> ()
  else
    Printf.fprintf out
      "builtin cat error: file <%s> doesn't exist.\n%!"
      filename

let command arg charstream _out =
  let tmp = Filename.temp_file (* ~temp_dir:"/tmp" *) "tmp" "plop" in
  let otmp = open_out tmp in
    output_charstream otmp charstream;
    close_out otmp;
    ignore(Sys.command ("cat " ^ tmp ^ " | " ^ arg));
    Sys.remove tmp


module Variable : sig
  val set: string -> charstream -> 'ignored -> unit
  val get: string -> charstream -> out_channel -> unit
  val unset: string -> charstream -> 'ignored -> unit
  val unsetall: 'string -> 'charstream -> 'out_channel -> unit
end = struct
  include Map.Make(String)
  let env = ref empty
  let unsetall _s _cs _out = env := empty
  let set s cs _ =
    let css = charstream_of_string s in
    let variable =
      read_until_one_of space_chars css
    in
    let value = string_of_charstream css ^ string_of_charstream cs in
      env := add variable value !env
  let get s cs out =
    try
      output_string out (find s !env)
    with Not_found ->
      let f, l, c = cs.where() in
      failwith (Printf.sprintf "You tried to get the value of variable %s, which doesn't exist. Location: file <%s> Line<%d> Column<%d>." s f l c)
  let unset s cs _ =
    try
      env := remove s !env
    with Not_found ->
      let f, l, c = cs.where() in
        failwith (Printf.sprintf "You tried to unset the value of variable %s, which doesn't exist. Location: file <%s> Line<%d> Column<%d>." s f l c)
end

let builtins : action_set ref =
  let cmd = Function command in
  let echo =
    Function(fun a _cs out -> output_string out a) in
  let cat =
    Function(fun filename _cs out -> cat out filename; flush out)
  in
  let set = Function(Variable.set) in
  let unset = Function(Variable.unset) in
  let unsetall = Function(Variable.unsetall) in
  let get = Function(Variable.get) in    
  let set_opentoken =
    Function(fun x _cs _out -> open_token := x)
  in
  let set_closetoken =
    Function(fun x _cs _out -> close_token := x)
  in
  let set_endline_comments_token =
    Function(fun x _cs _out -> endline_comments_token := x)
  in
  let set_open_comments_token =
    Function(fun x _cs _out -> open_comments_token := x)
  in
  let set_close_comments_token = 
    Function(fun x _cs _out -> close_comments_token := x)
  in
  let r =
    List.fold_left
      (fun r (k,e) -> Mpp_stringmap.add k e r)
      Mpp_stringmap.empty
      [
        "-set", set;
        "-get", get;
        "-unset", unset;
        "-unsetall", unsetall;
        "-cmd", cmd; 
        "-echo", echo; 
        "-cat", cat;
        "-setopen", set_opentoken;
        "-setclose", set_closetoken;
        "-setendlinecomments", set_endline_comments_token;
        "-setopencomments", set_open_comments_token;
        "-setclosecomments", set_close_comments_token;
      ]
  in ref r

(* **end library ********************************************* *)
(* *********************************************************** *)


let list_builtins () =
  Mpp_stringmap.iter (fun k _v -> Printf.printf "%s\n" k) !builtins;
  Pervasives.exit 0

let lookup_builtin action_name =
  match Mpp_stringmap.find action_name !builtins with
    | Function f -> f
    | Command s -> failwith "Command not yet implemented."

let exec (action_name:string) (arguments:string) (charstream:charstream) (out:out_channel) =
  if debug then Printf.eprintf "exec: %!";
  (* action_name : thing to do;
     arguments   : arguments on the first line;
     charstream  : what follows the first line (if any). *)
  if debug then Printf.eprintf "action_name:<%s> arguments:<%s>"
    action_name arguments;
  if action_name.[0] = '-' then
    (lookup_builtin action_name) arguments charstream out
  else
    failwith "General exec not yet implemented."
