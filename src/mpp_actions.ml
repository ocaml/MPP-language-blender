(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)
open Mpp_charstream
open Mpp_init
open Mpp_variables

type action =
  | Function of (action_name -> charstream -> out_channel -> unit)
  | Command of string
and action_name = string

type action_set = (action*documentation) Mpp_stringmap.t
and documentation = string

let actions : action_set ref = ref Mpp_stringmap.empty

let ignore_exec_error = ref false

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

let command arg charstream out =
  let file, line, column = charstream.where() in
  let tmp = Filename.temp_file (* ~temp_dir:"/tmp" *) "tmp" "plop" in
  let otmp = open_out tmp in
  let tmp2 = Filename.temp_file (* ~temp_dir:"/tmp" *) "tmp2" "plop" in
    output_charstream otmp charstream;
    close_out otmp;
    let ec = Sys.command ("( cat " ^ tmp ^ " | " ^ arg ^ " ) > " ^ tmp2 ) in
    let () = cat out tmp2 in
      Sys.remove tmp;
      Sys.remove tmp2;
      match ec with
        | 0 -> ()
        | _ ->
            if not (!ignore_exec_error) then
              Pervasives.failwith 
                (Printf.sprintf "Command <%s> ended with error <%d>. Location: %s:%d:%d." 
                   arg ec file line column)
            else
              ()


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
  let tryget = Function(Variable.tryget) in
  let ifdef = Function(Variable.ifdef) in
  let ifndef = Function(Variable.ifndef) in
  let elzeifdef = Function(Variable.elzeifdef) in
  let elze = Function(Variable.elze) in
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
  let error = Function(fun s cs _ -> 
    parse_error 
      ~msg:(Printf.sprintf "your message is <%s>. No matter what, I'm exiting." s)
      (cs.where());
    Pervasives.exit 1
  )
  in
  let r =
    List.fold_left
      (fun r (k,e,doc) -> Mpp_stringmap.add k (e,doc) r)
      Mpp_stringmap.empty
      [
        "ignore", Function(fun _ _ _ -> ()), "A command that does nothing with its arguments.";
        "ifdef", ifdef, "If the argument is a defined variable, then inputs the rest.";
        "tryget", tryget, "Get the value of a variable, and if it doesn't exist, it does nothing.";
        "error", error, "Stops MPP.";
        "ifndef", ifndef, "If the argument is not a defined variable, then inputs the rest, else does nothing.";
        "else", elze, "If the previous test was not satisfied, then outputs its arguments";
        "elseifdef", elzeifdef, "If the previous test was not satisfied and the variable exists, then outputs the rest.";
        "set", set, "Set the variable to the rest. Related: get, tryget, unset, unsetall.";
        "get", get, "Get the value of a variable, and if it does not exist, MPP stops. Related: set, tryget, unset, unsetall.";
        "unset", unset, "Unsets a variable. Related: tryget, get, tryget, unsetall.";
        "unsetall", unsetall, "Unset all variables. Related: tryget, get, tryget, unsetall.";
        "cmd", cmd, "Executes the rest of the line as a shell command. Following lines (if any) are given as input of the shell command."; 
        "echo", echo, "Prints the rest of the line."; 
        "cat", cat, "Prints the contents of a file.";
        "setopen", set_opentoken, "Sets the opening token. Related: setclose.";
        "setclose", set_closetoken, "Sets the closing token. Related: setopen.";
        "setendlinecomments", set_endline_comments_token, "Sets the endline comments token.";
        "setopencomments", set_open_comments_token, "Sets the opening comments token. Related: setclosecomments.";
        "setclosecomments", set_close_comments_token, "Sets the endline comments token. Related: setopencomments.";
      ]
  in ref r

(* **end library ********************************************* *)
(* *********************************************************** *)

let apply_builtin action_name location =
  try
    match Mpp_stringmap.find action_name !builtins with
      | Function f, _ -> f
      | Command s, _ -> Pervasives.failwith "Command not yet implemented."
  with Not_found ->
    if !ignore_non_existing_commands then
      begin
        fun _ _ _ -> ()
      end
    else
      begin
        parse_error
          ~msg:(Printf.sprintf "Command <%s> not found!" action_name)
          location;
        Pervasives.exit 1
      end


let exec (action_name:string) (arguments:string) (charstream:charstream) (out:out_channel) =
  if debug then 
    begin
      Printf.eprintf "exec: %!";
      (* action_name : thing to do; arguments : arguments on the first
         line; charstream : what follows the first line (if any). *)
      Printf.eprintf "action_name:<%s> arguments:<%s>\n%!"
        action_name arguments;
    end;
  if action_name.[0] <> '-' then
    (apply_builtin action_name (charstream.where())) arguments charstream out
  else
    begin
      (apply_builtin "cmd" (charstream.where()))
        (String.sub action_name 1 (String.length action_name - 1) ^ " " ^ arguments)
        charstream out;
      if debug then Printf.eprintf "???%!";
    end



let list_builtins () =
  let m =
    4 + Mpp_stringmap.fold
      (fun k _ r -> max (String.length k) r)
      !builtins
      0
  in
  let pad k = 
    k ^ String.make (max 1 (m - String.length k)) ' '
  in
    Mpp_stringmap.iter
      (fun k (_e, doc) ->
         Printf.printf "%s %s\n" (pad k) doc)
      !builtins;
    Pervasives.exit 0

let register (name:string) (f:action) (d:documentation) : unit =
  builtins := Mpp_stringmap.add name (f,d) !builtins

let _ =
  register
    "builtins"
    (Function (fun _ _ _ -> list_builtins()))
    "List all available builtins."


let builtins = () (* prevent builtins from being used outside. Perhaps I'll switch to using an mli file. *)


