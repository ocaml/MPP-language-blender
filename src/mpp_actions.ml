(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)
open Mpp_charstream
open Mpp_variables
module Out = Mpp_out

let debug = ref false

let ignore_non_existing_commands = ref false

type  action = (bool option ref -> action_name -> charstream ->  Out.t -> unit)
and action_name = string

type  action_set = (action * documentation) Mpp_stringmap.t
and documentation = string

let actions :  action_set ref = ref Mpp_stringmap.empty

let ignore_exec_error = ref false

(* *********************************************************** *)
(* **begin library ******************************************* *)
let cat (out:Out.t) filename =
  if Sys.file_exists filename then
    let i = open_in filename in
      try while true do
        Out.output_char out (input_char i)
      done with End_of_file -> ()
  else
    Printf.eprintf
      "builtin cat error: file <%s> doesn't exist.\n%!"
      filename

let last_cmd = ref 0

let command arg charstream (out:Out.t) =
  let tmp = Filename.temp_file (* ~temp_dir:"/tmp" *) "tmp" "plop" in
  let otmp = open_out tmp in
  let tmp2 = Filename.temp_file (* ~temp_dir:"/tmp" *) "tmp2" "plop" in
    output_charstream otmp charstream;
    close_out otmp;
    let ec = Sys.command ("( cat " ^ tmp ^ " | " ^ arg ^ " ) > " ^ tmp2 ) in
    let () = cat out tmp2 in
      Sys.remove tmp;
      Sys.remove tmp2;
      last_cmd := ec;
      ec

let ifcmd last_cond arg charstream out =
  if !debug then Printf.eprintf "ifcmd <%s> <%s>\n%!" (String.escaped arg) (String.escaped (string_of_charstream charstream));
  if !last_cmd = 0 then
    begin
      last_cond := Some true;
      Out.output_string out arg;
      Out.output_charstream out charstream;
    end
  else
    last_cond := Some false


let cmd arg charstream (out:Out.t) =
  let file, line, column = charstream.where() in
    match command arg charstream out with
      | 0 -> ()
      | ec ->
          if not (!ignore_exec_error) then
            Pervasives.failwith 
              (Printf.sprintf "Command <%s> ended with error <%d>. Location: %s:%d:%d." 
                 arg ec file line column)
          else
            ()

let copy ~trunc __last_cond filename cs (out:Out.t) =
  let s = Mpp_charstream.string_of_charstream cs in
  let o =
    if trunc then
      open_out_gen [Open_wronly;Open_creat;Open_trunc;Open_binary] 0o640 filename
    else
      open_out_gen [Open_wronly;Open_creat;Open_binary] 0o640 filename
  in
  let f = Out.Out_channel o in
    Out.output_string f s;
    Out.flush f;
    Pervasives.close_out o;
    Out.output_string out s
  

let builtins :  action_set ref =
  let cmd _ = cmd in
  let echo _ =
    (fun a _cs out -> Out.output_string out a) in
  let cat _ =
    (fun filename _cs out -> cat out filename; Out.flush out)
  in
  let set _ = Variable.set in
  let unset _ = Variable.unset in
  let unsetall _ = Variable.unsetall in
  let get _ = Variable.get in
  let tryget _ = Variable.tryget in
  let ifcmd = Variable.ifcmd in
  let ifdef = Variable.ifdef in
  let ifndef = Variable.ifndef in
  let elzeifdef = Variable.elzeifdef in
  let elze = Variable.elze in
  let error = (fun _ s cs _ -> 
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
        "ignore", (fun _ _ _ _ -> ()), "A command that does nothing with its arguments.";
        "ifcmd", ifcmd, "If the last external command returned 0, then inputs the rest.";
        "ifdef", ifdef, "If the argument is a defined variable, then inputs the rest.";
        "tryget", tryget, "Get the value of a variable, and if it doesn't exist, it does nothing.";
        "error", error, "Stops MPP.";
        "ifndef", ifndef, "If the argument is not a defined variable, then inputs the rest, else does nothing.";
        "else", elze, "If the previous test was not satisfied, then outputs its arguments";
        "elseifdef", elzeifdef, "If the previous test was not satisfied and the variable exists, then outputs the rest.";
        "set", set, "Set the variable to the rest. Related: get, tryget, unset, unsetall.";
        "get", get, "Get the value of a variable, and if it does not exist, MPP stops. Related: set, tryget, unset, unsetall.";
        "unset", unset, "Unset a variable. Related: tryget, get, tryget, unsetall.";
        "unsetall", unsetall, "Unset all variables. Related: tryget, get, tryget, unsetall.";
        "cmd", cmd, "Execute the rest of the line as a shell command. Following lines (if any) are given as input of the shell command.";
        "echo", echo, "Print the rest of the line.";
        "cat", cat, "Print the contents of a file.";
        "copy", copy ~trunc:false, "Copy the block to a file.";
        "tcopy", copy ~trunc:true, "Copy the block to a file, empty it first if it already exists.";
      ]
  in ref r

(* **end library ********************************************* *)
(* *********************************************************** *)

let apply_builtin action_name location =
  try
    match Mpp_stringmap.find action_name !builtins with
      | f, _ -> f
  with Not_found ->
    if !ignore_non_existing_commands then
      begin
      (* type  action = (bool option ref -> action_name -> charstream ->  Out.t -> unit) *)
        fun _last_cond _action_name _charstream _out -> ()
      end
    else
      begin
        parse_error
          ~msg:(Printf.sprintf "Command <%s> not found!" action_name)
          location;
        Pervasives.exit 1
      end


let exec (last_cond:bool option ref) (action_name:string) (arguments:string) (charstream:charstream) (out: Out.t) =
  if !debug then 
    begin
      Printf.eprintf "exec: %!";
      (* action_name : thing to do; arguments : arguments on the first
         line; charstream : what follows the first line (if any). *)
      Printf.eprintf "action_name:<%s> arguments:<%s>\n%!"
        action_name arguments;
    end;
  if action_name.[0] <> '-' then
    (apply_builtin action_name (charstream.where())) last_cond arguments charstream out
  else
    begin
      (apply_builtin "cmd" (charstream.where())) last_cond
        (String.sub action_name 1 (String.length action_name - 1) ^ " " ^ arguments)
        charstream out;
      if !debug then Printf.eprintf "???%!";
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

let register (name:string) (f: action) (d:documentation) : unit =
  builtins := Mpp_stringmap.add name (f,d) !builtins

let _ =
  register
    "builtins"
    (fun _ _ _ -> list_builtins())
    "List all available builtins."


let builtins = () (* prevent builtins from being used outside. Perhaps I'll switch to using an mli file. *)


