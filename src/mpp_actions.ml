(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)
open Mpp_charstream
module Out = Mpp_out

let space_chars = ref (Mpp_charset.of_list [' '; '\t'])
let debug = ref false

let ignore_non_existing_commands = ref false

let main_process : (charstream -> Out.t -> unit) ref = ref (fun _ _ -> assert false)

type action = last_condition -> nesting -> action_args -> charstream ->  Out.t -> unit
and action_args = charstream
and nesting = bool
and last_condition = bool option ref

type  action_set = (action * documentation) Mpp_stringmap.t
and documentation = string

let actions :  action_set ref = ref Mpp_stringmap.empty

let ignore_exec_error = ref false


let is_lazy, register_lazy =
  let module SS = Set.Make(String) in
  let lazy_set = ref SS.empty in
    ((fun action_name ->
        SS.mem action_name !lazy_set),
     (fun action_name -> 
        lazy_set := SS.add action_name !lazy_set
     ))

(* *********************************************************** *)
(* **BEGIN LIBRARY ******************************************* *)

(* BEGIN VARIABLES *)
(* variable environment *)
type set = string Mpp_stringmap.t

let environment : set = Mpp_stringmap.empty

module Mpp_conditions = struct
  let elze last_cond nesting (s:charstream) cs out =
    match !last_cond with
      | None ->
          Mpp_charstream.parse_error ~msg:"`else' without a previous matching conditional."
            (cs.Mpp_charstream.where());
          Pervasives.exit 1
      | Some c ->
          last_cond := None;
          if c then
            ()
          else
            begin
              if nesting then
                begin
                  let b1 = Buffer.create 42 in
                    !main_process s (Out.Buffer b1);
                    Out.output_buffer out b1;
                    Out.output_char out '\n';
                    let b2 = Buffer.create 42 in
                      !main_process cs (Out.Buffer b2);
                      Out.output_buffer out b2;
                end
              else
                begin
                  Out.output_charstream out s;
                  Out.output_char out '\n';
                  Out.output_charstream out cs;
                end
            end
end


module Variable : sig
  val set: charstream -> charstream -> 'ignored -> unit
  val get: charstream -> charstream ->  Out.t -> unit
  val tryget: charstream -> charstream ->  Out.t -> unit
  val unset: charstream -> charstream -> 'ignored -> unit
  val unsetall: 'string -> 'charstream ->  Out.t -> unit
  val ifdef: bool option ref -> bool -> charstream -> charstream ->  Out.t -> unit
  val ifndef: bool option ref -> bool -> charstream -> charstream ->  Out.t -> unit
  val elzeifdef: bool option ref -> bool -> charstream -> charstream ->  Out.t -> unit
  val elze: bool option ref -> bool -> charstream -> charstream ->  Out.t -> unit
end = struct
  include Mpp_conditions
  include Map.Make(String)
  let suppress_spaces s =
    let b = Buffer.create (String.length s - 1) in
      for i = 0 to String.length s - 1 do
        match s.[i] with
          | ' ' | '\t' | '\n' | '\r' ->
              ()
          | c -> Buffer.add_char b c
      done;
      Buffer.contents b

  let env = ref empty

  let unsetall _s _cs _out = env := empty

  let set s cs _ =
    let variable =
      read_until_one_of !space_chars s
    in
    let value = 
      match string_of_charstream cs with
        | "" -> string_of_charstream s
        | x -> 
            string_of_charstream s ^ "\n" ^ x
    in
      env := add variable value !env

  let get s cs out =
    let s = suppress_spaces (string_of_charstream s) in
      try
        Out.output_string out (find s !env)
      with Not_found ->
        parse_error
          ~msg:(Printf.sprintf "You tried to get the value of variable %s, which doesn't exist." s) 
          (cs.where());
        Pervasives.exit 1

  let tryget s cs out =
    let s = suppress_spaces (string_of_charstream s) in
      try
        Out.output_string out (find s !env)
      with Not_found ->
        ()

  let unset s cs _ =
    let s = suppress_spaces (string_of_charstream s) in
      try
        env := remove s !env
      with Not_found ->
        parse_error
          ~msg:(Printf.sprintf "You tried to get the value of variable %s, which doesn't exist." s) 
          (cs.where());
        Pervasives.exit 1


  let ifdef last_cond nesting (cs:charstream) bcs out =
    if !debug then Printf.eprintf "ifdef <%s> <%s>\n%!" (string_of_charstream cs) (String.escaped (string_of_charstream bcs));
    let s:string = read_until_one_of !space_chars cs in
      try
        begin
          ignore(find s !env); (*raises Not_found if not found*)
          last_cond := Some true;
          if nesting then
            begin
              let b1 = Buffer.create 42 in
                !main_process cs (Out.Buffer b1);
                Out.output_buffer out b1;
                Out.output_char out '\n';
                let b2 = Buffer.create 42 in
                  !main_process bcs (Out.Buffer b2);
                  Out.output_buffer out b2;
            end
          else
            begin
              Out.output_charstream out cs;
              Out.output_char out '\n';
              Out.output_charstream out bcs;
            end
        end
      with Not_found -> 
        last_cond := Some false

  let ifndef last_cond nesting (cs:charstream) bcs out =
    if !debug then Printf.eprintf "ifdef <%s> <%s>\n%!" (string_of_charstream cs) (String.escaped (string_of_charstream bcs));
    let s:string = read_until_one_of !space_chars cs in
      try
        begin
          ignore(find s !env); (*raises Not_found if not found*)
          last_cond := Some false;
        end
      with Not_found -> 
        last_cond := Some true;
        if nesting then
          begin
            let b1 = Buffer.create 42 in
              !main_process cs (Out.Buffer b1);
              Out.output_buffer out b1;
              Out.output_char out '\n';
              let b2 = Buffer.create 42 in
                !main_process bcs (Out.Buffer b2);
                Out.output_buffer out b2;
          end
        else
          begin
            Out.output_charstream out cs;
            Out.output_char out '\n';
            Out.output_charstream out bcs;
          end

  let elzeifdef (last_cond:bool option ref) (nesting:bool) (s:charstream) (cs:charstream) (out:Out.t) =
    match !last_cond with
      | Some c ->
          if c then
            ()
          else
            ifdef last_cond nesting s cs out
      | None ->
          parse_error ~msg:"`elseifdef' without a matching previous `if'."
            (cs.where());
          Pervasives.exit 1
end
  (* / END VARIABLES *)


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
    let ec = Sys.command ("( cat " ^ tmp ^ " | " ^ string_of_charstream arg ^ " ) > " ^ tmp2 ) in
    let () = cat out tmp2 in
      Sys.remove tmp;
      Sys.remove tmp2;
      last_cmd := ec;
      Out.flush out;
      ec

let ifcmd last_cond nesting arg charstream out =
  if !debug then 
    Printf.eprintf "ifcmd <%s> <%s>\n%!"
      (String.escaped (string_of_charstream arg)) (String.escaped (string_of_charstream charstream));
  if !last_cmd = 0 then
    begin
      last_cond := Some true;
      if nesting then
        begin
          let b1 = Buffer.create 42 in
            !main_process arg (Out.Buffer b1);
            Out.output_buffer out b1;
            Out.output_char out '\n';
          let b2 = Buffer.create 42 in
            !main_process charstream (Out.Buffer b2);
            Out.output_buffer out b2;
        end
      else
        begin
          Out.output_charstream out arg;
          Out.output_char out '\n';
          Out.output_charstream out charstream;
        end
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
                 (string_of_charstream arg) ec file line column)
          else
            ()

let copy ~trunc _last_cond _nesting filename cs (out:Out.t) =
  let filename = string_of_charstream filename in
  let s = string_of_charstream cs in
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
      

let builtins : action_set ref =
  let cmd _ _ = cmd in
  let echo _ _ =
    (fun a _cs out -> Out.output_charstream out a) in
  let cat _ _ =
    (fun filename _cs out -> cat out (string_of_charstream filename); Out.flush out)
  in
  let set _ _ = Variable.set in
  let unset _ _ = Variable.unset in
  let unsetall _ _ = Variable.unsetall in
  let get _ _ = Variable.get in
  let tryget _ _ = Variable.tryget in
  let ifcmd = ifcmd in
  let ifdef = Variable.ifdef in
  let ifndef = Variable.ifndef in
(*   let elzeifdef = Variable.elzeifdef in *)
  let elze = Variable.elze in
  let error _ _ s cs _ =
    parse_error 
      ~msg:(Printf.sprintf "your message is <%s>. No matter what, I'm exiting." (string_of_charstream s))
      (cs.where());
    Pervasives.exit 1
  in
  let r =
    List.fold_left
      (fun r (k,(e:bool option ref -> bool -> Mpp_charstream.charstream -> Mpp_charstream.charstream -> Out.t -> unit),doc) -> Mpp_stringmap.add k (e,doc) r)
      Mpp_stringmap.empty
      [
        "ignore", (fun _ _ _ _ _ -> ()), "A command that does nothing with its arguments.";
        "ifcmd", ifcmd, "If the last external command returned 0, then inputs the rest.";
        "ifdef", ifdef, "If the argument is a defined variable, then inputs the rest.";
        "tryget", tryget, "Get the value of a variable, and if it doesn't exist, it does nothing.";
        "error", error, "Stops MPP.";
        "ifndef", ifndef, "If the argument is not a defined variable, then inputs the rest, else does nothing.";
        "else", elze, "If the previous test was not satisfied, then outputs its arguments";
        (* "elseifdef", elzeifdef, "If the previous test was not satisfied and the variable exists, then outputs the rest."; *)
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
  in ref (r:action_set)

(* **end library ********************************************* *)
(* *********************************************************** *)

let apply_builtin action_name location =
  try
    match Mpp_stringmap.find action_name !builtins with
      | f, _ -> (f:bool option ref -> bool -> charstream -> charstream -> Out.t -> unit )
  with Not_found ->
    if !ignore_non_existing_commands then
      begin
        (* type  action = (bool option ref -> action_name -> charstream ->  Out.t -> unit) *)
        fun (_last_cond:bool option ref) (_nesting:bool) _action_name _charstream _out -> ()
      end
    else
      begin
        parse_error
          ~msg:(Printf.sprintf "Command <%s> not found!" action_name)
          location;
        Pervasives.exit 1
      end

let exec (nesting:bool) (last_cond:bool option ref) (action_name:string) (arguments:charstream) (charstream:charstream) (out: Out.t) =
  if !debug then
    begin
      Printf.eprintf "exec: %!";
      (* action_name : thing to do; arguments : arguments on the first
         line; charstream : what follows the first line (if any). *)
      Printf.eprintf "action_name:<%s> arguments:<%s>\n%!"
        action_name (string_of_charstream arguments);
    end;
  if action_name.[0] <> '-' then
    begin
      if nesting then
        begin
          if is_lazy action_name then
            begin (* nesting and lazy: TODO *)
              apply_builtin action_name (charstream.where()) last_cond nesting arguments charstream out
            end
          else
            begin (* nesting but not lazy, so expand now! *)
              let buff1 = Buffer.create 42 in
              let buff2 = Buffer.create 42 in
              let arguments =
                let l = arguments.where() in
                  !main_process arguments (Out.Buffer buff1);
                  charstream_of_string ~location:l (Buffer.contents buff1)
              in
              let charstream =
                let x = charstream.where() in
                  !main_process charstream (Out.Buffer buff2);
                  charstream_of_string ~location:x (Buffer.contents buff2)
              in
                apply_builtin action_name (charstream.where()) last_cond nesting arguments charstream out
            end
        end
    end
  else
    begin
      if nesting then
        begin
          (apply_builtin "cmd" (charstream.where())) last_cond nesting
            (charstream_of_string (String.sub action_name 1 (String.length action_name - 1) ^ " " ^ string_of_charstream arguments))
            charstream out
        end
      else
        (apply_builtin "cmd" (charstream.where())) last_cond nesting
          (charstream_of_string (String.sub action_name 1 (String.length action_name - 1) ^ " " ^ string_of_charstream arguments))
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

let _ =
  List.iter register_lazy [
    "ifdef";
    "ifndef";
    "else";
    "elseifdef";
    "ifcmd";
  ]


let builtins = () (* prevent builtins from being used outside. Perhaps I'll switch to using an mli file. *)


