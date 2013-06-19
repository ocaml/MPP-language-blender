(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)

open Mpp_charstream

let space_chars = ref (Mpp_charset.of_list [' '; '\t'])
let debug = ref false

module Out = Mpp_out

(* variable environment *)
type set = string Mpp_stringmap.t

let environment : set = Mpp_stringmap.empty

module Variable : sig
  val set: string -> charstream -> 'ignored -> unit
  val get: string -> charstream ->  Out.t -> unit
  val tryget: string -> charstream ->  Out.t -> unit
  val unset: string -> charstream -> 'ignored -> unit
  val unsetall: 'string -> 'charstream ->  Out.t -> unit
  val ifdef: bool option ref -> string -> charstream ->  Out.t -> unit
  val ifndef: bool option ref -> string -> charstream ->  Out.t -> unit
  val elzeifdef: bool option ref -> string -> charstream ->  Out.t -> unit
  val elze: bool option ref -> string -> charstream ->  Out.t -> unit
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
    let css = charstream_of_string s in
    let variable =
      read_until_one_of !space_chars css
    in
    let value = 
      match string_of_charstream cs with
        | "" -> string_of_charstream css
        | x -> 
            string_of_charstream css ^ "\n" ^ x
    in
      env := add variable value !env

  let get s cs out =
    let s = suppress_spaces s in
      try
        Out.output_string out (find s !env)
      with Not_found ->
        parse_error
          ~msg:(Printf.sprintf "You tried to get the value of variable %s, which doesn't exist." s) 
          (cs.where());
        Pervasives.exit 1

  let tryget s cs out =
    let s = suppress_spaces s in
      try
        Out.output_string out (find s !env)
      with Not_found ->
        ()

  let unset s cs _ =
    let s = suppress_spaces s in
      try
        env := remove s !env
      with Not_found ->
        parse_error
          ~msg:(Printf.sprintf "You tried to get the value of variable %s, which doesn't exist." s) 
          (cs.where());
        Pervasives.exit 1


  let ifdef last_cond s cs out =
    if !debug then Printf.eprintf "ifdef <%s> <%s>\n%!" s (String.escaped (string_of_charstream cs));
    let css = charstream_of_string s in
    let s = read_until_one_of !space_chars css in
      try
        begin
          ignore(find s !env); (*raises Not_found if not found*)
          last_cond := Some true;
          Out.output_charstream out css;
          Out.output_char out '\n';
          Out.output_charstream out cs
        end
      with Not_found -> 
        last_cond := Some false

  let ifndef last_cond s cs out =
    if !debug then Printf.eprintf "ifndef <%s> <%s>\n%!" s (String.escaped (string_of_charstream cs));
    let css = charstream_of_string s in
    let s = read_until_one_of !space_chars css in
      try
        begin
          ignore(find s !env);
          last_cond := Some false
        end
      with Not_found -> 
        last_cond := Some true;
        Out.output_charstream out css;
        Out.output_charstream out cs


  let elzeifdef last_cond s cs out =
    match !last_cond with
      | Some c ->
          if c then
            ()
          else
            ifdef last_cond s cs out
      | None ->
          parse_error ~msg:"`elseifdef' without a matching previous `if'."
            (cs.where());
          Pervasives.exit 1
end
