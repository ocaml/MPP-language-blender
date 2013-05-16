(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)

open Mpp_charstream
open Mpp_init

(* variable environment *)
type set = string Mpp_stringmap.t

let environment : set = Mpp_stringmap.empty

module Variable : sig
  val set: string -> charstream -> 'ignored -> unit
  val get: string -> charstream -> out_channel -> unit
  val unset: string -> charstream -> 'ignored -> unit
  val unsetall: 'string -> 'charstream -> 'out_channel -> unit
  val ifdef: string -> charstream -> out_channel -> unit
  val ifndef: string -> charstream -> out_channel -> unit
  val elzeifdef: string -> charstream -> out_channel -> unit
  val elze: string -> charstream -> out_channel -> unit
end = struct
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
      read_until_one_of space_chars css
    in
    let value = string_of_charstream css ^ string_of_charstream cs in
      env := add variable value !env
  let get s cs out =
    let s = suppress_spaces s in
      try
        output_string out (find s !env)
      with Not_found ->
        let f, l, c = cs.where() in
          Pervasives.failwith (Printf.sprintf "You tried to get the value of variable %s, which doesn't exist. Location: file <%s> Line<%d> Column<%d>." s f l c)
  let unset s cs _ =
    let s = suppress_spaces s in
      try
        env := remove s !env
      with Not_found ->
        let f, l, c = cs.where() in
          Pervasives.failwith (Printf.sprintf "You tried to unset the value of variable %s, which doesn't exist. Location: file <%s> Line<%d> Column<%d>." s f l c)

  let last_cond = ref true
  let last_cond_exists = ref false

  let ifdef s cs out =
    let s = suppress_spaces s in
      last_cond_exists := true;
      try
        begin
          ignore(find s !env);
          last_cond := true;
          output_charstream out cs
        end
      with Not_found -> 
        last_cond := false

  let ifndef s cs out =
    let s = suppress_spaces s in
      last_cond_exists := true;
      try 
        begin
          ignore(find s !env);
          last_cond := true;
          output_charstream out cs
        end
      with Not_found -> 
        last_cond := false

  let ifndef s cs out =
    let s = suppress_spaces s in
      last_cond_exists := true;
      try
        begin
          ignore(find s !env);
          last_cond := false;
          output_charstream out cs
        end
      with Not_found -> 
        last_cond := true

  let elze s cs out =
    if !last_cond_exists then
      begin
        last_cond_exists := false;
        if !last_cond then
          ()
        else
          begin
            output_string out s;
            output_char out ' ';
            output_charstream out cs
          end
      end
    else
      parse_error ~msg:"`else' without a matching previous `if'."
        (cs.where())

  let elzeifdef s cs out =
    if !last_cond_exists then
      begin
        if !last_cond then
          ()
        else
          ifdef s cs out
      end
    else
      parse_error ~msg:"`elseifdef' without a matching previous `if'."
        (cs.where())

end
