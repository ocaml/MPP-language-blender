(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)
(* This is on-going work which is not intended to be used at the
 * moment. So if you're reading this, don't be surprised if it doesn't
 * work yet! *)

(* **********************************************************************
   This block is intended to make this programme able to run on itself.
   **********************************************************************
   * [[ -setopen {{{]] {{{ -setclose }}}]] {{{ -setendlinecomments ###}}}
   * {{{ -setopencomments <<<}}}
   * {{{ -setclosecomments >>>}}}
   * <<< plop >>>
   * {{{ANamedBlock -echo echo from ANamedBlock ANamedBlock}}}
   ********************************************************************* *)



let debug = true
let debug = false

exception Exit of int
let exit i = raise (Exit i)

(* tokens *)
let open_token = ref "[["
let close_token = ref "]]"
let endline_comments_token = ref "%%"
let open_comments_token = ref "/*"
let close_comments_token = ref "*/"


type charstream = {
  take : unit -> char option;
  push : char -> unit;
  insert : charstream -> unit;
  where : unit -> location;
}
and filename = string
and location = filename*line*column
and line = int
and column = int

let string_of_charstream c =
  let b = Buffer.create 42 in
  let rec loop () =
    match c.take () with
      | None -> Buffer.contents b
      | Some c -> Buffer.add_char b c; loop()
  in loop()

let output_charstream out c =
  let rec loop () =
    match c.take () with
      | None -> ()
      | Some c -> output_char out c; loop()
  in loop()


module M = Map.Make(String)
type set = string M.t
type action =
  | Function of (string -> charstream -> unit)
  | Command of string
type action_set = action M.t

(* variable environment *)
let environment : set = M.empty
let actions : action_set ref = ref M.empty

module Char_set =
struct
  include Set.Make(Char) 
  let of_list l =
    List.fold_left (fun r e -> add e r) empty l 
end

let newline_chars = Char_set.of_list ['\n'; '\r']
let space_chars = Char_set.of_list [' '; '\t']
let blank_chars = Char_set.of_list ['\n'; '\r';' '; '\t']

let parse_error : ?start:location -> ?msg:string -> location -> unit = 
  fun ?start:start ?msg:message (location:location) ->
  let f, l, c = location in
    match start with
      | None ->
          begin match message with
            | None -> 
                Printf.eprintf
                  "Error file:<%s> line:%d column:%d.\n%!"
                  f l c
            | Some m ->
                Printf.eprintf
                  "Error <%s> file:<%s> line:%d column:%d.\n%!"
                  m f l c
          end
      | Some(filename,line,column) ->
          begin match message with
            | None -> 
                Printf.eprintf
                  "Error file<:%s> line:%d column:%d to file:<%s> line:%d column:%d.\n%!"
                  filename line column f l c
            | Some m ->
                Printf.eprintf
                  "Error <%s> file:<%s> line:%d column:%d to file:<%s> line:%d column:%d.\n%!"
                  m filename line column f l c 
          end

let charstream_peek ?(n=1) charstream =
  let limit = ref n in
  let b = Buffer.create n in
    for i = 1 to n do
      match charstream.take() with
        | Some c -> Buffer.add_char b c
        | None -> limit := min !limit (pred i)
    done;
    let res = Buffer.contents b in
      for i = !limit - 1 downto 0 do
        try charstream.push res.[i] with _ -> assert false
      done;
      res

let rec charstream_of_inchannel filename ?(line=1) ?(column=0) inchan =
  (* Eventually, we might want to get rid of the "double definition". *)
  let buffer : char list ref = ref [] in
  let line = ref line in
  let column = ref [column] in
  let incr_column () =
    (match !column with
       | [] -> assert false;
       | x :: tl -> column := (x+1) :: tl
    )
  in
  let where () =
    match !column with
      | [] -> assert false
      | c::_ -> filename, !line, c
  in
  let take () =
    match !buffer with
      | [] ->
          begin
            try match input_char inchan with
              | '\n' | '\r' as c -> 
                  incr line;
                  column := 0 :: !column;
                  Some c
              | c -> 
                  incr_column();
                  Some c
            with End_of_file -> None
          end
      | c::tl ->
          match c with 
            | '\n' | '\r' -> 
                incr line;
                column := 0 :: !column;
                buffer := tl;
                Some c
            | c -> 
                incr_column();
                buffer := tl;
                Some c
  in
  let push c =
    buffer := c :: !buffer
  in
  let csl = ref [{take;push;where;insert=fun _ -> assert false}] in
  let insert cs =
    csl := cs :: !csl
  in
  let rec where () =
    match !csl with
      | [] -> "very end of file", 0, 0
      | e::_ -> e.where()
  in
  let rec take () =
    match !csl with
      | [] -> None
      | e::tl ->
          match e.take() with
            | None -> csl := tl; take()
            | Some _ as res -> res
  in
  let rec push c =
    match !csl with
      | [] ->
          csl := [charstream_of_inchannel "/dev/null" (open_in "/dev/null")];
          push c
      | e::tl ->
          begin
            match c with
              | '\n' | '\r' ->
                  begin
                    decr line;
                    match !column with
                      | [] -> assert false; (* If this happens, either this block is broken 
                                               or too many characters have been pushed back,
                                               in both cases the program is broken and has to be fixed. *)
                      | _::tl -> column := tl
                  end
              | _ ->
                  match !column with
                    | cl::cls -> column := pred cl::cls
                    | [] -> column := [-1]
          end;
          e.push c
  in
    { take ; push ; insert ; where }

let charstream_of_string : ?location:location -> string -> charstream = fun ?(location=("<<anon-string>>",0,0)) s ->
  (*It's too inconvenient to keep an optimal complete version of this
    function, so I chose to rely on [charstream_of_inchannel], **for
    now**.  Also, it is not good to write the data in a
    file... especially because I never delete the file, so it is not
    safe at all. If I keep relying on [charstream_of_inchannel], then
    I might use Unix.pipe and perhaps threads as well because writing
    to a file descriptor is a blocking operation, however threads are
    not convinient at all.  *)
  let tmp = Filename.temp_file (* ~temp_dir:"/tmp" *) "tmp" "plop" in
  let () = at_exit (fun () -> Sys.remove tmp) in
  let octmp = open_out tmp in
  let () = output_string octmp s in
  let () = close_out octmp in
  let name, line, column = location in
    charstream_of_inchannel name ~line:line ~column:column (open_in tmp)


let match_token token charstream =
  if debug then 
    (let _filename, _line, _col = charstream.where () in
       Printf.printf "<token=%s@%d-%d----'%s'>\n%!" token _line _col (charstream_peek ~n:20 charstream));
  let rec loop i taken =
    if i >= String.length token then
      true, []
    else
      match charstream.take() with
        | None ->
            false, taken
        | Some c ->
            if token.[i] = c then 
              loop (succ i) (c::taken)
            else
              false, (c::taken)
  in match loop 0 [] with
    | true, _ ->
        true
    | false, taken ->
        List.iter charstream.push taken; (* do not List.rev!! *)
        false

let _x _  =
  let l = "
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)" in 
    for i = 0 to String.length l - 1 do
      let s = String.sub l i (String.length l - i) in
        Printf.printf "%b-" 
          (match_token
              "*/"
              (charstream_of_string s))
    done;
    Printf.printf "\n%!";;

let _x _ =
  match_token
    "*/"
    (charstream_of_string "(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)")
    

let rec eat (cs:Char_set.t) charstream =
  match charstream.take() with
    | None ->
        ()
    | Some c -> 
        if Char_set.mem c cs then 
          eat cs charstream
        else
          charstream.push c            


let read_until ?(failsafe=false) c charstream =
  if debug then Printf.printf "read_until '%s'\n%!" (Char.escaped c);
  let b = Buffer.create 128 in
  let rec loop () =
    match charstream.take() with
      | Some z ->
  let () = if debug then Printf.printf "peek<%s>\n%!" (charstream_peek ~n:20 charstream) in
          if c = z then
            begin
              charstream.push z;
              Buffer.contents b
            end
          else
            begin
              Buffer.add_char b z;
              loop ()
            end
      | None ->
          if failsafe then
            Buffer.contents b
          else
            begin
              parse_error ~msg:"Cound read until far enough" (charstream.where());
              exit 1
            end
  in loop ()

let read_until_one_of ?(failsafe=false) ?(push_back=false) (cs:Char_set.t) ?(exclude=Char_set.empty) ?(expect:string option) charstream =
  if debug then Printf.printf "read_until_one_of <%s>\n%!" (Char_set.fold (fun c r -> Printf.sprintf "%s%s;" r (Char.escaped c)) cs ""); 
  let b = Buffer.create 128 in
  let rec loop () =
    match charstream.take() with
      | Some z ->
          if Char_set.mem z cs  then
            begin
              if push_back then charstream.push z;
              Buffer.contents b
            end
          else if Char_set.mem z exclude then
            begin
              match expect with
                | None ->
                    parse_error ~msg:(Printf.sprintf "Forbidden character: <%s>" (Char.escaped z)) (charstream.where());
                    exit 1
                | Some m ->
                    parse_error ~msg:(Printf.sprintf "Forbidden character: <%s>. Expected: <%s>" (Char.escaped z) m) (charstream.where());
                    exit 1
            end
          else
            begin
              Buffer.add_char b z;
              loop ()
            end
      | None ->
          if failsafe then
            Buffer.contents b
          else
            begin
              parse_error ~msg:"Cound read until far enough" (charstream.where());
              exit 1
            end
  in loop ()

(* This has to be patched to accept other integers than natural numbers. *)
let parse_int charstream =
  if debug then Printf.printf "parse_int\n%!";
(*   let start = charstream.where() in *)
  let res = Buffer.create 42 in
  let rec loop () =
    match charstream.take() with
      | Some ('0' .. '9' as c) -> Buffer.add_char res c; loop()
      | Some '_' -> loop()
      | Some c -> charstream.push c
      | None -> ()
  in
    loop ();
    int_of_string (Buffer.contents res)


(* Parses a string in the format "([^\\]|(\\\\)|(\\\"))*" *)
let parse_string charstream =
  if debug then Printf.printf "parse_string\n%!";
  (* We assume that a '"' character has just been consumed. *)
  let start = charstream.where() in
  let res = Buffer.create 42 in
  let rec loop () =
    match charstream.take() with
      | Some '"' -> Buffer.contents res
      | Some '\\' ->
          begin
            match charstream.take() with
              | Some ('"' | '\\' as c) -> Buffer.add_char res c; loop()
              | Some c ->
                  parse_error
                    ~msg:"Unrecognized backslash usage."
                    (charstream.where());
                  exit 1
              | None ->
                  parse_error
                    ~msg:"Unrecognized backslash usage, and unexpected end of file."
                    (charstream.where());
                  exit 1
          end
      | Some c -> Buffer.add_char res c; loop()
      | None -> 
          parse_error
            ~start:start
            ~msg:"Unexpected end of file." 
            (charstream.where());
          exit 1
  in loop ()


(* Reads until an exact word is found. *)
let read_until_word ?(failsafe=false) charstream word =
  assert(word<>"");
  let start_location = charstream.where() in
  let res = Buffer.create (String.length word * 8) in
  let buf = Buffer.create (String.length word) in
  let rec loop i =
    if i >= String.length word then
      Buffer.contents res
    else
      match charstream.take() with
        | None ->
            if failsafe then
              Buffer.contents res
            else
              begin
                parse_error
                  ~start:start_location
                  ~msg:(Printf.sprintf "Could not reach the end of block ending with \"%s\"." (String.escaped word))
                  (charstream.where());
                exit 1
              end
        | Some c ->
            if c = word.[i] then
              begin
                Buffer.add_char buf c;
                loop (succ i)
              end
            else
              begin
                Buffer.add_buffer res buf;
                Buffer.clear buf;
                Buffer.add_char res c;
                loop 0
              end
  in
    loop 0


let split_on_char ?(keep_empty_strings=false) c s =
  let buf = Buffer.create 42 in
  let sl = String.length s in
  let make_res res =
    match Buffer.contents buf with
      | "" -> if keep_empty_strings then "" :: res else res
      | b -> b :: res
  in
  let rec loop index res =
    if index >= sl then
      make_res res
    else if s.[index] = c then
      let l = make_res res in (*here, order is important*)
        Buffer.clear buf;
        loop (index+1) l
    else
      begin
        Buffer.add_char buf s.[index];
        loop (index+1) res
      end
  in List.rev (loop 0 [])

(* let _ = split_on_char 'c' "ncdiunuidscnicndsncdisnciudnplop";;
   let _ = split_on_char ' ' "  eizbez ";;    *)

let plop =
  "choose_open_token", 
  (fun (location:location) -> function
     | [e] -> open_token := e
     | _ -> 
         parse_error ~msg:"Wrong use of choose_open_token." location)


let exec_command cmd arguments location =
  if debug then Printf.printf "exec_command\n%!";
  match Sys.command (cmd ^ arguments) with
    | 0 -> ()
    | rc ->
      Printf.eprintf
        "Warning: command <%s> exited with code %d\n%!"
        cmd
        rc

(* *********************************************************** *)
(* **begin library ******************************************* *)
let cat filename =
  if Sys.file_exists filename then
    let i = open_in filename in
      try while true do
        print_char (input_char i)
      done with End_of_file -> ()

let command arg charstream =
  let tmp = Filename.temp_file (* ~temp_dir:"/tmp" *) "tmp" "plop" in
  let otmp = open_out tmp in
    output_charstream otmp charstream;
    ignore(Sys.command ("cat " ^ tmp ^ " | " ^ arg));
    Sys.remove tmp

let builtins : action_set ref =
  let cmd = Function command in
  let echo =
    Function(fun a _ -> print_endline a) in
  let cat  =
    Function(fun filename _ -> cat filename) 
  in
  let set_opentoken =
    Function(fun x _ -> open_token := x)
  in
  let set_closetoken =
    Function(fun x _ -> close_token := x)
  in
  let set_endline_comments_token =
    Function(fun x _ -> endline_comments_token := x)
  in
  let set_open_comments_token =
    Function(fun x _ -> open_comments_token := x)
  in
  let set_close_comments_token = 
    Function(fun x _ -> close_comments_token := x)
  in
  let res =
    List.fold_left
      (fun r (k,e) -> M.add k e r)
      M.empty
      [
        "-cmd", cmd; 
        "-echo", echo; 
        "-cat", cat;
        "-setopen", set_opentoken;
        "-setclose", set_closetoken;
        "-setendlinecomments", set_endline_comments_token;
        "-setopencomments", set_open_comments_token;
        "-setclosecomments", set_close_comments_token;
      ]
  in ref res

(* **end library ******************************************* *)
(* *********************************************************** *)

let lookup_builtin action_name =
  match M.find action_name !builtins with
    | Function f -> f
    | Command s -> failwith "Command not yet implemented."

let exec (action_name:string) (arguments:string) (charstream:charstream) =
  if debug then Printf.printf "exec: %!";
  (* action_name : thing to do;
     arguments   : arguments on the first line;
     charstream  : what follows the first line (if any). *)
  if debug then Printf.printf "action_name:<%s> arguments:<%s>"
    action_name arguments;
  if action_name.[0] = '-' then
    lookup_builtin action_name arguments charstream
  else
    failwith "General exec not yet implemented."



let preprocess (charstream: charstream) =
  assert(!open_token <> "");
  assert(!close_token <> "");
  assert(!endline_comments_token <> "");
  assert(!open_comments_token <> "");
  assert(!close_comments_token <> "");
  
  (* entry point *)
  let rec loop () : unit =
    (*     begin match charstream.take () with *)
    (*       | None -> () *)
    (*       | Some c -> *)
    (*           Printf.printf "[%s]" (Char.escaped c); *)
    (*           charstream.push c; *)
    (*     end; *)
    begin
      if match_token !open_token charstream then
        open_token_action()
      else if match_token !close_token charstream then
        close_token_action()
      else if match_token !endline_comments_token charstream then
        endline_comments_token_action()
      else if match_token !open_comments_token charstream then
        open_comments_token_action()
      else if match_token !close_comments_token charstream then
        close_comments_token_action()
      else
        default (charstream.take())
    end

  and init() = 
    let builtin__input =
      Function(fun arg cs ->
        let x = open_in arg in
          charstream.insert (charstream_of_inchannel arg x);
          loop();
          close_in x
      )
    in
      builtins := M.add "-input" builtin__input !builtins

  (* default action *)
  and default = function
    | None -> ()
    | Some c ->
        (* Printf.printf "<%s>%!" (Char.escaped c); *)
        print_char c;
        flush stdout;
        loop()

  (* new block *)
  and open_token_action () =
    let _action_nested_option =
      match charstream.take() with
        | Some '\\' -> `Not_Nested
        | Some c -> charstream.push c; `Nested
        | None ->
            parse_error
              ~msg:"No characters left to read right after an opening!" 
              (charstream.where());
            exit 1
    in
    let () = 
      if debug then 
        Printf.printf "peek<%s>\n%!"
          (charstream_peek ~n:20 charstream) 
    in
    let block_name = (* block_name: syntactic "tool" *)
      match charstream.take() with
        | None ->
            parse_error
              ~msg:"No characters left to read right after an opening!" 
              (charstream.where());
            exit 1
        | Some (' ' | '\t') ->
            None
        | Some c ->
            charstream.push c;
            let () = if debug then Printf.printf "peek<%s>\n%!" (charstream_peek ~n:20 charstream) in
      Some (read_until ~failsafe:true ' ' charstream)
  in
  let () = eat space_chars charstream in
  let block_start_location = charstream.where() in
  let block_contents =
    (* the contents of the block *)
    match block_name with
      | Some name -> 
          if debug then Printf.printf "name=<%s>%!" name;
          read_until_word charstream (name^ !close_token)
      | None -> read_until_word charstream (!close_token)
  in
  let charstream = () in let _ = charstream in (* ~> to prevent its use afterwards *)
  let blockcharstream =
    (* the contents of the block is converted into a charstream *)
    charstream_of_string ~location:(block_start_location) block_contents
  in
  let action_name : string = (* name of the action *)
    eat space_chars blockcharstream;
    read_until_one_of
      space_chars
      ~exclude:newline_chars
      ~expect:"Zero or more spaces, and then an action name."
      blockcharstream
  in
  let action_arguments : string = (* action arguments *)
    match blockcharstream.take() with
      | Some c ->
          blockcharstream.push c;
          read_until_one_of ~failsafe:true newline_chars blockcharstream
      | None ->
          parse_error
            ~msg:"No characters left to read right after an opening!"
            (blockcharstream.where());
          exit 1
  in
    exec action_name action_arguments blockcharstream;
    loop ()

(* Closing a block that hasn't been opened is wrong. *)
and close_token_action () =
  parse_error ~msg:"Closing unopened block." (charstream.where());
  exit 1

(* Just ignore what has to be ignored. *)
and endline_comments_token_action () =
  let _l = read_until_one_of newline_chars charstream in
    if debug then Printf.printf  "comments: <%s>\n%!" _l;
    loop()

(* New comment block. *)
and open_comments_token_action () = 
  let _c = read_until_word charstream (!close_comments_token) in
    if debug then Printf.printf  "comments: <%s>\n%!" _c;
    loop()

(* Closing a comment block that hasn't been opened is wrong. *)
and close_comments_token_action () = 
  parse_error ~msg:"Closing unopened comments block." (charstream.where());
  exit 1
in
  init();
  loop()


let _ = 
  if Array.length Sys.argv > 1 then 
    try
(*       Printf.printf "plop...\n%!"; *)
      preprocess (charstream_of_inchannel Sys.argv.(1) (open_in Sys.argv.(1)));
(*       Printf.printf "...end.\n%!" *)
    with e -> 
      Printexc.print_backtrace stdout;
      Printf.printf "Exception raised: <%s>\nBacktrace:%!" (Printexc.to_string e)




