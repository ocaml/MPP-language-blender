open Mpp_init

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

module Mpp_charset = Mpp_charset


let parse_error : ?start:location -> ?msg:string -> location -> unit = 
  fun ?start:start ?msg:message (location:location) ->
    let f, l, c = location in
      match start with
        | None ->
            begin match message with
              | None -> 
                  Printf.eprintf
                    "Error in %s:%d:%d.\n%!"
                    f l c
              | Some m ->
                  Printf.eprintf
                    "Error: %s\nIn %s:%d:%d.\n%!"
                    m f l c
            end
        | Some(filename,line,column) ->
            begin match message with
              | None ->
                  if l <> 0 && c <> 0 then
                    Printf.eprintf
                      "Error from %s:%d:%d to %s:%d:%d.\n%!"
                      filename line column f l c
                  else
                    Printf.eprintf
                      "Error from %s:%d:%d to end of file.\n%!"
                      filename line column
              | Some m ->
                  if l <> 0 && c <> 0 then
                    Printf.eprintf
                      "Error: %s\nFrom %s:%d:%d to %s:%d:%d.\n%!"
                      m filename line column f l c
                  else
                    Printf.eprintf
                      "Error: %s\nFrom %s:%d:%d to end of file.\n%!"
                      m filename line column
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
        let x = res.[i] in
          charstream.push x
            (* try charstream.push x with e ->  *)
(*               Printf.printf "WTF: %s%!\n" (Printexc.to_string e); *)
(*               assert false *)
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
      | [] -> filename, !line, 0 (* assert false *)
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
      | [] -> "<end of file>", 0, 0
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
                      | [] -> 
                          column := [0]
                            (* assert false; *)
                          (* If this happens, either this block is broken 
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
       Printf.eprintf "<token=%s@%d-%d----'%s'>\n%!" token _line _col (charstream_peek ~n:20 charstream));
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


let rec eat (cs:Mpp_charset.t) charstream =
  match charstream.take() with
    | None ->
        ()
    | Some c -> 
        if Mpp_charset.mem c cs then 
          eat cs charstream
        else
          charstream.push c            


let read_until ?(failsafe=false) c charstream : string =
  if debug then Printf.eprintf "read_until '%s'\n%!" (Char.escaped c);
  let b = Buffer.create 128 in
  let rec loop () =
    match charstream.take() with
      | Some z ->
          let () =
            if debug then 
              Printf.eprintf "Peek<%s>\n%!"
                (String.escaped (charstream_peek ~n:20 charstream)) 
          in
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

let read_until_one_of ?(failsafe=false) ?(push_back=false) (cs:Mpp_charset.t) ?(exclude=Mpp_charset.empty) ?(expect:string option) charstream =
  if debug then Printf.eprintf "read_until_one_of [%s]\n%!" (Mpp_charset.fold (fun c r -> Printf.sprintf "%s%s" r (Char.escaped c)) cs ""); 
  let b = Buffer.create 128 in
  let rec loop () =
    match charstream.take() with
      | Some z ->
          if Mpp_charset.mem z cs  then
            begin
              if push_back then charstream.push z;
              Buffer.contents b
            end
          else if Mpp_charset.mem z exclude then
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
  if debug then Printf.eprintf "parse_int\n%!";
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
  if debug then Printf.eprintf "parse_string\n%!";
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
