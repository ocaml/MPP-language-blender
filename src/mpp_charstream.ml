(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)
(* open Mpp_init *)

let debug = ref false

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
type t = charstream


let parse_error : ?start:location -> ?msg:string -> location -> unit =
  fun ?start ?msg location ->
    let f, l, c = location in
      match start with
        | None ->
            begin match msg with
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
            begin match msg with
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


let output_charstream out c =
  let rec loop () =
    match c.take () with
      | None -> ()
      | Some c -> output_char out c; loop()
  in loop()

module Mpp_charset = Mpp_charset

let charstream_take_n n charstream =
  let b = Buffer.create n in
    for _ = 1 to n do
      match charstream.take() with
        | None -> ()
        | Some c -> Buffer.add_char b c
    done;
    Buffer.contents b

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

let null_charstream =
  let c = ref [] in
  let cs = ref [] in
  let insert e = cs := e :: !cs in
  let rec take () = match !cs with
    | x::tl ->
        begin match x.take() with
          | Some _ as r -> r
          | None -> cs := tl; take ()
        end
    | [] ->
        match !c with
        | [] -> None
        | e :: tl -> c := tl; Some e
  in
  let push e = match !cs with
    | [] -> c := e :: !c
    | x :: _ -> x.push e
  in
  let where () = match !cs with
    | [] -> "", -1, -1
    | x :: _ -> x.where ()
  in
  { take = take;
    push = push;
    insert = insert;
    where = where;
  }

let charstream_of_inchannel filename ?(line=1) ?(column=0) inchan =
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
  let where () =
    match !csl with
      | [] ->
          filename, !line, (match !column with x::_ -> x| _ -> 0)
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
          csl := [null_charstream];
          push c
      | e::_ ->
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
  { take = take;
    push = push;
    insert = insert;
    where = where;
  }


let charstream_of_string ?(location:location=("<anon-string>",0,0)) (s:string) : charstream =
  let i = ref 0 in
  let location = ref location in
  let t = ref
      (fun() ->
         if !i >= String.length s then None
         else
           begin match s.[!i] with
             | '\n' as c ->
               location :=
                 (match !location with
                  | fn, line, _ -> fn, line+1, 0);
               incr i;
               Some c
             | c ->
               location :=
                 (match !location with
                  | fn, line, col -> fn, line, col+1);
               incr i;
               Some c
           end)
  in
  let streams = ref [] in
  let stack = ref [] in
  let push c = match !streams with
    | [] -> stack := c :: !stack
    | s::_ -> s.push c
  in
  let rec take () = match !streams with
    | s::tl ->
      begin match s.take() with
        | None -> streams := tl; take()
        | Some _ as r -> r
      end
    | [] ->
      begin match !stack with
      | [] ->
        (match !t() with None -> t := (fun () -> None); None | r -> r)
      | c::tl ->
        stack := tl;
        Some c
      end
  in
  let insert cs = streams := cs :: !streams in
  let where () = match !streams with
    | [] -> !location
    | cs::_ -> cs.where()
  in
  { take = take;
    push = push;
    insert = insert;
    where = where;
  }


let match_token token charstream =
  if !debug then
    (let _filename, _line, _col = charstream.where () in
       Printf.eprintf "<token=%s@%d-%d----'%s'>\n%!" token _line _col (String.escaped(charstream_peek ~n:20 charstream)));
  let res =
    token <> ""
    &&
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
  in
    res


let rec eat (cs:Mpp_charset.t) charstream =
  match charstream.take() with
    | None ->
        ()
    | Some c ->
        if Mpp_charset.mem c cs then
          eat cs charstream
        else
          charstream.push c


let [@warning "-27"] read_until ?(caller="") ?(failsafe=false) c charstream : string =
  if !debug then Printf.eprintf "read_until '%s'\n%!" (Char.escaped c);
  let b = Buffer.create 128 in
  let rec loop () =
    match charstream.take() with
      | Some z ->
          let () =
            if !debug then
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
              parse_error
                ~msg:(Printf.sprintf "Couldn't read far enough. I could read <%s>." (Buffer.contents b))
                (charstream.where());
              exit 1
            end
  in loop ()

let read_until_one_of ?(caller="") ?(failsafe=false) ?(push_back=false) (cs:Mpp_charset.t) ?(exclude=Mpp_charset.empty) ?(expect:string option) charstream =
  if !debug then Printf.eprintf "read_until_one_of [%s]\n%!" (Mpp_charset.fold (fun c r -> Printf.sprintf "%s%s" r (Char.escaped c)) cs "");
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
              parse_error
                ~msg:(Printf.sprintf "Couldn't read far enough. I could read <%s>. I was looking for [%s].%s" (Buffer.contents b) (Mpp_charset.to_escaped_string cs) (if caller <> "" then Printf.sprintf " Caller: %s." caller else ""))
                (charstream.where());
              exit 1
            end
  in loop ()


(* This has to be patched to accept other integers than natural numbers. *)
let parse_int charstream =
  if !debug then Printf.eprintf "parse_int\n%!";
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


(* Reads until an exact word is found.
   The word should be deleted from the charstream. *)
let read_until_word ?(failsafe=false) ?(success=ref true) charstream word =
  assert(word<>"");
  let start_location = charstream.where() in
  let res = Buffer.create (String.length word * 8) in
  let buf = Buffer.create (String.length word) in
  let rec loop i =
    if i >= String.length word then
      begin
        success := true;
        Buffer.contents res
      end
    else
      match charstream.take() with
        | None ->
            success := false;
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

let parse_a_string cs =
  let location = cs.where() in
  let b = Buffer.create 10 in
  let rec loop() =
    match cs.take() with
      | None -> Buffer.contents b
      | Some '"' -> Buffer.contents b
      | Some '\\' ->
          begin match cs.take() with
            (* | '"' as c -> Buffer.add_char b c *)
            | Some ('\\' as c) -> Buffer.add_char b c
            | Some 'n' -> Buffer.add_char b '\n'
            | Some 'b' -> Buffer.add_char b '\b'
            | Some 'r' -> Buffer.add_char b '\r'
            | Some 't' -> Buffer.add_char b '\t'
            | Some '\n' -> ()
            | Some (('x'|'X'| '0' .. '2') as c0) ->
                begin
                  match cs.take() with
                    | Some ('A' .. 'F' | 'a' .. 'f' | '0' .. '9' as c1) ->
                        begin match cs.take() with
                          | Some ('A' .. 'F' | 'a' .. 'f' | '0' .. '9' as c2) ->
                              let s =
                                String.init 3
                                  (function
                                    | 0 -> c0
                                    | 1 -> c1
                                    | 2 -> c2
                                    | _ -> assert false)
                              in
                              Buffer.add_char b (char_of_int(int_of_string s))
                          | Some _ | None -> parse_error ~msg:"Error when parsing a string." location
                        end
                    | Some _ | None -> parse_error ~msg:"Error when parsing a string." location
                end
            | Some c -> Buffer.add_char b c
            | None -> ()
          end;
          loop()
      | Some c ->
          Buffer.add_char b c;
          loop()
  in
    loop()


(* In case one would want to use the standard OCaml Stream module. *)
let stream_of_charstream (cs:charstream) : char Stream.t =
  Stream.from (fun _ -> cs.take())


let append cs1 cs2 =
  let current_is_cs1 = ref true in
    {
      take  =
        (fun () ->
           if !current_is_cs1 then
             match cs1.take() with
               | (Some _) as c ->
                   c
               | None ->
                   current_is_cs1 := false;
                   cs2.take()
           else
             cs2.take()
        );
      push   = (fun c -> if !current_is_cs1 then cs1.push c else cs2.push c);
      insert = (fun c -> if !current_is_cs1 then cs1.insert c else cs2.insert c);
      where  = (fun () -> if !current_is_cs1 then cs1.where() else cs2.where())
    }

let delete_trailing_spaces s =
  if s = "" then
    s
  else
    let l = ref (String.length s) in
      while
        (match s.[!l - 1] with
          | '\n' | '\t' | ' ' | '\r' -> true
          | _ -> false)
      do
        decr l
      done;
      if !l = String.length s then
        s
      else
        String.sub s 0 !l



let string_of_charstream ?(keepcs=false) c =
  let x = c.where() in
  let b = Buffer.create 42 in
  let rec loop () =
    match c.take () with
      | None -> Buffer.contents b
      | Some c -> Buffer.add_char b c; loop()
  in
  let res = loop() in
    match keepcs with
      | true ->
          c.insert (charstream_of_string ~location:x res);
          res
      | false ->
          res
