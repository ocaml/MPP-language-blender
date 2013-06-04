(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)


let html_of_ocaml filename cs out =
  let ic = open_in filename in
    Ocamltohtml_lexer.oc := out;
    Printf.fprintf out "<pre class='ocaml'>\n";
    try
      let lexbuf = Lexing.from_channel ic in
        while true do
          ignore (Ocamltohtml_lexer.token lexbuf)
        done
    with Ocamltohtml_lexer.Eof ->
      Printf.fprintf out "</pre>";
      (try close_in ic with _ -> ())

let html_of_ocaml_default_css _ _ out =
  Printf.fprintf out "<style type='text/css'>/* <!-- */
.ocaml{background-color:#EEE}
.kwd{color: green;}
.kwd1{color: red;}
.kwd2{color: blue;}
.str{color: navy;}
.mname{color: orange;}
.com1{color: violet;}
.com2{color: fuchsia;}
.error{background-color:pink;}
/* --> */</style>\n%!"

let html_of_ocamlc args _cs out =
  let tmp = Filename.temp_file (* ~temp_dir:"/tmp" *) "tmp" "plop" in
  let tmp1 = Filename.temp_file (* ~temp_dir:"/tmp" *) "tmp" "plop" in
  match Sys.command (args ^ " > " ^ tmp1 ^ " 2> " ^ tmp) with
    | 2 ->
        let open Mpp_charstream in
(*         let () = Mpp_actions.cat out tmp in *)
        let cs = charstream_of_inchannel tmp (open_in tmp) in
        let _ = read_until '"' cs in
        let _ = charstream_take_n 1 cs in
        let filename = read_until '"' cs in
        let _ = charstream_take_n 8 cs in
        let line = int_of_string (read_until ',' cs) in
        let _ = charstream_take_n 13 cs in
        let colstart = int_of_string (read_until '-' cs) in
        let _ = charstream_take_n 1 cs in
        let colend = int_of_string (read_until ':' cs) in
        let fcs = charstream_of_inchannel filename (open_in filename) in
        let () = 
          for i = 2 to line do
            ignore(read_until '\n' fcs)
          done;
          ignore(fcs.take())
        in
        let s = charstream_take_n (colstart) (fcs) in
        let () = Printf.fprintf out "<pre class='ocaml'>" in
        let () = Ocamltohtml_lexer.oc := out in
        let () = Ocamltohtml_lexer.html_escape s in
        let es = charstream_take_n (colend - colstart + 1) fcs in
        let el = read_until '\n' fcs in
        let lexbuf1 = Lexing.from_string (es) in
        let lexbuf2 = Lexing.from_string (el) in
        let () =
          Printf.fprintf out "<span class='error'>";
          try while true do ignore(Ocamltohtml_lexer.token lexbuf1) done
          with Ocamltohtml_lexer.Eof -> 
            Printf.fprintf out "</span>";
            try while true do ignore(Ocamltohtml_lexer.token lexbuf2) done
            with Ocamltohtml_lexer.Eof -> ()
        in
        let () =
          ignore(cs.take());
          output_charstream out cs;
          Printf.fprintf out "</pre>\n"
        in ()
    | 0 -> ()
    | n -> 
        Printf.fprintf out "<pre class='error'>Command %s returned with code %d, I don't know what to do with it. Here's the output:\n" args n;
        Mpp_actions.cat out tmp1;
        Printf.fprintf out "</span>"

exception Break
exception Next

let fragment args cs out =
  let open Mpp_charstream in
  let open Mpp_init in
  let location = cs.where() in
  let s = Mpp_charstream.append (charstream_of_string args) cs in
  let args = () and cs = () in let () = ignore args; ignore cs in
  let filename = ref "/dev/stdin" in
  let fr = ref "" in
  let fc = ref min_int in
  let fl = ref min_int in
  let tr = ref "" in
  let tc = ref 0 in
  let tl = ref 0 in
  let cc = ref 0 in
  let cl = ref 0 in
  let rec parse_args () =
    eat space_chars s;
    match s.take() with
      | Some '-' -> 
          begin
            match read_until_one_of space_chars s with
              | "fl" | "fromline" ->
                  eat space_chars s;
                  fl := parse_int s
              | "fc" | "fromchar" ->
                  eat space_chars s;
                  fc := parse_int s
              | "tl" | "toline" ->
                  eat space_chars s;
                  tl := parse_int s
              | "tc" | "tochar" ->
                  eat space_chars s;
                  tc := parse_int s
              | "cc" | "countchar" ->
                  eat space_chars s;
                  cc := parse_int s
              | "cl" | "countline" ->
                  eat space_chars s;
                  cl := parse_int s
              | "fr" | "fromregexp" ->
                  eat space_chars s;
                  begin match s.take() with
                    | Some '"' ->
                        fr := parse_a_string s
                    | Some c ->
                        s.push c;
                        fr := read_until_one_of space_chars s
                    | None ->
                        parse_error ~msg:"Wrong usage of builtin frag (1)." location;
                        exit 1
                  end
              | "tr" | "toregexp" ->
                  eat space_chars s;
                  begin match s.take() with
                    | Some '"' ->
                        tr := parse_a_string s
                    | Some c ->
                        s.push c;
                        tr := read_until_one_of space_chars s
                    | None ->
                        parse_error ~msg:"Wrong usage of builtin frag (2)." location;
                        exit 1
                  end
              | wrong ->
                  parse_error
                    ~msg:(Printf.sprintf "Unknown option -%s in usage of builtin frag." 
                            (String.escaped wrong))
                    location;
                  exit 1
          end;
          parse_args()
      | None -> ()
      | Some '"' ->
          filename := parse_a_string s
      | Some c ->
          s.push c;
          filename := read_until_one_of ~failsafe:true space_chars s

  in
  let () = parse_args() in
  let fcs = charstream_of_inchannel !filename (open_in !filename) in
  let line_count () = match fcs.where() with _, l, _ -> l in
  let char_count = ref 0 in
  let input_char cs =
    match cs.take() with
      | (Some c) as r -> incr char_count; r
      | None -> None
  in
  let input_line cs =
    let b = Buffer.create 100 in
    let rec loop() =
      match input_char cs with
        | Some '\n' -> Buffer.contents b
        | Some c -> Buffer.add_char b c; loop()
        | None -> 
            match Buffer.contents b with
              | "" -> raise End_of_file
              | l -> l
    in loop()
  in
    (* First part: eat unwanted characters. *)
    try
      (* Regexp start? *)
      begin match !fr with
        | "" -> ()
        | x -> (* Read until regexp is matched *)
            let r = Str.regexp x in
              try while true do
                let l = input_line fcs in
                  if Str.string_match r l 0 then
                    raise Next
              done with End_of_file -> raise Next
      end;
      (* Line start? *)
      begin
        while line_count() < !fl do
          ignore(input_line fcs)
        done;
        if !fl > 0 then raise Next;
      end;
      begin
        while !char_count < !fc do
          ignore(input_char fcs)
        done
      end;
      raise Next
    with Next ->
      (* Second part: output wanted characters. *)
      try
        begin match !tr with
          | "" -> ()
          | x ->
              let r = Str.regexp x in
                while true do
                  let l = input_line fcs in
                    if Str.string_match r l 0 then
                      raise Break
                    else
                      (output_string out l; output_char out '\n')
                done;
                raise Break
        end;
        if !cc > 0 then
          begin
            while !cc > 0 do
              output_char out (match input_char fcs with Some c -> c | None -> raise End_of_file);
              decr cc
            done;
            raise Break
          end;
        if !cl > 0 then begin
          while !cl > 0 do
            output_string out (input_line fcs);
            output_char out '\n';
            decr cl
          done;
          raise Break
        end;
        begin
          for i = !char_count to !tc do
            output_char out (match input_char fcs with Some c -> c | None -> raise End_of_file)
          done;
          if !tc > 0 then raise Break
        end;
        begin
          for _x = line_count() to !tl do
            output_string out (input_line fcs);
            output_char out '\n'
          done;
          if !tl > 0 then raise Break
        end
      with End_of_file|Break ->
        flush out        
;;

let fragment args cs out =
  try fragment args cs out
  with e ->
    Printf.eprintf "Exception bizarre %s\n%!" (Printexc.to_string e);
    assert false

(* Register the functions to the builtin set. *)
let () =
  Mpp_actions.register "ocamlhtmlcss" (Mpp_actions.Function html_of_ocaml_default_css) "Prints a CSS for ocamlhtml.";
  Mpp_actions.register "ocamlhtml" (Mpp_actions.Function html_of_ocaml) "OCaml syntax highlighting (outputs HTML).";
  Mpp_actions.register "ocamlcerror" (Mpp_actions.Function html_of_ocamlc) "Execute an ocamlc or ocamlopt command, and process (i.e. turn it into HTML) the output error message if any.";
  Mpp_actions.register "frag" (Mpp_actions.Function fragment) "Input a fragment of a file. Options: One of [-fr -fl -fc] to respectively specifiy a regular expression (OCaml module Str flavour), a line or a character number, from where the input should start. One of [-tr -tl -tc -cc -cl] to respectively specify a regular expression, a line, a character number, a character count or a line count. Example: [frag -fr \".*Hello.*\" -cc 42] will output 42 characters after the first line that matches the \".*Hello.*\" regular expression. (Matching lines are indeed excluded.)";
  ()
