(* OCaml to HTML ************************************************** *
 * (c) 2007, Philippe Wang **************************************** *
 * Licence: any version of GNU GPL or CeCILL-B, choose one that fits
 * best your needs and legislation. ******************************* *)

let html_of_ocaml filename out =
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

let html_of_ocaml_default_css out =
  Printf.fprintf out "<style type='text/css'>/* <!-- */
.ocaml{background-color:#EEE}

.c {
  color: #408080;
  font-style: italic;
}
/* Comment */
.err {
  border: 1px solid #ff0000;
}
/* Error */
.k {
  color: #008000;
  font-weight: bold;
}
/* Keyword */
.o {
  color: #666666;
}
/* Operator */
.cm {
  color: #408080;
  font-style: italic;
}
/* Comment.Multiline */
.cp {
  color: #bc7a00;
}
/* Comment.Preproc */
.c1 {
  color: #408080;
  font-style: italic;
}
/* Comment.Single */
.cs {
  color: #408080;
  font-style: italic;
}
/* Comment.Special */


.kwd{color: green;}
.kwd1{color: red;}
.kwd2{color: blue;}
.str{color: navy;}
.mname{color: orange;}
.com1{color: violet;}
.com2{color: fuchsia;}
.error{background-color:pink;}
/* --> */</style>\n%!"

let html_of_ocamlc args out =
  let tmp = Filename.temp_file (* ~temp_dir:"/tmp" *) "tmp" "plop" in
  let tmp1 = Filename.temp_file (* ~temp_dir:"/tmp" *) "tmp" "plop" in
  match Sys.command (args ^ " > " ^ tmp1 ^ " 2> " ^ tmp) with
    | 2 ->
        let open Mpp_charstream in
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
        Mpp_out.cat (Mpp_out.Out_channel out) tmp1;
        Printf.fprintf out "</span>"


let _ =
  let input = ref "/dev/stdin" in
  let output = ref "/dev/stdout" in
  let html_box = ref false in
    Arg.parse
      (Arg.align 
          [
            "-h", Arg.Set(html_box), "Output HTML with header and footer.";
            "-i", Arg.Set_string input, "f.ml Treat f.ml as input file (default is /dev/stdin)" ;
            "-o", Arg.Set_string output, "f.html Treat f.html as output file (default is /dev/stdout)";
          ])
      (fun s -> Printf.eprintf "I don't know what to do with <%s>.\n%!" s)
      (Printf.sprintf "Usage: %s [-i f.ml] [-o f.html]" Sys.argv.(0));
    let ic = open_in !input 
    and oc = open_out !output in
      Ocamltohtml_lexer.ic := ic;
      Ocamltohtml_lexer.oc := oc;
      if !html_box then Printf.fprintf oc  "<?xml version='1.0' encoding='utf-8'?>
<html>
<head>
<style type='text/css'>/* <!-- */
.kwd{color: green;}
.kwd1{color: red;}
.kwd2{color: blue;}
.str{color: navy;}
.mname{color: orange;}
.com1{color: violet;}
.com2{color: fuchsia;}
/* --> */</style>
</head>
<body><pre>
%!";
      try
        let lexbuf = Lexing.from_channel ic in
          while true do
            ignore (Ocamltohtml_lexer.token lexbuf)
          done
      with Ocamltohtml_lexer.Eof ->
        if !html_box then Printf.fprintf oc "</pre></body></html>";
        (try close_in ic with _ -> ());
        exit 0
