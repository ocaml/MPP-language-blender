(* OCaml to HTML ************************************************** *
 * (c) 2007, Philippe Wang **************************************** *
 * Licence: any version of GNU GPL or CeCILL-B, choose one that fits
 * best your needs and legislation. ******************************* *)

let _ =
  let input = ref "/dev/stdin" in
  let output = ref "/dev/stdout" in
    Arg.parse
      (Arg.align 
          [
            "-i", Arg.Set_string input, "f.ml Treat f.ml as input file (default is /dev/stdin)" ;
            "-o", Arg.Set_string output, "f.html Treat f.html as output file (default is /dev/stdout)";
          ])
      (fun s -> Printf.eprintf "I don't know what to do with <%s>.\n%!" s)
      (Printf.sprintf "Usage: %s [-i f.ml] [-o f.html]" Sys.argv.(0));
    let ic = open_in !input 
    and oc = open_out !output in
      Ocamltohtml_lexer.ic := ic;
      Ocamltohtml_lexer.oc := oc;
      Printf.fprintf oc  "<?xml version='1.0' encoding='utf-8'?>
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
        Printf.fprintf oc "</pre></body></html>";
        (try close_in ic with _ -> ());
        exit 0
