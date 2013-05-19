(* OCaml to HTML ************************************************** *
 * (c) 2007, Philippe Wang **************************************** *
 * Licence: any version of GNU GPL or CeCILL-B, choose one that fits
 * best your needs and legislation. ******************************* *)

let _ =
  print_endline "<?xml version='1.0' encoding='utf-8'?>
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
";
  let input =
    if Array.length Sys.argv = 2 || Array.length Sys.argv = 3 then
      open_in Sys.argv.(1)
    else
      stdin
  in
    try
      let lexbuf = Lexing.from_channel input in
        while true do
          ignore (Ocamltohtml_lexer.token lexbuf)
        done
    with Ocamltohtml_lexer.Eof ->
      print_endline "</pre></body></html>";
      (try close_in input with _ -> ());
      exit 0
