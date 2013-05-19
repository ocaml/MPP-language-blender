(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)


let html_of_ocaml filename cs out =
  let ic = open_in filename in
    Ocamltohtml_lexer.ic := ic;
    Ocamltohtml_lexer.oc := out;
    Printf.fprintf out "<pre class='ocaml'>";
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
.kwd{color: green;}
.kwd1{color: red;}
.kwd2{color: blue;}
.str{color: navy;}
.mname{color: orange;}
.com1{color: violet;}
.com2{color: fuchsia;}
/* --> */</style>\n%!"

let () =
  Mpp_actions.register "ocamlhtmlcss" (Mpp_actions.Function html_of_ocaml_default_css);
  Mpp_actions.register "ocamlhtml" (Mpp_actions.Function html_of_ocaml);
  ()
