(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

let frag fl fc fr tl tc tr cl cc filename (out:string option) =
  match out with
    | None -> Libmpp_fragment.fragment fl fc fr tl tc tr cl cc filename (Mpp_out.Out_channel Pervasives.stdout)
    | Some f -> Libmpp_fragment.fragment fl fc fr tl tc tr cl cc filename (Mpp_out.Out_channel (Pervasives.open_out f))

(* open Core.Std *)

let command =
  Core.Std.Command.basic
    ~summary:"Extract a fragment from a file"
    ~readme:(fun () -> "This program extracts fragments from files. If you don't specify any options, it assumes you want the whole file. If you only specify where it starts, then it stops at the end of the file. If you only specify where it stops, it starts at the beginning. If you process very large files, please contact me (then I'll change the implementation so it can treat big files efficiently, as for the moment it seems useless).")
    Core.Std.Command.Spec.(
      empty
      +> flag "-fl" (optional int) ~doc:"line# Specify the line after which it starts to input"
      +> flag "-fc" (optional int) ~doc:"byte# Specify the byte number after which it starts to input"
      +> flag "-fr" (optional string) ~doc:"regexp Specify the regexp match after which it starts to input"
      +> flag "-tl" (optional int) ~doc:"line# Specify the line before which it stops"
      +> flag "-tc" (optional int) ~doc:"byte# Specify the byte number before which it stops"
      +> flag "-tr" (optional string) ~doc:"regexp Specify the regexp match before which it stops"
      +> flag "-cl" (optional int) ~doc:"#line Specify the number of lines after which it stops"
      +> flag "-cc" (optional int) ~doc:"#byte Specify the number of bytes after which it stops"
      +> flag "-o" (optional string) ~doc:"filename Specify the filename for the output"
      +> anon (maybe ("filename" %: string))
    )
    (fun (fl:int option) (fc:int option) (fr:string option) (tl:int option) 
       (tc:int option) (tr:string option) (cl:int option) (cc:int option) (filename:string option)
       (out:string option)
       () -> 
         frag fl fc fr tl tc tr cl cc filename out)

let () =
  Core.Std.Command.run ~version:"0.1" (* ~build_info:"fragment" *) command
