(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)

module Out = Mpp_out

let elze last_cond s cs out =
  match !last_cond with
    | None ->
        Mpp_charstream.parse_error ~msg:"`else' without a matching previous `if'."
          (cs.Mpp_charstream.where());
        Pervasives.exit 1
    | Some c ->
        last_cond := None;
        if c then
          ()
        else
          begin
            Out.output_string out s;
            Out.output_char out '\n';
            Out.output_charstream out cs
          end


 
