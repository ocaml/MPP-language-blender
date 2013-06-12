(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : CeCILL-B                                                  *)
(* http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html         *)
(***********************************************************************)

let last_cond = ref true
let last_cond_exists = ref false

let elze s cs out =
  if !last_cond_exists then
    begin
      last_cond_exists := false;
      if !last_cond then
        ()
      else
        begin
          output_string out s;
          output_char out '\n';
          Mpp_charstream.output_charstream out cs
        end
    end
  else
    begin
      Mpp_charstream.parse_error ~msg:"`else' without a matching previous `if'."
        (cs.Mpp_charstream.where());
      Pervasives.exit 1
    end
