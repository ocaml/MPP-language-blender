(***********************************************************************)
(* Meta Pre Processor, a language blender                              *)
(* (c) 2013 by Philippe Wang <philippe.wang@cl.cam.ac.uk>              *)
(* Licence : ISC                                                       *)
(* http://www.isc.org/downloads/software-support-policy/isc-license/   *)
(***********************************************************************)

module Out = Mpp_out

exception Break
exception Next

let fragment args cs (out:Out.t) =
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

